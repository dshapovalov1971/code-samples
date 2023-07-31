AWSTemplateFormatVersion: 2010-09-09
Transform:
  - set-common-parameters-macro
Resources:
  BounceProcessorLambdaFunction:
    Type: 'AWS::Lambda::Function'
    Properties:
      Role: !GetAtt 
        - RoleRole
        - Arn
      Code:
        ZipFile: |
          /*global fetch*/ 
          const 
              sm = new(require('@aws-sdk/client-secrets-manager').SecretsManager)({region: process.env.AWS_REGION}),
              ssm = new (require('@aws-sdk/client-ssm').SSM)({region: process.env.AWS_REGION});

          exports.handler = async event => (
            console.log(JSON.stringify(event, null, 2)),
            Promise.resolve(event.Records.map(m => [m.Sns.Message, JSON.parse(m.Sns.Message)]).flatMap(([r, m]) => 
                    m.notificationType === 'Complaint' && m.complaint?.complainedRecipients?.map(c => c.emailAddress)
                    || m.notificationType === 'Bounce' && r.search(/(spam|blocked)/i) < 0 && m.bounce?.bouncedRecipients?.map(c => c.emailAddress))
                .filter(e => e)
            ).then(e => e.length && Promise.all([
                    ssm.getParameter({Name: 'somi-rest-domain'}).then(p => p.Parameter.Value),
                    sm.getSecretValue({SecretId: 'somi/rest'}).then(s => JSON.parse(s.SecretString)),
                    e,
                ]).then(([hostname, secret, e]) => [hostname, `Basic ${Buffer.from(`${secret.username}:${secret.password}`).toString('base64')}`, e])
                .then(([hostname, a, e]) => Promise.all(e.map(e => fetch(`https://${hostname}/wp-json/wp/v2/users?context=edit&search=${decodeURIComponent(e).replace(/[&=?#]/g, '')}`, {
                        headers: { Authorization: a }  
                    }).then(r => r.json())
                    .then(users => users.length && fetch(`https://${hostname}/wp-json/wp/v2/users/${users[0].id}?force=true&reassign=`, {
                        headers: { Authorization: a },
                        method: 'DELETE',
                    })))))));
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 00f7d385-d871-472f-b055-faf1f5909860
    DependsOn:
      - BounceProcessorLambdaUnitTest
  RoleRole:
    Type: 'AWS::IAM::Role'
    Properties:
      AssumeRolePolicyDocument:
        Version: 2012-10-17
        Statement:
          - Effect: Allow
            Principal:
              Service: lambda.amazonaws.com
            Action: 'sts:AssumeRole'
      ManagedPolicyArns:
        - 'arn:aws:iam::aws:policy/SecretsManagerReadWrite'
        - 'arn:aws:iam::aws:policy/CloudWatchLogsFullAccess'
        - 'arn:aws:iam::aws:policy/AmazonSSMReadOnlyAccess'
        - 'arn:aws:iam::aws:policy/AWSXrayWriteOnlyAccess'
      Policies:
        - PolicyName: pinpoint-update-endpoint
          PolicyDocument:
            Version: 2012-10-17
            Statement:
              - Effect: Allow
                Action: 'mobiletargeting:UpdateEndpoint'
                Resource: '*'
    Metadata:
      'AWS::CloudFormation::Designer':
        id: d97d0534-36b1-4bcc-aed3-5366d9853e1d
  NewSignupProcessorLambdaFunction:
    Type: 'AWS::Lambda::Function'
    Properties:
      Role: !GetAtt 
        - RoleRole
        - Arn
      Code:
        ZipFile: !Sub |
          /*global fetch*/
          const
              pp = new(require('@aws-sdk/client-pinpoint').Pinpoint)(),
              sm = new(require('@aws-sdk/client-secrets-manager').SecretsManager)({ region: process.env.AWS_REGION }),
              ssm = new(require('@aws-sdk/client-ssm').SSM)({ region: process.env.AWS_REGION }),
              crypto = require('crypto');

          exports.handler = async event => (console.log(event), Promise.all([
                  ssm.getParameter({ Name: 'somi-rest-domain' }).then(p => p.Parameter.Value),
                  sm.getSecretValue({ SecretId: 'somi/rest' }).then(s => JSON.parse(s.SecretString)),
              ])).then(([hostname, secret]) => new Promise(r => {const i = setInterval(() => fetch(`https://${!hostname}/wp-json/wp/v2/users/${!event.ID}?context=edit`, {
                  headers: { 'Authorization': `Basic ${!Buffer.from(`${!secret.username}:${!secret.password}`).toString('base64')}` }
              }).then(r => r.json())
              .then(user => user.email && user.first_name && (user.meta["mepr-address-country"].length || user.meta["billing_country"].length) && (clearInterval(i),
                  r(pp.updateEndpoint({
                      ApplicationId: '${SOMIPinpointProject}',
                      EndpointId: crypto.createHash('md5').update(user.email.toLowerCase()).digest('hex'),
                      EndpointRequest: {
                          ChannelType: 'EMAIL',
                          Address: user.email.toLowerCase(),
                          User: {
                              UserAttributes: {
                                  FirstName: [user.first_name],
                                  LastName: [user.last_name],
                                  Registered: ['SOMI'],
                              }
                          },
                          Location: {
                              City: user.meta["mepr-address-city"].length ? user.meta["mepr-address-city"][0] : user.meta["billing_city"][0],
                              Country: user.meta["mepr-address-country"].length ? user.meta["mepr-address-country"][0] : user.meta["billing_country"][0],
                              PostalCode: user.meta["mepr-address-zip"].length ? user.meta["mepr-address-zip"][0] : user.meta["billing_postcode"][0],
                              Region: user.meta["mepr-address-state"].length ? user.meta["mepr-address-state"][0] : user.meta["billing_state"][0],
                          },
                      },
                  })))), 2000)}));
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 54bb6997-75b2-4129-a573-c2bb1583dba0
    DependsOn:
      - NewSignupProcessorLambdaUnitTest
  SOMIAPI:
    Type: 'AWS::ApiGateway::RestApi'
    Properties:
      Body: !Sub |
        openapi: "3.0.1"
        info:
          title: "SOMI"
        paths:
          /:
            post:
              responses:
                "200":
              x-amazon-apigateway-integration:
                type: "aws"
                httpMethod: "POST"
                uri: arn:aws:apigateway:${AWS::Region}:lambda:path/2015-03-31/functions/${NewSignupProcessorLambdaFunction.Arn}/invocations
                responses:
                  default:
                    statusCode: "200"
                requestParameters:
                  integration.request.header.X-Amz-Invocation-Type: "'Event'"
                requestTemplates:
                  application/x-www-form-urlencoded: |
                    {#foreach($kvPair in $input.path('$').split('&'))
                    #set($kvTokenised = $kvPair.split('='))
                    "$util.urlDecode($kvTokenised[0])":"$util.urlDecode($kvTokenised[1])"#if($foreach.hasNext),#end
                    #end}
                passthroughBehavior: "never"
      Policy:
        Version: 2012-10-17
        Statement:
          - Effect: Deny
            Principal: '*'
            Action: 'execute-api:Invoke'
            Resource: !Join 
              - ''
              - - 'execute-api:/'
                - '*'
            Condition:
              NotIpAddress:
                'aws:SourceIp': !GetAtt 
                  - SOMIStaticIP
                  - IpAddress
          - Effect: Allow
            Principal: '*'
            Action: 'execute-api:Invoke'
            Resource: !Join 
              - ''
              - - 'execute-api:/'
                - '*'
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 3fcccfb7-8c5e-4d5c-ba75-176c17ea418e
    DependsOn:
      - NewSignupProcessorLambdaFunction
  ProductionSOMIAPIDeployment:
    Type: 'AWS::ApiGateway::Deployment'
    Properties:
      RestApiId: !Ref SOMIAPI
      StageName: production
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 19ad4858-735d-43c4-acbd-bdeb37aab3e5
  SOMIRTBucket:
    Type: 'AWS::S3::Bucket'
    Properties:
      BucketName: somi-rt
    Metadata:
      'AWS::CloudFormation::Designer':
        id: b65ae041-5a1e-4887-b5a7-86cef0849160
  BackupPlan:
    Type: 'AWS::Backup::BackupPlan'
    Properties:
      BackupPlan:
        BackupPlanName: backup-plan
        BackupPlanRule:
          - RuleName: continuous
            TargetBackupVault: Default
            EnableContinuousBackup: true
            Lifecycle:
              DeleteAfterDays: 14
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 9e85646e-345d-4b0b-8e0a-3691d15e261b
  BackupSelection:
    Type: 'AWS::Backup::BackupSelection'
    Properties:
      BackupPlanId: !Ref BackupPlan
      BackupSelection:
        SelectionName: backup-resources
        IamRoleArn: !GetAtt 
          - BackupRole
          - Arn
        Resources:
          - 'arn:aws:s3:::*'
    Metadata:
      'AWS::CloudFormation::Designer':
        id: b6826491-7d73-4b12-aedc-1cef83ba8d2c
    DependsOn:
      - SOMIRTBucket
  BackupRole:
    Type: 'AWS::IAM::Role'
    Properties:
      AssumeRolePolicyDocument:
        Version: 2012-10-17
        Statement:
          - Effect: Allow
            Principal:
              Service:
                - backup.amazonaws.com
                - scheduler.amazonaws.com
                - states.amazonaws.com
                - pinpoint.amazonaws.com
            Action: 'sts:AssumeRole'
      Policies:
        - PolicyName: CreateExportJob
          PolicyDocument:
            Version: 2012-10-17
            Statement:
              - Action:
                  - 'mobiletargeting:CreateExportJob'
                  - 'mobiletargeting:List*'
                  - 'mobiletargeting:Get*'
                  - 'iam:PassRole'
                Resource: '*'
                Effect: Allow
      ManagedPolicyArns:
        - 'arn:aws:iam::aws:policy/AWSBackupServiceRolePolicyForS3Backup'
        - 'arn:aws:iam::aws:policy/AWSStepFunctionsFullAccess'
        - 'arn:aws:iam::aws:policy/AmazonSESReadOnlyAccess'
        - 'arn:aws:iam::aws:policy/CloudWatchLogsFullAccess'
        - 'arn:aws:iam::aws:policy/AmazonS3FullAccess'
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 9b827bdb-d5e9-4dd0-99d3-fed369aff16e
  NewSignupProcessorLambdaUnitTest:
    Type: 'Custom::LambdaUnitTestCustomResource'
    Properties:
      ServiceToken: !Sub >-
        arn:aws:lambda:${AWS::Region}:${AWS::AccountId}:function:LambdaUnitTestCustomResource
      UnitTest: !Sub |
        const 
            { handler } = require('./index.js'),
            { mockClient } = require('aws-sdk-client-mock'),
            { PinpointClient, UpdateEndpointCommand } = require('@aws-sdk/client-pinpoint'),
            { SSMClient, GetParameterCommand } = require('@aws-sdk/client-ssm'),
            { SecretsManagerClient, GetSecretValueCommand } = require('@aws-sdk/client-secrets-manager'),

            PinpointClientMock = mockClient(PinpointClient),
            SSMClientMock = mockClient(SSMClient),
            SecretsManagerClientMock = mockClient(SecretsManagerClient),

            testEvent = { 'ID': 0 },
            testPinpointParams = [
                {
                    ApplicationId: '${SOMIPinpointProject}',
                    EndpointId: '723d42f66c080b02ce346ba59bd5b9ef',
                    EndpointRequest: {
                        Address: 'test@mailinator.com',
                        ChannelType: 'EMAIL',
                        Location: {
                            City: 'City',
                            Country: 'USA',
                            PostalCode: '01234',
                            Region: 'State'
                        }, 
                        User: {
                            UserAttributes: {
                                FirstName: ['FirstName'],
                                LastName: ['LastName'],
                                Registered: ['SOMI']
                            } 
                        } 
                    }
                },
                {
                    ApplicationId: '${SOMIPinpointProject}',
                    EndpointId: '723d42f66c080b02ce346ba59bd5b9ef',
                    EndpointRequest: {
                        Address: 'test@mailinator.com',
                        ChannelType: 'EMAIL',
                        Location: {
                            City: 'City1',
                            Country: 'Canada',
                            PostalCode: '43210',
                            Region: 'State1'
                        }, 
                        User: {
                            UserAttributes: {
                                FirstName: ['FirstName'],
                                LastName: ['LastName'],
                                Registered: ['SOMI']
                            } 
                        } 
                    }
                },
            ];

        require('aws-sdk-client-mock-jest');
        SSMClientMock.resolves({ Parameter: { Value: 'domain' } });
        SecretsManagerClientMock.resolves({
            SecretString: '{"username":"username", "password": "password"}',
        });
        PinpointClientMock.resolves({});
        global.fetch = jest.fn().mockReturnValueOnce(
            Promise.resolve({
                json: () => Promise.resolve({
                    email: '',
                }),
            })
        ).mockReturnValueOnce(
            Promise.resolve({
                json: () => Promise.resolve({
                    email: 'test@mailinator.com',
                    first_name: '',
                }),
            })
        ).mockReturnValueOnce(
            Promise.resolve({
                json: () => Promise.resolve({
                    email: 'test@mailinator.com',
                    first_name: 'FirstName',
                    meta: {
                        'mepr-address-country': [],
                        billing_country: [],
                    }
                }),
            })
        ).mockReturnValueOnce(
            Promise.resolve({
                json: () => Promise.resolve({
                    email: 'test@mailinator.com',
                    first_name: 'FirstName',
                    last_name: 'LastName',
                    meta: {
                        'mepr-address-country': [],
                        'mepr-address-city': [],
                        'mepr-address-zip': [],
                        'mepr-address-state': [],
                        billing_country: ['USA'],
                        billing_city: ['City'],
                        billing_postcode: ['01234'],
                        billing_state: ['State']
                    }
                }),
            })
        ).mockReturnValue(
            Promise.resolve({
                json: () => Promise.resolve({
                    email: 'test@mailinator.com',
                    first_name: 'FirstName',
                    last_name: 'LastName',
                    meta: {
                        'mepr-address-country': ['Canada'],
                        'mepr-address-city': ['City1'],
                        'mepr-address-zip': ['43210'],
                        'mepr-address-state': ['State1'],
                        billing_country: [],
                        billing_city: [],
                        billing_postcode: [],
                        billing_state: []
                    }
                }),
            })
        );
        const origSetInterval = global.setInterval;
        global.setInterval = f => origSetInterval(f, 0);

        test('main', async () => {
            await handler(testEvent);
            expect(SSMClientMock).toReceiveCommandWith(GetParameterCommand, { Name: 'somi-rest-domain' });
            expect(SecretsManagerClientMock).toReceiveCommandWith(GetSecretValueCommand, { SecretId: 'somi/rest' });
            expect(global.fetch).toBeCalledWith('https://domain/wp-json/wp/v2/users/0?context=edit', {headers: {Authorization: 'Basic dXNlcm5hbWU6cGFzc3dvcmQ='}});
            expect(PinpointClientMock).toReceiveCommandWith(UpdateEndpointCommand, testPinpointParams[0]);
            PinpointClientMock.reset();
            await handler(testEvent);
            expect(PinpointClientMock).toReceiveCommandWith(UpdateEndpointCommand, testPinpointParams[1]);
        });
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 0112c583-5666-47d0-9b42-453be1f209aa
  BounceProcessorLambdaUnitTest:
    Type: 'Custom::LambdaUnitTestCustomResource'
    Properties:
      ServiceToken: !Sub >-
        arn:aws:lambda:${AWS::Region}:${AWS::AccountId}:function:LambdaUnitTestCustomResource
      UnitTest: |
        const 
            { handler } = require('./index.js'),
            { mockClient } = require('aws-sdk-client-mock'),
            { SSMClient, GetParameterCommand } = require('@aws-sdk/client-ssm'),
            { SecretsManagerClient, GetSecretValueCommand } = require('@aws-sdk/client-secrets-manager'),

            SSMClientMock = mockClient(SSMClient),
            SecretsManagerClientMock = mockClient(SecretsManagerClient),

            testEvent = {
                Records: [
                    {
                        Sns: {
                            Message: JSON.stringify({
                                notificationType: 'Unknown',
                            })
                        }
                    },
                    {
                        Sns: {
                            Message: JSON.stringify({
                                notificationType: 'Bounce',
                                bounce: {
                                    bouncedRecipients: [{ emailAddress: 'spam@mailinator.com' }]
                                },
                                bounceType: 'Spam',
                            })
                        }
                    },
                    {
                        Sns: {
                            Message: JSON.stringify({
                                notificationType: 'Bounce',
                                bounce: {
                                    bouncedRecipients: [{ emailAddress: 'blocked@mailinator.com' }]
                                },
                                bounceType: 'Blocked',
                            })
                        }
                    },
                    {
                        Sns: {
                            Message: JSON.stringify({
                                notificationType: 'Complaint',
                                complaint: {
                                    complainedRecipients: [
                                        { emailAddress: 'com&p=l?a#int1@mailinator.com' },
                                        { emailAddress: 'complaint2@mailinator.com' },
                        ]
                                },
                            })
                        }
                    },
                    {
                        Sns: {
                            Message: JSON.stringify({
                                notificationType: 'Bounce',
                                bounce: {
                                    bouncedRecipients: [
                                        { emailAddress: 'bounced1@mailinator.com' },
                                        { emailAddress: 'bounced2@mailinator.com' },
                        ]
                                },
                            })
                        }
                    },
                ],
            };

        require('aws-sdk-client-mock-jest');
        SSMClientMock.resolves({ Parameter: { Value: 'domain' } });
        SecretsManagerClientMock.resolves({
            SecretString: '{"username": "username", "password": "password"}'
        });
        let cnt = 0;
        global.fetch = jest.fn().mockReturnValueOnce(
            Promise.resolve({
                json: () => Promise.resolve([]),
            })
        ).mockReturnValue(
            Promise.resolve({
                json: () => Promise.resolve([{ id: cnt++ }]),
            })
        );

        test('not to process', async () => {
            const t = JSON.parse(JSON.stringify(testEvent));
            t.Records = t.Records.slice(0, 3);
            await handler(t);
            expect(SSMClientMock).toReceiveCommandTimes(GetParameterCommand, 0);
            expect(SecretsManagerClientMock).toReceiveCommandTimes(GetSecretValueCommand, 0);
        });

        test('main', async () => {
            await handler(testEvent);
            expect(SSMClientMock).toReceiveCommandTimes(GetParameterCommand, 1);
            expect(SSMClientMock).toReceiveCommandWith(GetParameterCommand, { Name: 'somi-rest-domain' });
            expect(SecretsManagerClientMock).toReceiveCommandTimes(GetSecretValueCommand, 1);
            expect(SecretsManagerClientMock).toReceiveCommandWith(GetSecretValueCommand, { SecretId: 'somi/rest' });
            expect(global.fetch).toBeCalledTimes(7);
            ['complaint1', 'complaint2', 'bounced1', 'bounced2'].forEach(e => expect(global.fetch).toBeCalledWith(`https://domain/wp-json/wp/v2/users?context=edit&search=${e}@mailinator.com`, {
                headers: { Authorization: 'Basic dXNlcm5hbWU6cGFzc3dvcmQ=' },
            }));
            [0, 1, 2].forEach(e => expect(global.fetch).toBeCalledWith(`https://domain/wp-json/wp/v2/users/${e}?force=true&reassign=`, {
                headers: { Authorization: 'Basic dXNlcm5hbWU6cGFzc3dvcmQ=' },
                method: 'DELETE',
            }));
        });
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 4fc6eabf-3e2f-4c51-aaef-0cb016c5f5ec
  SOMILightsailInstance:
    Type: 'AWS::Lightsail::Instance'
    Properties:
      InstanceName: SOMI-2022-07-23
      AddOns:
        - AddOnType: AutoSnapshot
          Status: Enabled
          AutoSnapshotAddOnRequest:
            SnapshotTimeOfDay: '06:00'
      BlueprintId: wordpress
      BundleId: micro_2_0
    Metadata:
      'AWS::CloudFormation::Designer':
        id: ceb699f7-526a-4df4-b868-6434b7307b23
  SOMILightsailAlarm:
    Type: 'AWS::Lightsail::Alarm'
    Properties:
      AlarmName: burst-capacity-time-alarm
      ComparisonOperator: LessThanThreshold
      ContactProtocols:
        - Email
      DatapointsToAlarm: 2
      EvaluationPeriods: 2
      MetricName: BurstCapacityTime
      MonitoredResourceName: !Ref SOMILightsailInstance
      NotificationEnabled: true
      NotificationTriggers:
        - ALARM
      Threshold: 300
      TreatMissingData: notBreaching
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 1d53082d-14d5-4320-96e9-5cc383d69c56
  SOMIStaticIP:
    Type: 'AWS::Lightsail::StaticIp'
    Properties:
      AttachedTo: !Ref SOMILightsailInstance
      StaticIpName: SOMI-Static-IP
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 39df71ff-2946-4f01-8ada-d140ada598e8
  LightsailContact:
    Type: 'Custom::LightsailContactCustomResource'
    Properties:
      ServiceToken: !Sub >-
        arn:aws:lambda:${AWS::Region}:${AWS::AccountId}:function:LightsailContactCustomResource
      contactEndpoint: !Sub 'admin@${Domain}'
      protocol: Email
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 589fa0a8-ee3b-4932-a875-3e12a2d4b2b1
  SOMIHostedZone:
    Type: 'AWS::Route53::HostedZone'
    Properties:
      Name: !Ref Domain
    Metadata:
      'AWS::CloudFormation::Designer':
        id: e718a1f2-f540-49b6-b35e-3e9e7d0b1e26
  SOMIRecordSetGroup:
    Type: 'AWS::Route53::RecordSetGroup'
    Properties:
      HostedZoneId: !Ref SOMIHostedZone
      RecordSets:
        - Name: !Ref Domain
          Type: A
          TTL: 300
          ResourceRecords:
            - !GetAtt 
              - SOMIStaticIP
              - IpAddress
        - Name: !Sub 'www.${Domain}'
          Type: CNAME
          TTL: 300
          ResourceRecords:
            - !Ref Domain
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 28132846-b9d5-46a0-b902-7456a0bda5c2
  SOMIEmailIdentity:
    Type: 'AWS::SES::EmailIdentity'
    Properties:
      EmailIdentity: !Ref Domain
      DkimAttributes:
        SigningEnabled: true
      MailFromAttributes:
        MailFromDomain: !Sub 'mail.${Domain}'
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 777b648b-7b2f-4c43-91b5-48899f0e0d2c
  BackupSESSuppressionListSchedule:
    Type: 'AWS::Scheduler::Schedule'
    Properties:
      FlexibleTimeWindow:
        Mode: FLEXIBLE
        MaximumWindowInMinutes: 60
      ScheduleExpression: rate(1 days)
      Target:
        Arn: !Ref BackupSESSuppressionList
        RoleArn: !GetAtt 
          - BackupRole
          - Arn
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 16f5dbbb-e44b-4718-9eb2-3c5d6958a951
  BackupSESSuppressionList:
    Type: 'AWS::StepFunctions::StateMachine'
    Properties:
      StateMachineType: EXPRESS
      RoleArn: !GetAtt 
        - BackupRole
        - Arn
      LoggingConfiguration:
        Destinations:
          - BackupSESSuppressionList
        IncludeExecutionData: true
        Level: ERROR
      DefinitionString: !Sub |
        {
          "StartAt": "Get existing backup files",
          "States": {
            "Delete existing backup files": {
              "Next": "Errors deleting?",
              "Parameters": {
                "Bucket": "${SOMIRTBucket}",
                "Delete": {
                  "Objects.$": "$.Contents"
                }
              },
              "Resource": "arn:aws:states:::aws-sdk:s3:deleteObjects",
              "Type": "Task",
              "ResultPath": null
            },
            "Errors deleting?": {
              "Choices": [
                {
                  "IsPresent": true,
                  "Next": "Fail",
                  "Variable": "$.DeleteResult.Error"
                }
              ],
              "Default": "More files exist?",
              "Type": "Choice"
            },
            "Fail": {
              "Type": "Fail"
            },
            "Get existing backup files": {
              "Next": "No files?",
              "Parameters": {
                "Bucket": "${SOMIRTBucket}",
                "Prefix": "-backup-ses-suppression-list/"
              },
              "Resource": "arn:aws:states:::aws-sdk:s3:listObjectsV2",
              "Type": "Task"
            },
            "List suppressed destinations": {
              "Next": "Save backup file",
              "Parameters": {
                "NextToken.$": "$.NextToken"
              },
              "Resource": "arn:aws:states:::aws-sdk:sesv2:listSuppressedDestinations",
              "Type": "Task"
            },
            "Map response": {
              "ItemProcessor": {
                "ProcessorConfig": {
                  "Mode": "INLINE"
                },
                "StartAt": "Transform",
                "States": {
                  "Transform": {
                    "End": true,
                    "Parameters": {
                      "Key.$": "$.Key"
                    },
                    "Type": "Pass"
                  }
                }
              },
              "ItemsPath": "$.Contents",
              "Next": "Delete existing backup files",
              "ResultPath": "$.Contents",
              "Type": "Map"
            },
            "More destinations?": {
              "Choices": [
                {
                  "IsPresent": true,
                  "Next": "List suppressed destinations",
                  "Variable": "$.NextToken"
                }
              ],
              "Default": "Success",
              "Type": "Choice"
            },
            "More files exist?": {
              "Choices": [
                {
                  "BooleanEquals": true,
                  "Next": "Get existing backup files",
                  "Variable": "$.IsTruncated"
                }
              ],
              "Default": "Set empty NextToken",
              "Type": "Choice"
            },
            "No files?": {
              "Choices": [
                {
                  "Next": "Set empty NextToken",
                  "NumericEquals": 0,
                  "Variable": "$.KeyCount"
                }
              ],
              "Default": "Map response",
              "Type": "Choice"
            },
            "Save backup file": {
              "Next": "More destinations?",
              "Parameters": {
                "Body.$": "$.SuppressedDestinationSummaries",
                "Bucket": "${SOMIRTBucket}",
                "Key.$": "States.Format('-backup-ses-suppression-list/{}', States.UUID())"
              },
              "Resource": "arn:aws:states:::aws-sdk:s3:putObject",
              "Type": "Task",
              "ResultPath": null
            },
            "Set empty NextToken": {
              "Next": "List suppressed destinations",
              "Type": "Pass",
              "Result": {
                "NextToken": null
              }
            },
            "Success": {
              "Type": "Succeed"
            }
          }
        }      
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 7af3dbb6-1d21-4697-85aa-8ce94be044c4
  BouncesTopic:
    Type: 'AWS::SNS::Topic'
    Properties:
      TopicName: Bounces
      Subscription:
        - Endpoint: !GetAtt 
            - BounceProcessorLambdaFunction
            - Arn
          Protocol: lambda
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 26b2ff21-0946-43e4-bcaf-a27a220785ef
  ErrorsTopic:
    Type: 'AWS::SNS::Topic'
    Properties:
      TopicName: Errors
      Subscription:
        - Endpoint: !Sub 'admin@${Domain}'
          Protocol: email
    Metadata:
      'AWS::CloudFormation::Designer':
        id: e014b2ec-94c0-430c-8292-77c71e0de6fa
  SESFeedbackNotifications:
    Type: 'Custom::SESFeedbackNotificationsCustomResource'
    Properties:
      ServiceToken: !Sub >-
        arn:aws:lambda:${AWS::Region}:${AWS::AccountId}:function:SESFeedbackNotificationsCustomResource
      Notifications:
        - Identity: !Ref SOMIEmailIdentity
          NotificationType: Bounce
          SnsTopic: !Ref BouncesTopic
        - Identity: !Ref SOMIEmailIdentity
          NotificationType: Complaint
          SnsTopic: !Ref BouncesTopic
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 39599efa-7e37-4687-b7f4-5eda42118222
  BackupPinpoint:
    Type: 'AWS::StepFunctions::StateMachine'
    Properties:
      StateMachineType: EXPRESS
      RoleArn: !GetAtt 
        - BackupRole
        - Arn
      LoggingConfiguration:
        Destinations:
          - BackupPinpointEndpoints
        IncludeExecutionData: true
        Level: ERROR
      DefinitionString: !Sub |
        {
          "StartAt": "Get existing backup files",
          "States": {
            "Delete existing backup files": {
              "Next": "Errors deleting?",
              "Parameters": {
                "Bucket": "${SOMIRTBucket}",
                "Delete": {
                  "Objects.$": "$.Contents"
                }
              },
              "Resource": "arn:aws:states:::aws-sdk:s3:deleteObjects",
              "Type": "Task",
              "ResultPath": null
            },
            "Errors deleting?": {
              "Choices": [
                {
                  "IsPresent": true,
                  "Next": "Fail",
                  "Variable": "$.DeleteResult.Error"
                }
              ],
              "Default": "More files exist?",
              "Type": "Choice"
            },
            "Fail": {
              "Type": "Fail"
            },
            "Get existing backup files": {
              "Next": "No files?",
              "Parameters": {
                "Bucket": "${SOMIRTBucket}",
                "Prefix": "-backup-pinpoint/"
              },
              "Resource": "arn:aws:states:::aws-sdk:s3:listObjectsV2",
              "Type": "Task"
            },
            "Map response": {
              "ItemProcessor": {
                "ProcessorConfig": {
                  "Mode": "INLINE"
                },
                "StartAt": "Transform",
                "States": {
                  "Transform": {
                    "End": true,
                    "Parameters": {
                      "Key.$": "$.Key"
                    },
                    "Type": "Pass"
                  }
                }
              },
              "ItemsPath": "$.Contents",
              "Next": "Delete existing backup files",
              "ResultPath": "$.Contents",
              "Type": "Map"
            },
            "More files exist?": {
              "Choices": [
                {
                  "BooleanEquals": true,
                  "Next": "Get existing backup files",
                  "Variable": "$.IsTruncated"
                }
              ],
              "Default": "CreateExportJob",
              "Type": "Choice"
            },
            "No files?": {
              "Choices": [
                {
                  "Next": "CreateExportJob",
                  "NumericEquals": 0,
                  "Variable": "$.KeyCount"
                }
              ],
              "Default": "Map response",
              "Type": "Choice"
            },
            "CreateExportJob": {
              "Type": "Task",
              "Parameters": {
                "ApplicationId": "${SOMIPinpointProject}",
                "ExportJobRequest": {
                  "RoleArn": "${BackupRole.Arn}",
                  "S3UrlPrefix": "s3://${SOMIRTBucket}/-backup-pinpoint/"
                }
              },
              "Resource": "arn:aws:states:::aws-sdk:pinpoint:createExportJob",
              "Next": "ListTemplates"
            },
            "ListTemplates": {
              "Type": "Task",
              "Parameters": {},
              "Resource": "arn:aws:states:::aws-sdk:pinpoint:listTemplates",
              "Next": "Save templates"
            },
            "Save templates": {
              "Type": "Map",
              "ItemProcessor": {
                "ProcessorConfig": {
                  "Mode": "INLINE"
                },
                "StartAt": "GetEmailTemplate",
                "States": {
                  "GetEmailTemplate": {
                    "Type": "Task",
                    "Parameters": {
                      "TemplateName.$": "$.TemplateName"
                    },
                    "Resource": "arn:aws:states:::aws-sdk:pinpoint:getEmailTemplate",
                    "Next": "Save template",
                    "OutputPath": "$.EmailTemplateResponse"

                  },
                  "Save template": {
                    "Type": "Task",
                    "End": true,
                    "Parameters": {
                      "Body.$": "$",
                      "Bucket": "${SOMIRTBucket}",
                      "Key.$": "States.Format('-backup-pinpoint/email-template-{}', $.TemplateName)"
                    },
                    "Resource": "arn:aws:states:::aws-sdk:s3:putObject"
                  }
                }
              },
              "End": true,
              "ItemsPath": "$.TemplatesResponse.Item"
            }            
          }
        }
    Metadata:
      'AWS::CloudFormation::Designer':
        id: ea36563b-d8dd-4972-b5a3-1803bc10b7ac
  BackupPinpointSchedule:
    Type: 'AWS::Scheduler::Schedule'
    Properties:
      FlexibleTimeWindow:
        Mode: FLEXIBLE
        MaximumWindowInMinutes: 60
      ScheduleExpression: rate(1 days)
      Target:
        Arn: !Ref BackupPinpoint
        RoleArn: !GetAtt 
          - BackupRole
          - Arn
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 98480a46-2010-4808-8dab-3acdebf8a176
  SOMIPinpointProject:
    Type: 'AWS::Pinpoint::App'
    Properties:
      Name: SOMI
    Metadata:
      'AWS::CloudFormation::Designer':
        id: a6ae7ff3-c39a-47c1-928e-ca986724ebad
  SOMIPinpointEmailChannel:
    Type: 'AWS::Pinpoint::EmailChannel'
    Properties:
      ApplicationId: !Ref SOMIPinpointProject
      Enabled: true
      FromAddress: !Sub 'info@{Domain}'
      Identity: !Sub >-
        arn:aws:ses:${AWS::Region}:${AWS::AccountId}:identity/${SOMIEmailIdentity}
    Metadata:
      'AWS::CloudFormation::Designer':
        id: a733261f-1deb-4c2c-9283-935db8cdeea0
    DependsOn:
      - SOMIEmailIdentity
  SOMIPinpointAllSegment:
    Type: 'AWS::Pinpoint::Segment'
    Properties:
      Name: All
      ApplicationId: !Ref SOMIPinpointProject
      SegmentGroups:
        Include: ANY
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 47c3a681-7554-479c-9f6d-b4569bc11b44
Metadata:
  'AWS::CloudFormation::Designer':
    00f7d385-d871-472f-b055-faf1f5909860:
      size:
        width: 60
        height: 60
      position:
        x: 360
        'y': 180
      z: 1
      embeds: []
      dependson:
        - 4fc6eabf-3e2f-4c51-aaef-0cb016c5f5ec
    d97d0534-36b1-4bcc-aed3-5366d9853e1d:
      size:
        width: 60
        height: 60
      position:
        x: 300
        'y': 80
      z: 1
      embeds: []
      isassociatedwith:
        - f0a475fd-0f4a-459e-ae6f-d13fe6199d2c
    54bb6997-75b2-4129-a573-c2bb1583dba0:
      size:
        width: 60
        height: 60
      position:
        x: 240
        'y': 180
      z: 1
      embeds: []
      dependson:
        - 0112c583-5666-47d0-9b42-453be1f209aa
    3fcccfb7-8c5e-4d5c-ba75-176c17ea418e:
      size:
        width: 60
        height: 60
      position:
        x: 120
        'y': 90
      z: 0
      embeds: []
      dependson:
        - 54bb6997-75b2-4129-a573-c2bb1583dba0
    19ad4858-735d-43c4-acbd-bdeb37aab3e5:
      size:
        width: 60
        height: 60
      position:
        x: 120
        'y': 180
      z: 0
      embeds: []
    b65ae041-5a1e-4887-b5a7-86cef0849160:
      size:
        width: 60
        height: 60
      position:
        x: -20
        'y': 180
      z: 0
      embeds: []
    9e85646e-345d-4b0b-8e0a-3691d15e261b:
      size:
        width: 60
        height: 60
      position:
        x: -20
        'y': 90
      z: 0
      embeds: []
    b6826491-7d73-4b12-aedc-1cef83ba8d2c:
      size:
        width: 60
        height: 60
      position:
        x: -120
        'y': 90
      z: 0
      embeds: []
      dependson:
        - b65ae041-5a1e-4887-b5a7-86cef0849160
    9b827bdb-d5e9-4dd0-99d3-fed369aff16e:
      size:
        width: 60
        height: 60
      position:
        x: -120
        'y': 180
      z: 1
      embeds: []
      isassociatedwith:
        - f0a475fd-0f4a-459e-ae6f-d13fe6199d2c
    0112c583-5666-47d0-9b42-453be1f209aa:
      size:
        width: 60
        height: 60
      position:
        x: 240
        'y': 280
      z: 0
      embeds: []
    4fc6eabf-3e2f-4c51-aaef-0cb016c5f5ec:
      size:
        width: 60
        height: 60
      position:
        x: 360
        'y': 280
      z: 0
      embeds: []
    ceb699f7-526a-4df4-b868-6434b7307b23:
      size:
        width: 60
        height: 60
      position:
        x: -120
        'y': 280
      z: 0
      embeds: []
    1d53082d-14d5-4320-96e9-5cc383d69c56:
      size:
        width: 60
        height: 60
      position:
        x: -20
        'y': 280
      z: 0
      embeds: []
    39df71ff-2946-4f01-8ada-d140ada598e8:
      size:
        width: 60
        height: 60
      position:
        x: -120
        'y': 370
      z: 0
      embeds: []
    589fa0a8-ee3b-4932-a875-3e12a2d4b2b1:
      size:
        width: 60
        height: 60
      position:
        x: -20
        'y': 370
      z: 0
      embeds: []
    e718a1f2-f540-49b6-b35e-3e9e7d0b1e26:
      size:
        width: 140
        height: 140
      position:
        x: 80
        'y': 280
      z: 0
      embeds: []
    28132846-b9d5-46a0-b902-7456a0bda5c2:
      size:
        width: 60
        height: 60
      position:
        x: 240
        'y': 370
      z: 0
      embeds: []
    777b648b-7b2f-4c43-91b5-48899f0e0d2c:
      size:
        width: 60
        height: 60
      position:
        x: 360
        'y': 370
      z: 0
      embeds: []
    16f5dbbb-e44b-4718-9eb2-3c5d6958a951:
      size:
        width: 60
        height: 60
      position:
        x: -220
        'y': 90
      z: 0
      embeds: []
    7af3dbb6-1d21-4697-85aa-8ce94be044c4:
      size:
        width: 60
        height: 60
      position:
        x: -220
        'y': 180
      z: 0
      embeds: []
    26b2ff21-0946-43e4-bcaf-a27a220785ef:
      size:
        width: 60
        height: 60
      position:
        x: 470
        'y': 280
      z: 0
      embeds: []
    e014b2ec-94c0-430c-8292-77c71e0de6fa:
      size:
        width: 60
        height: 60
      position:
        x: 470
        'y': 180
      z: 0
      embeds: []
    39599efa-7e37-4687-b7f4-5eda42118222:
      size:
        width: 60
        height: 60
      position:
        x: 470
        'y': 370
      z: 0
      embeds: []
    ea36563b-d8dd-4972-b5a3-1803bc10b7ac:
      size:
        width: 60
        height: 60
      position:
        x: -320
        'y': 180
      z: 0
      embeds: []
    98480a46-2010-4808-8dab-3acdebf8a176:
      size:
        width: 60
        height: 60
      position:
        x: -320
        'y': 90
      z: 0
      embeds: []
    a6ae7ff3-c39a-47c1-928e-ca986724ebad:
      size:
        width: 60
        height: 60
      position:
        x: -320
        'y': 280
      z: 0
      embeds: []
    a733261f-1deb-4c2c-9283-935db8cdeea0:
      size:
        width: 60
        height: 60
      position:
        x: -320
        'y': 370
      z: 0
      embeds: []
      dependson:
        - 777b648b-7b2f-4c43-91b5-48899f0e0d2c
    47c3a681-7554-479c-9f6d-b4569bc11b44:
      size:
        width: 60
        height: 60
      position:
        x: -220
        'y': 370
      z: 0
      embeds: []
    845fbf79-3596-4471-b5e2-e97f0cef7a6b:
      source:
        id: a733261f-1deb-4c2c-9283-935db8cdeea0
      target:
        id: 777b648b-7b2f-4c43-91b5-48899f0e0d2c
      z: 11
Parameters:
  Domain:
    Type: String
  AdditionalDomains:
    Type: CommaDelimitedList
  DNSRecords:
    Type: CommaDelimitedList
