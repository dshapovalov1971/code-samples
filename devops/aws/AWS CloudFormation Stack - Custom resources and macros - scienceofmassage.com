Metadata:
  'AWS::CloudFormation::Designer':
    47606fee-a0d0-452f-aadf-901438d62e02:
      size:
        width: 60
        height: 60
      position:
        x: -30
        'y': 160
      z: 1
      embeds: []
    53fbd7f2-fd33-48bb-a7a7-b8902eaa78f2:
      size:
        width: 60
        height: 60
      position:
        x: -30
        'y': 80
      z: 1
      embeds: []
    66cad9df-59d0-4a35-9465-c5f681e0895c:
      size:
        width: 60
        height: 60
      position:
        x: 70
        'y': 80
      z: 1
      embeds: []
    d94bb57b-cc76-4713-92b5-e0e6c54dd883:
      size:
        width: 60
        height: 60
      position:
        x: -150
        'y': 240
      z: 0
      embeds: []
    e40b5f53-2939-4fb8-8c5a-02b9f5acc341:
      size:
        width: 60
        height: 60
      position:
        x: -30
        'y': 240
      z: 1
      embeds: []
    9e059280-c7a6-49fb-9419-4a44d1a4adba:
      size:
        width: 60
        height: 60
      position:
        x: -150
        'y': 160
      z: 1
      embeds: []
      dependson:
        - d94bb57b-cc76-4713-92b5-e0e6c54dd883
    66baf6e2-8c01-4a18-a178-0ae491796658:
      size:
        width: 60
        height: 60
      position:
        x: -150
        'y': 80
      z: 1
      embeds: []
    296d5aba-f02b-49c3-aab5-7d5fcef234c6:
      size:
        width: 60
        height: 60
      position:
        x: -250
        'y': 80
      z: 1
      embeds: []
    52f9cd96-e161-4ff3-b49d-e0749bed198e:
      size:
        width: 60
        height: 60
      position:
        x: -250
        'y': 160
      z: 1
      embeds: []
AWSTemplateFormatVersion: 2010-09-09
Parameters:
  TemplatesBucketName:
    Type: String
    MinLength: 1
Mappings:
  Global:
    Lambda:
      'Fn::Transform':
        Name: 'AWS::Include'
        Parameters:
          Location: !Sub 's3://${TemplatesBucketName}/lambda-runtime'
Resources:
  MacrosLambdaRole:
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
        - 'arn:aws:iam::aws:policy/AmazonS3FullAccess'
        - 'arn:aws:iam::aws:policy/CloudWatchLogsFullAccess'
        - 'arn:aws:iam::aws:policy/AWSCloudFormationFullAccess'
        - 'arn:aws:iam::aws:policy/AWSLambda_FullAccess'
        - 'arn:aws:iam::aws:policy/AWSCodeBuildDeveloperAccess'
        - 'arn:aws:iam::aws:policy/AmazonRoute53DomainsFullAccess'
        - 'arn:aws:iam::aws:policy/AmazonSESFullAccess'
      Policies:
        - PolicyName: policy
          PolicyDocument:
            Version: 2012-10-17
            Statement:
              - Effect: Allow
                Action: 'lightsail:*'
                Resource: '*'
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 47606fee-a0d0-452f-aadf-901438d62e02
  SetCommonParametersFunction:
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 53fbd7f2-fd33-48bb-a7a7-b8902eaa78f2
    Type: 'AWS::Lambda::Function'
    Properties:
      Role: !GetAtt 
        - MacrosLambdaRole
        - Arn
      Timeout: 30
      Handler: index.handler
      Runtime: !FindInMap 
        - Global
        - Lambda
        - Runtime
      Code:
        ZipFile: !Sub |
          /*global fetch, Response*/ 
          const 
              s3 = new (require('@aws-sdk/client-s3').S3)({region: process.env.AWS_REGION}),
              cf = new (require('@aws-sdk/client-cloudformation').CloudFormation)({region: process.env.AWS_REGION});

          exports.handler = async event => Promise.all([
              Promise.all([
                      fetch('https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html').then(r => r.text()).then(body => {
                          const a = body.match(/(?<=>)nodejs\d+\.[^<]*(?=<)/g),
                                lv = Math.max(...a.map(e => Number(e.replace('nodejs', '').split('.')[0])));
                          return a.find(e => e.startsWith(`nodejs${!lv}.`));
                      }),
                      s3.getObject({
                          Bucket: '${TemplatesBucketName}',
                          Key: 'lambda-runtime',
                      }).then(r => new Response(r.Body).text()),
                  ]).then(([lambdaRuntime, savedRuntime]) => Promise.all([
                      lambdaRuntime,
                      savedRuntime === `Runtime: ${!lambdaRuntime}` || s3.putObject({
                          Bucket: '${TemplatesBucketName}',
                          Key: 'lambda-runtime',
                          Body: `Runtime: ${!lambdaRuntime}`,
                      }).then(r => cf.updateStack({
                          StackName: 'devops',
                          TemplateURL: 'https://${TemplatesBucketName}.s3.amazonaws.com/devops',
                          Parameters: [{
                              ParameterKey: 'TemplatesBucketName',
                              UsePreviousValue: true,
                          }],
                          Capabilities: ['CAPABILITY_IAM', 'CAPABILITY_AUTO_EXPAND'],
                      }))
                  ])).then(([lambdaRuntime, r]) => lambdaRuntime),
              fetch('https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Lambda-Insights-extension-versionsx86-64.html').then(r => r.text()).then(body => {
                      const a = body.match(/(?<=>)arn:aws:lambda:${AWS::Region}:\d+:layer:LambdaInsightsExtension:\d+(?=<)/g),
                            lv = Math.max(...a.map(e => Number(e.substring(e.lastIndexOf(':') + 1))));
                      return a.find(e => e.endsWith(`:${!lv}`));
                  }),
          ]).then(([lambdaRuntime, lambdaInsightsExtensionLayer]) => {
              event.templateParameterValues.DNSRecords?.map(r => r.split('#')).map(r => ({
                  Type: r[0],
                  Name: `${!r[1] === '@' ? '' : `${!r[1]}.`}${!event.templateParameterValues.Domain}`,
                  TTL: 300,
                  ResourceRecords: r[2].split('\\n'),
              })).forEach(r => event.fragment.Resources.SOMIRecordSetGroup.Properties.RecordSets.push(r));
              event.templateParameterValues.AdditionalDomains?.map(d => [d, d.split('.').map(e => `${!e.charAt(0).toUpperCase()}${!e.slice(1)}`).join('')]).forEach(([d, n]) => {
                  event.fragment.Resources[`SOMICertificateCustom${!n}`] = {
                      Type: 'AWS::CertificateManager::Certificate',
                      Properties: {
                          DomainName: d,
                          SubjectAlternativeNames: [`www.${!d}`],
                      },
                  };
                  event.fragment.Resources.SOMICloudFrontFunction ??= {
                      Type: 'AWS::CloudFront::Function',
                      Properties: {
                          AutoPublish: true,
                          FunctionCode: `
                                function handler(event) {
                                    return {
                                        statusCode: 301,
                                        statusDescription: 'Found',
                                        headers: {
                                            location: { value: 'https://www.${!event.templateParameterValues.Domain}' }
                                        },
                                    };
                                }        
                          `,
                          FunctionConfig: { Comment: '', Runtime: 'cloudfront-js-1.0'},
                          Name: 'redirect',
                      },
                  };
                  event.fragment.Resources[`SOMICloudFrontDistribution${!n}`] = {
                      Type: 'AWS::CloudFront::Distribution',
                      Properties: { DistributionConfig: {
                          Aliases: [d, `www.${!d}`],
                          DefaultCacheBehavior: {
                              TargetOriginId: event.templateParameterValues.Domain,
                              ViewerProtocolPolicy: 'redirect-to-https',
                              CachePolicyId: '658327ea-f89d-4fab-a63d-7e88639e58f6',
                              FunctionAssociations: [{ EventType: 'viewer-request', FunctionARN: { 'Fn::GetAtt': ['SOMICloudFrontFunction', 'FunctionARN'] }}],
                          },
                          Origins: [{
                              Id: event.templateParameterValues.Domain,
                              DomainName: event.templateParameterValues.Domain,
                              CustomOriginConfig: {
                                  OriginProtocolPolicy: 'https-only',
                                  OriginSSLProtocols: ['TLSv1.2'],
                              },
                          }],
                          Enabled: true,
                          PriceClass: 'PriceClass_100',
                          ViewerCertificate: {
                              AcmCertificateArn: { Ref: `SOMICertificateCustom${!n}` },
                              SslSupportMethod: 'sni-only',
                              MinimumProtocolVersion: 'TLSv1.2_2021',
                          },
                          HttpVersion: 'http2and3',
                      }},
                  };
                  event.fragment.Resources[`${!n}HostedZone`] = {
                      Type: 'AWS::Route53::HostedZone',
                      Properties: { Name: d },
                  };
                  event.fragment.Resources[`${!n}RecordSetGroup`] = {
                      Type: 'AWS::Route53::RecordSetGroup',
                      Properties: {
                          HostedZoneId: { Ref: `${!n}HostedZone`},
                          RecordSets: [{
                              Name: d,
                              Type: 'A',
                              AliasTarget: {
                                  DNSName: { 'Fn::GetAtt': [`SOMICloudFrontDistribution${!n}`, 'DomainName']},
                                  HostedZoneId: 'Z2FDTNDATAQYW2',
                              }
                          },{
                              Name: `www.${!d}`,
                              Type: 'A',
                              AliasTarget: {
                                  DNSName: { 'Fn::GetAtt': [`SOMICloudFrontDistribution${!n}`, 'DomainName']},
                                  HostedZoneId: 'Z2FDTNDATAQYW2',
                              }
                          }],
                      },
                  };
              });
              Object.entries(event.fragment.Resources).forEach(([k,v]) => {
                  switch (v.Type) {
                      case 'AWS::Lambda::Function':
                          v.Properties.Handler = 'index.handler';
                          v.Properties.TracingConfig = { Mode: 'Active' };
                          v.Properties.Runtime = lambdaRuntime;
                          (v.Properties.Layers ??= []).push(lambdaInsightsExtensionLayer);
                          v.Properties.Timeout = 30;
                          v.DependsOn?.filter(e => event.fragment.Resources[e].Type === 'Custom::LambdaUnitTestCustomResource')
                              .forEach(e => event.fragment.Resources[e].Properties.FunctionCode = v.Properties.Code.ZipFile);
                          break;
                      case 'AWS::ApiGateway::Deployment':
                          v.Properties.StageDescription = {
                              TracingEnabled: true,
                              MetricsEnabled: true,
                              DataTraceEnabled: true,
                              LoggingLevel: 'INFO',
                          };
                          break;
                      case 'AWS::S3::Bucket':
                          v.Properties.VersioningConfiguration = {Status: 'Enabled'};
                          v.Properties.LifecycleConfiguration = {Rules: [{
                              Id: 'delete-old-versions-after-14-days',
                              Status: 'Enabled',
                              NoncurrentVersionExpiration: {NoncurrentDays: 14},
                          }]};
                          break;
                      case 'Custom::LambdaUnitTestCustomResource':
                          v.Properties.LambdaRuntime = lambdaRuntime;
                          break;
                      case 'AWS::Route53::HostedZone':
                          event.fragment.Resources[`${!k}DomainNameServers`] = {
                              Type: 'Custom::DomainNameServersCustomResource',
                              Properties: {
                                ServiceToken: { 'Fn::Sub': 'arn:aws:lambda:${AWS::Region}:${AWS::AccountId}:function:DomainNameServersCustomResource' },
                                DomainName: v.Properties.Name,
                                Nameservers: { 'Fn::GetAtt': [k, 'NameServers'] },
                              },
                          };
                          break;
                      case 'AWS::StepFunctions::StateMachine':
                          event.fragment.Resources[`${!k}LogGroup`] = {
                              Type: 'AWS::Logs::LogGroup',
                              Properties: { LogGroupName: k },
                          };
                          v.Properties.LoggingConfiguration = {
                              Level: 'ERROR',
                              IncludeExecutionData: true,
                              Destinations: [{ CloudWatchLogsLogGroup: { LogGroupArn: { 'Fn::GetAtt': [`${!k}LogGroup`, 'Arn'] } } }],
                          };
                          break;
                  }
              });
              return {
                  requestId: event.requestId,
                  status: 'success',
                  fragment: event.fragment,
              };
          });
  SetCommonParametersMacro:
    Type: 'AWS::CloudFormation::Macro'
    Properties:
      Name: set-common-parameters-macro
      FunctionName: !Ref SetCommonParametersFunction
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 66cad9df-59d0-4a35-9465-c5f681e0895c
  LambdaTestProject:
    Type: 'AWS::CodeBuild::Project'
    Properties:
      Source:
        Type: NO_SOURCE
        BuildSpec: |
          version: 0.2
          phases:
            pre_build:
              commands:
                - npm i aws-sdk-client-mock aws-sdk-client-mock-jest
                - echo "$LAMBDA_CODE" > index.js
                - echo "$LAMBDA_UNITTEST" > index.test.js
                - |
                  cat > package.json << EOF
                    {
                      "jest": {
                        "collectCoverage": true,
                        "coverageThreshold": {
                          "global": {
                            "branches": 100,
                            "functions": 100,
                            "lines": 100,
                            "statements": 100
                          }
                        }
                      }
                    }
                  EOF
            build:
              commands:
                - npx -yes jest
            post_build:
              commands:
                - '[ -z RESPONSE_URL ] || curl -X PUT -d "{\"Status\": $([ $CODEBUILD_BUILD_SUCCEEDING -eq 1 ] && echo \"SUCCESS\" || echo \"FAILED\", \"Reason\": \"FAILED\"), ${RESPONSE_OBJECT:1}" $RESPONSE_URL'
          reports:
            coverage:
              base-directory: coverage
              files: '**/*'
              file-format: CLOVERXML
      Artifacts:
        Type: NO_ARTIFACTS
      ServiceRole: !GetAtt 
        - CodeBuildRole
        - Arn
      Environment:
        Type: ARM_CONTAINER
        ComputeType: BUILD_GENERAL1_SMALL
        Image: !Sub 
          - 'public.ecr.aws/sam/build-${Runtime}:latest'
          - Runtime: !FindInMap 
              - Global
              - Lambda
              - Runtime
        EnvironmentVariables:
          - Name: LAMBDA_CODE
            Value: ''
          - Name: LAMBDA_UNITTEST
            Value: ''
          - Name: RESPONSE_OBJECT
            Value: ''
          - Name: RESPONSE_URL
            Value: ''
    Metadata:
      'AWS::CloudFormation::Designer':
        id: d94bb57b-cc76-4713-92b5-e0e6c54dd883
  LambdaUnitTestCustomResource:
    Type: 'AWS::Lambda::Function'
    Properties:
      FunctionName: LambdaUnitTestCustomResource
      Role: !GetAtt 
        - MacrosLambdaRole
        - Arn
      Timeout: 600
      Handler: index.handler
      Runtime: !FindInMap 
        - Global
        - Lambda
        - Runtime
      Code:
        ZipFile: !Sub |
          /*global fetch*/ 
          const 
              cb = new (require('@aws-sdk/client-codebuild').CodeBuild)({region: process.env.AWS_REGION});

          exports.handler = async event =>
              Promise.resolve((console.log(event), {
                  PhysicalResourceId: event.PhysicalResourceId ?? event.LogicalResourceId,
                  StackId: event.StackId,
                  RequestId: event.RequestId,
                  LogicalResourceId: event.LogicalResourceId,
              })).then(ro => event.RequestType === 'Delete'
                  ? fetch(event.ResponseURL, {
                        method: 'PUT',
                        body: JSON.stringify({...ro, Status: 'SUCCESS'}),
                    })
                  : cb.startBuild({
                        projectName: '${LambdaTestProject}',
                        environmentVariablesOverride: [
                            {name: 'LAMBDA_CODE', value: event.ResourceProperties.FunctionCode},
                            {name: 'LAMBDA_UNITTEST', value: event.ResourceProperties.UnitTest},
                            {name: 'RESPONSE_OBJECT', value: JSON.stringify(ro)},
                            {name: 'RESPONSE_URL', value: event.ResponseURL},
                        ],
                    })
              );
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 9e059280-c7a6-49fb-9419-4a44d1a4adba
    DependsOn:
      - LambdaTestProject
  CodeBuildRole:
    Type: 'AWS::IAM::Role'
    Properties:
      AssumeRolePolicyDocument:
        Version: 2012-10-17
        Statement:
          - Effect: Allow
            Principal:
              Service: codebuild.amazonaws.com
            Action: 'sts:AssumeRole'
      ManagedPolicyArns:
        - 'arn:aws:iam::aws:policy/AWSCodeBuildAdminAccess'
        - 'arn:aws:iam::aws:policy/CloudWatchLogsFullAccess'
    Metadata:
      'AWS::CloudFormation::Designer':
        id: e40b5f53-2939-4fb8-8c5a-02b9f5acc341
  LightsailContactCustomResource:
    Type: 'AWS::Lambda::Function'
    Properties:
      FunctionName: LightsailContactCustomResource
      Role: !GetAtt 
        - MacrosLambdaRole
        - Arn
      Handler: index.handler
      Runtime: !FindInMap 
        - Global
        - Lambda
        - Runtime
      Code:
        ZipFile: !Sub |
          /*global fetch*/ 
          const 
              ls = new (require('@aws-sdk/client-lightsail').Lightsail)({region: process.env.AWS_REGION});

          exports.handler = async event => Promise.resolve((console.log(event),
              ['Delete', 'Update'].includes(event.RequestType) && ls.deleteContactMethod(event.OldResourceProperties ?? event.ResourceProperties)))
              .then(r => ['Create', 'Update'].includes(event.RequestType) && ls.createContactMethod(event.ResourceProperties))
              .then(() => 'SUCCESS', () => 'FAILED').then(r => fetch(event.ResponseURL, {
                  method: 'PUT',
                  body: JSON.stringify({
                      Status: r,
                      Reason: r,
                      PhysicalResourceId: event.PhysicalResourceId ?? event.LogicalResourceId,
                      StackId: event.StackId,
                      RequestId: event.RequestId,
                      LogicalResourceId: event.LogicalResourceId,
                  }),
              }));
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 66baf6e2-8c01-4a18-a178-0ae491796658
  DomainNameServersCustomResource:
    Type: 'AWS::Lambda::Function'
    Properties:
      FunctionName: DomainNameServersCustomResource
      Role: !GetAtt 
        - MacrosLambdaRole
        - Arn
      Handler: index.handler
      Runtime: !FindInMap 
        - Global
        - Lambda
        - Runtime
      Code:
        ZipFile: !Sub |
          /*global fetch*/ 
          const 
              r53d = new (require('@aws-sdk/client-route-53-domains').Route53Domains)({region: process.env.AWS_REGION});

          exports.handler = async event => Promise.resolve((console.log(event),
              ['Create', 'Update'].includes(event.RequestType) && r53d.updateDomainNameservers({
                  DomainName: event.ResourceProperties.DomainName,
                  Nameservers: event.ResourceProperties.Nameservers.map(ns => ({ Name: ns })),
              })))
              .then(() => 'SUCCESS', () => 'FAILED').then(r => fetch(event.ResponseURL, {
                  method: 'PUT',
                  body: JSON.stringify({
                      Status: r,
                      Reason: r,
                      PhysicalResourceId: event.PhysicalResourceId ?? event.LogicalResourceId,
                      StackId: event.StackId,
                      RequestId: event.RequestId,
                      LogicalResourceId: event.LogicalResourceId,
                  }),
              }));
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 296d5aba-f02b-49c3-aab5-7d5fcef234c6
  SESFeedbackNotificationsCustomResource:
    Type: 'AWS::Lambda::Function'
    Properties:
      FunctionName: SESFeedbackNotificationsCustomResource
      Role: !GetAtt 
        - MacrosLambdaRole
        - Arn
      Handler: index.handler
      Runtime: !FindInMap 
        - Global
        - Lambda
        - Runtime
      Code:
        ZipFile: !Sub |
          /*global fetch*/ 
          const 
              ses = new (require('@aws-sdk/client-ses').SES)({region: process.env.AWS_REGION});

          exports.handler = async event => (console.log(event),
              Promise.all(event.ResourceProperties.Notifications.map(n =>
                  ses.setIdentityNotificationTopic({...n, ...(event.RequestType === 'Delete' && { SnSTopic: null })}))))
              .then(() => 'SUCCESS', () => 'FAILED').then(r => fetch(event.ResponseURL, {
                  method: 'PUT',
                  body: JSON.stringify({
                      Status: r,
                      Reason: r,
                      PhysicalResourceId: event.PhysicalResourceId ?? event.LogicalResourceId,
                      StackId: event.StackId,
                      RequestId: event.RequestId,
                      LogicalResourceId: event.LogicalResourceId,
                  }),
              }));
    Metadata:
      'AWS::CloudFormation::Designer':
        id: 52f9cd96-e161-4ff3-b49d-e0749bed198e
