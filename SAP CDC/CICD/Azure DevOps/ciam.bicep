param location string = resourceGroup().location
param cdcApiKey string
param cdcDC string
@secure()
param cdcAppKey string
@secure()
param cdcAppSecret string


var nameStructure = split(resourceGroup().name, '-')
var resourcePrefix = '${nameStructure[0]}-${nameStructure[1]}'
var env = nameStructure[3]

resource configStore 'Microsoft.AppConfiguration/configurationStores@2023-03-01' = if (!contains(resourceGroup().tags, '${resourcePrefix}-CS-${env}-AppConfig')) {
  name: '${resourcePrefix}-CS-${env}-AppConfig'
  location: location
  sku: {
    name: 'standard'
  }
}

var keyValues = [
  [ 'e-approvers-list', 'application/json', '[{"a": "Syber Bearing System Designer","l": "sbsd-approvers-list@mailinator.com"},{"a": "P&A","l": "pa-approvers-list@mailinator.com"}]' ]
  [ 'e-google-analytics', 'application/json', '{"api_secret": "", "measurement_id": ""}' ]
  [ 'ciam-analytics-endpoint', 'application/json', '{"ciamAnalyticsApiEndpoint": "","SapCdcCiamSubscriptionKey": ""}']
  [ 'access-item-text', 'application/json', '[{"a": "Syber Bearing System Designer","l": "Syber Bearing System Designer tool"},{"a": "P&A","l": "relative pricing and availability information"},{"a": "_","l": "our Sample Online Account"}]' ]
  [ 'sap-cdc-data-center', 'string', 'us1.gigya.com' ]
  [ 'approved-roles', 'application/json', '[]' ]
  [ 'auto-approved-countries', 'application/json', '[]' ]
  [ 'competitors', 'application/json', '[]' ]
  [ 'embargoed-countries', 'application/json', '[]' ]
  [ 'generic-emails', 'application/json', '[]' ]
  [ 'approved-email-subject', 'application/json', '{"en": ""}' ]
  [ 'rejected-email-subject', 'application/json', '{"en": ""}' ]
  [ 'update-email-subject', 'application/json', '{"en": ""}' ]
  [ 'inactive-email-subject', 'application/json', '{"en": ""}' ]
]
resource configStoreKeyValue 'Microsoft.AppConfiguration/configurationStores/keyValues@2023-03-01' = [for (item, i) in keyValues: if (!contains(resourceGroup().tags, '${resourcePrefix}-CS-${env}-AppConfig')) {
  parent: configStore
  name: item[0]
  properties: {
    contentType: item[1]
    value: item[2]
  }
}]

resource keyVault 'Microsoft.KeyVault/vaults@2023-02-01' = if (!contains(resourceGroup().tags, '${resourcePrefix}-KV-${env}-KeyVault')) {
  name: '${resourcePrefix}-KV-${env}-KeyVault'
  location: location
  properties: {
    enabledForTemplateDeployment: true
    tenantId: tenant().tenantId
    accessPolicies: [ {
        tenantId: tenant().tenantId
        objectId: approvalWorkflow.identity.principalId
        permissions: {
          keys: [ 'all' ]
          secrets: [ 'all' ]
          certificates: [ 'all' ]
        }
      } ]
    sku: {
      name: 'standard'
      family: 'A'
    }
  }
}

var secrets = [
  'sap-cdc-app-key'
  'sap-cdc-app-secret'
]
resource keyVaultSecret 'Microsoft.KeyVault/vaults/secrets@2023-02-01' = [for (item, i) in secrets: if (!contains(resourceGroup().tags, '${resourcePrefix}-KV-${env}-KeyVault')) {
  parent: keyVault
  name: item
  properties: {
    value: ''
  }
}]

resource tag 'Microsoft.Resources/tags@2022-09-01' = {
  name: 'default'
  properties: {
    tags: {
      '${resourcePrefix}-CS-${env}-AppConfig': 'deployed'
      '${resourcePrefix}-KV-${env}-KeyVault': 'deployed'
    }
  }
}

resource logicAppOffice365Connector 'Microsoft.Web/connections@2016-06-01' = {
  name: '${resourcePrefix}-APIC-${env}-samplehelp-sample.com'
  location: location
  properties: {
    api: {
      id: subscriptionResourceId('Microsoft.Web/locations/managedApis', location, 'office365')
    }
  }
}

resource logicAppKeyVaultConnector 'Microsoft.Web/connections@2016-06-01' = {
  name: '${resourcePrefix}-APIC-${env}-keyvault'
  location: location
  properties: any({
    api: {
      id: subscriptionResourceId('Microsoft.Web/locations/managedApis', location, 'keyvault')
    }
    parameterValueSet: {
      name: 'oauthMI'
      values: {
        vaultName: {
          value: '${resourcePrefix}-KV-${env}-KeyVault'
        }
      }
    }
  })
}

resource logicAppStorageConnector 'Microsoft.Web/connections@2016-06-01' = {
  name: '${resourcePrefix}-APIC-${env}-azureblob'
  location: location
  properties: any({
    api: {
      id: subscriptionResourceId('Microsoft.Web/locations/managedApis', location, 'azureblob')
    }
    parameterValueSet: {
      name: 'managedIdentityAuth'
    }
  })
}

resource roleAssignmentAppConfigurationDataReaderApprovalWorkflow 'Microsoft.Authorization/roleAssignments@2022-04-01' = {// App Configuration Data Reader
  name: guid('approvalWorkflow', '516239f1-63e1-4d78-a4de-a74fb236a071', resourceGroup().id)
  properties: {
    roleDefinitionId: resourceId('microsoft.authorization/roleDefinitions', '516239f1-63e1-4d78-a4de-a74fb236a071')
    principalId: approvalWorkflow.identity.principalId
    principalType: 'ServicePrincipal'
  }
}

resource roleAssignmentAppConfigurationDataReaderEmailNotification 'Microsoft.Authorization/roleAssignments@2022-04-01' = {// App Configuration Data Reader
  name: guid('emailNotification', '516239f1-63e1-4d78-a4de-a74fb236a071', resourceGroup().id)
  properties: {
    roleDefinitionId: resourceId('microsoft.authorization/roleDefinitions', '516239f1-63e1-4d78-a4de-a74fb236a071')
    principalId: emailNotification.identity.principalId
    principalType: 'ServicePrincipal'
  }
}

resource roleAssignmentStorageEmailNotification 'Microsoft.Authorization/roleAssignments@2022-04-01' = {// Storage Blob Data Reader
  name: guid('emailNotification', '2a2b9908-6ea1-4ae2-8e65-a410df84e7d1', resourceGroup().id)
  properties: {
    roleDefinitionId: resourceId('microsoft.authorization/roleDefinitions', '2a2b9908-6ea1-4ae2-8e65-a410df84e7d1')
    principalId: emailNotification.identity.principalId
    principalType: 'ServicePrincipal'
  }
}

resource integrationAccount 'Microsoft.Logic/integrationAccounts@2019-05-01' = {
  name: '${resourcePrefix}-IA-${env}-IntegrationAccount'
  location: location
  sku: {
    name: 'Standard'
  }
  properties: {
    state: 'Enabled'
  }
}

resource storageAccount 'Microsoft.Storage/storageAccounts@2023-01-01' = {
  name: replace('${toLower(resourcePrefix)}-st-${toLower(env)}-storage', '-', '1')
  location: location
  kind: 'StorageV2'
  sku: {
    name: 'Standard_LRS'
  }
  properties: {
    accessTier: 'Hot'
  }
}

resource blobServices 'Microsoft.Storage/storageAccounts/blobServices@2023-01-01' = {
  parent: storageAccount
  name: 'default'
}

resource container 'Microsoft.Storage/storageAccounts/blobServices/containers@2023-01-01' = {
  parent: blobServices
  name: 'emailtemplates'
}

resource emailNotification 'Microsoft.Logic/workflows@2019-05-01' = {
  name: '${resourcePrefix}-LApp-${env}-EmailNotification'
  location: location
  identity: {
    type: 'SystemAssigned'
  }
  properties: {
    integrationAccount: {
      id: integrationAccount.id
    }
    definition: {
      '$schema': 'https://schema.management.azure.com/providers/Microsoft.Logic/schemas/2016-06-01/workflowdefinition.json#'
      actions: {
        Find_localized_email_template: {
          inputs: {
            from: '@body(\'Lists_email_templates\')?[\'value\']'
            where: '@equals(toLower(item()?[\'Name\']), toLower(concat(triggerBody()?[\'locale\'], \'.html\')))'
          }
          runAfter: {
            Lists_email_templates: [
              'Succeeded'
            ]
          }
          type: 'Query'
        }
        Get_email_subject: {
          inputs: {
            authentication: {
              audience: 'https://azconfig.io'
              type: 'ManagedServiceIdentity'
            }
            method: 'GET'
            uri: '${configStore.properties.endpoint}/kv/@{if(or(equals(triggerBody()?[\'subject\'], \'\'), equals(triggerBody()?[\'subject\'], null)), triggerBody()?[\'template\'], triggerBody()?[\'subject\'])}-subject?api-version=1.0'
          }
          runAfter: {}
          type: 'Http'
        }
        Get_email_template: {
          inputs: {
            host: {
              connection: {
                name: '@parameters(\'$connections\')[\'azureblob\'][\'connectionId\']'
              }
            }
            method: 'get'
            path: '/v2/datasets/@{encodeURIComponent(encodeURIComponent(\'${storageAccount.name}\'))}/GetFileContentByPath'
            queries: {
              inferContentType: true
              path: '/emailtemplates/@{triggerBody()?[\'template\']}/@{if(equals(length(body(\'Find_localized_email_template\')), 0), \'en\', triggerBody()?[\'locale\'])}.html'
              queryParametersSingleEncoded: true
            }
          }
          runAfter: {
            Find_localized_email_template: [
              'Succeeded'
            ]
          }
          type: 'ApiConnection'
        }
        Lists_email_templates: {
          inputs: {
            host: {
              connection: {
                name: '@parameters(\'$connections\')[\'azureblob\'][\'connectionId\']'
              }
            }
            method: 'get'
            path: '/v2/datasets/@{encodeURIComponent(encodeURIComponent(\'${storageAccount.name}\'))}/foldersV2/@{encodeURIComponent(encodeURIComponent(\'/emailtemplates/\',triggerBody()?[\'template\'],\'/\'))}'
            queries: {
              nextPageMarker: ''
              useFlatListing: false
            }
          }
          runAfter: {
            Set_email_subject: [
              'Succeeded'
            ]
          }
          type: 'ApiConnection'
        }
        Replace_placeholders: {
          inputs: {
            code: 'return workflowContext.actions.Get_email_template.outputs.body.replace(/___(.+)___/g, (_, n) => workflowContext.trigger.outputs.body[n]);'
          }
          runAfter: {
            Get_email_template: [
              'Succeeded'
            ]
          }
          type: 'JavaScriptCode'
        }
        Send_email: {
          inputs: {
            body: {
              Body: '<p>@{body(\'Replace_placeholders\')}</p>'
              Importance: 'Normal'
              Subject: '@{variables(\'email-subject\')[if(equals(variables(\'email-subject\')?[triggerBody()?[\'locale\']], null), \'en\', triggerBody()?[\'locale\'])]}'
              To: '@triggerBody()?[\'email\']'
            }
            host: {
              connection: {
                name: '@parameters(\'$connections\')[\'office365\'][\'connectionId\']'
              }
            }
            method: 'post'
            path: '/v2/Mail'
          }
          runAfter: {
            Replace_placeholders: [
              'Succeeded'
            ]
          }
          type: 'ApiConnection'
        }
        Set_email_subject: {
          inputs: {
            variables: [
              {
                name: 'email-subject'
                type: 'object'
                value: '@json(body(\'Get_email_subject\')?[\'value\'])'
              }
            ]
          }
          runAfter: {
            Get_email_subject: [
              'Succeeded'
            ]
          }
          type: 'InitializeVariable'
        }
      }
      contentVersion: '1.0.0.0'
      outputs: {}
      parameters: {
        '$connections': {
          defaultValue: {}
          type: 'Object'
        }
      }
      triggers: {
        manual: {
          inputs: {
            method: 'POST'
            schema: {
              properties: {
                accessItemText: {
                  type: 'string'
                }
                email: {
                  type: 'string'
                }
                firstName: {
                  type: 'string'
                }
                lastName: {
                  type: 'string'
                }
                locale: {
                  type: 'string'
                }
                subject: {
                  type: 'string'
                }
                template: {
                  type: 'string'
                }
              }
              type: 'object'
            }
          }
          kind: 'Http'
          operationOptions: 'EnableSchemaValidation'
          type: 'Request'
        }
      }
    }
    parameters: {
      '$connections': {
        value: {
          office365: {
            connectionId: logicAppOffice365Connector.id
            connectionName: logicAppOffice365Connector.name
            id: logicAppOffice365Connector.properties.api.id
          }
          azureblob: {
            connectionId: logicAppStorageConnector.id
            connectionName: logicAppStorageConnector.name
            connectionProperties: {
              authentication: {
                type: 'ManagedServiceIdentity'
              }
            }
            id: logicAppStorageConnector.properties.api.id
          }
        }
      }
    }
  }
}

resource approvalWorkflow 'Microsoft.Logic/workflows@2019-05-01' = {
  name: '${resourcePrefix}-LApp-${env}-ApprovalWorkflow'
  location: location
  identity: {
    type: 'SystemAssigned'
  }
  properties: {
    integrationAccount: {
      id: integrationAccount.id
    }
    definition: {
      '$schema': 'https://schema.management.azure.com/providers/Microsoft.Logic/schemas/2016-06-01/workflowdefinition.json#'
      actions: {
        CIAM_Event_Request_For_Approval: {
          inputs: {
            body: {
              AppData: {
                Permission: 'pending'
                Request: '@{join(body(\'New_access_filter\'), \',\')}'
              }
              EventType: 'request_for_approval'
              UserData: {
                CompanyRole: '@{variables(\'record\')[\'role\']}'
                EmailId: '@{variables(\'record\')[\'email\']}'
                FirstName: '@{variables(\'record\')[\'firstName\']}'
                LastName: '@{variables(\'record\')[\'lastName\']}'
                PhoneNumber: '@{variables(\'record\')[\'phone\']}'
                UID: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
              }
              WorkflowId: '@{workflow().run.name}'
            }
            headers: {
              'SapCdcCiam-Subscription-Key': '@{body(\'Convert_configuration\')[\'ciam-analytics-endpoint\'][\'SapCdcCiamSubscriptionKey\']}'
              'content-type': 'application/json'
            }
            method: 'POST'
            uri: '@{body(\'Convert_configuration\')[\'ciam-analytics-endpoint\'][\'ciamAnalyticsApiEndpoint\']}'
          }
          runAfter: {
            New_access_filter: [
              'Succeeded'
            ]
          }
          type: 'Http'
        }
        CIAM_Event_Workflow_Outcome: {
          inputs: {
            body: {
              AppData: {
                Permission: '@{if(or(variables(\'new_access\'), variables(\'registration\')), variables(\'result\'), \'no-new-access\')}'
                Request: '@{join(body(\'New_access_filter\'), \',\')}'
              }
              EventType: 'workflow_result'
              ProcessedBy: '@{body(\'Send_approval_email\')?[\'UserEmailAddress\']}'
              Remarks: '@{if(greater(length(intersection(body(\'Convert_configuration\')[\'embargoed-countries\'], array(variables(\'record\')[\'country\']))),0),\'embargoed country\',if(greater(length(intersection(body(\'Convert_configuration\')[\'competitors\'], array(toLower(split(variables(\'record\')[\'email\'], \'@\')[1])))),0),\'competitor\',if(equals(length(intersection(body(\'Convert_configuration\')[\'approved-roles\'], array(variables(\'record\')[\'role\']))),0),\'not approved role\',\'\')))}'
              UserData: {
                UID: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
              }
              WorkflowId: '@{workflow().run.name}'
            }
            headers: {
              'SapCdcCiam-Subscription-Key': '@{body(\'Convert_configuration\')[\'ciam-analytics-endpoint\'][\'SapCdcCiamSubscriptionKey\']}'
              'content-type': 'application/json'
            }
            method: 'POST'
            uri: '@{body(\'Convert_configuration\')[\'ciam-analytics-endpoint\'][\'ciamAnalyticsApiEndpoint\']}'
          }
          runAfter: {
            GA_Event_Workflow_Outcome1: [
              'Succeeded'
            ]
          }
          type: 'Http'
        }
        Check_result: {
          cases: {
            Approve: {
              actions: {
                Auto_approved: {
                  inputs: {
                    body: {
                      data: {
                        data: {
                          approved: true
                          lastApprovalStatus: '@{if(variables(\'new_access\'),\'approved\',\'no-new-access\')}'
                        }
                      }
                      status: 'ENRICH'
                    }
                    headers: {
                      'Content-Type': 'application/json; charset=utf-8'
                    }
                    statusCode: 200
                  }
                  kind: 'Http'
                  runAfter: {}
                  type: 'Response'
                }
              }
              case: 'approve'
            }
            Reject: {
              actions: {
                Auto_rejected: {
                  inputs: {
                    body: {
                      status: 'FAIL'
                    }
                    headers: {
                      'Content-Type': 'application/json; charset=utf-8'
                    }
                    statusCode: 200
                  }
                  kind: 'Http'
                  runAfter: {}
                  type: 'Response'
                }
              }
              case: 'reject'
            }
          }
          default: {
            actions: {
              Approval_emails: {
                actions: {
                  CIAM_Event_Approver_Email_Sent: {
                    inputs: {
                      body: {
                        AppData: {
                          Permission: 'pending'
                          Request: '@{join(body(\'New_access_filter\'), \',\')}'
                        }
                        EventType: 'approver_email_sent'
                        UserData: {
                          UID: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
                        }
                        WorkflowId: '@{workflow().run.name}'
                      }
                      headers: {
                        'SapCdcCiam-Subscription-Key': '@{body(\'Convert_configuration\')[\'ciam-analytics-endpoint\'][\'SapCdcCiamSubscriptionKey\']}'
                        'content-type': 'application/json'
                      }
                      method: 'POST'
                      uri: '@{body(\'Convert_configuration\')[\'ciam-analytics-endpoint\'][\'ciamAnalyticsApiEndpoint\']}'
                    }
                    runAfter: {
                      GA_Event_Approver_Email_Sent1: [
                        'Succeeded'
                      ]
                    }
                    type: 'Http'
                  }
                  GA_Event_Approver_Email_Sent: {
                    inputs: {
                      body: {
                        client_id: 'APPROVAL.WORKFLOW'
                        events: [
                          {
                            name: 'approver_email_sent'
                            params: '@body(\'Convert_configuration\')[\'e-google-analytics\'][\'parameters\']'
                          }
                        ]
                        user_id: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
                        user_properties: {
                          user_id_dimension: {
                            value: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
                          }
                        }
                      }
                      headers: {
                        'content-type': 'application/json'
                      }
                      method: 'POST'
                      queries: {
                        api_secret: '@{body(\'Convert_configuration\')[\'e-google-analytics\'][\'primary\'][\'api_secret\']}'
                        measurement_id: '@{body(\'Convert_configuration\')[\'e-google-analytics\'][\'primary\'][\'measurement_id\']}'
                      }
                      uri: 'https://www.google-analytics.com/mp/collect'
                    }
                    runAfter: {}
                    type: 'Http'
                  }
                  GA_Event_Approver_Email_Sent1: {
                    inputs: {
                      body: {
                        client_id: 'APPROVAL.WORKFLOW'
                        events: [
                          {
                            name: 'approver_email_sent'
                            params: '@body(\'Convert_configuration\')[\'e-google-analytics\'][\'parameters\']'
                          }
                        ]
                        user_id: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
                        user_properties: {
                          user_id_dimension: {
                            value: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
                          }
                        }
                      }
                      headers: {
                        'content-type': 'application/json'
                      }
                      method: 'POST'
                      queries: {
                        api_secret: '@{body(\'Convert_configuration\')[\'e-google-analytics\'][\'secondary\'][\'api_secret\']}'
                        measurement_id: '@{body(\'Convert_configuration\')[\'e-google-analytics\'][\'secondary\'][\'measurement_id\']}'
                      }
                      uri: 'https://www.google-analytics.com/mp/collect'
                    }
                    runAfter: {
                      GA_Event_Approver_Email_Sent: [
                        'Succeeded'
                      ]
                    }
                    type: 'Http'
                  }
                  Send_approval_email: {
                    inputs: {
                      body: {
                        Message: {
                          Body: 'First Name: @{variables(\'record\')[\'firstName\']}\n\nLast Name: @{variables(\'record\')[\'lastName\']}\n\nCompany: @{variables(\'record\')[\'company\']}\n\nIndustry: @{variables(\'record\')[\'industry\']}\n\nWebsite: @{variables(\'record\')[\'website\']}\n\nAddress: @{variables(\'record\')[\'address\']}\n\nCity: @{variables(\'record\')[\'city\']}\n\nState / Province: @{variables(\'record\')[\'state\']}\n\nPostal Code: @{variables(\'record\')[\'zip\']}\n\nCountry: @{variables(\'record\')[\'country\']}\n\nRole: @{variables(\'record\')[\'role\']}\n\nPhone: @{variables(\'record\')[\'phone\']}\n\nEmail: @{variables(\'record\')[\'email\']}\n\nCurrent access: @{variables(\'record\')[\'currentAccess\']}\n\nRequested access: @{join(body(\'New_access_filter\'), \',\')}\n'
                          HeaderText: '@{if(variables(\'registration\'), \'New registration\', \'Access request\')}'
                          HideHTMLMessage: true
                          Importance: 'Normal'
                          Options: 'Approve, Reject'
                          ShowHTMLConfirmationDialog: false
                          Subject: '@{if(equals(iterationIndexes(\'Approval_emails\'), 0), \'\', \'REMINDER: \')}@{concat(if(variables(\'registration\'), \'New Sample Registration Request Created: \', \'Sample Additional Access Requested: \'), join(body(\'New_access_filter\'), \',\'))} {{env-subject}}'
                          To: '@{body(\'Find_approvers_list\')}'
                          UseOnlyHTMLMessage: false
                        }
                        NotificationUrl: '@{listCallbackUrl()}'
                      }
                      host: {
                        connection: {
                          name: '@parameters(\'$connections\')[\'office365\'][\'connectionId\']'
                        }
                      }
                      path: '/approvalmail/$subscriptions'
                    }
                    limit: {
                      timeout: 'P1D'
                    }
                    runAfter: {
                      CIAM_Event_Approver_Email_Sent: [
                        'Succeeded'
                      ]
                    }
                    type: 'ApiConnectionWebhook'
                  }
                  Set_variable: {
                    inputs: {
                      name: 'result'
                      value: 'manual'
                    }
                    runAfter: {
                      Send_approval_email: [
                        'Succeeded'
                      ]
                    }
                    type: 'SetVariable'
                  }
                }
                expression: '@not(equals(body(\'Send_approval_email\')?[\'SelectedOption\'], null))'
                limit: {
                  count: 14
                }
                runAfter: {
                  Find_approvers_list: [
                    'Succeeded'
                  ]
                }
                type: 'Until'
              }
              Find_approvers_list: {
                inputs: {
                  code: 'const l = workflowContext.actions.Convert_configuration.outputs.body[\'e-approvers-list\'];\r\nreturn (l.find(e => workflowContext.trigger.outputs.body.data.params.data.accessRequest.split(\',\').includes(e.a)) || l.find(e => e.a === \'P&A\')).l;'
                }
                runAfter: {
                  Manual_approval: [
                    'Succeeded'
                  ]
                }
                type: 'JavaScriptCode'
              }
              If_approved: {
                actions: {
                  Approve_2: {
                    inputs: {
                      name: 'result'
                      value: 'approve'
                    }
                    runAfter: {}
                    type: 'SetVariable'
                  }
                }
                else: {
                  actions: {
                    Reject_3: {
                      inputs: {
                        name: 'result'
                        value: 'reject'
                      }
                      runAfter: {}
                      type: 'SetVariable'
                    }
                  }
                }
                expression: {
                  and: [
                    {
                      equals: [
                        '@body(\'Send_approval_email\')?[\'SelectedOption\']'
                        'Approve'
                      ]
                    }
                  ]
                }
                runAfter: {
                  Approval_emails: [
                    'Succeeded'
                  ]
                }
                type: 'If'
              }
              Manual_approval: {
                inputs: {
                  body: {
                    data: {
                      data: {
                        lastApprovalStatus: 'manual'
                      }
                    }
                    status: 'ENRICH'
                  }
                  headers: {
                    'Content-Type': 'application/json; charset=utf-8'
                  }
                  statusCode: 200
                }
                kind: 'Http'
                runAfter: {}
                type: 'Response'
              }
            }
          }
          expression: '@variables(\'result\')'
          runAfter: {
            Should_auto_reject: [
              'Succeeded'
            ]
          }
          type: 'Switch'
        }
        Compose_registration_variable_for_inline_code: {
          inputs: {
            locale: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'profile\']?[\'locale\']}'
            registration: '@variables(\'registration\')'
          }
          runAfter: {
            CIAM_Event_Workflow_Outcome: [
              'Succeeded'
            ]
          }
          type: 'Compose'
        }
        Convert_configuration: {
          inputs: {
            code: 'const r = {};\r\nworkflowContext.actions.Parse_configuration.outputs.body.items.forEach(i => r[i.key] = i.content_type === \'application/json\' ? JSON.parse(i.value) : i.value);\r\nreturn r;'
          }
          runAfter: {
            Parse_configuration: [
              'Succeeded'
            ]
          }
          type: 'JavaScriptCode'
        }
        Find_item_access_text: {
          inputs: {
            code: 'const locale = workflowContext.actions.Compose_registration_variable_for_inline_code.outputs.locale;\r\nlet l = workflowContext.actions.Convert_configuration.outputs.body[\'access-item-text\'];\r\nl = l[l[locale] ? locale : "en"];\r\nreturn (!workflowContext.actions.Compose_registration_variable_for_inline_code.outputs.registration && l.find(e => workflowContext.trigger.outputs.body.data.params.data.accessRequest.split(\',\').includes(e.a)) || l.find(e => e.a == \'_\')).l;'
          }
          runAfter: {
            Compose_registration_variable_for_inline_code: [
              'Succeeded'
            ]
          }
          type: 'JavaScriptCode'
        }
        GA_Event_Request_For_Approval: {
          inputs: {
            body: {
              client_id: 'APPROVAL.WORKFLOW'
              events: [
                {
                  name: 'request_for_approval'
                  params: '@body(\'Convert_configuration\')[\'e-google-analytics\'][\'parameters\']'
                }
              ]
              user_id: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
              user_properties: {
                user_id_dimension: {
                  value: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
                }
              }
            }
            headers: {
              'content-type': 'application/json'
            }
            method: 'POST'
            queries: {
              api_secret: '@{body(\'Convert_configuration\')[\'e-google-analytics\'][\'primary\'][\'api_secret\']}'
              measurement_id: '@{body(\'Convert_configuration\')[\'e-google-analytics\'][\'primary\'][\'measurement_id\']}'
            }
            uri: 'https://www.google-analytics.com/mp/collect'
          }
          runAfter: {
            Convert_configuration: [
              'Succeeded'
            ]
          }
          type: 'Http'
        }
        GA_Event_Request_For_Approval1: {
          inputs: {
            body: {
              client_id: 'APPROVAL.WORKFLOW'
              events: [
                {
                  name: 'request_for_approval'
                  params: '@body(\'Convert_configuration\')[\'e-google-analytics\'][\'parameters\']'
                }
              ]
              user_id: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
              user_properties: {
                user_id_dimension: {
                  value: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
                }
              }
            }
            headers: {
              'content-type': 'application/json'
            }
            method: 'POST'
            queries: {
              api_secret: '@{body(\'Convert_configuration\')[\'e-google-analytics\'][\'secondary\'][\'api_secret\']}'
              measurement_id: '@{body(\'Convert_configuration\')[\'e-google-analytics\'][\'secondary\'][\'measurement_id\']}'
            }
            uri: 'https://www.google-analytics.com/mp/collect'
          }
          runAfter: {
            Convert_configuration: [
              'Succeeded'
            ]
          }
          type: 'Http'
        }
        GA_Event_Workflow_Outcome: {
          inputs: {
            body: {
              client_id: 'APPROVAL.WORKFLOW'
              events: [
                {
                  name: 'workflow_result'
                  params: '@addProperty(body(\'Convert_configuration\')[\'e-google-analytics\'][\'parameters\'],\'dlv_details\',if(variables(\'new_access\'),variables(\'result\'),\'no-new-access\'))'
                }
              ]
              user_id: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
              user_properties: {
                user_id_dimension: {
                  value: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
                }
              }
            }
            headers: {
              'content-type': 'application/json'
            }
            method: 'POST'
            queries: {
              api_secret: '@{body(\'Convert_configuration\')[\'e-google-analytics\'][\'primary\'][\'api_secret\']}'
              measurement_id: '@{body(\'Convert_configuration\')[\'e-google-analytics\'][\'primary\'][\'measurement_id\']}'
            }
            uri: 'https://www.google-analytics.com/mp/collect'
          }
          runAfter: {
            Check_result: [
              'Succeeded'
            ]
          }
          type: 'Http'
        }
        GA_Event_Workflow_Outcome1: {
          inputs: {
            body: {
              client_id: 'APPROVAL.WORKFLOW'
              events: [
                {
                  name: 'workflow_result'
                  params: '@addProperty(body(\'Convert_configuration\')[\'e-google-analytics\'][\'parameters\'],\'dlv_details\',if(variables(\'new_access\'),variables(\'result\'),\'no-new-access\'))'
                }
              ]
              user_id: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
              user_properties: {
                user_id_dimension: {
                  value: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
                }
              }
            }
            headers: {
              'content-type': 'application/json'
            }
            method: 'POST'
            queries: {
              api_secret: '@{body(\'Convert_configuration\')[\'e-google-analytics\'][\'secondary\'][\'api_secret\']}'
              measurement_id: '@{body(\'Convert_configuration\')[\'e-google-analytics\'][\'secondary\'][\'measurement_id\']}'
            }
            uri: 'https://www.google-analytics.com/mp/collect'
          }
          runAfter: {
            GA_Event_Workflow_Outcome: [
              'Succeeded'
            ]
          }
          type: 'Http'
        }
        Get_CDC_app_key: {
          inputs: {
            host: {
              connection: {
                name: '@parameters(\'$connections\')[\'keyvault\'][\'connectionId\']'
              }
            }
            method: 'get'
            path: '/secrets/@{encodeURIComponent(\'sap-cdc-app-key\')}/value'
          }
          runAfter: {
            Not_applicable_update_or_registration: [
              'Succeeded'
            ]
          }
          runtimeConfiguration: {
            secureData: {
              properties: [
                'outputs'
              ]
            }
          }
          type: 'ApiConnection'
        }
        Get_CDC_app_secret: {
          inputs: {
            host: {
              connection: {
                name: '@parameters(\'$connections\')[\'keyvault\'][\'connectionId\']'
              }
            }
            method: 'get'
            path: '/secrets/@{encodeURIComponent(\'sap-cdc-app-secret\')}/value'
          }
          runAfter: {
            Not_applicable_update_or_registration: [
              'Succeeded'
            ]
          }
          runtimeConfiguration: {
            secureData: {
              properties: [
                'outputs'
              ]
            }
          }
          type: 'ApiConnection'
        }
        Get_runtime_configuration: {
          inputs: {
            authentication: {
              audience: 'https://azconfig.io'
              type: 'ManagedServiceIdentity'
            }
            method: 'GET'
            uri: '${configStore.properties.endpoint}/kv?api-version=1.0'
          }
          runAfter: {
            Not_applicable_update_or_registration: [
              'Succeeded'
            ]
          }
          type: 'Http'
        }
        Initialize_new_access: {
          inputs: {
            variables: [
              {
                name: 'new_access'
                type: 'boolean'
                value: '@not(equals(length(split(variables(\'record\')[\'currentAccess\'], \',\')), length(union(split(variables(\'record\')[\'currentAccess\'], \',\'), split(variables(\'record\')[\'accessRequest\'], \',\')))))'
              }
            ]
          }
          runAfter: {
            Is_registration_1: [
              'Succeeded'
            ]
          }
          type: 'InitializeVariable'
        }
        Initialize_record: {
          inputs: {
            variables: [
              {
                name: 'record'
                type: 'object'
              }
            ]
          }
          runAfter: {
            Initialize_result: [
              'Succeeded'
            ]
          }
          type: 'InitializeVariable'
        }
        Initialize_result: {
          inputs: {
            variables: [
              {
                name: 'result'
                type: 'string'
                value: 'manual'
              }
            ]
          }
          runAfter: {
            Not_applicable_update_or_registration: [
              'Succeeded'
            ]
          }
          type: 'InitializeVariable'
        }
        Is_approved_2: {
          actions: {
            If_need_to_assign_organization_member: {
              actions: {
                Assign_organization_member: {
                  inputs: {
                    body: 'apiKey=@{triggerBody()?[\'apiKey\']}&userKey=@{encodeUriComponent(body(\'Get_CDC_app_key\')?[\'value\'])}&secret=@{encodeUriComponent(body(\'Get_CDC_app_secret\')?[\'value\'])}&httpStatusCodes=true&UID=@{variables(\'record\')[\'uid\']}&bpid=@{variables(\'record\')[\'bpid\']}&job=@{encodeUriComponent(variables(\'record\')[\'role\'])}&status=active&roleNames=@{encodeUriComponent(if(and(greaterOrEquals(indexOf(variables(\'record\')[\'accessRequest\'], \'Syber Bearing System Designer\'), 0), less(indexOf(variables(\'record\')[\'accessRequest\'], \'P&A\'), 0)), concat(variables(\'record\')[\'accessRequest\'], \',P&A\'), variables(\'record\')[\'accessRequest\']))}'
                    headers: {
                      'content-type': 'application/x-www-form-urlencoded'
                    }
                    method: 'POST'
                    uri: 'https://accounts.@{body(\'Convert_configuration\')[\'sap-cdc-data-center\']}/accounts.b2b.setAccountOrganizationInfo'
                  }
                  runAfter: {}
                  type: 'Http'
                }
              }
              expression: {
                or: [
                  {
                    equals: [
                      '@variables(\'new_access\')'
                      true
                    ]
                  }
                  {
                    equals: [
                      '@variables(\'registration\')'
                      true
                    ]
                  }
                ]
              }
              runAfter: {}
              type: 'If'
            }
          }
          else: {
            actions: {
              Send_rejected_email: {
                inputs: {
                  body: {
                    accessItemText: '@{body(\'Find_item_access_text\')}'
                    email: '@{variables(\'record\')[\'email\']}'
                    firstName: '@{variables(\'record\')[\'firstName\']}'
                    lastName: '@{variables(\'record\')[\'lastName\']}'
                    locale: '@{variables(\'record\')[\'locale\']}'
                    subject: '@{if(variables(\'registration\'), null, \'update-email\')}'
                    template: 'rejected-email'
                  }
                  host: {
                    triggerName: 'manual'
                    workflow: {
                      id: emailNotification.id
                    }
                  }
                }
                operationOptions: 'DisableAsyncPattern'
                runAfter: {}
                type: 'Workflow'
              }
            }
          }
          expression: {
            and: [
              {
                equals: [
                  '@variables(\'result\')'
                  'approve'
                ]
              }
            ]
          }
          runAfter: {
            Is_registration_2: [
              'Succeeded'
            ]
          }
          type: 'If'
        }
        Is_registration_1: {
          actions: {
            Set_record_from_registration: {
              inputs: {
                name: 'record'
                value: {
                  accessRequest: '@{triggerBody()?[\'data\']?[\'params\']?[\'data\']?[\'accessRequest\']}'
                  address: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'address\']}'
                  city: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'city\']}'
                  company: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'work\']?[\'company\']}'
                  country: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'country\']}'
                  currentAccess: ''
                  email: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'profile\']?[\'email\']}'
                  firstName: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'firstName\']}'
                  industry: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'work\']?[\'industry\']}'
                  lastName: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'lastName\']}'
                  locale: '@{toLower(triggerBody()?[\'data\']?[\'params\']?[\'lang\'])}'
                  phone: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'phones\'][0]?[\'number\']}'
                  role: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'work\']?[\'title\']}'
                  state: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'state\']}'
                  website: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'work\']?[\'location\']}'
                  zip: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'zip\']}'
                }
              }
              runAfter: {}
              type: 'SetVariable'
            }
          }
          else: {
            actions: {
              Get_role_names: {
                inputs: {
                  from: '@body(\'Parse_"Get_user_roles"\')?[\'requestedPolicies\']'
                  select: '@item()?[\'policyName\']'
                }
                runAfter: {
                  'Parse_"Get_user_roles"': [
                    'Succeeded'
                  ]
                }
                type: 'Select'
              }
              Get_user_info: {
                inputs: {
                  body: 'apiKey=@{triggerBody()?[\'apiKey\']}&userKey=@{encodeUriComponent(body(\'Get_CDC_app_key\')?[\'value\'])}&secret=@{encodeUriComponent(body(\'Get_CDC_app_secret\')?[\'value\'])}&httpStatusCodes=true&include=data,profile,groups&extraProfileFields=address,phones,work,locale&UID=@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
                  headers: {
                    'content-type': 'application/x-www-form-urlencoded'
                  }
                  method: 'POST'
                  uri: 'https://accounts.@{body(\'Convert_configuration\')[\'sap-cdc-data-center\']}/accounts.getAccountInfo'
                }
                runAfter: {}
                type: 'Http'
              }
              Get_user_roles: {
                inputs: {
                  body: 'apiKey=@{triggerBody()?[\'apiKey\']}&userKey=@{encodeUriComponent(body(\'Get_CDC_app_key\')?[\'value\'])}&secret=@{encodeUriComponent(body(\'Get_CDC_app_secret\')?[\'value\'])}&httpStatusCodes=true&roleIds=@{replace(string(coalesce(body(\'Parse_"Get_user_info"\')?[\'groups\']?[\'organizations\'][0]?[\'roles\'], \'[]\')), \'[]\', \'[""]\')}'
                  headers: {
                    'content-type': 'application/x-www-form-urlencoded'
                  }
                  method: 'POST'
                  uri: 'https://accounts.@{body(\'Convert_configuration\')[\'sap-cdc-data-center\']}/accounts.b2b.auth.getPolicies'
                }
                runAfter: {
                  'Parse_"Get_user_info"': [
                    'Succeeded'
                  ]
                }
                type: 'Http'
              }
              'Parse_"Get_user_info"': {
                inputs: {
                  content: '@body(\'Get_user_info\')'
                  schema: {
                    properties: {
                      UID: {
                        type: 'string'
                      }
                      groups: {
                        properties: {
                          organizations: {
                            items: {
                              properties: {
                                bpid: {
                                  type: 'string'
                                }
                                job: {
                                  type: 'string'
                                }
                                orgName: {
                                  type: 'string'
                                }
                                roles: {
                                  items: {
                                    type: 'string'
                                  }
                                  type: 'array'
                                }
                              }
                              required: [
                                'orgName'
                                'bpid'
                                'job'
                              ]
                              type: 'object'
                            }
                            type: 'array'
                          }
                        }
                        type: 'object'
                      }
                      profile: {
                        properties: {
                          country: {
                            type: 'string'
                          }
                          email: {
                            type: 'string'
                          }
                          firstName: {
                            type: 'string'
                          }
                          lastName: {
                            type: 'string'
                          }
                        }
                        type: 'object'
                      }
                    }
                    type: 'object'
                  }
                }
                runAfter: {
                  Get_user_info: [
                    'Succeeded'
                  ]
                }
                type: 'ParseJson'
              }
              'Parse_"Get_user_roles"': {
                inputs: {
                  content: '@body(\'Get_user_roles\')'
                  schema: {
                    properties: {
                      policiesNotFound: {
                        items: {
                          properties: {
                            roleId: {
                              type: 'string'
                            }
                          }
                          required: [
                            'roleId'
                          ]
                          type: 'object'
                        }
                        type: 'array'
                      }
                      requestedPolicies: {
                        items: {
                          properties: {
                            policyName: {
                              type: 'string'
                            }
                            roleId: {
                              type: 'string'
                            }
                          }
                          required: [
                            'policyName'
                            'roleId'
                          ]
                          type: 'object'
                        }
                        type: 'array'
                      }
                    }
                    type: 'object'
                  }
                }
                runAfter: {
                  Get_user_roles: [
                    'Succeeded'
                  ]
                }
                type: 'ParseJson'
              }
              Set_record_from_user_info: {
                inputs: {
                  name: 'record'
                  value: {
                    accessRequest: '@{triggerBody()?[\'data\']?[\'params\']?[\'data\']?[\'accessRequest\']}'
                    address: '@{body(\'Parse_"Get_user_info"\')?[\'profile\']?[\'address\']}'
                    bpid: '@{body(\'Parse_"Get_user_info"\')?[\'groups\']?[\'organizations\'][0]?[\'bpid\']}'
                    city: '@{body(\'Parse_"Get_user_info"\')?[\'profile\']?[\'city\']}'
                    company: '@{body(\'Parse_"Get_user_info"\')?[\'groups\']?[\'organizations\'][0]?[\'orgName\']}'
                    country: '@{body(\'Parse_"Get_user_info"\')?[\'profile\']?[\'country\']}'
                    currentAccess: '@{join(body(\'Get_role_names\'), \',\')}'
                    email: '@{body(\'Parse_"Get_user_info"\')?[\'profile\']?[\'email\']}'
                    firstName: '@{coalesce(triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'firstName\'], body(\'Parse_"Get_user_info"\')?[\'profile\']?[\'firstName\'])}'
                    industry: '@{body(\'Parse_"Get_user_info"\')?[\'profile\']?[\'work\']?[\'industry\']}'
                    lastName: '@{coalesce(triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'lastName\'], body(\'Parse_"Get_user_info"\')?[\'profile\']?[\'lastName\'])}'
                    locale: '@{toLower(coalesce(triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'locale\'], body(\'Parse_"Get_user_info"\')?[\'profile\']?[\'locale\']))}'
                    phone: '@{triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'phones\'][0]?[\'number\']}'
                    role: '@{coalesce(triggerBody()?[\'data\']?[\'params\']?[\'profile\']?[\'work\']?[\'title\'], body(\'Parse_"Get_user_info"\')?[\'groups\']?[\'organizations\'][0]?[\'job\'])}'
                    state: '@{body(\'Parse_"Get_user_info"\')?[\'profile\']?[\'state\']}'
                    uid: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
                    website: '@{body(\'Parse_"Get_user_info"\')?[\'profile\']?[\'work\']?[\'location\']}'
                    zip: '@{body(\'Parse_"Get_user_info"\')?[\'profile\']?[\'zip\']}'
                  }
                }
                runAfter: {
                  Get_role_names: [
                    'Succeeded'
                  ]
                }
                type: 'SetVariable'
              }
            }
          }
          expression: {
            and: [
              {
                equals: [
                  '@variables(\'registration\')'
                  true
                ]
              }
            ]
          }
          runAfter: {
            GA_Event_Request_For_Approval: [
              'Succeeded'
            ]
            GA_Event_Request_For_Approval1: [
              'Succeeded'
            ]
            Get_CDC_app_key: [
              'Succeeded'
            ]
            Get_CDC_app_secret: [
              'Succeeded'
            ]
            Initialize_record: [
              'Succeeded'
            ]
          }
          type: 'If'
        }
        Is_registration_2: {
          actions: {
            Get_UID_for_new_user: {
              actions: {
                Delay: {
                  inputs: {
                    interval: {
                      count: 1
                      unit: 'Second'
                    }
                  }
                  runAfter: {
                    'Parse_"Find_UID_by_email"': [
                      'Succeeded'
                    ]
                  }
                  type: 'Wait'
                }
                Find_UID_by_email: {
                  inputs: {
                    body: 'apiKey=@{triggerBody()?[\'apiKey\']}&userKey=@{encodeUriComponent(body(\'Get_CDC_app_key\')?[\'value\'])}&secret=@{encodeUriComponent(body(\'Get_CDC_app_secret\')?[\'value\'])}&httpStatusCodes=true&identifierSchema=gigya.com/identifiers/email&id=@{encodeUriComponent(variables(\'record\')[\'email\'])}'
                    headers: {
                      'content-type': 'application/x-www-form-urlencoded'
                    }
                    method: 'POST'
                    uri: 'https://accounts.@{body(\'Convert_configuration\')[\'sap-cdc-data-center\']}/accounts.identifiers.find'
                  }
                  runAfter: {}
                  type: 'Http'
                }
                'Parse_"Find_UID_by_email"': {
                  inputs: {
                    content: '@body(\'Find_UID_by_email\')'
                    schema: {
                      properties: {
                        apiVersion: {
                          type: 'integer'
                        }
                        callId: {
                          type: 'string'
                        }
                        errorCode: {
                          type: 'integer'
                        }
                        identifiers: {
                          properties: {
                            'gigya.com/identifiers/email': {
                              items: {
                                type: 'string'
                              }
                              type: 'array'
                            }
                            'gigya.com/identifiers/uid': {
                              items: {
                                type: 'string'
                              }
                              type: 'array'
                            }
                          }
                          type: 'object'
                        }
                        statusCode: {
                          type: 'integer'
                        }
                        statusReason: {
                          type: 'string'
                        }
                        time: {
                          type: 'string'
                        }
                      }
                      type: 'object'
                    }
                  }
                  runAfter: {
                    Find_UID_by_email: [
                      'Succeeded'
                    ]
                  }
                  type: 'ParseJson'
                }
              }
              expression: '@equals(body(\'Parse_"Find_UID_by_email"\')?[\'errorCode\'], 0)'
              limit: {
                count: 15
                timeout: 'PT1H'
              }
              operationOptions: 'FailWhenLimitsReached'
              runAfter: {}
              type: 'Until'
            }
            Is_approved_1: {
              actions: {
                Approve_user_for_login: {
                  inputs: {
                    body: 'apiKey=@{triggerBody()?[\'apiKey\']}&userKey=@{encodeUriComponent(body(\'Get_CDC_app_key\')?[\'value\'])}&secret=@{encodeUriComponent(body(\'Get_CDC_app_secret\')?[\'value\'])}&httpStatusCodes=true&UID=@{variables(\'record\')[\'uid\']}&data={"approved": true}'
                    headers: {
                      'content-type': 'application/x-www-form-urlencoded'
                    }
                    method: 'POST'
                    uri: 'https://accounts.@{body(\'Convert_configuration\')[\'sap-cdc-data-center\']}/accounts.setAccountInfo'
                  }
                  runAfter: {}
                  type: 'Http'
                }
                CIAM_Event_Create_Org: {
                  inputs: {
                    body: {
                      EventType: 'create_org'
                      OrgData: {
                        BPID: '@{body(\'Parse_"Register_organization"\')?[\'bpid\']}'
                        OrgName: '@{variables(\'record\')[\'company\']}'
                      }
                      UserData: {
                        UID: '@{triggerBody()?[\'data\']?[\'accountInfo\']?[\'UID\']}'
                      }
                    }
                    headers: {
                      'SapCdcCiam-Subscription-Key': '@{body(\'Convert_configuration\')[\'ciam-analytics-endpoint\'][\'SapCdcCiamSubscriptionKey\']}'
                      'content-type': 'application/json'
                    }
                    method: 'POST'
                    uri: '@{body(\'Convert_configuration\')[\'ciam-analytics-endpoint\'][\'ciamAnalyticsApiEndpoint\']}'
                  }
                  runAfter: {
                    'Parse_"Register_organization"': [
                      'Succeeded'
                    ]
                  }
                  type: 'Http'
                }
                Finalize_user_registration: {
                  inputs: {
                    body: 'apiKey=@{triggerBody()?[\'apiKey\']}&userKey=@{encodeUriComponent(body(\'Get_CDC_app_key\')?[\'value\'])}&secret=@{encodeUriComponent(body(\'Get_CDC_app_secret\')?[\'value\'])}&httpStatusCodes=true&siteUID=@{variables(\'record\')[\'uid\']}'
                    headers: {
                      'content-type': 'application/x-www-form-urlencoded'
                    }
                    method: 'POST'
                    uri: 'https://accounts.@{body(\'Convert_configuration\')[\'sap-cdc-data-center\']}/accounts.notifyLogin'
                  }
                  runAfter: {
                    Approve_user_for_login: [
                      'Succeeded'
                    ]
                  }
                  type: 'Http'
                }
                'Parse_"Register_organization"': {
                  inputs: {
                    content: '@body(\'Register_organization\')'
                    schema: {
                      properties: {
                        apiVersion: {
                          type: 'integer'
                        }
                        bpid: {
                          type: 'string'
                        }
                        callId: {
                          type: 'string'
                        }
                        createdTime: {
                          type: 'string'
                        }
                        errorCode: {
                          type: 'integer'
                        }
                        info: {
                          properties: {
                            country: {
                              type: 'string'
                            }
                          }
                          type: 'object'
                        }
                        lastUpdateTime: {
                          type: 'integer'
                        }
                        memberLimit: {
                          type: 'integer'
                        }
                        orgName: {
                          type: 'string'
                        }
                        requester: {
                          properties: {
                            email: {
                              type: 'string'
                            }
                            firstName: {
                              type: 'string'
                            }
                            jobFunction: {
                              type: 'string'
                            }
                            lastName: {
                              type: 'string'
                            }
                          }
                          type: 'object'
                        }
                        source: {
                          type: 'string'
                        }
                        statusCode: {
                          type: 'integer'
                        }
                        statusReason: {
                          type: 'string'
                        }
                        time: {
                          type: 'string'
                        }
                        type: {
                          type: 'string'
                        }
                      }
                      type: 'object'
                    }
                  }
                  runAfter: {
                    Register_organization: [
                      'Succeeded'
                    ]
                  }
                  type: 'ParseJson'
                }
                Register_organization: {
                  inputs: {
                    body: 'apiKey=@{triggerBody()?[\'apiKey\']}&userKey=@{encodeUriComponent(body(\'Get_CDC_app_key\')?[\'value\'])}&secret=@{encodeUriComponent(body(\'Get_CDC_app_secret\')?[\'value\'])}&httpStatusCodes=true&organization={"name":"@{encodeUriComponent(variables(\'record\')[\'company\'])}", "country": "@{encodeUriComponent(variables(\'record\')[\'country\'])}", "city": "@{encodeUriComponent(variables(\'record\')[\'city\'])}", "state": "@{encodeUriComponent(variables(\'record\')[\'state\'])}", "zip_code": "@{encodeUriComponent(variables(\'record\')[\'zip\'])}", "street_address": "@{encodeUriComponent(variables(\'record\')[\'address\'])}", "ZWEBSITE": "@{encodeUriComponent(variables(\'record\')[\'website\'])}", "ZINDUSTRY": "@{encodeUriComponent(variables(\'record\')[\'industry\'])}"}&requester={"lastName": "@{encodeUriComponent(variables(\'record\')[\'lastName\'])}","firstName": "@{encodeUriComponent(variables(\'record\')[\'firstName\'])}","email": "@{encodeUriComponent(variables(\'record\')[\'email\'])}","jobFunction":"@{encodeUriComponent(variables(\'record\')[\'role\'])}"}&status=approved'
                    headers: {
                      'content-type': 'application/x-www-form-urlencoded'
                    }
                    method: 'POST'
                    uri: 'https://accounts.@{body(\'Convert_configuration\')[\'sap-cdc-data-center\']}/accounts.b2b.registerOrganization'
                  }
                  runAfter: {
                    Finalize_user_registration: [
                      'Succeeded'
                    ]
                  }
                  type: 'Http'
                }
                Set_BPID_in_record: {
                  inputs: '@addProperty(variables(\'record\'), \'bpid\', body(\'Parse_"Register_organization"\')?[\'bpid\'])'
                  runAfter: {
                    CIAM_Event_Create_Org: [
                      'Succeeded'
                    ]
                  }
                  type: 'Compose'
                }
                Update_record_2: {
                  inputs: {
                    name: 'record'
                    value: '@outputs(\'Set_BPID_in_record\')'
                  }
                  runAfter: {
                    Set_BPID_in_record: [
                      'Succeeded'
                    ]
                  }
                  type: 'SetVariable'
                }
              }
              else: {
                actions: {
                  Delete_pending_user: {
                    inputs: {
                      body: 'apiKey=@{triggerBody()?[\'apiKey\']}&userKey=@{encodeUriComponent(body(\'Get_CDC_app_key\')?[\'value\'])}&secret=@{encodeUriComponent(body(\'Get_CDC_app_secret\')?[\'value\'])}&httpStatusCodes=true&UID=@{variables(\'record\')[\'uid\']}'
                      headers: {
                        'content-type': 'application/x-www-form-urlencoded'
                      }
                      method: 'POST'
                      uri: 'https://accounts.@{body(\'Convert_configuration\')[\'sap-cdc-data-center\']}/accounts.deleteAccount'
                    }
                    runAfter: {}
                    type: 'Http'
                  }
                }
              }
              expression: {
                and: [
                  {
                    equals: [
                      '@variables(\'result\')'
                      'approve'
                    ]
                  }
                ]
              }
              runAfter: {
                Update_record: [
                  'Succeeded'
                ]
              }
              type: 'If'
            }
            Set_UID_in_record: {
              inputs: '@addProperty(variables(\'record\'), \'uid\', body(\'Parse_"Find_UID_by_email"\')?[\'identifiers\']?[\'gigya.com/identifiers/uid\'][0])'
              runAfter: {
                Get_UID_for_new_user: [
                  'Succeeded'
                ]
              }
              type: 'Compose'
            }
            Update_record: {
              inputs: {
                name: 'record'
                value: '@outputs(\'Set_UID_in_record\')'
              }
              runAfter: {
                Set_UID_in_record: [
                  'Succeeded'
                ]
              }
              type: 'SetVariable'
            }
          }
          else: {
            actions: {
              Is_approved: {
                actions: {
                  Send_approved_email: {
                    inputs: {
                      body: {
                        accessItemText: '@{body(\'Find_item_access_text\')}'
                        email: '@{variables(\'record\')[\'email\']}'
                        firstName: '@{variables(\'record\')[\'firstName\']}'
                        lastName: '@{variables(\'record\')[\'lastName\']}'
                        locale: '@{variables(\'record\')[\'locale\']}'
                        subject: '@{if(variables(\'registration\'), null, \'update-email\')}'
                        template: 'approved-email'
                      }
                      host: {
                        triggerName: 'manual'
                        workflow: {
                          id: emailNotification.id
                        }
                      }
                    }
                    operationOptions: 'DisableAsyncPattern'
                    runAfter: {}
                    type: 'Workflow'
                  }
                }
                expression: {
                  and: [
                    {
                      equals: [
                        '@variables(\'result\')'
                        'approve'
                      ]
                    }
                    {
                      equals: [
                        '@variables(\'new_access\')'
                        true
                      ]
                    }
                  ]
                }
                runAfter: {}
                type: 'If'
              }
            }
          }
          expression: {
            and: [
              {
                equals: [
                  '@variables(\'registration\')'
                  true
                ]
              }
            ]
          }
          runAfter: {
            Find_item_access_text: [
              'Succeeded'
            ]
          }
          type: 'If'
        }
        New_access_filter: {
          inputs: {
            from: '@split(variables(\'record\')[\'accessRequest\'], \',\')'
            where: '@not(contains(variables(\'record\')[\'currentAccess\'], item()))'
          }
          runAfter: {
            Is_registration_1: [
              'Succeeded'
            ]
          }
          type: 'Query'
        }
        Not_applicable_update_or_registration: {
          actions: {
            Return_OK: {
              inputs: {
                body: {
                  status: 'OK'
                }
                headers: {
                  'Content-Type': 'application/json; charset=utf-8'
                }
                statusCode: 200
              }
              kind: 'Http'
              runAfter: {}
              type: 'Response'
            }
            Terminate: {
              inputs: {
                runStatus: 'Succeeded'
              }
              runAfter: {
                Return_OK: [
                  'Succeeded'
                ]
              }
              type: 'Terminate'
            }
          }
          expression: {
            or: [
              {
                and: [
                  {
                    not: {
                      equals: [
                        '@variables(\'registration\')'
                        true
                      ]
                    }
                  }
                  {
                    equals: [
                      '@triggerBody()?[\'data\']?[\'params\']?[\'data\']?[\'accessRequest\']'
                      null
                    ]
                  }
                ]
              }
              {
                and: [
                  {
                    equals: [
                      '@variables(\'registration\')'
                      true
                    ]
                  }
                  {
                    equals: [
                      '@triggerBody()?[\'data\']?[\'params\']?[\'preferences\']?[\'privacy\']?[\'privacySample\']?[\'isConsentGranted\']'
                      null
                    ]
                  }
                ]
              }
            ]
          }
          runAfter: {
            Set_registration_variable: [
              'Succeeded'
            ]
          }
          type: 'If'
        }
        Parse_configuration: {
          inputs: {
            content: '@body(\'Get_runtime_configuration\')'
            schema: {
              properties: {
                items: {
                  items: {
                    properties: {
                      content_type: {
                        type: 'string'
                      }
                      etag: {
                        type: 'string'
                      }
                      key: {
                        type: 'string'
                      }
                      label: {}
                      last_modified: {
                        type: 'string'
                      }
                      locked: {
                        type: 'boolean'
                      }
                      tags: {
                        properties: {}
                        type: 'object'
                      }
                      value: {
                        type: 'string'
                      }
                    }
                    required: [
                      'etag'
                      'key'
                      'label'
                      'content_type'
                      'value'
                      'tags'
                      'locked'
                      'last_modified'
                    ]
                    type: 'object'
                  }
                  type: 'array'
                }
              }
              type: 'object'
            }
          }
          runAfter: {
            Get_runtime_configuration: [
              'Succeeded'
            ]
          }
          type: 'ParseJson'
        }
        Set_registration_variable: {
          inputs: {
            variables: [
              {
                name: 'registration'
                type: 'boolean'
                value: '@and(equals(triggerBody()?[\'data\']?[\'accountInfo\']?[\'preferences\']?[\'privacy\'], null), equals(triggerBody()?[\'data\']?[\'accountInfo\']?[\'data\']?[\'approved\'], null))'
              }
            ]
          }
          runAfter: {}
          type: 'InitializeVariable'
        }
        Should_auto_reject: {
          actions: {
            Reject_1: {
              inputs: {
                name: 'result'
                value: 'reject'
              }
              runAfter: {}
              type: 'SetVariable'
            }
          }
          else: {
            actions: {
              Should_auto_approve: {
                actions: {
                  Approve_1: {
                    inputs: {
                      name: 'result'
                      value: 'approve'
                    }
                    runAfter: {}
                    type: 'SetVariable'
                  }
                }
                expression: {
                  or: [
                    {
                      and: [
                        {
                          equals: [
                            '@length(intersection(body(\'Convert_configuration\')[\'generic-emails\'], array(toLower(split(variables(\'record\')[\'email\'], \'@\')[1]))))'
                            0
                          ]
                        }
                        {
                          greater: [
                            '@length(intersection(body(\'Convert_configuration\')[\'auto-approved-countries\'], array(variables(\'record\')[\'country\'])))'
                            0
                          ]
                        }
                      ]
                    }
                    {
                      and: [
                        {
                          equals: [
                            '@variables(\'registration\')'
                            false
                          ]
                        }
                        {
                          equals: [
                            '@variables(\'new_access\')'
                            false
                          ]
                        }
                      ]
                    }
                  ]
                }
                runAfter: {}
                type: 'If'
              }
            }
          }
          expression: {
            and: [
              {
                or: [
                  {
                    greater: [
                      '@length(intersection(body(\'Convert_configuration\')[\'embargoed-countries\'], array(variables(\'record\')[\'country\'])))'
                      0
                    ]
                  }
                  {
                    greater: [
                      '@length(intersection(body(\'Convert_configuration\')[\'competitors\'], array(toLower(split(variables(\'record\')[\'email\'], \'@\')[1]))))'
                      0
                    ]
                  }
                  {
                    equals: [
                      '@length(intersection(body(\'Convert_configuration\')[\'approved-roles\'], array(variables(\'record\')[\'role\'])))'
                      0
                    ]
                  }
                ]
              }
              {
                or: [
                  {
                    equals: [
                      '@variables(\'registration\')'
                      true
                    ]
                  }
                  {
                    equals: [
                      '@variables(\'new_access\')'
                      true
                    ]
                  }
                ]
              }
            ]
          }
          runAfter: {
            CIAM_Event_Request_For_Approval: [
              'Succeeded'
            ]
            Initialize_new_access: [
              'Succeeded'
            ]
          }
          type: 'If'
        }
      }
      contentVersion: '1.0.0.0'
      outputs: {}
      parameters: {
        '$connections': {
          defaultValue: {}
          type: 'Object'
        }
      }
      triggers: {
        manual: {
          conditions: []
          inputs: {
            method: 'POST'
            schema: {
              properties: {
                apiKey: {
                  type: 'string'
                }
                callID: {
                  type: 'string'
                }
                data: {
                  properties: {
                    accountInfo: {
                      properties: {
                        UID: {
                          type: 'string'
                        }
                        created: {
                          type: 'string'
                        }
                        createdTimestamp: {
                          type: 'integer'
                        }
                        data: {
                          properties: {}
                          type: 'object'
                        }
                        isActive: {
                          type: 'boolean'
                        }
                        isRegistered: {
                          type: 'boolean'
                        }
                        isVerified: {
                          type: 'boolean'
                        }
                        lastUpdated: {
                          type: 'string'
                        }
                        lastUpdatedTimestamp: {
                          type: 'integer'
                        }
                        oldestDataUpdated: {
                          type: 'string'
                        }
                        oldestDataUpdatedTimestamp: {
                          type: 'integer'
                        }
                        preferences: {
                          properties: {}
                          type: 'object'
                        }
                        profile: {
                          properties: {
                            email: {
                              type: 'string'
                            }
                            firstName: {
                              type: 'string'
                            }
                            lastName: {
                              type: 'string'
                            }
                            locale: {
                              type: 'string'
                            }
                          }
                          type: 'object'
                        }
                        socialProviders: {
                          type: 'string'
                        }
                        subscriptions: {
                          properties: {}
                          type: 'object'
                        }
                        verified: {
                          type: 'string'
                        }
                        verifiedTimestamp: {
                          type: 'integer'
                        }
                      }
                      type: 'object'
                    }
                    context: {
                      properties: {
                        clientIP: {
                          type: 'string'
                        }
                      }
                      type: 'object'
                    }
                    params: {
                      properties: {
                        lang: {
                          type: 'string'
                        }
                        newPassword: {
                          type: 'string'
                        }
                        password: {
                          type: 'string'
                        }
                        preferences: {
                          properties: {
                            privacy: {
                              properties: {
                                privacySample: {
                                  properties: {
                                    isConsentGranted: {
                                      type: 'boolean'
                                    }
                                  }
                                  type: 'object'
                                }
                              }
                              type: 'object'
                            }
                            terms: {
                              properties: {
                                termsSample: {
                                  properties: {
                                    isConsentGranted: {
                                      type: 'boolean'
                                    }
                                  }
                                  type: 'object'
                                }
                              }
                              type: 'object'
                            }
                          }
                          type: 'object'
                        }
                        profile: {
                          properties: {
                            address: {
                              type: 'string'
                            }
                            city: {
                              type: 'string'
                            }
                            country: {
                              type: 'string'
                            }
                            firstName: {
                              type: 'string'
                            }
                            lastName: {
                              type: 'string'
                            }
                            phones: {
                              items: {
                                properties: {
                                  number: {
                                    type: 'string'
                                  }
                                  type: {
                                    type: 'string'
                                  }
                                }
                                required: [
                                  'type'
                                  'number'
                                ]
                                type: 'object'
                              }
                              type: 'array'
                            }
                            state: {
                              type: 'string'
                            }
                            work: {
                              properties: {
                                company: {
                                  type: 'string'
                                }
                                industry: {
                                  type: 'string'
                                }
                                location: {
                                  type: 'string'
                                }
                                title: {
                                  type: 'string'
                                }
                              }
                              type: 'object'
                            }
                            zip: {
                              type: 'string'
                            }
                          }
                          type: 'object'
                        }
                        rba: {
                          properties: {}
                          type: 'object'
                        }
                        subscriptions: {
                          properties: {
                            EventsAndTrainings: {
                              properties: {
                                email: {
                                  properties: {
                                    isSubscribed: {
                                      type: 'boolean'
                                    }
                                  }
                                  type: 'object'
                                }
                              }
                              type: 'object'
                            }
                            LocalSampleRepresentative: {
                              properties: {
                                email: {
                                  properties: {
                                    isSubscribed: {
                                      type: 'boolean'
                                    }
                                  }
                                  type: 'object'
                                }
                              }
                              type: 'object'
                            }
                            NewProductAndServices: {
                              properties: {
                                email: {
                                  properties: {
                                    isSubscribed: {
                                      type: 'boolean'
                                    }
                                  }
                                  type: 'object'
                                }
                              }
                              type: 'object'
                            }
                          }
                          type: 'object'
                        }
                      }
                      type: 'object'
                    }
                  }
                  type: 'object'
                }
                extensionPoint: {
                  type: 'string'
                }
                parentApiKey: {
                  type: 'string'
                }
              }
              type: 'object'
            }
          }
          kind: 'Http'
          operationOptions: 'EnableSchemaValidation'
          type: 'Request'
        }
      }
    }
    parameters: {
      '$connections': {
        value: {
          keyvault: {
            connectionId: logicAppKeyVaultConnector.id
            connectionName: logicAppKeyVaultConnector.name
            connectionProperties: {
              authentication: {
                type: 'ManagedServiceIdentity'
              }
            }
            id: logicAppKeyVaultConnector.properties.api.id
          }
          office365: {
            connectionId: logicAppOffice365Connector.id
            connectionName: logicAppOffice365Connector.name
            id: logicAppOffice365Connector.properties.api.id
          }
        }
      }
    }
  }
}

resource setCDCSendEmailURL 'Microsoft.Resources/deploymentScripts@2023-08-01' = {
  name: '${resourcePrefix}-DS-${env}-setCDCSendEmailURL'
  location: location
  kind: 'AzurePowerShell'
  properties: {
    azPowerShellVersion: '10.4'
    retentionInterval: 'P1D'
    scriptContent: 'Invoke-WebRequest -Uri https://idx.${cdcDC}/idx.setGlobalVariable -Method POST -Body "apiKey=${cdcApiKey}&userKey=${uriComponent(cdcAppKey)}&secret=${uriComponent(cdcAppSecret)}&httpStatusCodes=true&type=string&name=sendEmailURL&value=${uriComponent(listCallbackUrl('${emailNotification.id}/triggers/manual', '2019-05-01').value)}"'
  }
}
