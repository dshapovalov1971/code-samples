param location string = resourceGroup().location

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
  [ 'access-item-text', 'application/json', '[{"a": "Syber Bearing System Designer","l": "Syber Bearing System Designer tool"},{"a": "P&A","l": "relative pricing and availability information"},{"a": "_","l": "our Online Account"}]' ]
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

resource keyVault 'Microsoft.KeyVault/vaults@2023-07-01' = if (!contains(resourceGroup().tags, '${resourcePrefix}-KV-${env}-KeyVault')) {
  name: '${resourcePrefix}-KV-${env}-KeyVault'
  location: location
  properties: {
    enabledForTemplateDeployment: true
    tenantId: tenant().tenantId
    accessPolicies: [ {
        tenantId: tenant().tenantId
        objectId: logicApp.identity.principalId
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
resource keyVaultSecret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = [for (item, i) in secrets: if (!contains(resourceGroup().tags, '${resourcePrefix}-KV-${env}-KeyVault')) {
  parent: keyVault
  name: item
  properties: {
    value: ''
  }
}]

resource tag 'Microsoft.Resources/tags@2023-07-01' = {
  name: 'default'
  properties: {
    tags: {
      '${resourcePrefix}-CS-${env}-AppConfig': 'deployed'
      '${resourcePrefix}-KV-${env}-KeyVault': 'deployed'
    }
  }
}

resource logicAppOffice365Connector 'Microsoft.Web/connections@2016-06-01' = {
  name: '${resourcePrefix}-APIC-${env}-engineeringhelp-sample.com'
  location: location
  kind: 'V2'
  properties: {
    api: {
      id: subscriptionResourceId('Microsoft.Web/locations/managedApis', location, 'office365')
    }
  }
}

resource logicAppOffice365ConnectorAccessPolicy 'Microsoft.Web/connections/accessPolicies@2016-06-01' = {
  name: '${logicAppOffice365Connector.name}/${logicApp.name}-Access-Policy'
  location: location
  dependsOn: [logicAppOffice365Connector]
  properties: {
    principal: {
      type: 'ActiveDirectory'
      identity: {
        objectId: logicApp.identity.principalId
        tenantId: tenant().tenantId
      }
    }
  }
}

resource logicAppOffice365UsersConnector 'Microsoft.Web/connections@2016-06-01' = {
  name: '${resourcePrefix}-APIC-${env}-office365users'
  location: location
  kind: 'V2'
  properties: {
    api: {
      id: subscriptionResourceId('Microsoft.Web/locations/managedApis', location, 'office365users')
    }
  }
}

resource logicAppOffice365UsersConnectorAccessPolicy 'Microsoft.Web/connections/accessPolicies@2016-06-01' = {
  name: '${logicAppOffice365UsersConnector.name}/${logicApp.name}-Access-Policy'
  location: location
  dependsOn: [logicAppOffice365UsersConnector]
  properties: {
    principal: {
      type: 'ActiveDirectory'
      identity: {
        objectId: logicApp.identity.principalId
        tenantId: tenant().tenantId
      }
    }
  }
}

resource logicAppKeyVaultConnector 'Microsoft.Web/connections@2016-06-01' = {
  name: '${resourcePrefix}-APIC-${env}-keyvault'
  location: location
  kind: 'V2'
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

resource logicAppKeyVaultConnectorAccessPolicy 'Microsoft.Web/connections/accessPolicies@2016-06-01' = {
  name: '${logicAppKeyVaultConnector.name}/${logicApp.name}-Access-Policy'
  location: location
  dependsOn: [logicAppKeyVaultConnector]
  properties: {
    principal: {
      type: 'ActiveDirectory'
      identity: {
        objectId: logicApp.identity.principalId
        tenantId: tenant().tenantId
      }
    }
  }
}

resource logicAppStorageConnector 'Microsoft.Web/connections@2016-06-01' = {
  name: '${resourcePrefix}-APIC-${env}-azureblob'
  location: location
  kind: 'V2'
  properties: any({
    api: {
      id: subscriptionResourceId('Microsoft.Web/locations/managedApis', location, 'azureblob')
    }
    parameterValueSet: {
      name: 'managedIdentityAuth'
    }
  })
}

resource logicAppStorageConnectorAccessPolicy 'Microsoft.Web/connections/accessPolicies@2016-06-01' = {
  name: '${logicAppStorageConnector.name}/${logicApp.name}-Access-Policy'
  location: location
  dependsOn: [logicAppStorageConnector]
  properties: {
    principal: {
      type: 'ActiveDirectory'
      identity: {
        objectId: logicApp.identity.principalId
        tenantId: tenant().tenantId
      }
    }
  }
}

resource roleAssignmentAppConfigurationDataReaderLogicApp 'Microsoft.Authorization/roleAssignments@2022-04-01' = {// App Configuration Data Reader
  name: guid('LogicApp', '516239f1-63e1-4d78-a4de-a74fb236a071', resourceGroup().id)
  properties: {
    roleDefinitionId: resourceId('microsoft.authorization/roleDefinitions', '516239f1-63e1-4d78-a4de-a74fb236a071')
    principalId: logicApp.identity.principalId
    principalType: 'ServicePrincipal'
  }
}

resource roleAssignmentStorageLogicApp 'Microsoft.Authorization/roleAssignments@2022-04-01' = {// Storage Blob Data Reader
  name: guid('LogicApp', '2a2b9908-6ea1-4ae2-8e65-a410df84e7d1', resourceGroup().id)
  properties: {
    roleDefinitionId: resourceId('microsoft.authorization/roleDefinitions', '2a2b9908-6ea1-4ae2-8e65-a410df84e7d1')
    principalId: logicApp.identity.principalId
    principalType: 'ServicePrincipal'
  }
}

resource storageAccount 'Microsoft.Storage/storageAccounts@2023-05-01' = {
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

resource blobServices 'Microsoft.Storage/storageAccounts/blobServices@2023-05-01' = {
  parent: storageAccount
  name: 'default'
}

resource container 'Microsoft.Storage/storageAccounts/blobServices/containers@2023-01-01' = {
  parent: blobServices
  name: 'emailtemplates'
}

resource appServicePlan 'Microsoft.Web/serverfarms@2022-03-01' = {
  name: '${resourcePrefix}-ASP-${env}-LogicAppStandard'
  location: location
  sku: {
    name: 'WS1'
  }
}

var storageAccountConnectionString = 'DefaultEndpointsProtocol=https;AccountName=${storageAccount.name};EndpointSuffix=${environment().suffixes.storage};AccountKey=${storageAccount.listKeys().keys[0].value}'
resource logicApp 'Microsoft.Web/sites@2023-12-01' = {
  name: '${resourcePrefix}-LApp-${env}-LogicApp'
  location: location
  kind: 'functionapp,workflowapp'
  identity: {
    type: 'SystemAssigned'
  }
  properties: {
    serverFarmId: appServicePlan.id
    publicNetworkAccess: 'Enabled'
    siteConfig: {
      appSettings: [
        {name: 'FUNCTIONS_EXTENSION_VERSION', value: '~4'}
        {name: 'AzureWebJobsStorage', value: storageAccountConnectionString}
        {name: 'WEBSITE_CONTENTAZUREFILECONNECTIONSTRING', value: storageAccountConnectionString}
        {name: 'WEBSITE_CONTENTSHARE', value: 'logic-app-standard'}
        {name: 'FUNCTIONS_WORKER_RUNTIME', value: 'node'}
        {name: 'WEBSITE_NODE_DEFAULT_VERSION', value: '~20'}
        {name: 'connAzureBlobApiId', value: logicAppStorageConnector.properties.api.id}
        {name: 'connAzureBlobId', value: logicAppStorageConnector.id}
        {name: 'connAzureBlobRuntimeUrl', value: logicAppStorageConnector.properties.connectionRuntimeUrl}
        {name: 'connOffice365ApiId', value: logicAppOffice365Connector.properties.api.id}
        {name: 'connOffice365Id', value: logicAppOffice365Connector.id}
        {name: 'connOffice365RuntimeUrl', value: logicAppOffice365Connector.properties.connectionRuntimeUrl}
        {name: 'connOffice365UsersApiId', value: logicAppOffice365UsersConnector.properties.api.id}
        {name: 'connOffice365UsersId', value: logicAppOffice365UsersConnector.id}
        {name: 'connOffice365UsersRuntimeUrl', value: logicAppOffice365UsersConnector.properties.connectionRuntimeUrl}
        {name: 'connKeyVaultApiId', value: logicAppKeyVaultConnector.properties.api.id}
        {name: 'connKeyVaultId', value: logicAppKeyVaultConnector.id}
        {name: 'connKeyVaultRuntimeUrl', value: logicAppKeyVaultConnector.properties.connectionRuntimeUrl}
        {name: 'configStoreEndpoint', value: configStore.properties.endpoint}
        {name: 'storageAccountName', value: storageAccount.name}
      ]
      use32BitWorkerProcess: false      
    }
  }
}
