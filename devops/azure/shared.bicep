param location string = resourceGroup().location
param marketingSubscriptionId string
param cdcApiKey string
param cdcDC string
@secure()
param cdcAppKey string
@secure()
param cdcAppSecret string
param frontdoorApiUrl string


var nameStructure = split(resourceGroup().name, '-')
var resourcePrefix = '${nameStructure[0]}-${nameStructure[1]}'
var env = nameStructure[3]

resource approvalWorkflow 'Microsoft.Logic/workflows@2019-05-01' existing = {
  name: '${resourcePrefix}-LApp-${env}-ApprovalWorkflow'
  scope: resourceGroup('${marketingSubscriptionId}', '${resourcePrefix}-RG-${env}-SAPCDCCIAM')
}

resource tag 'Microsoft.Resources/tags@2022-09-01' = {
  name: 'default'
  properties: {
    tags: {
      '${resourcePrefix}-APIM-${env}-ApiManagement': 'deployed'
    }
  }
}

resource apiManagementInstance 'Microsoft.ApiManagement/service@2022-08-01' = {
  name: '${resourcePrefix}-APIM-${env}-SharedApim'
  location: location
  sku: {
    capacity: 1
    name: 'Basic'
  }
  properties: {
    virtualNetworkType: 'None'
    publisherEmail: 'IT-TERI-SERVERSYSTEMS@sample.com'
    publisherName: 'sample.com'
  }
}

resource apiNamedValues 'Microsoft.ApiManagement/service/namedValues@2022-08-01' = if (!contains(resourceGroup().tags, '${resourcePrefix}-APIM-${env}-ApiManagement')) {
  parent: apiManagementInstance
  name: 'sap-cdc-data-center'
  properties: {
    displayName: 'sap-cdc-data-center'
    value: 'us1.gigya.com'
  }
}

resource jwksAPI 'Microsoft.ApiManagement/service/apis@2022-08-01' = {
  parent: apiManagementInstance
  name: 'jwks'
  properties: {
    path: 'ciam/jwks'
    protocols: [ 'https' ]
    type: 'http'
    displayName: 'jwks'
    subscriptionRequired: false
  }
}

resource jwksGet 'Microsoft.ApiManagement/service/apis/operations@2022-08-01' = {
  parent: jwksAPI
  name: 'jwks'
  properties: {
    method: 'GET'
    displayName: 'jwks'
    urlTemplate: '/jwks'
  }
}

resource jwksGetPolicies 'Microsoft.ApiManagement/service/apis/operations/policies@2022-08-01' = {
  parent: jwksGet
  name: 'policy'
  properties: {
    format: 'xml'
    value: '''
      <policies>
          <inbound>
              <base />
              <return-response>
                  <set-header name="content-type" exists-action="override">
                      <value>application/json</value>
                  </set-header>
                  <set-body>{
    "jwks_uri": "https://accounts.{{sap-cdc-data-center}}/accounts.getJWTPublicKey?V2=true",
    "response_types_supported": ["id_token"],
    "authorization_endpoint": "https://fidm.us1.gigya.com/oidc/op/v1.0/4_VJko3p0RVscKSUNECSTpFA/authorize",
    "issuer": " "
}
                  </set-body>
              </return-response>
          </inbound>
          <backend>
              <base />
          </backend>
          <outbound>
              <base />
          </outbound>
          <on-error>
              <base />
          </on-error>
      </policies>'''
  }
}

resource approvalWorkflowAPI 'Microsoft.ApiManagement/service/apis@2022-08-01' = {
  parent: apiManagementInstance
  name: 'approval-workflow'
  dependsOn: [jwksGetPolicies]
  properties: {
    path: 'ciam/extensions'
    protocols: [ 'https' ]
    type: 'http'
    displayName: 'approval-workflow'
    subscriptionRequired: false
  }
}

resource approvalWorkflowPost 'Microsoft.ApiManagement/service/apis/operations@2022-08-01' = {
  parent: approvalWorkflowAPI
  name: 'manual-invoke'
  properties: {
    method: 'POST'
    displayName: 'manual-invoke'
    urlTemplate: '/approval-workflow'
  }
}

resource approvalWorkflowPostPolicies 'Microsoft.ApiManagement/service/apis/operations/policies@2022-08-01' = {
  parent: approvalWorkflowPost
  name: 'policy'
  properties: {
    format: 'rawxml'
    value: '<policies><inbound><base /><validate-jwt token-value="@((string)context.Request.Body.As<JObject>(true)["jws"])" failed-validation-httpcode="401" require-expiration-time="false" require-signed-tokens="true"><openid-config url="https://${frontdoorApiUrl}/${jwksAPI.properties.path}${jwksGet.properties.urlTemplate}" /></validate-jwt><set-body>@{string epl = ((string)context.Request.Body.As<JObject>()["jws"]).Split(\'.\')[1].Replace(\'-\', \'+\').Replace(\'_\', \'/\');return Encoding.UTF8.GetString(Convert.FromBase64String(epl.PadRight(epl.Length + (4 - epl.Length % 4) % 4, \'=\')));}</set-body><set-backend-service id="apim-generated-policy" base-url="${approvalWorkflow.properties.accessEndpoint}/triggers" /><set-method id="apim-generated-policy">POST</set-method><rewrite-uri id="apim-generated-policy" template="${replace(replace(listCallbackURL('${approvalWorkflow.id}/triggers/manual', '2019-05-01').value, '${approvalWorkflow.properties.accessEndpoint}/triggers', ''), '&', '&amp;')}" /><set-header id="apim-generated-policy" name="Ocp-Apim-Subscription-Key" exists-action="delete" /></inbound><backend><base /></backend><outbound><base /></outbound><on-error><base /></on-error></policies>'
  }
}

resource modifyCDCExtension 'Microsoft.Resources/deploymentScripts@2023-08-01' = {
  name: '${resourcePrefix}-DS-${env}-modifyCDCExtension'
  location: location
  kind: 'AzurePowerShell'
  properties: {
    azPowerShellVersion: '10.4'
    retentionInterval: 'P1D'
    scriptContent: 'Invoke-WebRequest -Uri https://idx.${cdcDC}/accounts.extensions.modify -Method POST -Body "apiKey=${cdcApiKey}&userKey=${uriComponent(cdcAppKey)}&secret=${uriComponent(cdcAppSecret)}&httpStatusCodes=true&extensionFuncUrl=https%3A%2F%2F${uriComponent(frontdoorApiUrl)}%2F${uriComponent(approvalWorkflowAPI.properties.path)}${uriComponent(approvalWorkflowPost.properties.urlTemplate)}&extensionId=$(((Invoke-RestMethod -Uri https://idx.${cdcDC}/accounts.extensions.list -Method POST -Body ""apiKey=${cdcApiKey}&userKey=${uriComponent(cdcAppKey)}&secret=${uriComponent(cdcAppSecret)}&httpStatusCodes=true"").result | Where-Object {$_.friendlyName -eq ""Approval Workflow""}).id)"'
  }
}
