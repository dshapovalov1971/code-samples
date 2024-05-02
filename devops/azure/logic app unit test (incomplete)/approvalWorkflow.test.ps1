param (
    # Resoure Group
    [Parameter(Mandatory=$true)][string]$rg,    
    # Workflow
    [Parameter(Mandatory=$true)][string]$wf,
    # Agent Name
    [Parameter(Mandatory=$true)][string]$an,
    # Build Defnition Name
    [Parameter(Mandatory=$true)][string]$dn
)
. $PSScriptRoot/common-test-utils.ps1

Run-UnitTest 'not-applicable-registration'
Run-UnitTest 'not-applicable-update'
Run-UnitTest 'auto-reject-embargoed-country'
Run-UnitTest 'auto-reject-competitor'
Run-UnitTest 'auto-reject-role'
Run-UnitTest 'auto-reject-role-update'
Run-UnitTest 'auto-approve-country-non-generic-email'
Run-UnitTest 'auto-approve-update'
Run-UnitTest 'manual-approve'

Finalize-UnitTests
