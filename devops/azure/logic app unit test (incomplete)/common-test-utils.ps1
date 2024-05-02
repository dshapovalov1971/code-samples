Install-Module PSWriteHTML -AllowClobber -Force
Install-Module PSSharedGoods -AllowClobber -Force

$lapp = $rg.Replace('RG','LApp') -replace '[^-]+$','LogicApp'
$subId = (Get-AzContext).Subscription.Id
$cn = (Get-PSCallStack)[1].Command
$triggerUrl = ((Invoke-AzRestMethod -Method Post -Path /subscriptions/$subId/resourceGroups/$rg/providers/Microsoft.Web/sites/$lapp/hostruntime/runtime/webhooks/workflow/api/management/workflows/$wf/triggers/manual/listCallbackUrl?api-version=2023-01-01).Content | ConvertFrom-Json -Depth 100).value
if (!$triggerUrl) {throw 'triggerUrl is empty'}
$testResults = New-Object xml
$testResults.LoadXML('<?xml version="1.0" encoding="utf-8"?><testsuites></testsuites>')
$ts = $testResults.CreateElement('testsuite')
$ts.SetAttribute('timestamp', (Get-Date -Format 'o'))
$ts.SetAttribute('name', $cn)
$ts.SetAttribute('hostname', $an)
$testResults.DocumentElement.AppendChild($ts)
[Newtonsoft.Json.Linq.JObject]::Parse((Get-Content -Path "logic-app/$($wf.Replace('-UnitTest', ''))/workflow.json")).SelectTokens('$..actions') | % {$testCoverage = @{}} {$_.Properties()} |
    % {$testCoverage[$_.Name] = @{Hits=0; Branches=@{If=2; Until=2; Switch=$($_.Value['cases']?.Properties()).Count + 1}[$_.Value['type'].ToString()] ?? 0; BranchHits=@{}}}
$baseName = "$([System.IO.Path]::GetFileNameWithoutExtension((Get-PSCallStack)[1].Command))"
New-Item -ItemType Directory -Force -Path reports

function Run-UnitTest {
    param (
        # Test name
        [Parameter(Mandatory=$true)][string]$tn
    )

    $start = Get-Date
    curl -s --fail-with-body -H "Content-Type: application/json" -d "@$PSScriptRoot/$baseName.$tn.payload.json" $triggerUrl
    echo ''
    $wfRun = @()
    do {
        Start-Sleep 1
        $wfRun=((Invoke-AzRestMethod -Path "/subscriptions/$subId/resourceGroups/$rg/providers/Microsoft.Web/sites/$lapp/hostruntime/runtime/webhooks/workflow/api/management/workflows/$wf/runs?api-version=2023-01-01&`$top=1").Content | ConvertFrom-Json -Depth 100).value
    } while (!$wfRun.length -or $wfRun[0].properties.status -eq 'Running')

    $rar = @()
    $link = "https://dummy/subscriptions/$subId/resourceGroups/$rg/providers/Microsoft.Web/sites/$lapp/hostruntime/runtime/webhooks/workflow/api/management/workflows/$wf/runs/$($wfRun[0].name)/actions?api-version=2023-01-01"
    do {
        $res = (Invoke-AzRestMethod -Path ([System.Uri]$link).PathAndQuery).Content | ConvertFrom-Json -Depth 100
        $link = $res.nextLink
        $rar += $res.value
    } while ($link)

    $rar = $rar | % {$_.properties | Add-Member -NotePropertyName name -NotePropertyValue $_.name; $_.properties} |
        Select-Object -ExcludeProperty startTime,endTime,inputsLink,outputsLink,canResubmit,correlation *,`
            @{l='input';e={$_.inputsLink ? (curl -s $_.inputsLink.uri | ConvertFrom-Json -Depth 100) : $null}},`
            @{l='output';e={$_.outputsLink ? (curl -s $_.outputsLink.uri | ConvertFrom-Json -Depth 100) : $null}} |
        Sort-Object -Property name | Where-Object {$_.status -ne 'Skipped'}
    $rar | % {
        $_.output.headers ? ($_.output.headers = @{}) : $null
        $_.input.content.items ? ($_.input.content.items | % {
                $_.etag ? ($_.etag = 'etag') : $null
                $_.last_modified ? ($_.last_modified = '2023-01-01T00:00:00Z') : $null
            }) : $null
        $_.output.body.items ? ($_.output.body.items | % {
                $_.etag ? ($_.etag = 'etag') : $null
                $_.last_modified ? ($_.last_modified = '2023-01-01T00:00:00Z') : $null
            }) : $null
    }
    $rar | ConvertTo-Json -Depth 100 > "reports/$baseName.$tn.run-action.json"
    
    echo ''
    $errCount = (Compare-Object ((Get-Content -Path "$PSScriptRoot/$baseName.$tn.run-action.json" | ConvertFrom-Json -Depth 100 | ConvertTo-Json -Depth 100) -split [Environment]::NewLine) `
                   (($rar | ConvertTo-Json -Depth 100) -split [Environment]::NewLine) -CaseSensitive).length
    New-HTML {New-HTMLTable `
        -DataTable ('{"_":'+(Get-Content -Path "$PSScriptRoot/$baseName.$tn.run-action.json")+'}' | ConvertFrom-Json -Depth 100 | ConvertTo-FlatObject -Depth 100),('{"_":'+($rar | ConvertTo-Json -Depth 100)+'}' | ConvertFrom-Json -Depth 100 | ConvertTo-FlatObject -Depth 100) `
        -Compare -HighlightDifferences -Filtering -WordBreak break-all} -FilePath "reports/$baseName.$tn.html"
    
    $rar | % {$testCoverage[$_.Name].Hits++; $testCoverage[$_.Name].Branches ? $testCoverage[$_.Name].BranchHits[$_.input?.expressionResult ?? 'null']++ : $null}

    $rt = (New-TimeSpan $start (Get-Date)).TotalSeconds
    $tr = $testResults.CreateElement('testcase')
    $ts.SetAttribute('tests', [int]$ts.GetAttribute('tests') + 1)
    if ($errCount) {
        $ts.SetAttribute('failures', [int]$ts.GetAttribute('failures') + 1)
        $fail = $testResults.CreateElement('failure')
        $fail.SetAttribute('message', 'Test output does not match expected result. Please see attachment for details')
        $tr.AppendChild($fail)
    }
    $ts.SetAttribute('time', [float]$ts.GetAttribute('time') + $rt)
    $tr.SetAttribute('name', $tn)
    $tr.SetAttribute('time', $rt)
    $tr.SetAttribute('classname', $cn)
    $tr.SetAttribute('owner', $dn)
    $so = $testResults.CreateElement('system-out')
    $so.AppendChild($testResults.CreateTextNode("[[ATTACHMENT|$baseName.$tn.html]]"))
    $tr.AppendChild($so)
    $ts.AppendChild($tr)
}

function Finalize-UnitTests {
    $testResults.Save("reports/$baseName.results.xml")

    $module = $baseName -replace '.test'
    $testCoverageXML = New-Object xml
    $testCoverageXML.LoadXML('<?xml version="1.0" encoding="utf-8"?>
        <!DOCTYPE coverage SYSTEM "http://cobertura.sourceforge.net/xml/coverage-04.dtd">
        <coverage version="1.0" complexity="0.0">
        </coverage>')
    $testCoverageXML.DocumentElement.SetAttribute('timestamp', (Get-Date -UFormat %s))
    $testCoverageXML.DocumentElement.SetAttribute('lines-valid', ($tvc=$testCoverage.Keys.Count))
    $testCoverageXML.DocumentElement.SetAttribute('lines-covered', ($tcc=($testCoverage.Values | Where-Object {$_.Hits}).Count))
    $testCoverageXML.DocumentElement.SetAttribute('line-rate', $tcc / $tvc)
    $testCoverageXML.DocumentElement.SetAttribute('branches-valid', ($bvc=($testCoverage.Values | % {$_.Branches} | measure -Sum).Sum))
    $testCoverageXML.DocumentElement.SetAttribute('branches-covered', ($bcc=($testCoverage.Values | % {$_.BranchHits.Keys.Count} | measure -Sum).Sum))
    $testCoverageXML.DocumentElement.SetAttribute('branch-rate', $bvc ? $bcc / $bvc : 1)
    $pkgs = $testCoverageXML.CreateElement('packages')
    $pkg = $testCoverageXML.CreateElement('package')
    $pkg.SetAttribute('name', $module)
    $pkg.SetAttribute('line-rate', $tcc / $tvc)
    $pkg.SetAttribute('branch-rate', $bvc ? $bcc / $bvc : 1)
    $pkg.SetAttribute('complexity', '0.0')
    $clss = $testCoverageXML.CreateElement('classes')
    $cls = $testCoverageXML.CreateElement('class')
    $cls.SetAttribute('name', $module)
    $cls.SetAttribute('filename', "reports/$module.logicapp")
    $cls.SetAttribute('line-rate', $tcc / $tvc)
    $cls.SetAttribute('branch-rate', $bvc ? $bcc / $bvc : 1)
    $cls.SetAttribute('complexity', '0.0')
    $mtds = $testCoverageXML.CreateElement('methods')
    $lines = $testCoverageXML.CreateElement('lines')
    rm reports/$module.logicapp
    $testCoverage.GetEnumerator() | % {$i = 1} {
        $line = $testCoverageXML.CreateElement('line')
        $line.SetAttribute('branch', !!$_.Value.Branches)
        $line.SetAttribute('number', $i)
        $line.SetAttribute('hits', $_.Value.Hits)
        $lines.AppendChild($line)
        "Step: $($_.Key), Hits: $($_.Value.Hits), Branches: $($_.Value.Branches), BranchHits: $($_.Value.BranchHits.GetEnumerator())" >> reports/$module.logicapp
        $i++
    }
    $cls.AppendChild($mtds)
    $cls.AppendChild($lines)
    $clss.AppendChild($cls)
    $pkg.AppendChild($clss)
    $pkgs.AppendChild($pkg)
    $testCoverageXML.DocumentElement.AppendChild($pkgs)
    $testCoverageXML.Save("reports/$baseName.coverage.xml")
}
