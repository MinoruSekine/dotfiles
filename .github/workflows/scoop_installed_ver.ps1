Param($app)
$regexp = "^\s*" + $app + ": (?<version>[\d\.]+?)[^\d\.].*$"
$scoop_status_result = scoop status
foreach ($line in $scoop_status_result) {
    if ($line -match $regexp) {
	break;
    }
}
echo $Matches.version
