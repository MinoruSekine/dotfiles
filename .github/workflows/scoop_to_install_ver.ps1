Param($app)
$regexp = "^\s*" + $app + " \((?<version>[\d\.]+?)\).*$"
$scoop_search_result = scoop search $app
foreach ($line in $scoop_search_result) {
    if ($line -match $regexp) {
	break;
    }
}
echo $Matches.version
