Param($apps_list_txt_file)
$apps_list = Get-Content $apps_list_txt_file
foreach ($app in $apps_list) {
    scoop install $app
}
