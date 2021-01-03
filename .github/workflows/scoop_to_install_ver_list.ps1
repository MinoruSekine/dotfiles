Param($in_apps_list_txt_file,$out_to_install_ver_list_txt_file)
$apps_list = Get-Content $in_apps_list_txt_file
$sub_script_path = $PSScriptRoot + "scoop_to_install_ver.ps1"
foreach ($app in $apps_list) {
    echo $app >> $out_to_install_ver_list_txt_file
    $sub_script_path $app >> $out_to_install_ver_list_txt_file
}
