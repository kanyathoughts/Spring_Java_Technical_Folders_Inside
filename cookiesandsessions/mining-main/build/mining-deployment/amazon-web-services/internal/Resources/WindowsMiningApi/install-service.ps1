param ( [string]$Password, [string]$MiningVersion, [string]$OrientIp, [string]$KeycloakIp )

if ($KeycloakIp -eq $null) {
    KeycloakIp = ""
}

$user = whoami

C:\Users\Administrator\server\Install-Api-Service.bat $user $Password $MiningVersion $OrientIp $KeycloakIp