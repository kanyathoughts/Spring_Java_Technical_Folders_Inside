param ( [string]$ArchivePath, [string]$DestinationPath )

$global:ProgressPreference = 'SilentlyContinue'
Expand-Archive -LiteralPath $ArchivePath -DestinationPath $DestinationPath -Force