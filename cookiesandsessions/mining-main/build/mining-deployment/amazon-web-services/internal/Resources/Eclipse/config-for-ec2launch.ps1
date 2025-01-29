# This script is used to set up the Windows Ec2 instance. This should be run before a new AMI is generated from this instance. 

# Schedules Ec2Launch to run on next boot. Used to generate Administrator password
C:\ProgramData\Amazon\EC2-Windows\Launch\Scripts\InitializeInstance.ps1 -Schedule

# Sends a ready message to the Ec2 console after successful startup
C:\ProgramData\Amazon\EC2-Windows\Launch\Scripts\SendWindowsIsReady.ps1 -Schedule