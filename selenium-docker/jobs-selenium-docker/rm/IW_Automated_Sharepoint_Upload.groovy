@Library('TestUtils') _

/**
 * Uploads the content of a local folder to a library in sharepoint
 * 
 * @param libraryName The sharepoint target libary for the upload
 * @param localFolder The folder that contains the files that need to be uploaded
 * @param emptyLibraryBeforeUpload Delete all files in the remote library before uploading new files
 */

node('Tool-sharepoint') {
	def gitUtils = new GitUtils()
    def filesToUploadDir = "${pwd()}\\filesToUpload\\"
    def networkDrive = 'Z:'
	buildDescription "libraryName=${libraryName} localFolder=${localFolder} emptyLibraryBeforeUpload=${emptyLibraryBeforeUpload}"
	
	try {
		stage('connect shared drive') {
		    withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-Shared-Network-Drive', passwordVariable: 'pw', usernameVariable: 'user')]) {
		        retry(3) {
				    bat "net use ${networkDrive} \\\\vm-pd-build.innowake.hq\\sharefile-sync /user:INNOWAKE.HQ\\${user} ${pw}"
				}
			}
		}
		
		stage('prepare workspace') {
		    deleteDir()
			gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/release-management/automated-sharepoint-upload.git", 'master')
			bat "xcopy /s ${localFolder} ${filesToUploadDir}"
		}
	
		stage('execute powershell script') {
    		withCredentials([usernamePassword(credentialsId: 'USAppModQMUserSVC-at-deloitte-com-User-PW', passwordVariable: 'password', usernameVariable: 'username')]) {
    		    retry(3) {
    			    powershell script: "./uploadToSharepoint.ps1 ${username} '${password}' '${libraryName}' '${filesToUploadDir}' \$${emptyLibraryBeforeUpload}"
    			}
    		}
		}
	} catch (ex) {
		unstable "Exception ${ex}"
	} finally {
		stage('disconnect shared drive') {
			bat returnStatus: true, script: "net use ${networkDrive} /delete"
		}
	}
}
