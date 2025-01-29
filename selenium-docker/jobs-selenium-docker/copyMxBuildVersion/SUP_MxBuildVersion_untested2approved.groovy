@Library('TestUtils') _

/**
 * Copies a maxenso build from /Shared Documents/delivery/archive/innowake/ to /Shared Documents/delivery/approved/innowake/.
 * 
 * @param mxBuildVersion The maxenso build which will be moved.
 *        type: Extended Choice 
 * @param folderNameSuffix The suffix which will be added to the name of the folder (e.g.: name of the customer).
 *        type: String
 * 
 */

timestamps {

	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def spUtils = new SharepointUtils()
	def mxVersionUtils = new MxVersionUtils()
	def language = 'en'
	    
	node('OS-Linux && Docker-host') {
		deleteDir()
		docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
			
			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			
			stage('Download files') {
		
				def spFolder = '/Shared Documents/delivery/archive/innowake/'		
				catchError(buildResult: 'UNSTABLE', catchInterruptions: false, message: 'Tests of file download failed', stageResult: 'UNSTABLE') {
					/*Download files*/
					spUtils.downloadFile("${spFolder}${mxBuildVersion}.zip", "${workspace}/")
					if (spUtils.fileExists("${spFolder}innowake-eclipse-complete-${mxBuildVersion}-${language}-win-min.zip") == true) {
						spUtils.downloadFile("${spFolder}innowake-eclipse-complete-${mxBuildVersion}-${language}-win-min.zip", "${workspace}/")
					} else {
						spUtils.downloadFile("${spFolder}innowake-eclipse-complete-${mxBuildVersion}-${language}-win.zip", "${workspace}/")
					}
	                if (spUtils.fileExists("${spFolder}innowake-eclipse-complete-${mxBuildVersion}-${language}-mac-min.zip") == true) {
						spUtils.downloadFile("${spFolder}innowake-eclipse-complete-${mxBuildVersion}-${language}-mac-min.zip", "${workspace}/")
					} else {
						spUtils.downloadFile("${spFolder}innowake-eclipse-complete-${mxBuildVersion}-${language}-mac.zip", "${workspace}/")
					}
					if (spUtils.fileExists("${spFolder}innowake-eclipse-complete-${mxBuildVersion}-${language}-linux-min.zip") == true) {
						spUtils.downloadFile("${spFolder}innowake-eclipse-complete-${mxBuildVersion}-${language}-linux-min.zip", "${workspace}/")
					} else {
						spUtils.downloadFile("${spFolder}innowake-eclipse-complete-${mxBuildVersion}-${language}-linux.zip", "${workspace}/")
					}			
				}
			}
			
			stage('Upload files') {
				
				def spTestBaseFolder = '/Shared Documents/delivery/approved/innowake/'
				def mxBuildSplit = mxBuildVersion.tokenize('.')
				def mxBuildVersionsize = mxBuildSplit.size()-1
				mxBuildSplit.remove(mxBuildVersionsize)
				def mxBuildVersion1 = mxBuildSplit.join('.') 
				def subFolder
				if (folderNameSuffix.isEmpty()) {
					subFolder = "${mxBuildVersion1}"
				} else {
					subFolder = "${mxBuildVersion1} ${folderNameSuffix}"
				}
				def spFolder = "${spTestBaseFolder}${subFolder}"				
				catchError(buildResult: 'UNSTABLE', catchInterruptions: false, message: 'Tests of file upload failed', stageResult: 'UNSTABLE') {
					/* Create folder. */
					spUtils.createFolder(spTestBaseFolder, subFolder)
			
					/*Upload files*/
					def uploadResult = spUtils.uploadFile("${spFolder}/", "${mxBuildVersion}.zip")
					if (uploadResult == false) {
						unstable "Upload of ${mxBuildVersion}.zip failed"
					}
					def uploadResult1 = spUtils.uploadFile("${spFolder}/", "innowake-eclipse-complete-${mxBuildVersion}-${language}-win-min.zip")
					if (uploadResult1 == false) {
						spUtils.uploadFile("${spFolder}/", "innowake-eclipse-complete-${mxBuildVersion}-${language}-win.zip")
					}
					def uploadResult2 = spUtils.uploadFile("${spFolder}/", "innowake-eclipse-complete-${mxBuildVersion}-${language}-mac-min.zip")
					if (uploadResult2 == false) {
						spUtils.uploadFile("${spFolder}/", "innowake-eclipse-complete-${mxBuildVersion}-${language}-mac.zip")
					}
					def uploadResult3 = spUtils.uploadFile("${spFolder}/", "innowake-eclipse-complete-${mxBuildVersion}-${language}-linux-min.zip")
					if (uploadResult3 == false) {
						spUtils.uploadFile("${spFolder}/", "innowake-eclipse-complete-${mxBuildVersion}-${language}-linux.zip")
					}
				}
			}		
		}
	}
}