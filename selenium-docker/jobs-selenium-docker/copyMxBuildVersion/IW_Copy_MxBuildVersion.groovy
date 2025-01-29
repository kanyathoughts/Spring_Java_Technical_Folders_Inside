import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
@Library('TestUtils') _

/**
 * Pipeline job to download a maxenso/innowake build from Sharepoint and upload the artifacts to sharepoint.
 * The build to be copied is given by a parameter, set by the user that starts the job.
 * If the build to copy is a runtime only build, then the job searches for the previous complete build and copies the missing artifacts from there.
 * 
 *
 * @param mxBuildVersion The build to copy.
 * @param rebuild_number Only used in case of mining rebuild. Passed by the calling job
 * @param forceCopyForRuntimeOnlyBuild If selected the job will copy a runtime only build even if the version is alpha or beta
 */

node('OS-Linux && ! built-in') {

	timestamps() {
		def mxVersionUtils = new MxVersionUtils()
		def spUtils = new SharepointUtils()
		def miscUtils = new MiscUtils()
		def dockerUtils = new DockerUtils()
		def iwDir
		def runtimeOnlyBuildTxt
		def sourceFolder
		def completeSourceFolder
		def isCompleteBuild = false
		def jobWorkspace = pwd()
		def artifactVersion = ''
		def completeSourceFolderWithoutRebuild
		def isAlphaOrBeta = false;
		
		deleteDir()
		
		docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
			try {
				stage('initialisation') {
					
					echo "mxBuildVersion: ${mxBuildVersion}"
					if (! rebuild_number.isEmpty()) {
						echo "rebuild number = ${rebuild_number}"
						artifactVersion = "${mxBuildVersion}-${rebuild_number}"
						echo "set artifact version to ${artifactVersion}"
					}
					buildName "#${env.BUILD_ID} - ${artifactVersion ?: mxBuildVersion}"
					sourceFolder = artifactVersion ?: mxBuildVersion
					iwDir = spUtils.getInnowakeDir()
				}
				
				stage('define-complete-version') {
					if (spUtils.fileExists("${iwDir}/${artifactVersion ?: mxBuildVersion}.zip")) {
						completeSourceFolder = sourceFolder
						isCompleteBuild = true
						completeSourceFolderWithoutRebuild = mxBuildVersion
						echo 'The selected build is already a complete version build.'
					} else if (spUtils.fileExists("${iwDir}/${mxBuildVersion}_sc_and_runtime_only.zip")) {
						def searchString
						if (mxBuildVersion.contains('alpha')) {
							def versionSplit = mxBuildVersion.tokenize('-')
							searchString = versionSplit[0] + '-' + versionSplit[1]
							isAlphaOrBeta = true
						} else if (mxBuildVersion.contains('beta')) {
							def versionSplit = mxBuildVersion.tokenize('-')
							searchString = versionSplit[0] + '-' + versionSplit[1]
							isAlphaOrBeta = true
						} else {
							searchString = mxBuildVersion.substring(0, mxBuildVersion.length() - 2)
						}
						/* Search for a complete build on the same branch */
						def allVersions = spUtils.listFiles("${iwDir}/") as Set
						allVersions = allVersions.findAll { it.matches("${searchString}.*[0-9]\\.zip") }
						allVersions = allVersions.sort()
						if (allVersions.size() == 0) {
							error 'Could not find a complete build version to copy artifacts from.'
						}
						completeSourceFolder = allVersions[allVersions.size() - 1].split('.zip')[0]
						completeSourceFolderWithoutRebuild = completeSourceFolder
						sourceFolder = "${mxBuildVersion}_sc_and_runtime_only"
						runtimeOnlyBuildTxt = "Complete build is taken from ${completeSourceFolder}"
						echo runtimeOnlyBuildTxt
					} else {
						error 'No files found for download.'
					}
				}
			
				if (isAlphaOrBeta && ! Boolean.parseBoolean(forceCopyForRuntimeOnlyBuild)) {
					echo 'Skipped copying a runtime only build for this version'
				} else {
					stage('download-version') {
						if (! spUtils.downloadFile("${iwDir}/${completeSourceFolder}.zip", './')) {
							error "Download of ${iwDir}/${completeSourceFolder}.zip failed"
						}
						sh "unzip ${completeSourceFolder}.zip -d ${completeSourceFolder}" 
						if (! isCompleteBuild) {
							if (! spUtils.downloadFile("${iwDir}/${mxBuildVersion}_sc_and_runtime_only.zip", './')) {
								error "Download of ${iwDir}/${mxBuildVersion}_sc_and_runtime_only.zip failed"
							}
							sh "unzip ${sourceFolder}.zip -d ${sourceFolder}" 
						}
					}
					
					stage('upload-mining-artifacts') {
						if (miscUtils.directoryExists("${completeSourceFolder}/mining")) {
							sh "mkdir -p mining && cp -p -r '${completeSourceFolder}'/mining/* 'mining'"						
							spUtils.uploadJobArtifact(sourceFolder, "mining/orientdb/orientdb-mining-${sourceFolder}.zip")
							spUtils.uploadJobArtifact(sourceFolder, "mining/keycloak/mining-keycloak-extension-${sourceFolder}.jar")
							spUtils.uploadJobArtifact(sourceFolder, "mining/keycloak/mining-keycloak-theme-${sourceFolder}.jar")
							spUtils.uploadJobArtifact(sourceFolder, "mining/server/mining-api-server-dist-${sourceFolder}.jar")
						} else {
							echo "Mining Artifacts unavailable for ${completeSourceFolder}"
						}
					}
					
					stage('upload-all-plugins-and-install-tool') {
						def eclipseSourceFolder = "${completeSourceFolder}/ndt-mdd-mee/eclipse${mxVersionUtils.getEclipseSubfolder(completeSourceFolder)}"
						def artifactPrefix = mxVersionUtils.hasMaxensoInArtifactName(completeSourceFolder) ? 'maxenso' : 'innowake'
						sh "mv '${eclipseSourceFolder}/${artifactPrefix}-all-plugins-${completeSourceFolderWithoutRebuild}.zip' '${eclipseSourceFolder}/${artifactPrefix}-all-plugins.zip'"
						zipFileName = "${eclipseSourceFolder}/${artifactPrefix}-all-plugins.zip"
						spUtils.uploadJobArtifact(sourceFolder, zipFileName)
						sh "rm -rf ${zipFileName}"
						
						if ( ! mxVersionUtils.isSince_iw18(completeSourceFolder) || completeSourceFolder.startsWith('18.0.1')) {
							sh "mv '${eclipseSourceFolder}/${artifactPrefix}-help-plugins-de-${completeSourceFolderWithoutRebuild}.zip' '${eclipseSourceFolder}/${artifactPrefix}-help-plugins-de.zip'"
							zipFileName = "${eclipseSourceFolder}/${artifactPrefix}-help-plugins-de.zip"
							spUtils.uploadJobArtifact(sourceFolder, zipFileName)
							sh "rm -rf ${zipFileName}"
						}
							if (mxVersionUtils.hasRequiredPluginsZip(completeSourceFolder)) {
							sh "mv '${eclipseSourceFolder}/${artifactPrefix}-required-plugins-${completeSourceFolderWithoutRebuild}.zip' '${eclipseSourceFolder}/${artifactPrefix}-required-plugins.zip'"
							zipFileName = "${eclipseSourceFolder}/${artifactPrefix}-required-plugins.zip"
							spUtils.uploadJobArtifact(sourceFolder, zipFileName)
							sh "rm -rf ${zipFileName}"
						}
						sh "mkdir -p install-tool && cp -p -r '${eclipseSourceFolder}/install-tool_${completeSourceFolderWithoutRebuild}'/* 'install-tool'"
						sh "zip -r install-tool.zip install-tool"
						zipFileName = "install-tool.zip"
						spUtils.uploadJobArtifact(sourceFolder, zipFileName)
						sh "rm -rf ${zipFileName}"
					}
					
					if (rebuild_number.isEmpty()) {
						stage('upload-sc-server') {
							sh "mkdir -p soa-server && cp -p -r '${sourceFolder}'/sc/server/* 'soa-server'"
							sh "zip -r soa-server.zip soa-server"
							zipFileName = "soa-server.zip"
							spUtils.uploadJobArtifact(sourceFolder, zipFileName)
							sh "rm -rf ${zipFileName}"
							runtimeOnlyBuildTxt += "\n${sourceFolder}/sc/server/*"
						}
						
						stage('upload-lcm') {
							sh "mv '${completeSourceFolder}/ndt-mdd-mee/server/request-server/lcm-core-${completeSourceFolderWithoutRebuild}.package.zip' '${completeSourceFolder}/ndt-mdd-mee/server/request-server/request-server.zip'"
							zipFileName = "${completeSourceFolder}/ndt-mdd-mee/server/request-server/request-server.zip"
							spUtils.uploadJobArtifact(sourceFolder, zipFileName)
							sh "rm -rf ${zipFileName}"
						}
						
						stage('upload-control-center') {
							sh "mkdir -p control-center && cp -p -r '${sourceFolder}'/sc/control-center/* 'control-center'"
							sh "zip -r control-center.zip control-center"
							zipFileName = "control-center.zip"
							spUtils.uploadJobArtifact(sourceFolder, zipFileName)
							sh "rm -rf ${zipFileName}"
							runtimeOnlyBuildTxt += "\n${sourceFolder}/sc/control-center/*"
						}
						
						stage('upload-lcm-api-client') {
							sh "mkdir -p lcm-api-client && unzip -q '${completeSourceFolder}/ndt-mdd-mee/api/lcm-api-client-${completeSourceFolderWithoutRebuild}/lcm-api-client-${completeSourceFolderWithoutRebuild}.package.zip' -d lcm-api-client"
							sh "zip -r lcm-api-client.zip lcm-api-client"
							zipFileName = "lcm-api-client.zip"
							spUtils.uploadJobArtifact(sourceFolder, zipFileName)
							sh "rm -rf ${zipFileName}"
						}
						
						stage('upload-sc-workbench') {
							sh "mkdir -p workbench && cp -p -r '${sourceFolder}'/sc/workbench/* 'workbench'"
							sh "zip -r workbench.zip workbench"
							zipFileName = "workbench.zip"
							spUtils.uploadJobArtifact(sourceFolder, zipFileName)
							sh "rm -rf ${zipFileName}"
							runtimeOnlyBuildTxt += "\n${sourceFolder}/sc/workbench/*"
						}
						
						 stage('upload-sc-termx-emulator') {
							sh "mkdir -p termx-emulator && cp -p -r '${sourceFolder}'/sc/termx-emulator/* 'termx-emulator'"
							sh "zip -r termx-emulator.zip termx-emulator"
							zipFileName = "termx-emulator.zip"
							spUtils.uploadJobArtifact(sourceFolder, zipFileName)
							sh "rm -rf ${zipFileName}"
							runtimeOnlyBuildTxt += "\n${sourceFolder}/sc/termx-emulator/*"
						}
						
						stage('upload-script-actions') {
							def files = findFiles(glob: "**/*.script-actions")
							for (file in files){
								 zipFileName = "${file}"
								 spUtils.uploadJobArtifact(sourceFolder, zipFileName)
							} 
						}
						
						stage('add-runtime-only-file') {
							if (isCompleteBuild) {
								echo 'No file created because the build is complete.'
							} else {
								sh "echo '${runtimeOnlyBuildTxt}' > runtime-only-build.txt"
								spUtils.uploadJobArtifact(sourceFolder, "runtime-only-build.txt")
							}
						}
					} else {
						Utils.markStageSkippedForConditional('upload-sc-server')
						Utils.markStageSkippedForConditional('upload-lcm')
						Utils.markStageSkippedForConditional('upload-control-center')
						Utils.markStageSkippedForConditional('upload-lcm-api-client')
						Utils.markStageSkippedForConditional('upload-sc-workbench')
						Utils.markStageSkippedForConditional('upload-sc-termx-emulator')
						Utils.markStageSkippedForConditional('upload-script-actions')
						Utils.markStageSkippedForConditional('add-runtime-only-file')
					}
					
					stage('trigger IW_Eclipse_Bundle_Build') {
						if (spUtils.fileExists("${iwDir}/${artifactVersion ?: mxBuildVersion}.zip")) {
							if (! rebuild_number.isEmpty()) {
								mxBuildVersion = artifactVersion
							}
							build job: 'IW_Eclipse_Bundle_Build', parameters: [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion)], wait: true
						} else {
							echo "Skipping IW_Eclipse_Bundle_Build as it is only a runtime build"
						}
					}
				}
			} catch (ex) {
				/* Remove the target folder, so that no partial copies remain. */
				error "exception ${ex}"
			}
		}
	}
}
