@Library('TestUtils') _

/**
 * Creates a product package for customers.
 *
 * @param mxBuildVersion The maxenso/innowake build to test, eg: /media/approved/21.0.0 cdhs only
 *  Type: Choice parameter
 * @param customer Name of the customer that will get the product package.
 *  Type: String parameter
 * @param plattform contains the possbile plattforms to run on.
 *  Type: String parameter
 * @param uploadToSharePoint contains true if the artifacts should be uploaded to sharepoint, false otherwise.
 *
 */
node('Docker-host') {
	//default fields
	def mxVersionUtils = new MxVersionUtils()
	def dockerUtils = new DockerUtils()
	def gitUtils = new GitUtils()
	def miscUtils = new MiscUtils()
	def spUtils = new SharepointUtils()
	def prodUtils = new ProductDeliveryUtils()
	def workDir = pwd()

	//initialized fields after initialisation stage
	def sourceFolder        //${mxBuildDirRoot}/${mxBuildSubfolder}, often the same as mxBuild
	def targetFolder        //MyTargetFolder/isHere
	def artifactPrefix

	//initialized fields after parse-and-attribute stage
	//lists containing the paths to the files/folders which are necessary
	def parsedFoldersList = []
	def parsedFilesList = []

	def readDocuLines
	def docuIsGerman = [] as Set
	def toDocuItems = []
	def docuErrors = []
	def language = 'en'
	def fileName = ''
	
	def linuxEclipseZip = mxVersionUtils.getEclipseZip(mxBuildVersion)
	def windowsEclipseZip = linuxEclipseZip.replaceAll("linux-gtk-x86_64.tar.gz", "win32-x86_64.tar.gz")
	def statusWindowsEclipse

	def envImage = docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8'))

	stage('cleanup') {
		deleteDir()
	}
	
	docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
	
		stage('initialisation-linux') {

			mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)

			sourceFolder = "${workspace}/${mxBuildVersion}"
			def productName = (mxBuildVersion > '18') ? 'innowake' : 'maxenso'
			def packageNameExtensions = (customer == 'None') ? '' : customer + '_'
			targetFolder = packageNameExtensions + productName + '_' + mxBuildVersion
			buildName "#${env.BUILD_ID} - ${targetFolder}"
			buildDescription "plattform=${plattform} customer=${customer}"
			sh "mkdir ${targetFolder}"
			artifactPrefix = mxVersionUtils.hasMaxensoInArtifactName(mxBuildVersion) ? 'maxenso' : 'innowake'
			
			def spFolder = '/Shared Documents/delivery/archive/innowake/'
			/*Download file*/
			spUtils.downloadFile("${spFolder}${mxBuildVersion}.zip", "${workspace}/")
			sh "unzip -q ${workspace}/${mxBuildVersion}.zip -d ${workspace}/${mxBuildVersion}"

			//checkout source paths file
			def fileContentAsString
			dir('customer-txtfiles') {
				withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
					def branch = (mxVersion == '99.9') ? 'master' : mxVersion
					sh "git clone --quiet --branch ${branch} ${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/release-management/customer-txtfiles.git ."
					fileContentAsString = readFile "${customer}.txt"
				}
			}

			assert (! fileContentAsString.isEmpty())
			(parsedFilesList, parsedFoldersList) = prodUtils.parseContent(fileContentAsString)
			(parsedFilesList, parsedFoldersList) = prodUtils.attributeSourcePaths(parsedFilesList, parsedFoldersList, mxBuildVersion)
		}
		
		stage('initialisation-windows') {
			if (! spUtils.downloadFile("${spUtils.getBaseEclipseDir()}/${windowsEclipseZip}", './')) {
				error "Download of ${spUtils.getBaseEclipseDir()}/${windowsEclipseZip} failed"
			}
			gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), '.')
		}

		stage('copy-parsed-data') {
			def parsedFilesForNexus = parsedFilesList.findAll{it.contains('nexus')}
			parsedFilesList -= parsedFilesForNexus  
            
			//files which are stored in nexus
			dir('fromNexus') {
				withMaven(maven: 'Default') {
					parsedFilesForNexus.each {
						(fPath, fNameWithEnding) = prodUtils.getFilePathAndFileName(it)
						(fileName, fileVersion, fileType, groupID) = prodUtils.getGroupAndArtifactName(fNameWithEnding, fPath)

						if (fileName.isEmpty() || fileVersion.isEmpty() || fileType.isEmpty() ||groupID.isEmpty()) {
							unstable 'Could not parse all relevant information for nexus download, skipping...'
						} else {
							catchError(buildResult: 'UNSTABLE', message: "Could not build file ${fileName} in version ${fileVersion} from nexus", stageResult: 'UNSTABLE') {
								sh "$MVN_CMD dependency:copy -Dartifact=${groupID}:${fileName}:${fileVersion}:${fileType} -DoutputDirectory=."
							}
						}
					}
				}
               
				if (parsedFilesForNexus.size() > 0) {
					sh "cp -r '.' '../${targetFolder}'"
				}
				deleteDir()
			}
            
			prodUtils.copy(parsedFilesList, parsedFoldersList, sourceFolder, targetFolder)
		}
		
		stage('create-eclipse-artifact') {
			artifactPrefix = mxVersionUtils.hasMaxensoInArtifactName(mxBuildVersion) ? 'maxenso' : 'innowake'
			buildEclipse(windowsEclipseZip, "-${mxBuildVersion}-${language}-windows")
			statusWindowsEclipse = sh returnStatus: true, script: "grep ERROR ${workDir}/tmp-${mxBuildVersion}-${language}-windows/install-tool/installation.log"
			if (statusWindowsEclipse != 1) {
				unstable 'Windows eclipse build failed'
			}
			
			sh "rm ${workDir}/${artifactPrefix}-eclipse-${mxBuildVersion}-${language}-windows/${artifactPrefix}.lic"
			zip dir: "${workDir}/${artifactPrefix}-eclipse-${mxBuildVersion}-${language}-windows", zipFile: "${workDir}/${artifactPrefix}-eclipse-${mxBuildVersion}-${language}-windows.zip"
		}

		stage('create-workspace-artifact') {
			def customerArtifactName = "${customer}_${artifactPrefix}_${mxBuildVersion}.zip"
			zip zipFile: customerArtifactName, archive: false, dir: targetFolder
		}
        
		stage('copy-migration-artifacts') {
			if (customer.contains('GIP_Migration')) {
				copyArtifacts([
					projectName: 'TF_Natural_Java_Script_Signing_KIDICAP_Mini',
					filter: "mee-source-migration-natural-dist-*.zip",
					selector: lastSuccessful(),
					parameters: "mxBuildVersion=${mxBuildVersion}",
					fingerprintArtifacts: true,
				])
			}
		}

		stage('create-documentation-artifact') {
			if (Boolean.parseBoolean(IsDocumentationRequired)) {
				try {
					//read docu name file
					def docuAsString = readFile "customer-txtfiles/${customer}_docu.txt"
					readDocuLines = docuAsString.split('\n')
					readDocuLines = readDocuLines.collect { it.trim() }
					readDocuLines = readDocuLines.findAll {! it.isEmpty()}

					if (readDocuLines.isEmpty()) {
						unstable "Documentation txt is empty!"
					}

					//acquire version information
					if (Integer.parseInt(mxVersion.substring(0, 2)) < 18) {
						unstable 'There is no documentation for versions below 18, skipping stage ...'
					} else {
						readDocuLines.each { docuItem ->
							def curlOutput

							withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-Nexus', passwordVariable: 'nexusPw', usernameVariable: 'nexusUser')]) {
								curlOutput = sh returnStdout: true, script: "curl -X GET http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/innoWake_${docuItem}/ --user \'${nexusUser}:${nexusPw}\' --header \'Accept: application/xml\'"

								//mark special cases which are only available in german for later maven processing
								if (curlOutput.contains('404 - Not Found')) {
									echo "404 - Could not find any documentaion for ${docuItem} in english, trying german..."
									curlOutput = sh returnStdout: true, script: "curl -X GET http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/Anwenderhandbuch_${docuItem}/ --user \'${nexusUser}:${nexusPw}\' --header \'Accept: application/xml\'"
									docuIsGerman += docuItem
								}
							}

							if (curlOutput.contains('404 - Not Found')) {
								unstable "404 - Could not find any documentaion for ${docuItem}"
								docuErrors.add(docuItem)
							} else {
								catchError(buildResult: 'UNSTABLE', message: 'Could not parse documentation xml', stageResult: 'UNSTABLE') {
									def list = prodUtils.parseDocumentation(curlOutput)

									list = prodUtils.attributeDocumentation(list, docuItem)

									list = prodUtils.removeAttributedItems(list, mxVersion)

									if (list.isEmpty()) {
										unstable "Could not find any documentation artifact for ${docuItem} in version ${mxVersion}..."
										docuErrors.add(docuItem)
									} else {
										list = prodUtils.dateSort(list)
									}

									toDocuItems = prodUtils.findItemWithVersion(list, toDocuItems, mxVersion)
								}
							}
						}
					}

					dir('documentation') {
						//download documentation
						withMaven(maven: 'Default') {
							toDocuItems.each {
								def artifactVersion = it.get(2).replace('<text>', '').replace('</text>', '')

								def languagePrefix = (docuIsGerman.contains(it.get(0))) ? 'Anwenderhandbuch' : 'innoWake'
								sh "$MVN_CMD dependency:copy -Dartifact=innowake.documentation:${languagePrefix}_${it.get(0)}:${artifactVersion}:pdf -DoutputDirectory=."
							}
						}

						def lsReturn = sh returnStdout: true, script: 'ls'
						docuFileNames = lsReturn.split('\n').collect { it.trim() }

						prodUtils.renameDocumentation(docuFileNames, mxBuildVersion)
					}

					def documentationArtifactName = "${customer}_${artifactPrefix}_${mxBuildVersion}_documentation.zip"
					zip zipFile: documentationArtifactName, archive: false, dir: 'documentation'

					//output files which no docu could be found
					if (docuErrors.size() > 0) {
						unstable "For the following ${docuErrors.size()} item(s), no documentation could be found:\n${docuErrors}"
					}

				} catch(java.io.IOException ex1) {
					unstable "Could not find ${customer}_docu.txt, skipping stage..."
				}
			}
		}
		
		stage('upload-to-sharepoint') {
			if (Boolean.parseBoolean(uploadToSharePoint)) {
				def libraryName = "Shared Documents/share/QM - Quality Management/customer-delivery/${customer}/"
				if (Boolean.parseBoolean(IsDocumentationRequired)) {
					def uploadResult = spUtils.uploadFile(libraryName, "${customer}_${artifactPrefix}_${mxBuildVersion}_documentation.zip")
					if (uploadResult == false) {
						unstable "Upload of ${customer}_${artifactPrefix}_${mxBuildVersion}_documentation.zip failed"
					}
				}
				def uploadResult1 = spUtils.uploadFile(libraryName, "${customer}_${artifactPrefix}_${mxBuildVersion}.zip")
				if (uploadResult1 == false) {
					unstable "Upload of ${customer}_${artifactPrefix}_${mxBuildVersion}.zip failed"
				}
				def uploadResult2 = spUtils.uploadFile(libraryName, "${artifactPrefix}-eclipse-${mxBuildVersion}-en-windows.zip")
				if (uploadResult2 == false) {
					unstable "Upload of ${artifactPrefix}-eclipse-${mxBuildVersion}-en-windows.zip failed"
				}
				if (customer.contains('GIP_Migration')) {
					def uploadResult3 = spUtils.uploadFile(libraryName, "mee-source-migration-natural-dist-${mxBuildVersion}.zip")
					if (uploadResult3 == false) {
						unstable "Upload of mee-source-migration-natural-dist-${mxBuildVersion}.zip failed"
					}
				}
			} else {
				echo 'Upload to sharepoint is deactivated'
			}
		}
					
		stage('upload-artifact-to-jira') {
			if (ticketNumber.isEmpty() == false) {
				jiraUploadAttachment file:"${customer}_${artifactPrefix}_${mxBuildVersion}.zip" , idOrKey: ticketNumber, site: 'IRIS'
			}
		}
		
		stage('upload-documentation-to-jira') {
			if (Boolean.parseBoolean(IsDocumentationRequired)) {
				if (ticketNumber.isEmpty() == false) {
					jiraUploadAttachment file:"${customer}_${artifactPrefix}_${mxBuildVersion}_documentation.zip" , idOrKey: ticketNumber, site: 'IRIS'
				}
			}
		}
		
		stage('upload-eclipse-to-jira') {
			if (ticketNumber.isEmpty() == false) {
				jiraUploadAttachment file:"${artifactPrefix}-eclipse-${mxBuildVersion}-en-windows.zip" , idOrKey: ticketNumber, site: 'IRIS'
			}
		}
		
		if (customer.contains('GIP_Migration')) {
			stage('upload-mee-source-migration-natural-dist') {
				if (ticketNumber.isEmpty() == false) {
					jiraUploadAttachment file:"mee-source-migration-natural-dist-${mxBuildVersion}.zip" , idOrKey: ticketNumber, site: 'IRIS'
				}
			}
		}
	}
}

/**
 * Copies all files to the workspace, extracts them if necessary, builds the eclipse.
 * @param eclipseZip name of the base eclipse bundle
 * @param suffix The suffix that is added to the eclipse and the temp folder 
 */
def buildEclipse(eclipseZip, suffix) {
	def mxVersionUtils = new MxVersionUtils()
	def artifactPrefix = mxVersionUtils.hasMaxensoInArtifactName(mxBuildVersion) ? 'maxenso' : 'innowake'
	def workDir = pwd()
	def eclipseDir = "${workDir}/${artifactPrefix}-eclipse${suffix}"
	def tmpDir = "${workDir}/tmp${suffix}"
	
	sh "tar -xf ${eclipseZip} -C ${workDir}"
	if (suffix != '') {
		sh "mv ${workDir}/eclipse ${eclipseDir}"
	}
	unzip dir: "${tmpDir}/bundles", quiet: true, zipFile: "${workDir}/${mxBuildVersion}/ndt-mdd-mee/eclipse/${artifactPrefix}-all-plugins-${mxBuildVersion}.zip"	
	unzip dir: "${tmpDir}/bundles", quiet: true, zipFile: "${workDir}/${mxBuildVersion}/ndt-mdd-mee/eclipse/${artifactPrefix}-required-plugins-${mxBuildVersion}.zip"
	sh "cp -r ${workDir}/${mxBuildVersion}/ndt-mdd-mee/eclipse/install-tool_${mxBuildVersion} ${tmpDir}/install-tool"
	
	def installConfiguration = "${tmpDir}/install-tool/install-configuration.ini"
	sh "sed -i \"s|show.tool.ui = true|show.tool.ui = false|g\" ${installConfiguration}"
	sh "sed -i \"s|eclipse.installation.path = C:/Programme/eclipse/|eclipse.installation.path = ${eclipseDir}|g\" ${installConfiguration}"
	sh "sed -i \"s|iw.bundles.path = C:/tmp/unpacked|iw.bundles.path = ${tmpDir}/bundles|g\" ${installConfiguration}"
	sh "sed -i \"s|iw.license.path = C:/Programme/eclipse/maxenso.lic|iw.license.path = ${workDir}/maxenso.lic|g\" ${installConfiguration}"
	
	dir("${tmpDir}/install-tool") {
		sh "java -jar install-lib/lib.jar"
	}
	sh "cat ${tmpDir}/install-tool/installation.log"
}