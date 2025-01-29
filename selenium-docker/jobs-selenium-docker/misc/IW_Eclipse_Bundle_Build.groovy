@Library('TestUtils') _

/**
 * Builds an innowake bundle for a specific product version, both for Windows and Linux, publishes them as artifacts and uploads them to Sharepoint.
 * @param mxBuildVersion The product build the Eclipse bundles shall be built for
 * @param javaVersion The java version the the eclipse will be built with
 */

node('Docker-host') {

	timestamps {
		
	    def mxVersionUtils = new MxVersionUtils()
	    def gitUtils = new GitUtils()
	    def dockerUtils = new DockerUtils()
	    def spUtils = new SharepointUtils()
	    def miscUtils = new MiscUtils()
	
	    buildName "#${BUILD_NUMBER} - ${mxBuildVersion}"
	
	    def workDir = pwd()
	
	    def linuxEclipseZip = mxVersionUtils.getEclipseZip(mxBuildVersion)
	    def windowsEclipseZip = linuxEclipseZip.replaceAll("linux-gtk-x86_64.tar.gz", "win32-x86_64.tar.gz")
	    
	    def statusLinuxEclipse
	    def statusWindowsEclipse
	
	    deleteDir()
	    docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
	
	        stage('init') {
	            if (! spUtils.downloadFile("${spUtils.getBaseEclipseDir()}/${linuxEclipseZip}", './')) {
	            	error "Download of ${spUtils.getBaseEclipseDir()}/${linuxEclipseZip} failed"
	            }
	            if (! spUtils.downloadFile("${spUtils.getBaseEclipseDir()}/${windowsEclipseZip}", './')) {
	            	error "Download of ${spUtils.getBaseEclipseDir()}/${windowsEclipseZip} failed"
	            }
	            gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), '.')
	            copyArtifacts([
	                    projectName         : 'TF_Build_Eclipse_Anttasks_Plugin',
	                    filter              : '**/*.jar',
	                    selector            : lastSuccessful(),
	                    parameters          : "withComSunXmlBindLibrary=${mxVersionUtils.isSince_iw19(mxBuildVersion)}",
	                    target              : workDir,
	                    fingerprintArtifacts: true,
	                    flatten             : true
	            ])

				spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'innowake-all-plugins.zip', './')
    			spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'innowake-required-plugins.zip', './')
    			spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'install-tool.zip', './')
	        }
	        
	        stage('build innowake-Eclipse for windows') {
				buildEclipse(windowsEclipseZip, '-windows')
	            statusWindowsEclipse = sh returnStatus: true, script: "grep ERROR ${workDir}/tmp-windows/install-tool/installation.log"
	            if (statusWindowsEclipse != 1) {
	                unstable 'Windows eclipse build failed'
	            }
	        }
	        
	        stage('upload Eclipse for Windows') {
				when(statusWindowsEclipse == 1, 'Build of Windows Eclipse failed') {
	            	def zipFileName = "eclipse-windows.zip"
		           	zip dir: "${workDir}/eclipse-windows", zipFile: zipFileName
		           	def uploadResult = spUtils.uploadJobArtifact(mxBuildVersion, zipFileName)
		           	echo "uploadResult = ${uploadResult}"
		           	if ( ! uploadResult) {
		                echo 'Upload of Windows eclipse failed'
		           	}
				}
	        }
	
	        stage('build innowake-Eclipse for linux') {
	        	buildEclipse(linuxEclipseZip, '')
	            statusLinuxEclipse = sh returnStatus: true, script: "grep ERROR ${workDir}/tmp/install-tool/installation.log"
	            if (statusLinuxEclipse != 1) {
	                unstable 'Linux eclipse build failed'
	            }
	        }
	        
	        stage('upload Eclipse for Linux') {
				when(statusLinuxEclipse == 1, 'Build of Linux Eclipse failed') {
	            	def zipFileName = "eclipse-linux.zip"
		           	zip dir: "${workDir}/eclipse", zipFile: zipFileName
		           	def uploadResult = spUtils.uploadJobArtifact(mxBuildVersion, zipFileName)
		           	echo "uploadResult = ${uploadResult}"
		           	if ( ! uploadResult) {
		                echo 'Upload of Linux eclipse failed'
		           	}
				}
	        }
	
	    }
	}
}

/**
 * Copies all files to the workspace, extracts them if necessary, adjusts
 * install-configuration.ini, builds the eclipse. Afterwards the method 
 * modifies the eclipse.ini and copies the innowake-eclipse-anttask to the bundle.
 * @param eclipseZip name of the base eclipse bundle
 * @param suffix The suffix that is added to the eclipse and the temp folder 
 */
def buildEclipse(eclipseZip, suffix) {
	def mxVersionUtils = new MxVersionUtils()
	def workDir = pwd()
	def eclipseDir = "${workDir}/eclipse${suffix}"
	def tmpDir = "${workDir}/tmp${suffix}"
	
	sh "tar -xf ${eclipseZip} -C ${workDir}"
	if (suffix != '') {
		sh "mv ${workDir}/eclipse ${eclipseDir}"
	}
	def artifactPrefix = mxVersionUtils.hasMaxensoInArtifactName(mxBuildVersion) ? 'maxenso' : 'innowake'

	unzip dir: eclipseDir, quiet: true, zipFile: "innowake-required-plugins.zip"
	unzip dir: "${tmpDir}/bundles", quiet: true, zipFile: "innowake-all-plugins.zip"
	unzip dir: "${tmpDir}", quiet: true, zipFile: "install-tool.zip"
	
	def installConfiguration = "${tmpDir}/install-tool/install-configuration.ini"
	sh "sed -i \"s|show.tool.ui = true|show.tool.ui = false|g\" ${installConfiguration}"
	sh "sed -i \"s|eclipse.installation.path = C:/Programme/eclipse/|eclipse.installation.path = ${eclipseDir}|g\" ${installConfiguration}"
	sh "sed -i \"s|iw.bundles.path = C:/tmp/unpacked|iw.bundles.path = ${tmpDir}/bundles|g\" ${installConfiguration}"
	sh "sed -i \"s|iw.license.path = C:/Programme/eclipse/maxenso.lic|iw.license.path = ${workDir}/maxenso.lic|g\" ${installConfiguration}"
	
	dir("${tmpDir}/install-tool") {
		sh "java -jar install-lib/lib.jar"
	}
	sh "cat ${tmpDir}/install-tool/installation.log"
	
	def eclipseIni = "${eclipseDir}/eclipse.ini"
	sh "sed -i \"s|-Xms.*|-Xms1024m|g\" ${eclipseIni}"
	sh "sed -i \"s|-Xmx.*|-Xmx4096m|g\" ${eclipseIni}"
	sh "echo '-Dorg.eclipse.swt.internal.gtk.cairoGraphics=false' >> ${eclipseIni}"
	/* Disable tip-of-the-day */
	sh "echo '-Dorg.eclipse.tips.startup.default=disable' >> ${eclipseIni}"
	
	sh "cp ${workDir}/innowake-eclipse-anttask.jar ${eclipseDir}/dropins"
}
