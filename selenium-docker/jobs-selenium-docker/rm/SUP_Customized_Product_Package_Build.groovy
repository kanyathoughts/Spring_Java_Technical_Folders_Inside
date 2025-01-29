@Library("TestUtils") _

/**
 * Creates a product package for customers.
 * 
 * @param mxBuild The maxenso/innowake build to test
 * @param documentation If set the available user manuals are copied to the product package
 * @param customer Name of the customer that will get the product package. 
 * If 'None' is selected package will contain all available products and user manuals
 */

node('Docker-host') {
    def mxVersionUtils = new MxVersionUtils()
    def dockerUtils = new DockerUtils()
    
    def mxBuildVersion
    def sourceFolder
    def targetFolder
    def productName
    def mxVersion
	def artifactPrefix
	def runtimeArtifactPrefix
    
    def envImage = docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8'))
	envImage.inside('-v /data/mxJars:/data/mxJars:ro -v /media/untested:/media/untested -v /media/approved:/media/approved') {
    	stage('Initialisation') {
    		sh 'rm -rf *'
    		echo "mxBuild: ${mxBuild}"
    		/* mxBuild is a subfolder of the form /media/untested/7.1.0.12 or /media/approved/16.0.0.12.
    		 * This has to be split. mxBuildDirRoot has to receive either /media/untested or /media/approved,
    		 * and from mxBuild the prefix has to be stripped.
    		 */
    		def mxBuildSplit = mxBuild.tokenize('/');
    		def mxBuildDirRoot = "/media/${mxBuildSplit[1]}"
    		mxBuildSubfolder = mxBuildSplit[2]
    		/* Sometimes a build is named like "approved/16.0.1.01 for StMELF only".
    		 * In such cases the descriptive name fragments shall be discarded.
    		 */
    		mxBuildSplit = mxBuildSubfolder.tokenize(' ')
    		mxBuildVersion = mxBuildSplit[0]
    		sourceFolder = "${mxBuildDirRoot}/${mxBuildSubfolder}"
    		echo "mxBuildVersion ${mxBuildVersion}"
    		mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
    		productName = (mxBuildVersion > '18') ? 'innowake' : 'maxenso'
    		def packageNameExtensions = (customer == 'None') ? '' : customer + '_' 
    		targetFolder = packageNameExtensions + productName + '_' + mxBuildVersion
    		buildName "#${env.BUILD_ID} - ${targetFolder}"
    		buildDescription "plattform=${plattform} documentation=${documentation} customer=${customer}"
    		sh "mkdir ${targetFolder}"
			if (mxVersionUtils.hasMaxensoInArtifactName(mxBuildVersion)) {
				artifactPrefix = 'maxenso'
				runtimeArtifactPrefix = 'innowake-maxenso'
			} else {
				artifactPrefix = 'innowake'
				runtimeArtifactPrefix = 'innowake'
			}
    	}
    	
    	stage('Copy folders and jars') {
    		sh "cp -r '${sourceFolder}/ndt-mdd-mee/api' ${targetFolder}"
    		sh "cp -r '${sourceFolder}/sc' ${targetFolder}"
    		sh "cp -r '${sourceFolder}/ndt-mdd-mee/server/ndt-services' ${targetFolder}/sc"
    		sh "cp -r '${sourceFolder}/ndt-mdd-mee/server/request-server' ${targetFolder}/sc"
    		sh "cp -r '${sourceFolder}/ndt-mdd-mee/expert' ${targetFolder}"
        }
    	
    	stage('Copy-eclipse') {
    		def pluginsDir = "${targetFolder}/eclipse/plugins"
    		def compilerExtensionDir = "${targetFolder}/mee-compiler-extensions"
    		sh "mkdir -p ${pluginsDir}"
    		sh "mkdir -p ${compilerExtensionDir}"
    		def eclipseSourceFolder = "${sourceFolder}/ndt-mdd-mee/eclipse${mxVersionUtils.getEclipseSubfolder(mxBuildVersion)}"
    		sh "cp '${eclipseSourceFolder}/${artifactPrefix}-product-plugins-${mxBuildVersion}.zip' '${pluginsDir}/${artifactPrefix}-product-plugins-${mxBuildVersion}.zip'"
    		sh "cp -r '${eclipseSourceFolder}/${mxVersionUtils.getCompilerExtensionsFileNamePrefix(mxBuildVersion)}-${mxBuildVersion}.package.zip' '${compilerExtensionDir}/mee-compiler-extensions-bundle-${mxBuildVersion}.zip'"
    		sh "cp -r '${eclipseSourceFolder}/install-tool_${mxBuildVersion}' '${targetFolder}/eclipse/install-tool_${mxBuildVersion}'"
    		
    		def language = 'en'
    		if (customer == 'ZDF') {
    			language = 'de'
    			sh "cp '${eclipseSourceFolder}/${artifactPrefix}-help-plugins-${language}-${mxBuildVersion}.zip' '${pluginsDir}/${artifactPrefix}-help-plugins-${language}-${mxBuildVersion}.zip'"
    		}
    		
    		def workDir = pwd()
    		withAnt(installation: 'Default') {
    			/* 1. Passing the property
    			 *      -Dcontrib-jar=${env.ANT_HOME}/lib/ant-contrib.jar
    			 *    overcomes an issue of the ant script called here. There is a check in the ant script, that ant-contrib of a specific version is available.
    			 *    The version is hard-coded, but can be overwritten by this property.
    			 * 2. The ant script refers to an environment variable I_IW, which contains a network file system path.
    			 *    The environment variable is currently configured in the central Jenkins configuration. So don't try to find it in this script.
    			 */
    		    env.IW_I = '/media/approved'
    			sh "ant -f '${eclipseSourceFolder}/create-${artifactPrefix}-eclipse-bundle.xml' -Dplattform=${plattform[0]} -Dlocale=${language[0]} -Dbundle-type=c -Doutpath=${workDir}/${targetFolder}/eclipse -DexcludeMee=n"
    		}
    		sh "mv ${targetFolder}/eclipse/${artifactPrefix}*.zip ${targetFolder}/eclipse/${productName}-eclipse-${mxBuildVersion}-${language}-${plattform}.zip"
    	}
    	
	    stage('Modify package for customer') {
	        def expectedFileCount
	        def documentationPaths
	        //Delete pom files
	        sh "find . -name '*.pom' -delete"
	        sh "find . -name '*pom.xml' -delete"
			// Delete standard eclipse to reduce package size. Customer does not need this bare eclipse
			sh "rm ${targetFolder}/eclipse/eclipse-SDK*.zip"
	        
	        switch (customer) {
	            case 'ZDF':
	                expectedFileCount = 381
	                sh "rm -r ${targetFolder}/api/mdd-api-plugin-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/expert/ndt-fieldtracing-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/expert/mee-source-migration-${mxBuildVersion}/natural-${mxBuildVersion}.script-actions"
	                sh "rm -r ${targetFolder}/expert/mee-source-migration-${mxBuildVersion}/cobol-${mxBuildVersion}.script-actions"
	                sh "rm -r ${targetFolder}/sc/broker/cobol"
	                sh "rm -r ${targetFolder}/sc/broker/dotnet"
	                sh "rm -r ${targetFolder}/sc/broker/extended"
	                sh "rm -r ${targetFolder}/sc/doc"
	                sh "rm -r ${targetFolder}/sc/servicehandler"
	                sh "rm -r ${targetFolder}/sc/termx-emulator"
	                sh "rm -r ${targetFolder}/sc/workbench"
	                documentationPaths = [
	                    "/base/installation-guidelines-docbook/docbook_de/output/pdf/Installationsanleitung_innoWake_${mxVersion}.pdf",
	            		"/base/system-requirements-docbook/docbook_de/output/pdf/Systemvoraussetzungen_innoWake_${mxVersion}.pdf",
	            		"/mee/mee-batch-docbook/docbook_de/output/pdf/Anwenderhandbuch_Batch_${mxVersion}.pdf",
	            		"/mee/mee-natural-docbook/docbook_de/output/pdf/Anwenderhandbuch_meeclipse-Natural_${mxVersion}.pdf",
	            		"/ndt/ndt-lcm-docbook/docbook_de/output/pdf/Anwenderhandbuch_lifecycle-manager_${mxVersion}.pdf",
	            		"/ndt/ndt-natclipse-docbook/docbook_de/output/pdf/Anwenderhandbuch_natclipse_${mxVersion}.pdf",
	            		"/sc/soa-connector-docbook/docbook_de/output/pdf/Anwenderhandbuch_soa-connector_${mxVersion}.pdf"
	            	]
	                break
	            case 'GIP':
	                expectedFileCount = 52
	                if (mxBuildVersion >= '19.2') {
	                	sh "rm -r ${targetFolder}/api/base-performance-workbench-${mxBuildVersion}"
	                }
	                sh "rm -r ${targetFolder}/api/mdd-api-plugin-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/expert"
	                sh "rm -r ${targetFolder}/sc"
					documentationPaths = [
						"/base/installation-guidelines-docbook/docbook_en/output/pdf/installation-guidelines_innoWake_${mxVersion}.pdf",
	            		"/base/system-requirements-docbook/docbook_en/output/pdf/system-requirements_innoWake_${mxVersion}.pdf",
	            		"/mee/mee-natural-docbook/docbook_en/output/pdf/user-manual_meeclipse-Natural_${mxVersion}.pdf",
	            		"/ndt/ndt-lcm-docbook/docbook_en/output/pdf/user-manual_lifecycle-manager_${mxVersion}.pdf",
	            		"/ndt/ndt-natanalyzer-docbook/docbook_en/output/pdf/user-manual_natanalyzer_${mxVersion}.pdf",
	            		"/ndt/ndt-natclipse-docbook/docbook_en/output/pdf/user-manual_natclipse_${mxVersion}.pdf"
	            	]
	                break
	            case 'RZV':
	                expectedFileCount = 42
	                if (mxBuildVersion >= '19.2') {
	                    sh "rm -r ${targetFolder}/api/base-performance-workbench-${mxBuildVersion}"
	                }
	                sh "rm -r ${targetFolder}/api/lcm-api-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/api/lcm-api-client-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/api/mdd-api-plugin-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/api/ndt-lcm-plugin-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/expert"
	                sh "rm -r ${targetFolder}/mee-compiler-extensions"
	                sh "rm -r ${targetFolder}/sc"
					documentationPaths = [
						"/base/installation-guidelines-docbook/docbook_en/output/pdf/installation-guidelines_innoWake_${mxVersion}.pdf",
	            		"/base/system-requirements-docbook/docbook_en/output/pdf/system-requirements_innoWake_${mxVersion}.pdf",
	            		"/mee/mee-natural-docbook/docbook_en/output/pdf/user-manual_meeclipse-Natural_${mxVersion}.pdf",
	            		"/ndt/ndt-natclipse-docbook/docbook_en/output/pdf/user-manual_natclipse_${mxVersion}.pdf"
	            	]
	                break
	            case 'KRZ-SWD':
	                expectedFileCount = 49
	                sh "rm -r ${targetFolder}/api/lcm-api-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/api/lcm-api-client-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/api/mdd-api-plugin-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/api/ndt-lcm-plugin-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/expert/ndt-fieldtracing-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/expert/mee-source-migration-${mxBuildVersion}/natural-${mxBuildVersion}.script-actions"
	                sh "rm -r ${targetFolder}/expert/mee-source-migration-${mxBuildVersion}/cobol-${mxBuildVersion}.script-actions"
	                sh "rm -r ${targetFolder}/sc"
					documentationPaths = [
						"/base/installation-guidelines-docbook/docbook_en/output/pdf/installation-guidelines_innoWake_${mxVersion}.pdf",
	            		"/base/system-requirements-docbook/docbook_en/output/pdf/system-requirements_innoWake_${mxVersion}.pdf",
	            		"/mee/mee-natural-docbook/docbook_en/output/pdf/user-manual_meeclipse-Natural_${mxVersion}.pdf",
	            		"/ndt/ndt-natclipse-docbook/docbook_en/output/pdf/user-manual_natclipse_${mxVersion}.pdf",
	            	]
	                break
	            case 'Kohls':
	                expectedFileCount = 395
	                sh "rm -r ${targetFolder}/api/lcm-api-client-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/api/mdd-api-plugin-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/api/ndt-lcm-plugin-${mxBuildVersion}"
	                sh "rm -r ${targetFolder}/expert"
	                sh "rm -r ${targetFolder}/sc/broker/cobol"
	                sh "rm -r ${targetFolder}/sc/broker/dotnet"
	                sh "rm -r ${targetFolder}/sc/broker/extended"
	                sh "rm -r ${targetFolder}/sc/doc"
	                sh "rm -r ${targetFolder}/sc/request-server"
	                sh "rm -r ${targetFolder}/sc/servicehandler"
	                sh "rm -r ${targetFolder}/sc/workbench"
	                //TODO remove unnecessary user manuals
					documentationPaths = [
						"/base/installation-guidelines-docbook/docbook_en/output/pdf/installation-guidelines_innoWake_${mxVersion}.pdf",
	            		"/base/system-requirements-docbook/docbook_en/output/pdf/system-requirements_innoWake_${mxVersion}.pdf",
	            		"/mdd/mdd-mab-docbook/docbook_en/output/pdf/user-manual_application-builder_${mxVersion}.pdf",
	            		"/mdd/mdd-vaadin-docbook/docbook_en/output/pdf/user-manual_vaadin_${mxVersion}.pdf",
	            		"/mee/mee-batch-docbook/docbook_en/output/pdf/user-manual_Batch_${mxVersion}.pdf",
	            		"/mee/mee-cobol-docbook/docbook_en/output/pdf/user-manual_meeclipse-Cobol_${mxVersion}.pdf",
	            		"/mee/mee-databridge-docbook/docbook_en/output/pdf/user-manual_databridge_${mxVersion}.pdf",
	            		"/mee/mee-natural-docbook/docbook_en/output/pdf/user-manual_meeclipse-Natural_${mxVersion}.pdf",
	            		"/mee/mee-vsam-docbook/docbook_en/output/pdf/user-manual_VSAM_${mxVersion}.pdf",
	            		"/ndt/ndt-adaclipse-docbook/docbook_en/output/pdf/user-manual_adaclipse_${mxVersion}.pdf",
	            		"/ndt/ndt-batchclipse-docbook/docbook_en/output/pdf/user-manual_batchclipse_${mxVersion}.pdf",
	            		"/ndt/ndt-cobolclipse-docbook/docbook_en/output/pdf/user-manual_cobolclipse_${mxVersion}.pdf",
	            		"/ndt/ndt-fieldtracer-docbook/docbook_en/output/pdf/user-manual_fieldtracer_${mxVersion}.pdf",
	            		"/ndt/ndt-lcm-docbook/docbook_en/output/pdf/user-manual_lifecycle-manager_${mxVersion}.pdf",
	            		"/ndt/ndt-natanalyzer-docbook/docbook_en/output/pdf/user-manual_natanalyzer_${mxVersion}.pdf",
	            		"/ndt/ndt-natclipse-docbook/docbook_en/output/pdf/user-manual_natclipse_${mxVersion}.pdf",
	            		"/sc/soa-connector-docbook/docbook_en/output/pdf/user-manual_soa-connector_${mxVersion}.pdf",
	            		"/sc/termx-docbook/docbook_en/output/pdf/user-manual_termx_${mxVersion}.pdf"
	            	]
	                break
				case 'Bela':
					expectedFileCount = 403
					sh "rm -r ${targetFolder}/api/${runtimeArtifactPrefix}-custom-default-${mxBuildVersion}.jar"
					sh "rm -r ${targetFolder}/api/${runtimeArtifactPrefix}-deps-general-${mxBuildVersion}.jar"
					sh "rm -r ${targetFolder}/api/${runtimeArtifactPrefix}-deps-hibernate-${mxBuildVersion}.jar"
					sh "rm -r ${targetFolder}/api/${runtimeArtifactPrefix}-deps-jasper-${mxBuildVersion}.jar"
					sh "rm -r ${targetFolder}/api/${runtimeArtifactPrefix}-deps-jta-${mxBuildVersion}.jar"
					sh "rm -r ${targetFolder}/api/${runtimeArtifactPrefix}-deps-logging-${mxBuildVersion}.jar"
					sh "rm -r ${targetFolder}/api/${runtimeArtifactPrefix}-deps-test-${mxBuildVersion}.jar"
					sh "rm -r ${targetFolder}/api/${runtimeArtifactPrefix}-deps-vaadin-${mxBuildVersion}.jar"
					sh "rm -r ${targetFolder}/api/${runtimeArtifactPrefix}-deps-vaadin-push-${mxBuildVersion}.jar"
					sh "rm -r ${targetFolder}/api/${runtimeArtifactPrefix}-runtime-${mxBuildVersion}-javadoc.jar"
					sh "rm -r ${targetFolder}/api/${runtimeArtifactPrefix}-runtime-dist-${mxBuildVersion}.jar"
					sh "rm -r ${targetFolder}/api/innowake-mee-udf-oracle5-${mxBuildVersion}.jar"
					sh "rm -r ${targetFolder}/api/lcm-api-client-${mxBuildVersion}"
					sh "rm -r ${targetFolder}/api/mdd-api-plugin-${mxBuildVersion}"
					sh "rm -r ${targetFolder}/api/ndt-lcm-plugin-${mxBuildVersion}"
					sh "rm -r ${targetFolder}/api/symmetric-ebcdic-charsets-${mxBuildVersion}"
					sh "rm -r ${targetFolder}/expert"
					sh "rm -r ${targetFolder}/mee-compiler-extensions"
					sh "rm -r ${targetFolder}/sc/broker/cobol"
					sh "rm -r ${targetFolder}/sc/broker/dotnet"
					sh "rm -r ${targetFolder}/sc/broker/extended"
					sh "rm -r ${targetFolder}/sc/request-server"
					sh "rm -r ${targetFolder}/sc/servicehandler"
					sh "rm -r ${targetFolder}/sc/termx-emulator"
					withMaven(maven: 'Default') {
						def groupIdPrefix = mxVersionUtils.hasMaxensoInArtifactName(mxBuildVersion) ? 'innowake.products.maxenso.runtime' : 'innowake.products.runtime'
						sh "$MVN_CMD dependency:copy -Dartifact=${groupIdPrefix}.core:${artifactPrefix}-runtime-core-natural-dist:${mxBuildVersion} -DoutputDirectory=${targetFolder}/api"
						sh "$MVN_CMD dependency:copy -Dartifact=${groupIdPrefix}.swing:${artifactPrefix}-runtime-swing-natural-dist:${mxBuildVersion} -DoutputDirectory=${targetFolder}/api"
					}
					documentationPaths = [
						"/base/installation-guidelines-docbook/docbook_en/output/pdf/installation-guidelines_innoWake_${mxVersion}.pdf",
						"/base/system-requirements-docbook/docbook_en/output/pdf/system-requirements_innoWake_${mxVersion}.pdf",
						"/mdd/mdd-mab-docbook/docbook_en/output/pdf/user-manual_application-builder_${mxVersion}.pdf",
						"/mdd/mdd-vaadin-docbook/docbook_en/output/pdf/user-manual_vaadin_${mxVersion}.pdf",
						"/ndt/ndt-adaclipse-docbook/docbook_en/output/pdf/user-manual_adaclipse_${mxVersion}.pdf",
						"/ndt/ndt-natanalyzer-docbook/docbook_en/output/pdf/user-manual_natanalyzer_${mxVersion}.pdf",
						"/ndt/ndt-natclipse-docbook/docbook_en/output/pdf/user-manual_natclipse_${mxVersion}.pdf",
						"/sc/soa-connector-docbook/docbook_en/output/pdf/user-manual_soa-connector_${mxVersion}.pdf",
						"/mdd/mdd-guiimporter-docbook/docbook_de/output/pdf/Anwenderhandbuch_gui-importer_${mxVersion}.pdf"
					]
					break
	            default:
	                expectedFileCount = 684
	                echo 'This is a full package. Nothing will be removed!'
					documentationPaths = [
						"/base/installation-guidelines-docbook/docbook_en/output/pdf/installation-guidelines_innoWake_${mxVersion}.pdf",
	            		"/base/system-requirements-docbook/docbook_en/output/pdf/system-requirements_innoWake_${mxVersion}.pdf",
	            		"/mdd/mdd-mab-docbook/docbook_en/output/pdf/user-manual_application-builder_${mxVersion}.pdf",
	            		"/mdd/mdd-vaadin-docbook/docbook_en/output/pdf/user-manual_vaadin_${mxVersion}.pdf",
	            		"/mee/mee-batch-docbook/docbook_en/output/pdf/user-manual_Batch_${mxVersion}.pdf",
	            		"/mee/mee-cobol-docbook/docbook_en/output/pdf/user-manual_meeclipse-Cobol_${mxVersion}.pdf",
	            		"/mee/mee-databridge-docbook/docbook_en/output/pdf/user-manual_databridge_${mxVersion}.pdf",
	            		"/mee/mee-natural-docbook/docbook_en/output/pdf/user-manual_meeclipse-Natural_${mxVersion}.pdf",
	            		"/mee/mee-vsam-docbook/docbook_en/output/pdf/user-manual_VSAM_${mxVersion}.pdf",
	            		"/ndt/ndt-adaclipse-docbook/docbook_en/output/pdf/user-manual_adaclipse_${mxVersion}.pdf",
	            		"/ndt/ndt-batchclipse-docbook/docbook_en/output/pdf/user-manual_batchclipse_${mxVersion}.pdf",
	            		"/ndt/ndt-cobolclipse-docbook/docbook_en/output/pdf/user-manual_cobolclipse_${mxVersion}.pdf",
	            		"/ndt/ndt-fieldtracer-docbook/docbook_en/output/pdf/user-manual_fieldtracer_${mxVersion}.pdf",
	            		"/ndt/ndt-lcm-docbook/docbook_en/output/pdf/user-manual_lifecycle-manager_${mxVersion}.pdf",
	            		"/ndt/ndt-natanalyzer-docbook/docbook_en/output/pdf/user-manual_natanalyzer_${mxVersion}.pdf",
	            		"/ndt/ndt-natclipse-docbook/docbook_en/output/pdf/user-manual_natclipse_${mxVersion}.pdf",
	            		"/sc/soa-connector-docbook/docbook_en/output/pdf/user-manual_soa-connector_${mxVersion}.pdf",
	            		"/sc/termx-docbook/docbook_en/output/pdf/user-manual_termx_${mxVersion}.pdf"
	            	]
	                break
	        }
	        if (Boolean.parseBoolean(documentation)) {
	            getDocumentation(mxVersion, documentationPaths, targetFolder)
	            expectedFileCount += documentationPaths.size() + 1
	        }
	        if (mxVersion >= '19.2') {
	            /* Since 19.2 an additional jar file has to be expected: innowake-maxenso-deps-jre-<version>.jar resp. innowake-deps-jre-<version>.jar */
	            expectedFileCount += 1
	        }
	        checkFileCount(expectedFileCount)
	    }
    	
	    stage('Create zip file') {
	        zip zipFile: "${targetFolder}.zip", archive: false, dir: '.'
	        archiveArtifacts '*.zip'
	    }
	}
}

/**
 * Counts all files and folder that will be part of the product package.
 * If the actual fiele count does not match the expected file count the build
 * result will be 'Unstable' and further information will be printed to the console.
 * 
 * @param expectedFileCount Amount of expected files and folder in the package
 */
def checkFileCount(expectedFileCount) {
    def actualFileCount = sh returnStdout: true, script: 'find | wc -l'
    actualFileCount = actualFileCount.replace('\n', '').replace('\r', '')
    echo "Actual file count = ${actualFileCount}\nExpected file count = ${expectedFileCount}"
    if (Integer.parseInt(actualFileCount) != expectedFileCount) {
        unstable 'Actual and expected file count don\'t match. This needs to be investigated!'
    }
}

/**
 * Checks out the whole documentation branch and copies all specified user manuals to a documentation folder.
 * If user manuals are not available the build result will be 'Unstable' and further
 * information will be printed to the console.
 * 
 * @param mxVersion The maxenso/innowake branch to test
 * @param documentationPaths Array that contains the user manuals that need to be checked out
 * @param targetFolder Root folder of the product package
 */
def getDocumentation(mxVersion, documentationPaths, targetFolder) {
	def gitUtils = new GitUtils()
	def docuUrl
	withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
		docuUrl = gitUtils.getGitUrlDocumentation('USAppModQMUserSVC-Access-Token', gitlabToken)
	}
	sh "git clone --branch ${mxVersion} ${docuUrl}"
	
    sh "mkdir ${targetFolder}/documentation"
	def notAvailableUserManuals = []
	
	documentationPaths.each { documentationPath ->
        try {
			sh "cp documentation${documentationPath} ${targetFolder}/documentation"
        } catch (ex) {
			notAvailableUserManuals.add(documentationPath)
			echo documentationPath + ' does not exist'
            currentBuild.result = "UNSTABLE"
		}
	}
	if (notAvailableUserManuals.size() != 0) {
	    echo 'Following user manuals are not available'
		echo notAvailableUserManuals.toString()
	}
	sh 'rm -r documentation'
}
