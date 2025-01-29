@Library('TestUtils') _

/**
 * Run the nat2java on the Mannheimer2 project against a certain maxenso build.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param javaVersion The java version the test will run with
 *        type: Choice 
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */
 
nodeTF('OS-Windows') {
    timestamps {
        def mxVersionUtils = new MxVersionUtils()
        def gitUtils = new GitUtils()
        def miscUtils = new MiscUtils()
        def resultComparisonUtils = new ResultComparisonUtils()
        def dockerUtils = new DockerUtils()
        def spUtils = new SharepointUtils()

        def workDirWindows
        def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/mannheimer/mannheimer-script-signing.git'
        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
        def eclipseDir = "eclipse-${mxBuildVersion}"
        def antFlags = ''
        def antPropertiesWindows
        
        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"
        
        try {
	        stage('setup-workspace') {
	        	/* On a Windows node */
	            workDirWindows = pwd()
	            antPropertiesWindows = "-DworkDir=${workDirWindows} -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion}"
	            
	            deleteDir()
	            gitUtils.checkoutGitProject(workDirWindows, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
	            gitUtils.getLicenseProject(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), 'licenses')
	            spUtils.downloadIwEclipseWindows(mxBuildVersion, eclipseDir)
	            /* WQATF-435 - fetch artifacts from Nexus using maven. */
	            withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings', publisherStrategy: 'EXPLICIT') {
	            	bat "$MVN_CMD -f ${workDirWindows}\\pom-mannheimer-dependencies.xml dependency:copy-dependencies -DoutputDirectory=LIBS"
	            	bat "$MVN_CMD -f ${workDirWindows}\\pom-innowake-migration-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=LIBS"
	            	bat "$MVN_CMD -f ${workDirWindows}\\pom-innowake-runtime-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=LIBS"
	            }
	            bat 'MOVE /Y LIBS\\base-replace-*.jar conti\\Mannheimer-Expert\\lib'
	            bat 'MOVE /Y LIBS\\db2jcc4-*.jar conti\\Mannheimer-Java\\lib'
	            bat 'COPY /Y LIBS\\mee-source-migration-*.jar conti\\Mannheimer-Expert\\lib'
	            bat 'COPY /Y LIBS\\innowake-*.jar conti\\Mannheimer-Java\\lib'
	            withAnt(installation: 'Default') {
	            	bat "ant ${antFlags} ${antPropertiesWindows} init-workspace"
	            }
	        }
	         
            stage('signing') {
                withAnt(installation: 'Default') {
            		/* Compile expert scripts and pack the class files into a jar file. Then initialize the script signing. */
            		bat "ant ${antFlags} ${antPropertiesWindows} compile-expert-scripts"
            		bat "ant ${antFlags} ${antPropertiesWindows} pack-expert-scripts"
            		/* We are on a Windows node now, but the signing will be executed on a Linux node in a Docker container.
            		 * The required material is moved between the two nodes by Jenkins stashing.
            		 * The required material is
            		 * - ant script: build.xml, build.properties
            		 * - licenses: the signing license is one of them
            		 * - the jar file with the expert scripts in the expert project
            		 * - the folder for the signature, but not the signature file itself
            		 */
            		stash name: 'beforeSigning',
            			  includes: "build.xml, build.properties, licenses/*, conti/Mannheimer-Expert/lib/*, conti/Mannheimer-Expert/signature/*, ${eclipseDir}/plugins/innowake.base.etc_*/lib/**/*, LIBS/*"
                }
                
                /*
                 * Script signing requires:
                 * - a license, which is protected by a key file
                 * - the key file, which is protected by a password
                 * - the password for the key file
                 * The signing procedure itself is restricted to a specific user, hostname and MAC address, which are specified in the signing license.
                 * They are read from the license file, which is syntactically Java properties file.
                 * The signing has to be executed in an environment with the specific user + hostname + MAC address. This is achieved by executing the signing in a Docker container.
                 * This procedure does not require a headless Eclipse.
                 */
                nodeTF('Docker-host') {
                	/* Switched to a Linux node */
                	deleteDir()
                	unstash 'beforeSigning'
                	sh 'chmod a+w conti/Mannheimer-Expert/signature/*'
                	def linuxWorkDir = pwd()
            		def licenseProperties = readProperties file: './licenses/script-signing.lic'
            		def hostname = licenseProperties['host-name']
            		def macAddress = licenseProperties['mac-addresses'].split(',')[0]
            		def antPropertiesLinux = "-DworkDir=${linuxWorkDir} -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion}"
            		
                	withCredentials([string(credentialsId: 'Script-Signing-Key-File-Password', variable: 'signingPw')]) {
                		def containerParms = "--user jenkins:jenkins --hostname ${hostname} --mac-address ${macAddress} -w ${linuxWorkDir} -v ${linuxWorkDir}:${linuxWorkDir}:rw"
                		def containerCmd = "sh -c \"ant ${antFlags} ${antPropertiesLinux} -DscriptSigningKeyFilePassword=${signingPw} -DmxJarsDir=LIBS exec-script-signing\""
                		docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).withRun(containerParms, containerCmd) {
                			cntr ->
                			sh "docker container wait ${cntr.id}"
                			/* docker.image().withRun() does not write the docker log to the job output, as docker.image().inside() does. So
                			 * - this is done explicitely here, and additionally
                			 * - the docker log is captured in a log file to be published as job artifact.
                			 */
                			sh "docker logs ${cntr.id}"
                			dir('log') {
                				def logFile = "exec-script-signing.log"
                				def ret = sh returnStdout: true, script: "docker logs ${cntr.id} > ${logFile}"
                				/* If a particular success message did not occur, the signing must have failed. */
                				def rc = sh returnStatus: true, script: "grep 'Number of signed jars' ${logFile}"
                				if (rc != 0) {
                					error 'Expert script signing failed!'
                				}
                			}
                		}
                	}
            		stash name: 'afterSigning', includes: 'conti/Mannheimer-Expert/signature/*'
                }
                /* Switched back to Windows node */
            }

            stage('pack-delivery') {
        		withAnt(installation: 'Default') {
                	bat "ant ${antFlags} ${antPropertiesWindows} init-nat2java"
                	unstash 'afterSigning'
                	bat "del ${workDirWindows}\\${eclipseDir}\\innowake.lic"
                	bat "copy ${workDirWindows}\\licenses\\continentale-customer.lic ${workDirWindows}\\${eclipseDir}\\innowake.lic"
        			bat "ant ${antFlags} ${antPropertiesWindows} pack-delivery"
        		}
            }
            
            stage('run-nat2java') {
            	dir('conti') {
            		bat 'Mannheimer_antstarter.bat'
            	}
            }
        } catch (ex) {
        	miscUtils.errorWithStackTrace(ex)
        } finally {
        	stage('finalize') {				
        		archiveArtifacts allowEmptyArchive: true, artifacts: '**/*.log'			
        		spUtils.uploadJobArtifact(mxBuildVersion, "Continentale-${mxBuildVersion}.zip")				
        	}
        }
	}
}
