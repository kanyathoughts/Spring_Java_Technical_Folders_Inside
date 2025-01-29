@Library('TestUtils') _

/**
 * Job script to compile and run automated LeanFT test cases for IMS workbench  
 * 
 * @param  mxBuildVersion The IMS-WB build to test.
 * @param testCase Name including the package of the test you want to run.
 * @param executeOn The Jenkins node the test will run on
 * @param useDifferentTestProjectBranch By default the branch of the LeanFT project is checked out based on the selected mxVersion. 
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

timestamps {
	def spUtils = new SharepointUtils()
	def gitUtils = new GitUtils()
	def mxVersionUtils = new MxVersionUtils()
	
	node(executeOn) {
		catchError(buildResult: 'UNSTABLE', stageResult: 'UNSTABLE') {
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
			def testProject = 'leanft-imsworkbench'
			def jobWorkspace = pwd()
			def workspace = "${jobWorkspace}\\ims-workbench-dist-${mxBuildVersion}"
			
			buildName "#${env.BUILD_ID} - ${testCase} - ${mxBuildVersion}"
			buildDescription "mxBuildVersion=${mxBuildVersion} reuse_workspace=${reuse_workspace} executeOn=${executeOn} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} testCase=${testCase}"
			
			stage('init') {
				// Sometimes the previous processes are not killed in time or at all
				killRunningIMSWBProcesses()
				if(Boolean.parseBoolean(reuse_workspace)){
					echo 'Skipping "Checkout test project"'
				} else {
					deleteDir()
					if (mxBuildVersion.startsWith('22.2')) {
						if (! spUtils.downloadFile("/Shared Documents/delivery/untested/innowake/ims-workbench/IW22-IR2/ims-workbench-dist-${mxBuildVersion}.zip", './')) {
							echo "Download of /Shared Documents/delivery/untested/innowake/ims-workbench/IW22-IR2/ims-workbench-dist-${mxBuildVersion}.zip failed"
							if(! spUtils.downloadFile("${spUtils.getJenkinsArtifactsDir()}/LD_LeanFT_IMS_Workbench_Testjob/ims-workbench-dist-${mxBuildVersion}.zip", './')) {
								error "Download of /Shared Documents/share/QA/jenkins-artifacts/ims-workbench-dist-${mxBuildVersion}.zip failed"
							}
						}
					} else {
						if (! spUtils.downloadFile("/Shared Documents/delivery/untested/innowake/ims-workbench/ims-workbench-dist-${mxBuildVersion}.zip", './')) {
							echo "Download of /Shared Documents/delivery/untested/innowake/ims-workbench/ims-workbench-dist-${mxBuildVersion}.zip failed"
							if(! spUtils.downloadFile("${spUtils.getJenkinsArtifactsDir()}/LD_LeanFT_IMS_Workbench_Testjob/ims-workbench-dist-${mxBuildVersion}.zip", './')) {
								error "Download of /Shared Documents/share/QA/jenkins-artifacts/ims-workbench-dist-${mxBuildVersion}.zip failed"
							}
						}
					}
					unzip dir: "ims-workbench-dist-${mxBuildVersion}", zipFile: "ims-workbench-dist-${mxBuildVersion}.zip", quiet: true
			
					copyArtifacts projectName: 'TF_IMS_DB_Csharp_Test',
						filter: "ims-db-csharp_${mxBuildVersion}.dll",
						target: "ims-workbench-dist-${mxBuildVersion}",
						selector: lastSuccessful(),
						parameters: "mxBuildVersion=${mxBuildVersion}",
						fingerprintArtifacts: true
				if (testCase == "GenerateDVViewFunctionalityTest") {
				    def content = '''#IMS Config Details
#Thu Feb 18 17:19:49 IST 2021
ims.migration.properties.file=Default values in innowake.mee.source.ims2java.expert.Properties
ims.copybook.libraries=C:\\\\Users\\\\usleanftsvc\\\\Documents\\\\copybooks\\\\
ims.db.name.resolver=innowake.mee.ims.api.env.database.preset.DefaultImsDbNameResolver
ims.dbd.copybook.mapping.file=None, no mapping, no DV view generation is possible5'''
					writeFile encoding: 'UTF-8', file: "ims-workbench-dist-${mxBuildVersion}/IMSConfig.properties", text: content
					} else {
					def content = '''#IMS Config Details
#Thu Feb 18 17:19:49 IST 2021
ims.migration.properties.file=Default values in innowake.mee.source.ims2java.expert.Properties
ims.copybook.libraries=None - user needs to be prompted if empty1
ims.db.name.resolver=innowake.mee.ims.api.env.database.preset.DefaultImsDbNameResolver
ims.dbd.copybook.mapping.file=None, no mapping, no DV view generation is possible5'''
					writeFile encoding: 'UTF-8', file: "ims-workbench-dist-${mxBuildVersion}/IMSConfig.properties", text: content
					}
					gitUtils.checkoutGitProject(testProject, "${gitUtils.getGitUrlQef()}/innowake-test-projects/ui-test-projects/leanft-imsworkbench.git", testProjectBranch)

					dir("ims-workbench-dist-${mxBuildVersion}"){
						bat "ren ims-db-csharp_${mxBuildVersion}.dll ims-db-csharp.dll"
					}
			    }
				dir("ims-workbench-dist-${mxBuildVersion}"){
               		  bat "start /b ims-workbench.exe --remote-debugging-port=8181"
                }
			}

		 	stage('run test') {
            	try { 
					timeout(time: 1, unit: 'HOURS') {
                		withJava(javaVersion) {
                    		withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
                        		dir(testProject) {
                            		bat "mvn clean -DmxBuildVersion=${mxBuildVersion} -DworkspaceDir=${workspace} -DjavaVersion=${javaVersion} -Dtest=${testCase} test"
                        		}
                    		}
                		}
           			}
				} catch (e) {
					unstable e.toString()
        		}
        	}
     	
			stage('archive') {
				dir(testProject) {
					def runResultsDir = 'RunResults'
					publishHTML allowMissing: false, alwaysLinkToLastBuild: true, keepAll: true, reportDir: runResultsDir, reportFiles: 'runresults.html', reportName: testCase, reportTitles: "#${env.BUILD_ID} - ${mxBuildVersion}: ${testCase}"
				}
			}
   		}
	}
}

/**
* kills the running processes of IMS Workbench.
* It looks for and kills all processes with the name ims-workbench.exe and 
* also the related java.exe process that has the status 'Unknown'.
*/
def killRunningIMSWBProcesses() {
	try {
		bat 'tasklist | find /i "ims-workbench.exe" && taskkill /im ims-workbench.exe /F'
		bat 'for /f "tokens=2 delims=," %%P in (\'tasklist /v /fo csv ^| findstr /i /r "java.exe.*Unknown"\') do taskkill /f /pid %%~P'
	} catch (e) {
		bat 'echo process "ims-workbench.exe" not running.'
	}
}
