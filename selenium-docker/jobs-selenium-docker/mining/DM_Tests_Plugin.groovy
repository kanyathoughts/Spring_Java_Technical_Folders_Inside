import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
@Library('TestUtils') _

/**
 * Compile and run all available test cases for mining
 * 
 * @param mxBuildVersion	 	the trunk build version to use
 * @param deploy 			 	whether a deploy to Mining Staging is desired or not
 * @param executeTestsOn	 	the node where to execute the tests
 * @param executeEnvironmentOn	the node where to execute the environment
 */

node(executeEnvironmentOn) {

	timestamps {
     
	    buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	    buildDescription "mxBuildVersion=${mxBuildVersion} deploy=${deploy} executeTestsOn=${executeTestsOn} executeEnvironmentOn=${executeEnvironmentOn}"
	
		def miscUtils = new MiscUtils()
	    def workDir = pwd()
		def startMiningEnvironmentResult
	    def testCases = [
			'testcases.mining.plugin.SmokeTest',
			'testcases.mining.plugin.DataDictionaryCobolTest',
			'testcases.mining.plugin.FieldTracerCobolTest', 
			'testcases.mining.plugin.FieldTracerNaturalTest',
			'testcases.mining.plugin.IdentifyCandidatesTest',
			'testcases.mining.plugin.IdentifyBusinessRuleCandidatesTest', 
			'testcases.mining.plugin.IdentifyBusinessVariablesTest',
			'testcases.mining.plugin.IdentifyCandidatesNaturalTest',
			'testcases.mining.plugin.IdentifyCandidatesPL1Test',
			'testcases.mining.plugin.DataDictionaryNaturalTest',
			'testcases.mining.plugin.JobViewTest',
			'testcases.mining.plugin.CobolDependencyTest',
			'testcases.mining.plugin.BasicOutlineViewTest',
			'testcases.mining.plugin.HotKeysTest', 
			'testcases.mining.plugin.ToolbarActions', 
			'testcases.mining.plugin.TaxonomyTest',
			'testcases.mining.plugin.TechnicalTaxonomyNaturalTest',
			'testcases.mining.plugin.LicensingCobolclipseTest',
			'testcases.mining.plugin.LicensingEnableAllTest',
			'testcases.mining.plugin.LicensingMiningWithTrunkVersionTest',
			'testcases.mining.plugin.LicensingMiningWrongVersionTest',
			'testcases.mining.plugin.LicensingWithoutLicenseTest', 		
			'testcases.mining.plugin.LanguageCodedMapDetection',
			'testcases.mining.plugin.LargeSourceCodeFileUploadTest',
			'testcases.mining.plugin.ModuleDescriptionNaturalTest',
			'testcases.mining.plugin.ApiServerConnectionModuleDescriptionTest',
			'testcases.mining.plugin.AnnotationCobolclipseBMSTest',
			'testcases.mining.plugin.AnnotationsCobolSourceEditorTest',
			'testcases.mining.plugin.AnnotationsEclipseJavaEditorTest',
			'testcases.mining.plugin.AnnotationsMeeclipseJavaEditorTest',
			'testcases.mining.plugin.AnnotationBatchclipseBatchEditorTest',
			'testcases.mining.plugin.AnnotationsNaturalSourceEditorTest',
			'testcases.mining.plugin.ProjectSpecificSettingsTest',
			'testcases.mining.plugin.ImportDiscoveryExpertExcelTest',
			'testcases.mining.plugin.JobLogDownloadTest'
		]
		def unstableStagesString = ''
		def failedStagesString = ''
		def totalTestAmount = 0
		def miningServerUrl = 'http://qef-linux6-us-dm.deloitte.com:8080/'
		
		if (executeEnvironmentOn == "USLinux5-DM") {
			miningServerUrl = 'http://qef-linux5-us-dm.deloitte.com:8080/'
		} else if (executeEnvironmentOn == "USLinux7-DM") {
			miningServerUrl = 'http://qef-linux7-us-dm.deloitte.com:8080/'
		} else if (executeEnvironmentOn == "USLinux8-DM") {
			miningServerUrl = 'http://qef-linux8-us-dm.deloitte.com:8080/'
		} else if (executeEnvironmentOn == "USLinux1") {
			miningServerUrl = 'http://qef-linux1.deloitte.com:8080/'
		} else if (executeEnvironmentOn == "USLinux3") {
			miningServerUrl = 'http://qef-linux3.deloitte.com:8080/'
		}
		
		stage ('check if already running') {
			if(currentBuild.previousBuild) {
				if (currentBuild.previousBuild.rawBuild.isBuilding() || currentBuild.previousBuild.previousBuild.rawBuild.isBuilding()) {
					error("A previous build is still running.")
				}
			}
		}
			
	    stage ('clean up workspace') {
	        deleteDir()
	    }
	        
		startMiningEnvironmentResult = startMiningEnvironment(mxBuildVersion, false, executeEnvironmentOn)
		if (startMiningEnvironmentResult.result == 'SUCCESS') {
			stage ('run smoketest without keycloak') {
				def buildResult = build job: 'DM_LeanFT_Mining_Testjob', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
					string(name: 'testcase', value: 'testcases.mining.plugin.SmokeTest'),
					booleanParam(name: 'reuse_workspace', value: false),
					[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']],
					string(name: 'miningServerURL', value: miningServerUrl),
					booleanParam(name: 'runWithoutKeycloak', value: true)
				]
				miscUtils.evaluateBuildResult(buildResult)
			}
		} else {
			echo 'Mining Environment was not launched correctly. Skipping smoketest run without keycloak.'
		}
			
	    startMiningEnvironmentResult = startMiningEnvironment(mxBuildVersion, true, executeEnvironmentOn)
		
	    echo startMiningEnvironmentResult.result
		
		if (startMiningEnvironmentResult.result == 'SUCCESS') {
			def reuseWorkspace = false
			testCases.each {
				testCase ->
				
				if (testCase == 'testcases.mining.plugin.LanguageCodedMapDetection') {
					startMiningEnvironment(mxBuildVersion, true, executeEnvironmentOn)
				}
				def stageName = testCase.replace('testcases.mining.','').replace('testcases.','')
				if(!stageName.equals('plugin.SmokeTest')){
			        stageName = stageName.replaceAll('Test$', '')
			    }
				stage (stageName) {
					def buildResult = build job: 'DM_LeanFT_Mining_Testjob', propagate: false, parameters: [
						string(name: 'mxBuildVersion', value: mxBuildVersion), 
						string(name: 'testcase', value: testCase), 
						booleanParam(name: 'reuse_workspace', value: reuseWorkspace), 
						[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']],
						string(name: 'miningServerURL', value: miningServerUrl)
					]
					miscUtils.evaluateBuildResult(buildResult)
					
					if (buildResult.result.equals('UNSTABLE')) {				
						unstableStagesString += "${stageName}:${buildResult.number},"
					} else if (buildResult.result.equals('FAILURE')) {
						failedStagesString += "${stageName}:${buildResult.number},"
					}
					
					totalTestAmount++
					
					reuseWorkspace = true
					echo "Build result test case ${testCase}: ${buildResult.result}"
					
					copyArtifacts filter: 'RunResults/*/**', optional: true, projectName: 'DM_LeanFT_Mining_Testjob', selector: specific(buildResult.number.toString()), target: "${workDir}/${testCase}"
				
					if (testCase == 'testcases.mining.plugin.SmokeTest') {
						def smokeTestResult = buildResult.result
						if (buildResult.result == 'SUCCESS') {
							echo 'Deploy criterium is met.'
							if (deploy == 'true') {
								stage('deploy to Mining Staging') {
									def deployResult = build job: 'DM_Docker_Deploy_Staging_Env', wait: true, propagate: false, parameters: [extendedChoice(name: 'miningBuildVersion', value: mxBuildVersion)]
									if (deployResult.result == 'SUCCESS') {               		    
										stage ('Sending success mail notifications') {
											def subject = 'Successfully deployed to Mining Staging'
											body = "The Mining build with version ${mxBuildVersion} has been successfully smoke-tested and deployed to Mining Staging."
											body += '<br/><br/>Mining Staging: http://qef-linux2-us.deloitte.com:8080/ (User: admin, Password: Worx2000)'
											body += '<br/><br/>The eclipse for this version can be downloaded here: https://resources.deloitte.com/:f:/r/sites/DOL-c-DTTL-AppModGermanyFS/Shared%20Documents/delivery/untested/mining?csf=1&web=1&e=dZztBp'
											mail body: body, mimeType: 'text/html', subject: subject, to: miscUtils.getMailReceivers(currentBuild.projectName,'Success')
										}
										stage ('Sending failure mail notifications') {
											Utils.markStageSkippedForConditional('Sending failure mail notifications')
										}
									} else {
										miscUtils.evaluateBuildResult(deployResult)
										stage ('Sending success mail notifications') {
											Utils.markStageSkippedForConditional('Sending success mail notifications')
										}
										stage ('Sending failure mail notifications') {
											Utils.markStageSkippedForConditional('Sending failure mail notifications')
										}
									}
								}
							} else {
								echo 'Deploy parameter was not set or it was a rebuild. Not deploying.'
								stage('deploy to Mining Staging') {
									Utils.markStageSkippedForConditional('deploy to Mining Staging')
								}
								stage ('Sending success mail notifications') {
									Utils.markStageSkippedForConditional('Sending success mail notifications')
								}
								stage ('Sending failure mail notifications') {
									Utils.markStageSkippedForConditional('Sending failure mail notifications')
								}
							}
						} else {
							echo 'Deploy criterium not met (Test not successful). Not deploying.'
							stage('deploy to Mining Staging') {
								Utils.markStageSkippedForConditional('deploy to Mining Staging')
							}
							stage ('Sending success mail notifications') {
								Utils.markStageSkippedForConditional('Sending success mail notifications')
							}
							if (deploy == 'true') {
								stage ('Sending failure mail notifications') {
									def subject = "Mining smoke-tests ${smokeTestResult}"
									body = "The smoke-tests of the Mining build with version ${mxBuildVersion} are ${smokeTestResult}."
									body += '<br/><br/>Therefore, not auto-deploying to Staging.'
									body += '<br/><br/><b><font color=\"red\">This is not necessarily a product bug. This can also be caused by the testing framework.<font/><b/>.'	                     
									mail body: body, mimeType: 'text/html', subject: subject, to: miscUtils.getMailReceivers(currentBuild.projectName,'Failure')
								}
							} else {
								stage ('Sending failure mail notifications') {
									Utils.markStageSkippedForConditional('Sending failure mail notifications')
								}
							}
						}
					}
				}
			}
			
			stage('plugin.VersionHandshakeTest') {
				def buildResult = build job: 'DM_Test_Version_Handshake', propagate: false, parameters: [
					string(name: 'mxBuildVersion', value: mxBuildVersion), 
					[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']]]
					miscUtils.evaluateBuildResult(buildResult)
					
					if (buildResult.result.equals('UNSTABLE')) {				
						unstableStagesString += "plugin.VersionHandshakeTest:${buildResult.number},"
					} else if (buildResult.result.equals('FAILURE')) {
						failedStagesString += "plugin.VersionHandshakeTest:${buildResult.number},"
					}
					
					totalTestAmount++
				
					echo "Build result of VersionHandshakeTest: ${buildResult.result}"
			}
			
			stage('LicenseCheckForMiningAPIServer') {
				def buildResult = build job: 'DM_License_Check_For_Mining_API_Server', propagate: false, parameters: [
					string(name: 'mxBuildVersion', value: mxBuildVersion), 
					[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']]]
					miscUtils.evaluateBuildResult(buildResult)
					
					if (buildResult.result.equals('UNSTABLE')) {				
						unstableStagesString += "LicenseCheckForMiningAPIServer:${buildResult.number},"
					} else if (buildResult.result.equals('FAILURE')) {
						failedStagesString += "LicenseCheckForMiningAPIServer:${buildResult.number},"
					}
					
					totalTestAmount++
				
					echo "Build result of LicenseCheckForMiningAPIServer: ${buildResult.result}"
			}
			
			stage('Archive run results') {
				def unstableStages = []
				def failedStages = []
				def logOutput = ''
				if (unstableStagesString) {
					logOutput += 'Unstable stages:\n'
					unstableStages = unstableStagesString.split(',')
					unstableStages.each {
						unstableStage ->
						def stageInfo = unstableStage.split(':')
						logOutput += "${stageInfo[0]}\n"
					}
				}
				if (failedStagesString) {
					logOutput += 'Failed stages:\n'
					failedStages = failedStagesString.split(',')
					failedStages.each {
						failedStage ->
						def stageInfo = failedStage.split(':')
						logOutput += "${stageInfo[0]}\n"
					}
				}
				if (logOutput) {
					echo "${logOutput}"
				}
				
				def unstablePercentage = ((unstableStages.size()+failedStages.size()) / totalTestAmount) * 100 
				def stablePercentage = 100 - unstablePercentage
				
				echo "\nTotal Plugin tests: ${totalTestAmount}"
				echo "\nPercentage of unstable tests: ${unstablePercentage}%"
				echo "\nPercentage of stable tests: ${stablePercentage}%"
				
				buildDescription "${currentBuild.getDescription()} totalTestCount=${totalTestAmount} ${unstableStagesString ? 'unstableStages='+unstableStagesString : ''} ${failedStagesString ? 'failedStages='+failedStagesString : ''}"			
			}
		} else {
			error 'Mining Environment was not launched correctly.'
		}
	}
}

def startMiningEnvironment(String mxBuildVersion, boolean withKeycloak, String executeEnvironmentOn) {
	def miscUtils = new MiscUtils()
	stage ("start mining environment keycloak = ${withKeycloak}") {
		startMiningEnvironmentResult = build job: 'DM_Docker_Start_Mining_Env', propagate: false, parameters: [
			extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
			booleanParam(name: 'withKeycloak', value: withKeycloak),
			[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeEnvironmentOn], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
		miscUtils.evaluateBuildResult(startMiningEnvironmentResult)
	}
	if (startMiningEnvironmentResult.result != 'SUCCESS') {
		stage ("restart mining environment keycloak = ${withKeycloak}") {
			echo 'first start of mining environment without keycloak did not work -> trigger restart'
			startMiningEnvironmentResult = build job: 'DM_Docker_Start_Mining_Env', propagate: false, parameters: [
			extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
			booleanParam(name: 'withKeycloak', value: withKeycloak),
			[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeEnvironmentOn], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
		miscUtils.evaluateBuildResult(startMiningEnvironmentResult)
		}
	}
	return startMiningEnvironmentResult
}
