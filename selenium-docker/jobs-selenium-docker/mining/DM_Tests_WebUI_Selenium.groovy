import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
@Library('TestUtils') _

/**
 * Compile and run all available test cases for mining
 * 
 * @param mxBuildVersion	 	the trunk build version to use
 * @param executeTestsOn	 	the node where to execute the tests
 * @param executeEnvironmentOn	the node where to execute the mining environment
 */

node(executeEnvironmentOn) {

	timestamps {
	     
	    buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	    buildDescription "mxBuildVersion=${mxBuildVersion} executeTestsOn=${executeTestsOn} executeEnvironmentOn=${executeEnvironmentOn}"
	
		def miscUtils = new MiscUtils()
	    def workDir = pwd()
		def startMiningEnvironmentResult
	    def testCases = [
	    	'testcases.webui.ClientProjectManagementTest',
			'testcases.webui.URLTitleSchemaTest',
			'testcases.webui.TaxonomyPropagationTest',
			'testcases.webui.UserCustomizableModuleTableTest',
			'testcases.webui.ExportTablesTest',
			'testcases.webui.SchemaSupportTest',
			'testcases.webui.CallChainExportTest',
			'testcases.webui.BulkExecutionOfMiningActionsTest',
			'testcases.webui.GeneralTest',
			'testcases.webui.CustomDataDictionaryPropertiesTest',
			'testcases.webui.GraphFileTest',
			'testcases.webui.ModuleCustomPropertiesTest',
			'testcases.webui.AnnotationCategoryTest',
			'testcases.webui.MetricsUISummaryTest',			
			'testcases.webui.MetricsUITechnologiesTest',
			'testcases.webui.ManageTaxonomiesTest',			
			'testcases.webui.ListOfNotReferencedModulesTest',
			'testcases.iam.ProjectEditorTest',
			'testcases.iam.ProjectViewerTest',
			'testcases.iam.ProjectManagerTest',
			'testcases.iam.ClientAdminTest',
			'testcases.webui.CallChainIgnoreModulesAndDataFlowTest',
			'testcases.webui.MetricsUIMcCabeTest',
			'testcases.webui.CookieConsentTest',
			'testcases.webui.ClientProjectManagementAdminTest',
			'testcases.webui.FilterMergeTest',
			'testcases.webui.DependencyAnalysisNaturalTest',
			'testcases.webui.DPGraphToolbarTest',
			'testcases.webui.ControlFlowGraphCTest'
		]
		def unstableStagesString = ''
		def failedStagesString = ''
		def totalTestAmount = 0
		def miningServerUrl = 'http://qef-linux8-us-dm.deloitte.com:8080/'
		
		if (executeEnvironmentOn == "USLinux5-DM") {
			miningServerUrl = 'http://qef-linux5-us-dm.deloitte.com:8080/'
		} else if (executeEnvironmentOn == "USLinux6-DM") {
			miningServerUrl = 'http://qef-linux6-us-dm.deloitte.com:8080/'
		} else if (executeEnvironmentOn == "USLinux7-DM") {
			miningServerUrl = 'http://qef-linux7-us-dm.deloitte.com:8080/'
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
	    		
	    startMiningEnvironmentResult = startMiningEnvironment(mxBuildVersion, true, executeEnvironmentOn)
		
	    echo startMiningEnvironmentResult.result
		
		if (startMiningEnvironmentResult.result == 'SUCCESS') {
			def reuseWorkspace = false
			testCases.each {
				testCase ->
				def stageName = testCase.replace('testcases.','')
	
				stage (stageName) {
					def buildResult = build job: 'DM_Selenium_Mining_Testjob', propagate: false, parameters: [ 
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
					
					copyArtifacts filter: 'artifact/reports/*/**', optional: true, projectName: 'DM_Selenium_Mining_Testjob', selector: specific(buildResult.number.toString()), target: "${workDir}/${testCase}"	
				}
			}
		} else {
			error 'Mining Environment was not launched correctly.'
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
			
			echo "\nTotal Web UI tests: ${totalTestAmount}"
			echo "\nPercentage of unstable tests: ${unstablePercentage}%"
			echo "\nPercentage of stable tests: ${stablePercentage}%"		
			
			buildDescription "${currentBuild.getDescription()} totalTestCount=${totalTestAmount} ${unstableStagesString ? 'unstableStages='+unstableStagesString : ''} ${failedStagesString ? 'failedStages='+failedStagesString : ''}"			
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
			startMiningEnvironmentResult = build job: 'DM_Docker_Start_Mining_Env', propagate: true, parameters: [
			extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
			booleanParam(name: 'withKeycloak', value: withKeycloak),
			[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeEnvironmentOn], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
		miscUtils.evaluateBuildResult(startMiningEnvironmentResult)
		}
	}
	return startMiningEnvironmentResult
}
