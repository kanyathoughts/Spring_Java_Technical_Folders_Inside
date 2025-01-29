import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
@Library('TestUtils') _

/**
 * Compile and run all available test cases for mining
 * 
 * @param mxBuildVersion	 	the trunk build version to use
 * @param executeTestsOn	 	the node where to execute the tests
 * @param executeEnvironmentOn	the node where to execute the environment
 */

node(executeEnvironmentOn) {

	timestamps {
	     
	    buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	    buildDescription "mxBuildVersion=${mxBuildVersion} executeTestsOn=${executeTestsOn} executeEnvironmentOn=${executeEnvironmentOn}"
	
		def miscUtils = new MiscUtils()
	    def workDir = pwd()
		def startMiningEnvironmentResult
	    def testCases = [
	    	'testcases.discovery.MinificationTest',
			'testcases.discovery.DB2PLSQLSupportTest',
	    	'testcases.discovery.UnifyOutputFormatTest',
			'testcases.discovery.ExportEffortSummaryTest',
			'testcases.discovery.FileTypeDetectionOrderTest',
			'testcases.discovery.DiscoverDNATest',
			'testcases.discovery.PL1DependencyTest',
			'testcases.discovery.JavaStoredProcedureDependencyTest',
			'testcases.discovery.JavaStage2Part1Test',
			'testcases.discovery.UploadFilesTest',
			'testcases.discovery.JavaPackageFiltersTest',
			'testcases.discovery.JavaCrossProjectDependencyTest',
			'testcases.discovery.IKJEFT01ToProcedureToCOBOLCallTest',
			'testcases.discovery.CheckSqlModuleDependencies',
			'testcases.discovery.DeselectSheetsTest',
			'testcases.discovery.ECLStage1Test',
			'testcases.discovery.ECLFileDependencyTest',
			'testcases.discovery.UnisysCobolDependencyTest',
			'testcases.discovery.UnisysCobolEnterDependencyTest',
			'testcases.discovery.UnisysCobolLibrarySupportTest',
			'testcases.discovery.CancelSourceUploadTest',
			'testcases.discovery.BatchWithPlaceholderTest',
			'testcases.discovery.IncrementalModulesTest',
			'testcases.discovery.IncrementalSourceCodeUploadTest',
			'testcases.discovery.SQLPLDependencies',
			'testcases.discovery.UnchangedUndiscoveredEntitesTest',
			'testcases.discovery.PL1IMSDependencyTest',
			'testcases.discovery.EsqlCDependencyTest',
			'testcases.discovery.SingleModuleDNATest',
			'testcases.discovery.DiscoveryMiscellaneousTest',
			'testcases.discovery.JCLDependencyTest',
			'testcases.discovery.SQLSupportTest',
			'testcases.discovery.JCLNaturalDependencyTest',
			'testcases.discovery.ConfigOverwriteTest',
			'testcases.discovery.UnchangedSourceFilesTest'
		]
		def totalTestAmount = 0
		def unstableStagesString = ''
		def failedStagesString = ''
		def miningServerUrl = 'http://qef-linux5-us-dm.deloitte.com:8080/'
		
		if (executeEnvironmentOn == "USLinux7-DM") {
			miningServerUrl = 'http://qef-linux7-us-dm.deloitte.com:8080/'
		} else if (executeEnvironmentOn == "USLinux6-DM") {
			miningServerUrl = 'http://qef-linux6-us-dm.deloitte.com:8080/'
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
	    		
	    startMiningEnvironmentResult = startMiningEnvironment(mxBuildVersion, true, executeEnvironmentOn)
		
	    echo startMiningEnvironmentResult.result
		
		if (startMiningEnvironmentResult.result == 'SUCCESS') {
			def reuseWorkspace = false
			testCases.each {
				testCase ->
				
				if (testCase == 'testcases.discovery.DiscoveryOnFilesTest') {
					startMiningEnvironment(mxBuildVersion, true, executeEnvironmentOn)
				}
				def stageName = testCase.replace('testcases.discovery.','').replace('testcases.','')
	
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
			
			echo "\nTotal Discovery tests: ${totalTestAmount}"
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
