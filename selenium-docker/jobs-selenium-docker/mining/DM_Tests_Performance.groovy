import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
import hudson.tasks.test.*
@Library('TestUtils') _

/**
 * Compile and run all available test cases for mining
 * 
 * @param mxBuildVersion	 	the trunk build version to use
 * @param executeTestsOn	 	the node where to execute the tests
 */

node('DM') {

	timestamps {
	
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "mxBuildVersion=${mxBuildVersion} executeTestsOn=${executeTestsOn} "
	
		def miscUtils = new MiscUtils()
		def workDir = pwd()
		def startMiningEnvironmentResult
		def testCases = [
			'testcases.mining.performance.QATestprojectsDiscoveryTest',
			'testcases.mining.performance.DiscoverCodePerformanceTest',
			'testcases.mining.performance.DiscoverMetricsIncrementalScanTest',
			'testcases.mining.performance.ModuleSearchTest',
			'testcases.mining.performance.LargeCallChainExportTest'
		]
		def totalTestAmount = 0
		def unstableStagesString = ''
		def failedStagesString = ''
		def build_numbers = []
	
		stage('check if already running') {
			if (currentBuild.previousBuild) {
				if (currentBuild.previousBuild.rawBuild.isBuilding() || currentBuild.previousBuild.previousBuild.rawBuild.isBuilding()) {
					error("A previous build is still running.")
				}
			}
		}
	
		stage('clean up workspace') {
			deleteDir()
		}
	
		stage('start mining environment') {
			build job: 'DM_Windows_Start_Stop_Mining_Env', parameters: [
				string(name: 'mxBuildVersion', value: mxBuildVersion),
				[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']],
				string(name: 'envAction', value: 'Start')
			]
		}
	
		def reuseWorkspace = false
		testCases.each { testCase ->
			//def stageName = testCase.replace('testcases.mining.performance.','').replace('testcases.','')
			def stageName = testCase.tokenize('.')[3]
	
			if (testCase.contains('ModuleSearchTest') || testCase.contains('LargeCallChainExportTest')) {
				stage('switch to QATestproject backup') {
					build job: 'DM_Windows_Start_Stop_Mining_Env', propagate: false, parameters: [
						string(name: 'mxBuildVersion', value: mxBuildVersion),
						[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']],
						string(name: 'envAction', value: 'Stop')
						]
					build job: 'DM_Windows_Start_Stop_Mining_Env', propagate: false, parameters: [
						string(name: 'mxBuildVersion', value: mxBuildVersion),
						[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']],
						string(name: 'envAction', value: 'Start')
						]
					build job: 'DM_Windows_Start_Stop_Mining_Env', propagate: false, parameters: [
						string(name: 'mxBuildVersion', value: mxBuildVersion),
						[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']],
						string(name: 'envAction', value: 'Backup')
						]
				}
			}
	
			stage(stageName) {
				def buildResult = build job: 'DM_LeanFT_Mining_Performance_Testjob', propagate: false, parameters: [
					string(name: 'mxBuildVersion', value: mxBuildVersion),
					string(name: 'testcase', value: testCase),
					booleanParam(name: 'reuse_workspace', value: reuseWorkspace),
					[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']]
				]
				miscUtils.evaluateBuildResult(buildResult)
	
				if (buildResult.result.equals('UNSTABLE')) {
					unstableStagesString += "${stageName}:${buildResult.number},"
				} else if (buildResult.result.equals('FAILURE')) {
					failedStagesString += "${stageName}:${buildResult.number},"
				}
				
				// add build number to list to execute Confluence Reporting
				build_numbers.add("${buildResult.number}")
	
				totalTestAmount++
				echo "Build result test case ${testCase}: ${buildResult.result}"
				copyArtifacts filter: 'RunResults/*/**', optional: true, projectName: 'DM_LeanFT_Mining_Performance_Testjob', selector: specific(buildResult.number.toString()), target: "${workDir}/${stageName}"
				build job: 'DM_Windows_Start_Stop_Mining_Env', propagate: false, parameters: [
						string(name: 'mxBuildVersion', value: mxBuildVersion),
						[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']],
						string(name: 'envAction', value: 'Stop')
						]
				build job: 'DM_Windows_Start_Stop_Mining_Env', propagate: false, parameters: [
						string(name: 'mxBuildVersion', value: mxBuildVersion),
						[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']],
						string(name: 'envAction', value: 'Start')
						]
			}
		}
	
		stage('stop mining environment') {
			build job: 'DM_Windows_Start_Stop_Mining_Env', propagate: false, parameters: [
				string(name: 'mxBuildVersion', value: mxBuildVersion),
				[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']],
				string(name: 'envAction', value: 'Stop')
			]
		}
	
		stage('Archive run results') {
			def unstableStages = []
			def failedStages = []
			def logOutput = ''
			if (unstableStagesString) {
				logOutput += 'Unstable stages:\n'
				unstableStages = unstableStagesString.split(',')
				unstableStages.each { unstableStage ->
					def stageInfo = unstableStage.split(':')
					logOutput += "${stageInfo[0]}\n"
				}
			}
			if (failedStagesString) {
				logOutput += 'Failed stages:\n'
				failedStages = failedStagesString.split(',')
				failedStages.each { failedStage ->
					def stageInfo = failedStage.split(':')
					logOutput += "${stageInfo[0]}\n"
				}
			}
			if (logOutput) {
				echo "${logOutput}"
			}
	
			def unstablePercentage = ((unstableStages.size()+failedStages.size()) / totalTestAmount) * 100
			def stablePercentage = 100 - unstablePercentage
			echo "\nTotal Performance tests: ${totalTestAmount}"
			echo "\nPercentage of unstable tests: ${unstablePercentage}%"
			echo "\nPercentage of stable tests: ${stablePercentage}%"
			buildDescription "${currentBuild.getDescription()} totalTestCount=${totalTestAmount} ${unstableStagesString ? 'unstableStages='+unstableStagesString : ''} ${failedStagesString ? 'failedStages='+failedStagesString : ''}"
		}
	
		stage('Report to Confluence') {
			if (! build_numbers.isEmpty()) {
				build_numbers.each { build_number ->
					def buildResult = build job: 'DM_Confluence_Performance_Reporting', propagate: false, parameters: [
						string(name: 'build_number', value: build_number)
					]
					miscUtils.evaluateBuildResult(buildResult)
				}
			}
		}
	}
}

// Returns a list of regex matches, or an empty list if no matches are found
def List getRegexMatch(String line, java.util.regex.Pattern pattern) {
	return (line =~ pattern) ? (line =~ pattern)[0] : []
}

// Parses format hh:mm:sec to minutes
def int timeToMinutes(String time) {
	def timeUnits = time.split(':')
	def hours = timeUnits[0] as int
	def minutes = timeUnits[1] as int
	def seconds = timeUnits[2] as int
	minutes += (seconds < 30) ? 0 : 1

	return hours*60+minutes
}
