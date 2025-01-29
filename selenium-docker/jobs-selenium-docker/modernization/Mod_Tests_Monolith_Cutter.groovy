import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
@Library('TestUtils') _
/**
 * This job runs all specified monolith cutter tests in a pipeline.
 * 
 * @param buildTag				The db cutter version/tag to use.
 * @param deploy				Whether to deploy to the staging server or not.
 * @param executeTestsOn		The node where to execute the UI tests.
 * @param executeEnvironmentOn	The node where the environment under test should be started.	
 */
node('MOD') {
	buildName "#${env.BUILD_ID} - ${buildTag}"
	buildDescription "buildTag=${buildTag} deploy=${deploy}"
	def miscUtils = new MiscUtils()
	def environmentBuildResult
	def unstableStagesString = ''
	def failedStagesString = ''
	def totalTestAmount = 0
	def testCases = [] //['monolithcutter.Smoketest']

	stage ('check if already running') {
		if (currentBuild.previousBuild.rawBuild.isBuilding() || currentBuild.previousBuild.previousBuild.rawBuild.isBuilding()) {
			error("A previous build is still running.")
		}
	}
	
	stage ('clean up workspace') {
		deleteDir()
	}

	stage ('start monolith cutter environment') {
		environmentBuildResult = build job: 'Mod_Docker_Start_Monolith_Cutter_Env', propagate: false, parameters: [
			string(name: 'buildTag', value: buildTag)
		]
		miscUtils.evaluateBuildResult(environmentBuildResult)
		echo "Build result of Start_Monolith_Cutter_Environment: ${environmentBuildResult.result}"
	}
	
	if (environmentBuildResult.result == 'SUCCESS') {
		testCases.each {
			testCase ->
			stage (stageName) {
				def buildResult = build job: 'Mod_Selenium_Testjob', propagate: false, parameters: [
					string(name: 'buildTag', value: buildTag), 
					string(name: 'testcase', value: testCase), 
					[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']],
				]
				miscUtils.evaluateBuildResult(buildResult)
				
				if (buildResult.result.equals('UNSTABLE')) {				
					unstableStagesString += "${stageName}:${buildResult.number},"
				} else if (buildResult.result.equals('FAILURE')) {
					failedStagesString += "${stageName}:${buildResult.number},"
				}
				
				totalTestAmount++
				
				echo "Build result test case ${testCase}: ${buildResult.result}"
				
				copyArtifacts filter: 'RunResults/*/**', optional: true, projectName: 'Mod_Selenium_Testjob', selector: specific(buildResult.number.toString()), target: "${workDir}/${testCase}"
			}
		}
	}
	
	stage('Archive run results') {
/*			def unstableStages = []
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
			
			echo "\nTotal Monolith Cutter tests: ${totalTestAmount}"
			echo "\nPercentage of unstable tests: ${unstablePercentage}%"
			echo "\nPercentage of stable tests: ${stablePercentage}%"
			
			buildDescription "${currentBuild.getDescription()} totalTestCount=${totalTestAmount} ${unstableStagesString ? 'unstableStages='+unstableStagesString : ''} ${failedStagesString ? 'failedStages='+failedStagesString : ''}"	*/		
	}
}
