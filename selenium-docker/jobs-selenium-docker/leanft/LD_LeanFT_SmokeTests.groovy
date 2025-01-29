@Library('TestUtils') _

/**
 * Job script to compile and run leanft Smoke Test cases for maxenso/innowake on US Server
 * 
 * @param mxBuildVersion 	The maxenso/innowake build to test.
 * @param executeOn 		The Jenkins node the test will run on
 * @param javaVersion 		The java version the test will run with
 */

node {
     
    buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	buildDescription "executeOn=${executeOn} mxBuildVersion=${mxBuildVersion} javaVersion=${javaVersion}"
	
	def miscUtils = new MiscUtils()
    def workDir = pwd()
    def reuseWorkspace = false
	def unstableTestCases = [:]
	def successfulTestCases = [:]
	def scEnvironmentHost
	def scEnvironmentNode
	def scBuildResult

    def testCases = [
		'testcases.smoketests.VersionTest',
		'testcases.smoketests.NatclipseAndLCMTest',
		'testcases.smoketests.ControlCenterTest',
		'testcases.smoketests.CobolClipseTest',
		'testcases.smoketests.NatCreatorTest',
		'testcases.smoketests.ApplicationBuilderTest',
		'testcases.smoketests.MeeclipseTest',
		'testcases.smoketests.NatanalyzerTest'
	]
	
    stage('clean up workspace') {
        deleteDir()
    }
        
    stage('start LD_Start_SC_Env')    {
		scBuildResult = startSCEnvironment(mxBuildVersion, javaVersion)
		echo scBuildResult.result
		scEnvironmentHost = scBuildResult.buildVariables['scEnvironmentHost']
		scEnvironmentNode = scBuildResult.buildVariables['scEnvironmentNode']
	}
    
    testCases.each {
        testCase ->
        stage(testCase) {
			def job = [job: 'LD_LeanFT_Testjob', propagate: false, parameters: [
				extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), 
				string(name: 'testcase', value: testCase), 
				booleanParam(name: 'reuse_workspace', value: reuseWorkspace), 
				[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeOn], nodeEligibility: [$class: 'AllNodeEligibility']],
				string(name: 'scEnvironmentHost', value: scEnvironmentHost),
				string(name: 'javaVersion', value: javaVersion)
			]]
            def buildResult = build job
			// If the build is unstable repeat the testcase and check again if the build is successfull. If the build is unstable a second time the test ist marked as unstable
			if (buildResult.result == 'SUCCESS') {
				successfulTestCases.put(testCase, buildResult.absoluteUrl)
			} else {
				echo "Restarting test case ${testCase}"
				buildResult = build job
				miscUtils.evaluateBuildResult(buildResult)
				if (buildResult.result == 'SUCCESS') {
					successfulTestCases.put(testCase, buildResult.absoluteUrl)
				} else {
					unstableTestCases.put(testCase, buildResult.absoluteUrl)
				}
			}
			reuseWorkspace = true
            echo "Build result test case ${testCase}: ${buildResult.result}"
			//copyArtifacts filter: 'RunResults/*/**', optional: true, projectName: 'LD_LeanFT_Testjob', selector: specific(buildResult.number.toString()), target: "${workDir}/${testCase}"
			publishHTML([allowMissing: true, alwaysLinkToLastBuild: true, keepAll: true, reportDir: "${testCase}/RunResults", reportFiles: 'runresults.html', reportName: testCase, reportTitles: "#${env.BUILD_ID} - ${mxBuildVersion}: ${testCase}"])
		}
    }
	
	stage('stop LD_Stop_SC_Env') {
		scBuildResult = build job: 'LD_Stop_SC_Env', propagate: false, parameters: [
			[$class: 'NodeParameterValue', name: 'executeOn', labels: [scEnvironmentNode], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
		miscUtils.evaluateBuildResult(scBuildResult)
	}
	
    stage('archive run results') {
		echo 'Successful test cases'
		echo successfulTestCases.toString()
		echo 'Unstable test cases'
		echo unstableTestCases.toString()
		
        // archiveArtifacts '**/*'
    }
}

def startSCEnvironment(String mxBuildVersion, String javaVersion) {
	def miscUtils = new MiscUtils()
	scBuildResult = build job: 'LD_Start_SC_Env', propagate: false, parameters: [
			extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
			string(name: 'javaVersion', value: javaVersion),
			[$class: 'NodeParameterValue', name: 'executeOn', labels: ['USLinux1'], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
	miscUtils.evaluateBuildResult(scBuildResult)
	if (scBuildResult.result != 'SUCCESS') {
		echo 'SC Environment was not launched. Second try to start SC Environment.'
		scBuildResult = build job: 'LD_Start_SC_Env', propagate: false, parameters: [
			extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
			string(name: 'javaVersion', value: javaVersion),
			[$class: 'NodeParameterValue', name: 'executeOn', labels: ['USLinux1'], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
		miscUtils.evaluateBuildResult(scBuildResult)
	}
	return scBuildResult
}
