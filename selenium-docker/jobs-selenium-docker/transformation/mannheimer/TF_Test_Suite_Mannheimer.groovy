@Library('TestUtils') _

/**
 * Runs a complete Mannheimer test:
 * - nat2java
 * - start DB2 DB in docker with Mannheimer data
 * - Mannheimer test suite
 * - stop DB2 DB in docker
 * 
 * @param mxBuildVersion  The maxenso build to use.
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * @param javaVersion The java version the test will run with
 */

def miscUtils = new MiscUtils()
def mxVersionUtils = new MxVersionUtils()
def dockerUtils = new DockerUtils()

def unstableProductions = [:]
def successfulProductions = [:]

nodeTF('Docker-host') {
	catchError {
	    timestamps() {
	    	buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	    	
	    	def fullBuild
	    	docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
	    		fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
	    	}
	    	buildDescription "withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} fullBuild=${fullBuild}"
			
	    	stage('expert-script-signing') {
	    		def buildResult = build job: 'TF_Natural_Java_Script_Signing_Mannheimer', quietPeriod: 0, propagate: false, parameters: [
	    		    extendedChoice(name: 'mxBuildVersion', value: fullBuild), 
	    		    string(name: 'javaVersion', value: javaVersion)
	    		]				
	    		miscUtils.evaluateBuildResultWithExit(buildResult, unstableProductions, successfulProductions)
	    	}
                    
	    	stage('nat2java') {
				def accessibilityIssue = true
				def counter = 0
				def buildResult
				while (accessibilityIssue && counter < 3) {
		    		buildResult = build job: 'TF_Natural_Java_Migration_Mannheimer', quietPeriod: 0, propagate: false, parameters: [
		    		    extendedChoice(name: 'mxBuildVersion', value: fullBuild), 
		    		    string(name: 'javaVersion', value: javaVersion)
		    		]			
					accessibilityIssue = buildResult.getRawBuild().getLog().contains('is not accessible. There is no content available')
					counter++
				}	
	    		miscUtils.evaluateBuildResultWithExit(buildResult, unstableProductions, successfulProductions)
	    	}

	    	stage('test') {
	    		def buildResult = build job: 'TF_Natural_Java_Test_Mannheimer', quietPeriod: 0, propagate: false, parameters: [
	    		    extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), 
	    		    booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
	    		    string(name: 'javaVersion', value: javaVersion),
	    		    extendedChoice(name: 'nat2javaBuild', value: fullBuild)					
	    		]
	    		miscUtils.evaluateBuildResult(buildResult, unstableProductions, successfulProductions)
	  
	    		echo 'Successful productions:'
	    		echo successfulProductions.toString()
	    		echo 'Unstable productions:'
	    		echo unstableProductions.toString()
	    	}
	    }
	}
	miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}\nFollowing productions are unstable:\n${unstableProductions.toString()}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
}