@Library('TestUtils') _

/**
 * Runs a complete KIDICAP_mini test:
 * - signing of the customers expert scripts
 * - nat2java, incl. signature and using a customer license
 * - start Oracle DB in docker with AGTV data
 * - production AGTV
 * - stop Oracle DB in docker
 * - start Oracle DB in docker with OFDAN data
 * - production OFDAN
 * - stop Oracle DB in docker
 * - start Oracle DB in docker with OFDVE data
 * - production OFDVE
 * - stop Oracle DB in docker
 * - start Oracle DB in docker with AGTV data
 * - production REGRESSION
 * - stop Oracle DB in docker
 * 
 * @param mxBuildVersion  The maxenso build to use.
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * @param javaVersion The java version the test will run with
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set. 
 */

def miscUtils = new MiscUtils()
def mxVersionUtils = new MxVersionUtils()
def dockerUtils = new DockerUtils()

def unstableProductions = [:]
def successfulProductions = [:]

nodeTF('Docker-host') {
	catchError {    
		timestamps() {
			def buildResult
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
	        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			
			def fullBuild
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
				fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
			}
			buildDescription "withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} fullBuild=${fullBuild} testProjectBranch=${testProjectBranch}"
	
            stage('expert-script-signing') {
                buildResult = build job: 'TF_Natural_Java_Script_Signing_KIDICAP_Mini', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: fullBuild), 
					string(name: 'javaVersion', value: javaVersion),
					booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)	
				]
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableProductions, successfulProductions)
            }
            
            stage('nat2java') {
                buildResult = build job: 'TF_Natural_Java_Migration_KIDICAP_Mini', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: fullBuild), 
					booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), 
					string(name: 'javaVersion', value: javaVersion),
					booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)	
				]
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableProductions, successfulProductions)
            }
            
            def productions = ['AGTV', 'OFDAN', 'OFDVE', 'REGRESSION']
            productions.each {
                production ->
                stage(production) {
					def jobName = "TF_Natural_Java_Test_KIDICAP_Mini_${production}"
                    buildResult = build job: jobName, propagate: false, parameters: [
						extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), 
						booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), 
						string(name: 'javaVersion', value: javaVersion), 
						extendedChoice(name: 'kidicapBuild', value: fullBuild),
						booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
						listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)	
					]
					miscUtils.evaluateBuildResult(buildResult, unstableProductions, successfulProductions)
                }
            }
            
            stage('reference-collector') {
                when (fullBuild.startsWith('22.0') || fullBuild >= '22.2', 'Job is only available for 22.0, trunk and newer versions.') {
                    buildResult = build job: 'TF_Natural_Java_Reference_Collector_KIDICAP_Mini', propagate: false, parameters: [
                        extendedChoice(name: 'mxBuildVersion', value: fullBuild), 
                        string(name: 'javaVersion', value: javaVersion),
						booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
						listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)	
                    ]
                    miscUtils.evaluateBuildResult(buildResult, unstableProductions, successfulProductions)
                }
            }
			
			echo 'Successful productions:'
			echo successfulProductions.toString()
			echo 'Unstable productions:'
			echo unstableProductions.toString()
	    }
	}
	miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}\nFollowing productions are unstable:\n${unstableProductions.toString()}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
}