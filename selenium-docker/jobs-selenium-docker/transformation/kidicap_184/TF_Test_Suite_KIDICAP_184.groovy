@Library('TestUtils') _

/**
 * Runs a complete KIDICAP 18.4 test:
 * - nat2java
 * - start Oracle DB in docker with AGTV data
 * - production AGTV
 * - stop Oracle DB in docker
 * 
 * @param mxBuildVersion  The maxenso build to use.
 * @param parallelMode  Switch the parallelMode of the KIDICAP application on/off
 * @param withCodeCoverage  Whether the code coverage will be measured or not.
 * 		  type: Boolean
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
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
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			
			def fullBuild
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
				fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
			}
			buildDescription "parallelMode=${parallelMode} withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} fullBuild=${fullBuild} testProjectBranch=${testProjectBranch}"

			stage('nat2java') {
				def timeout = true
				def counter = 0
				def buildResult
				while (timeout && counter < 3) {
					buildResult = build job: 'TF_Natural_Java_Migration_KIDICAP_184', propagate: false, parameters: [
						extendedChoice(name: 'mxBuildVersion', value: fullBuild),
						booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
						string(name: 'javaVersion', value: javaVersion),
						booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
						listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)	
					]
					timeout = buildResult.getRawBuild().getLog().contains('Cancelling nested steps due to timeout')
					counter++
				}
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableProductions, successfulProductions)
			}
			
			stage('schema generation') {
				def buildResult = build job: 'TF_Natural_Java_Schema_Generation_KIDICAP_184', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: fullBuild),
					booleanParam(name: 'parallelMode', value: parallelMode),
					booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
					string(name: 'javaVersion', value: javaVersion),
					booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)	
				]
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableProductions, successfulProductions)
			}

			stage('production AGTV') {
				// WQATF-701: To be able to test runtime only builds we have to set updateDbSchema to false if the build under test is not a full build.
				def updateDbSchema = fullBuild == mxBuildVersion
				def performanceProfiling = Boolean.parseBoolean(withCodeCoverage) ?  false : true
				def buildResult = build job: 'TF_Natural_Java_Test_KIDICAP_184_AGTV', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
					booleanParam(name: 'parallelMode', value: parallelMode),
					booleanParam(name:'withCodeCoverage', value: withCodeCoverage),
					booleanParam(name: 'performanceProfiling', value: performanceProfiling),
					string(name: 'javaVersion', value: javaVersion),
					extendedChoice(name: 'kidicapBuild', value: fullBuild),
					booleanParam(name: 'updateDbSchema', value: updateDbSchema),
					booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)	
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
