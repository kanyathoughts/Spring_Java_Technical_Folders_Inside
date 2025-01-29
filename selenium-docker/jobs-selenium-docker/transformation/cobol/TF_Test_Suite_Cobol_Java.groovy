@Library('TestUtils') _

/**
 * Runs a complete Cobol_Java test:
 * - TF_Cobol_Java_Migration (migration of Cobol files to java)
 * - TF_Cobol_Java_Test
 *
 * @param mxBuildVersion  The maxenso build to use.
 * @param javaVersion The java version the test will run with.
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

def miscUtils = new MiscUtils()
def mxVersionUtils = new MxVersionUtils()
def dockerUtils = new DockerUtils()

timestamps() {
	def unstableStages = [:]
	def successfulStages = [:]

	nodeTF('Docker-host') {
		catchError {
			echo "mxBuildVersion = ${mxBuildVersion}"
			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			
			def fullBuild
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
				fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
			}
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
			buildDescription "javaVersion=${javaVersion} fullBuild = ${fullBuild}  testProjectBranch=${testProjectBranch}"

			stage('TF_Cobol_Java_Migration') {
				def buildResult = build job: 'TF_Cobol_Java_Migration', quietPeriod: 0, propagate: false,
					parameters: [
						extendedChoice(name: 'mxBuildVersion', value: fullBuild),
						string(name: 'javaVersion', value: javaVersion),
						booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
						listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)]
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
			}
	
			stage('TF_Cobol_Java_Test') {
				def buildResult = build job: 'TF_Cobol_Java_Test', quietPeriod: 0, propagate: false,
					parameters: [
						extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
						string(name: 'javaVersion', value: javaVersion),
						booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
						listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)]
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages, "with javaVersion=${javaVersion}")
			}
			
			echo 'Successful stages:'
			echo successfulStages.toString()
			echo 'Unstable stages:'
			echo unstableStages.toString()
		}
		miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}\nFollowing stages are unstable:\n${unstableStages.toString()}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
	}
}