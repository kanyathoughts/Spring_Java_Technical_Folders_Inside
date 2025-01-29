@Library('TestUtils') _

/**
 * Runs a complete CaGen UI test:
 * - TF_CaGen_Java_Migration_UI
 * - TF_CaGen_Java_Test_UI
 *
 * @param caGenBuildVersion The caGen build to use.
 * @param javaVersion The java version the test will run with
 */

def miscUtils = new MiscUtils()

timestamps {
	def unstableStages = [:]
	def successfulStages = [:]

	nodeTF('Docker-host && Region-US') {
		catchError {
			buildName "#${env.BUILD_ID} - ${caGenBuildVersion}"
			buildDescription "javaVersion=${javaVersion}"

			stage('TF_CaGen_Java_Migration_UI') {
				def buildResult = build job: 'TF_CaGen_Java_Migration_UI', propagate: false, parameters: [
					string(name: 'caGenBuildVersion', value: caGenBuildVersion),
					extendedChoice(name: 'javaVersion', value: javaVersion)
				]
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
			}

			stage('TF_CaGen_Java_Test_UI') {
				def buildResult = build job: 'TF_CaGen_Java_Test_UI', propagate: false, parameters: [
					string(name: 'caGenBuildVersion', value: caGenBuildVersion),
					extendedChoice(name: 'javaVersion', value: javaVersion),
					extendedChoice(name: 'browser', value: 'chrome')
				]
				miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)

				build job: 'TF_CaGen_Java_Test_UI', propagate: false, parameters: [
					string(name: 'caGenBuildVersion', value: caGenBuildVersion),
					extendedChoice(name: 'javaVersion', value: javaVersion),
					extendedChoice(name: 'browser', value: 'edge')
				]
			}

			echo 'Successful stages:'
			echo successfulStages.toString()
			echo 'Unstable stages:'
			echo unstableStages.toString()
		}
		miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} caGenBuildVersion = ${caGenBuildVersion}\nFollowing stages are unstable:\n${unstableStages.toString()}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
	}
}