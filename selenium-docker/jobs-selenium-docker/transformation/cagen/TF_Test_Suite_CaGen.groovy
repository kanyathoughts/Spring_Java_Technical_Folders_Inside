@Library('TestUtils') _

/**
 * Runs a complete CaGen test:
 * - TF_CaGen_Java_Migration (migration of Gen Model to Java application)
 * - TF_CaGen_Java_Test_Mssql_Integration
 * - TF_CaGen_Java_Test_Mssql_Functional
 * - TF_CaGen_Java_Test_Oracle_Integration
 * - TF_CaGen_Java_Test_Oracle_Functional
 *
 * @param caGenBuildVersion The caGen build to use.
 * @param javaVersion The java version the test will run with
 */

def miscUtils = new MiscUtils()

timestamps() {
	def unstableStages = [:]
	def successfulStages = [:]

	nodeTF('Docker-host && Region-US') {
		catchError {
			buildName "#${env.BUILD_ID} - ${caGenBuildVersion}"
			buildDescription "javaVersion=${javaVersion}"

			stage('TF_CaGen_Java_Migration') {
				def buildResult = build job: 'TF_CaGen_Java_Migration', quietPeriod: 0, propagate: false,
						parameters: [
						        string(name: 'caGenBuildVersion', value: caGenBuildVersion),
						        extendedChoice(name: 'javaVersion', value: javaVersion)
						]
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
            }

			stage('TF_CaGen_Java_Test_Mssql_Integration') {
				def buildResult = build job: 'TF_CaGen_Java_Test_Mssql_Integration', quietPeriod: 0, propagate: false,
						parameters: [
						        string(name: 'caGenBuildVersion', value: caGenBuildVersion),
						        booleanParam(name: 'smokeTestsOnly', value: false),
						        extendedChoice(name: 'javaVersion', value: javaVersion)
						]
				miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)
			}

			stage('TF_CaGen_Java_Test_Mssql_Functional') {
				def buildResult = build job: 'TF_CaGen_Java_Test_Mssql_Functional', quietPeriod: 0, propagate: false,
                        parameters: [
                                string(name: 'caGenBuildVersion', value: caGenBuildVersion),
                                extendedChoice(name: 'javaVersion', value: javaVersion)
                        ]
                miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)
            }

            stage('TF_CaGen_Java_Test_Oracle_Integration') {
                def buildResult = build job: 'TF_CaGen_Java_Test_Oracle_Integration', quietPeriod: 0, propagate: false,
                        parameters: [
                                string(name: 'caGenBuildVersion', value: caGenBuildVersion),
                                booleanParam(name: 'smokeTestsOnly', value: false),
                                extendedChoice(name: 'javaVersion', value: javaVersion)
                        ]
                miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)
            }

            stage('TF_CaGen_Java_Test_Oracle_Functional') {
                def buildResult = build job: 'TF_CaGen_Java_Test_Oracle_Functional', quietPeriod: 0, propagate: false,
                        parameters: [
                                string(name: 'caGenBuildVersion', value: caGenBuildVersion),
                                extendedChoice(name: 'javaVersion', value: javaVersion)
                        ]
                miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)
            }

            echo 'Successful stages:'
            echo successfulStages.toString()
            echo 'Unstable stages:'
            echo unstableStages.toString()
		}
		miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} caGenBuildVersion = ${caGenBuildVersion}\nFollowing stages are unstable:\n${unstableStages.toString()}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
	}
}