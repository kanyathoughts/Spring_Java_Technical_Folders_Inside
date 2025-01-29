@Library('TestUtils') _

/**
 * Job script to trigger Activation_Server_Tests with given mxBuildVersion.
 * 
 * @param mxBuildVersion 	The maxenso/innowake build to test.
 */
node {

    buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
    buildDescription "mxBuildVersion=${mxBuildVersion}"

	def miscUtils = new MiscUtils()

    stage('Activation_Server_Tests') {
	    miscUtils.startTestSuite('Activation_Server_Tests', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion)])
    }

}