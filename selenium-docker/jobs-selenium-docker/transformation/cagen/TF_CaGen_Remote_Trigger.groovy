 @Library('TestUtils') _
 
/**
 * This job is intended to be triggered from a remote Jenkins instance
 * This happens every night if the PD build was successfull.
 *
 * @param mxBuildVersion maxenso/innowake version. Passed by the calling job
 * @param caGenBuildVersion cagen version. Passed by the calling job
 */
 
nodeTF {
	def mxVersionUtils = new MxVersionUtils()
	def miscUtils = new MiscUtils()
	
	buildName "#${env.BUILD_ID} - ${caGenBuildVersion}"
	buildDescription "mxBuildVersion=${mxBuildVersion}"
	
	miscUtils.startTestSuite('TF_Test_Suite_CaGen', [extendedChoice(name: 'caGenBuildVersion', value: caGenBuildVersion), string(name: 'javaVersion', value: 'java11')])

	stage('TF_Test_Suite_CaGen_UI') {
		when(caGenBuildVersion >= '22.1', 'We will skip the TF_Test_Suite_CaGen_UI stage, because the stage should' +
				' only be run against a caGenBuildVersion that supports gui applications.') {
			def buildResult = build job: 'TF_Test_Suite_CaGen_UI', propagate: false, parameters: [
					string(name: 'caGenBuildVersion', value: caGenBuildVersion),
					extendedChoice(name: 'javaVersion', value: 'java11')
			]
			miscUtils.evaluateBuildResult(buildResult)
		}
	}
}
