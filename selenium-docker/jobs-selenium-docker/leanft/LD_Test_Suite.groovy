@Library('TestUtils') _

/**
 * Job script to compile and run all available test cases for LegacyDevOps
 * 
 * @param mxBuildVersion 	The maxenso/innowake build to test.
 * @param javaVersion 		The java version the test will run with
 * @param isHotfix			Specifies which node to use. By default USLeanft11-LD is used. 
 							If isHotfix is set to true, USLeanft1 will be used.
 */

node('OS-Linux'){

    buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	buildDescription "mxBuildVersion=${mxBuildVersion} javaVersion=${javaVersion}"

	def miscUtils = new MiscUtils()
	
	isHotfix = Boolean.parseBoolean(isHotfix)
	def executeOn = (isHotfix) ? 'USLeanft1' : 'USLeanft11-LD'

    miscUtils.startTestSuite('LD_LeanFT_SmokeTests', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), [$class: 'NodeParameterValue', name: 'executeOn', labels: [executeOn], nodeEligibility: [$class: 'AllNodeEligibility']], string(name: 'javaVersion', value: javaVersion)])
	miscUtils.startTestSuite('LD_LeanFT_CompleteTest', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), [$class: 'NodeParameterValue', name: 'executeOn', labels: [executeOn], nodeEligibility: [$class: 'AllNodeEligibility']], string(name: 'javaVersion', value: javaVersion)])
}
