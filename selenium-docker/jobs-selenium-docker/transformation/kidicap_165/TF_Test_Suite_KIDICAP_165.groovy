@Library('TestUtils') _

/**
 * Runs a complete KIDICAP test:
 * - nat2java (once with and once without copycodes)
 * - production AGTV
 * - production OFDAN
 * - production AGTV2_part0103
 * - production AGTV2_part0409
 * - production AGTV2_part1020
 * 
 * @param mxBuildVersion  The maxenso build to use.
 * @param withCodeCoverage  Whether the code coverage will be measured or not.
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
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			
			def fullBuild
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
				fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
			}
			buildDescription "withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} fullBuild=${fullBuild} testProjectBranch=${testProjectBranch}"

			stage('nat2java') {
				runNat2java(fullBuild, 'true', successfulProductions, unstableProductions)
				runNat2java(fullBuild, 'false', successfulProductions, unstableProductions)
			}
			
			stage('schema generation') {
				def buildResult = build job: 'TF_Natural_Java_Schema_Generation_KIDICAP_165', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: fullBuild),
					booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
					string(name: 'javaVersion', value: javaVersion),
					booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)
				]
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableProductions, successfulProductions)
			}
			
			def productions = ['AGTV2Part0409', 'AGTV2Part1020', 'AGTV2Part0103', 'AGTV', 'OFDAN']
			productions.each {
				production ->
				stage(production) {
					def jobName = "TF_Natural_Java_Test_KIDICAP_165_${production}"					
					// WQATF-701: To be able to test runtime only builds we have to set updateDbSchema to false until WQATF-433 is done
					def updateDbSchema = false
					if (jobName.contains('_AGTV2') && fullBuild == mxBuildVersion) {
						updateDbSchema = true
					}
					def buildResult = build job: jobName, propagate: false, parameters: [
							extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
							booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
							string(name: 'javaVersion', value: javaVersion),
							string(name: 'kidicapBuild', value: fullBuild),
							booleanParam(name: 'updateDbSchema', value: updateDbSchema),
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

/**
 * Runs the KIDICAP nat2java and evaluates the results
 *
 * @param fullBuild  mxBuildVersion of a full build e.g. 19.4.04
 * @param transformCopycodes  True if the copycodes are to be transformed, false if not
 * @param successfulProductions  Map with all successful productions.
 * @param unstableProductions  Map with all unstable productions.
 * @return Jenkins build result
 */
def runNat2java(fullBuild, transformCopycodes, successfulProductions, unstableProductions) {
	def miscUtils = new MiscUtils()
	def timeout = true
	def counter = 0
	def buildResult
	while (timeout && counter < 3) {
		buildResult = build job: 'TF_Natural_Java_Migration_KIDICAP_165', propagate: false, parameters: [
			extendedChoice(name: 'mxBuildVersion', value: fullBuild),
			booleanParam(name: 'transformCopycodes', value: transformCopycodes),
			booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
			string(name: 'javaVersion', value: javaVersion),
			booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
			listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)]
		timeout = buildResult.getRawBuild().getLog().contains('Cancelling nested steps due to timeout')
		counter++
	}
	miscUtils.evaluateBuildResultWithExit(buildResult, unstableProductions, successfulProductions, "with transformCopycodes=${transformCopycodes}")
	return buildResult
}

