@Library('TestUtils') _

/**
 * Runs a complete NMSLO test:
 * - TF_Cobol_Csharp_Migration_NMSLO
 * - TF_Cobol_Csharp_Test_NMSLO
 *
 * @param mxBuildVersion  The maxenso build to use.
 * @param javaVersion The java version the test will run with
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
	def listExtension = mxVersionUtils.getVersionListByArtifactUrlSuffix('releases/innowake/products/mee/visualstudio/extensions/appmod-vs-extensions')
	def listCsharp = mxVersionUtils.getVersionListByArtifactUrlSuffix('releases-dotnet/InnoWake.NET.Dependencies-dist')
	def cobol2csharpExecuted = false
	def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
	
	nodeTF('Docker-host') {
		catchError {
			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			
			def fullBuild
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
				fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
			}
			buildDescription "javaVersion=${javaVersion} fullBuild=${fullBuild}  testProjectBranch=${testProjectBranch}"
			
			stage('TF_Cobol_Csharp_Migration_NMSLO') {
				when(listExtension.contains(fullBuild), "Artifact 'appmod-vs-extensions' is not available in version ${fullBuild}") {
					def buildResult = build job: 'TF_Cobol_Csharp_Migration_NMSLO', quietPeriod: 0, propagate: false, 
						parameters: [
							extendedChoice(name: 'mxBuildVersion', value: fullBuild),
							booleanParam(name: 'withCodeCoverage', value: 'true'),
							string(name: 'javaVersion', value: javaVersion),
							booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
							listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)]
					cobol2csharpExecuted = true
					miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
				}
			}
			
			stage('TF_Cobol_Csharp_Test_NMSLO') {
				when(listCsharp.contains(mxBuildVersion), "Artifact is not available in version ${mxBuildVersion}") {
					when(cobol2csharpExecuted, 'cobol2csharp did not run') {
						def buildResult = build job: 'TF_Cobol_Csharp_Test_NMSLO', quietPeriod: 0, propagate: false, 
							parameters: [
								extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
								extendedChoice(name: 'environmentLabel', value: 'perf-environment'),
								booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
								listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)
							]
						miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)
					}
				}
			}
			
			echo 'Successful stages:'
			echo successfulStages.toString()
			echo 'Unstable stages:'
			echo unstableStages.toString()
		}
		miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}\nFollowing stages are unstable:\n${unstableStages.toString()}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
	}
}
