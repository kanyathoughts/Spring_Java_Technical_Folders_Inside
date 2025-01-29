@Library('TestUtils') _

/**
 * Test Suite for Async API
 * 
 * @param mxBuildVersion  The maxenso build to use.
 *        type: Choice Parameter
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranchCsharp The branch that will be used for C# if useDifferentTestProjectBranch is set.
 * @param differentTestProjectBranchJava The branch that will be used for Java if useDifferentTestProjectBranch is set.
 */

timestamps {
	def miscUtils = new MiscUtils()
	def mxVersionUtils = new MxVersionUtils()
	
	nodeTF {
		def unstableStages = [:]
		def successfulStages = [:]
		
		catchError {
			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			buildDescription "javaVersion=${javaVersion}"
			
			stage('TF_Cobol_Java_Test_Cobol_Async_API') {
				def buildResult = build job: 'TF_Cobol_Java_Test_Cobol_Async_API', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
					string(name: 'javaVersion', value: javaVersion),
					booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranchJava)
				]
				miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)
			}
			
			stage('TF_Cobol_Csharp_Test_Cobol_Async_API') {
				def listDotNetVersions = mxVersionUtils.getVersionListByArtifactUrlSuffix('releases-dotnet/InnoWake.NET.Dependencies-dist', 'releases-dotnet/innowake.runtime-dotnet-dist', 'releases-dotnet/innowake.runtime-dotnet-testframework-dist', 'releases-dotnet/innowake.runtime-torpedo-dotnet-dist')
				when(mxBuildVersion > '22.2', 'Job only supports versions > 22.2') {
					when(listDotNetVersions.contains(mxBuildVersion), "Artifact is not available in version ${mxBuildVersion}") {
						def buildResult = build job: 'TF_Cobol_Csharp_Test_Cobol_Async_API', propagate: false, parameters: [
							extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
							booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
							listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranchCsharp)
						]
						miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)
					}
				}
			}
		}
		miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
	}
}