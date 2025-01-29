@Library('TestUtils') _

/**
 * Runs a complete natural_vsam by triggering the following jobs (in the given order):
 *  - TF_Natural_Java_Migration_VSAM.groovy
 *  - TF_Natural_Java_Test_VSAM.groovy
 * @param mxBuildVersion  The maxenso build to use.
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranchJava The branch for Java project that will be used if useDifferentTestProjectBranch is set.
 * @param differentTestProjectBranchCsharp The branch for Csharp project that will be used if useDifferentTestProjectBranch is set.
 */

def miscUtils = new MiscUtils()
def mxVersionUtils = new MxVersionUtils()
def dockerUtils = new DockerUtils()

def unstableStages = [:]
def successfulStages = [:]

def listCsharp = mxVersionUtils.getVersionListByArtifactUrlSuffix('releases-dotnet/InnoWake.NET.Dependencies-dist', 'releases-dotnet/innowake.runtime-dotnet-dist',
		'releases-dotnet/innowake.runtime-dotnet-testframework-dist', 'releases-dotnet/innowake.runtime-torpedo-dotnet-dist')

nodeTF('Docker-host') {
	catchError {
	
	    timestamps() {
			def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
			def testProjectBranchJava = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranchJava, mxBuildVersion)
			def testProjectBranchCsharp = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranchCsharp, mxBuildVersion)
						
			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			def fullBuild
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
				fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
			}
			buildDescription "fullBuild=${fullBuild} testProjectBranchJava=${testProjectBranchJava} testProjectBranchCsharp=${testProjectBranchCsharp}"
	    
	        stage('TF_Natural_Java_Migration_VSAM') {
				def buildResult = build job: 'TF_Natural_Java_Migration_VSAM', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: fullBuild),
					string(name: 'javaVersion', value: javaVersion),
					booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranchJava)
				]
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
			}
	
	        stage('TF_Natural_Java_Test_VSAM') {
				def buildResult = build job: 'TF_Natural_Java_Test_VSAM', propagate: false, parameters: [
						extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
						string(name: 'javaVersion', value: javaVersion),
						booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
						listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranchJava)
					]
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
	        }

			stage('TF_Natural_Csharp_Migration_VSAM') {
				when(mxVersion == '99.9', 'Nat2Csharp migration is only available in trunk.') { 
					def buildResult = build job: 'TF_Natural_Csharp_Migration_VSAM', propagate: false, parameters: [
						extendedChoice(name: 'mxBuildVersion', value: fullBuild),
						string(name: 'javaVersion', value: javaVersion),
						booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
						listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranchCsharp)
					]
					miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
				}
			}

			stage('TF_Natural_Csharp_Test_VSAM') {
				when(mxVersion == '99.9', 'VSAM tests in Csharp are only available in trunk.') { 
					when(listCsharp.contains(mxBuildVersion), "Artifact is not available in version ${mxBuildVersion}") {
						['v4.7.2', 'v4.8'].each {
							dotnetFrameworkVersion ->
							def buildResult = build job: 'TF_Natural_Csharp_Test_VSAM', propagate: false, parameters: [
								extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
								string(name: 'dotnetFrameworkVersion', value: dotnetFrameworkVersion),
								booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
								listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranchCsharp)
							]
							miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages, "with dotnetFrameworkVersion=${dotnetFrameworkVersion}")
						}
					}
				}
	        }
			
			echo 'Successful stages:'
			echo successfulStages.toString()
			echo 'Unstable stages:'
			echo unstableStages.toString()
		}
	}
	miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}\nFollowing stages are unstable:\n${unstableStages.toString()}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
}