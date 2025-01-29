@Library('TestUtils') _

/**
 * Runs a complete license validation by triggering the following jobs (in the given order):
 *  - TF_Cobol_Java_Migration_License_Validation.groovy
 *  - TF_Cobol_Csharp_Migration_License_Validation.groovy
 *  - TF_Cobol_Java_Test_License_Validation.groovy
 *  - TF_Cobol_Csharp_Test_License_Validation.groovy
 * @param mxBuildVersion  The maxenso build to use.
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set. 
 */

def miscUtils = new MiscUtils()
def mxVersionUtils = new MxVersionUtils()
def dockerUtils = new DockerUtils()

def unstableStages = [:]
def successfulStages = [:]

def listCsharp = mxVersionUtils.getVersionListByArtifactUrlSuffix('releases-dotnet/InnoWake.NET.Dependencies-dist', 'releases-dotnet/innowake.runtime-dotnet-dist')

nodeTF('Docker-host') {
	catchError {
	
	    timestamps() {
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
			def javaMigrationBuildSelector
			def csharpMigrationBuildSelector
			
	        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	        
			def fullBuild
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
				fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
			}
			buildDescription "javaVersion=${javaVersion} fullBuild=${fullBuild} testProjectBranch=${testProjectBranch}"
	
	        stage('TF_Cobol_Java_Migration_License_Validation') {
				def buildResult = build job: 'TF_Cobol_Java_Migration_License_Validation', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: fullBuild),
					string(name: 'javaVersion', value: javaVersion),
					booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)
				]
				javaMigrationBuildSelector = mxVersionUtils.getBuildSelector(mxBuildVersion, fullBuild, buildResult.number)
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
			}
	
	        stage('TF_Cobol_Csharp_Migration_License_Validation') {
				def buildResult = build job: 'TF_Cobol_Csharp_Migration_License_Validation', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: fullBuild),
					string(name: 'javaVersion', value: javaVersion),
					booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)
				]
				csharpMigrationBuildSelector = mxVersionUtils.getBuildSelector(mxBuildVersion, fullBuild, buildResult.number)
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
			}
	
	        stage('TF_Cobol_Java_Test_License_Validation') {						
				def buildResult = build job: 'TF_Cobol_Java_Test_License_Validation', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
					string(name: 'javaVersion', value: javaVersion),
					string(name: 'migrationBuild', value: javaMigrationBuildSelector),
					booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)
				]
				miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages, "with javaVersion=${javaVersion}")				
			}
	
	        stage('TF_Cobol_Csharp_Test_License_Validation') {
				when(listCsharp.contains(mxBuildVersion), "Artifact is not available in version ${mxBuildVersion}") {
					['v4.7.2', 'v4.8'].each {
						dotnetFrameworkVersion ->
						def buildResult = build job: 'TF_Cobol_Csharp_Test_License_Validation', propagate: false, parameters: [
							extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
							string(name: 'dotnetFrameworkVersion', value: dotnetFrameworkVersion),
							string(name: 'migrationBuild', value: csharpMigrationBuildSelector),
							booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
							listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)
						]
						miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages, "with dotnetFrameworkVersion=${dotnetFrameworkVersion}")
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