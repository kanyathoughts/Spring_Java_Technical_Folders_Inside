@Library('TestUtils') _

/**
 * Runs a complete natural_csharp by triggering the following jobs (in the given order):
 *  - TF_Natural_Csharp_Migration_VontobelSt.groovy
 *  - TF_Natural_Csharp_Test_VontobelSt.groovy
 * @param mxBuildVersion  The maxenso build to use.
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set. 
 * @param differentMigrationProjectBranch The branch that will be used for migration if useDifferentTestProjectBranch is set. 
 */

def miscUtils = new MiscUtils()
def mxVersionUtils = new MxVersionUtils()
def spUtils = new SharepointUtils()
def dockerUtils = new DockerUtils()

def unstableStages = [:]
def successfulStages = [:]

def listCsharp = mxVersionUtils.getVersionListByArtifactUrlSuffix('releases-dotnet/InnoWake.NET.Dependencies-dist', 'releases-dotnet/innowake.runtime-dotnet-dist',
		'releases-dotnet/innowake.runtime-dotnet-testframework-dist', 'releases-dotnet/innowake.runtime-torpedo-dotnet-dist')
def listExtension = mxVersionUtils.getVersionListByArtifactUrlSuffix('releases/innowake/products/mee/visualstudio/extensions/appmod-vs-extensions')
def artifactDir
def migrationArtifactsExist

nodeTF('Docker-host') {
	catchError {
	
	    timestamps() {
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
	        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			
			def fullBuild
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
				fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
			}
			buildDescription "javaVersion=${javaVersion} fullBuild=${fullBuild} testProjectBranch=${testProjectBranch}"
	    
	        stage('TF_Natural_Csharp_Migration_VontobelSt') {
				def buildResult = build job: 'TF_Natural_Csharp_Migration_VontobelSt', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: fullBuild),
					string(name: 'javaVersion', value: javaVersion),
					booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					listGitBranches(name: 'differentTestProjectBranch', value: differentMigrationProjectBranch)
				]
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
			}
	
	        stage('TF_Natural_Csharp_Test_VontobelSt') {
				when(listCsharp.contains(mxBuildVersion), "Artifact is not available in version ${mxBuildVersion}") {
					['v4.7.2', 'v4.8'].each {
						dotnetFrameworkVersion ->
						def buildResult = build job: 'TF_Natural_Csharp_Test_VontobelSt', propagate: false, parameters: [
							extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
							string(name: 'dotnetFrameworkVersion', value: dotnetFrameworkVersion),
							booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
							listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch),
							booleanParam(name: 'attributeMigrationTest', value: false)
						]
						miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages, "with dotnetFrameworkVersion=${dotnetFrameworkVersion}")
					}
				}
	        }

			stage('TF_Natural_Csharp_Test_VontobelSt_Attribute_Migration') {
			    docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
    				artifactDir = spUtils.getJenkinsArtifactsDir() + "/TF_Natural_Csharp_Migration_VontobelSt/${mxBuildVersion}"
    				migrationArtifactsExist = spUtils.fileExists("${artifactDir}/natural_csharp_src_attribute.zip")
			    }
			    
				when(migrationArtifactsExist, "Attribute migration artifacts not found in the sharepoint") {				
					when(listCsharp.contains(mxBuildVersion), "Artifact is not available in version ${mxBuildVersion}") {
						['v4.7.2', 'v4.8'].each {
							dotnetFrameworkVersion ->
							def buildResult = build job: 'TF_Natural_Csharp_Test_VontobelSt', propagate: false, parameters: [
								extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
								string(name: 'dotnetFrameworkVersion', value: dotnetFrameworkVersion),
								booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
								listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch),
								booleanParam(name: 'attributeMigrationTest', value: true)
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