@Library('TestUtils') _

/**
* Run the COBOL_ASYNC_API tests.
* @param mxBuildVersion The product build to be tested. Has been selected from the untested of approved builds.
* @param javaVersion The java version the test will run with
* @param useDifferentTestProjectBranch By default the project is checked out with branch master.
* 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
* @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
*/

def mxVersionUtils = new MxVersionUtils()
def gitUtils = new GitUtils()
def dockerUtils = new DockerUtils()
def miscUtils = new MiscUtils()

nodeTF('Docker-host') {
	catchError {	
		timestamps {
			
			def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/testcase-cobol-async-api.git'
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
			def workDir = pwd()
			
			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"						
					
			deleteDir();	
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
														
				stage('initialisation') {
					gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
					gitUtils.getLicenseFile(mxVersionUtils.getBranchForMxBuildVersion(mxBuildVersion), workDir)
				}
													
				stage('test') {
					try {
						withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
							sh "$MVN_CMD clean test -Dinnowake.version=${mxBuildVersion} -DmvnCompilerVersion=${miscUtils.getCompilerLevel(javaVersion)}"
							sh returnStatus: true, script: "mv ${workDir}/target/jacoco.exec ${workDir}/target/async-api-jacoco.exec"
							archiveArtifacts allowEmptyArchive: true, artifacts: 'target/async-api-jacoco.exec'
						}
					} catch (ex) {
						unstable "after mvn test: ${ex}"
					}
				}
			}
		}
	}
}
