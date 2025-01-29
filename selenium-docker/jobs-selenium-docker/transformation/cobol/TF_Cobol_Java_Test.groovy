@Library('TestUtils') _

/**
 * Job script to compile and run cobol-java unit tests
 *
 * @param mxBuildVersion The maxenso/innowake build to test.
 * @param javaVersion The java version the test will run with
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

def gitUtils = new GitUtils()
def mxVersionUtils = new MxVersionUtils()
def dockerUtils = new DockerUtils()
def miscUtils = new MiscUtils()

nodeTF('Docker-host') {
	catchError {
		def jobWorkspace = pwd()
		def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		def projectNameJava = 'cobol-java'
		def fullBuild

		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"

		deleteDir()
		docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("--network=host -v jenkins-m2-repo:/var/m2_repo:rw") {

			fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
			buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} fullBuild=${fullBuild}"

			stage('prepare tests') {
				def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/cobol-java-test.git'
				gitUtils.checkoutGitProject(jobWorkspace, "${gitUtils.getGitUrlQef()}${remoteProjectLocation}", testProjectBranch)
				gitUtils.getLicenseFile(mxVersionUtils.getBranchForMxBuildVersion(mxBuildVersion), '.')

				//clear the folder with expected java sources
				sh "rm -rf ${projectNameJava}/src-cobol-java/*"

				//copy newly migrated sources
				copyArtifacts([
						projectName: 'TF_Cobol_Java_Migration',
						filter: '**/src-cobol-java/**',
						selector: lastSuccessful(),
						parameters: "mxBuildVersion=${fullBuild}",
						target: "${projectNameJava}/src-cobol-java/",
						fingerprintArtifacts: true,
						flatten: true
				])
			}
			stage('run test') {
				try {
					dir(projectNameJava) {
						withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
							sh "$MVN_CMD clean test -Dinnowake.version=${mxBuildVersion} -Dinnowake.license.location=${jobWorkspace}"
						}
					}
				} catch (e) {
					unstable e.toString()
				} finally {
					//Archive jacoco code coverage report
					sh returnStatus: true, script: "mv ${jobWorkspace}/cobol-java/target/jacoco.exec ${jobWorkspace}/cobol-java/target/cobol-java-jacoco.exec"
					archiveArtifacts allowEmptyArchive: true, artifacts: '**/*.exec'
				}
			}
		}
	}
	miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
}
