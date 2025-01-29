@Library('TestUtils') _

/**
 * Run a production of the KIDICAP project.
 *
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        If parameter "kidicap_build" is set to "Latest stable build", this parameter is used to select a nat2java run with the same parameter setting for retrieval of KIDICAP artifacts.
 *        type: Extended Choice
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		  If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

def gitUtils = new GitUtils()
def mxVersionUtils = new MxVersionUtils()
def dockerUtils = new DockerUtils()
def miscUtils = new MiscUtils()

nodeTF('Docker-host') {
	catchError {
		timestamps {
			def dockerImage = 'postgres:14.6';
			def dbVolumeName = "exec_sql_db_volume"
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
			def workDir = pwd()

			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			
			sh returnStatus: true, script: "docker volume rm ${dbVolumeName}"
			sh "docker volume create ${dbVolumeName}"
			
			deleteDir()
			catchError(stageResult: 'FAILURE') {
				docker.image(dockerImage).withRun("-e POSTGRES_PASSWORD=mysecretpassword -v ${dbVolumeName}:/var/lib/postgresql/data") {
					container ->
					stage('Waiting for DB') {
						// sleep is required to wait until the database is up and running
						sleep 30
					}
					docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("--network=container:${container.id} -v jenkins-m2-repo:/var/m2_repo:rw") {
						def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
						buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} fullBuild=${fullBuild}"
						
						stage('Set up workspace') {
							def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/exec-sql.git'
							gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
							gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), workDir)
						}
		
						stage('Run tests') {
							try {
								withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
									sh "$MVN_CMD -DmvnCompilerVersion=${miscUtils.getCompilerLevel(javaVersion)} -Dinnowake.version=${mxBuildVersion} -Dinnowake.version.full.build=${fullBuild} test"

									sh returnStatus: true, script: "mv ${workDir}/target/jacoco.exec ${workDir}/target/execsql-jacoco.exec"
									archiveArtifacts allowEmptyArchive: true, artifacts: 'target/execsql-jacoco.exec'
								}
							} catch (e) {
								unstable 'Deviations in test results'
							}
						}
					}
				}
			}
			sh returnStatus: true, script: "docker volume rm ${dbVolumeName}"
		}
	}
	miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
}
