@Library('TestUtils') _

/**
 * Run the CICS tests.
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
			def oracleImageTag = dockerUtils.pullImage('oracle-MIGRATED', mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion))

			def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/cics-test-suite.git'
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
			def workDir = pwd()
			
			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"

			docker.image(oracleImageTag).withRun { container ->

				stage('waiting-for-DB') {
					dockerUtils.waitForOracle(container.id, 5)
				}

				deleteDir()
				docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("--network=container:${container.id} -v /data/mxJars:/data/mxJars:ro -v jenkins-m2-repo:/var/m2_repo:rw") {
					def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
					def versionParameters = "-Dinnowake.version=${mxBuildVersion} -Dinnowake.version.full.build=${fullBuild}"
					buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} fullBuild=${fullBuild}"
					
					stage('initialisation') {
						gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)

						gitUtils.getLicenseFile(mxVersionUtils.getBranchForMxBuildVersion(mxBuildVersion), workDir)
						def currentDbHost = 'localhost'
						def currentDbPort = '1521'
						dir('./src/main/resources') {
							sh "sed -i 's,integration.server.port=1700,integration.server.port=${currentDbPort},g' configuration.properties"
							sh "sed -i 's,integration.server.name=@//qmsrv1.innowake.hq,integration.server.name=@//${currentDbHost},g' configuration.properties"
						}
						withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
							sh "$MVN_CMD -U clean dependency:resolve ${versionParameters}"
						}
					}

					stage('compile') {
						withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
							sh "$MVN_CMD compile ${versionParameters}"
						}
					}

					stage('test') {
						try {
							withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
								sh "$MVN_CMD test ${versionParameters} -DmvnCompilerVersion=${miscUtils.getCompilerLevel(javaVersion)}"

								sh returnStatus: true, script: "mv ${workDir}/target/jacoco.exec ${workDir}/target/cics-jacoco.exec"
								archiveArtifacts allowEmptyArchive: true, artifacts: 'target/cics-jacoco.exec'
							}
						} catch (ex) {
							unstable "after mvn test: ${ex}"
						}
					}
				}
			}
		}
	}
	miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
}