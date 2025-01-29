@Library('TestUtils') _

/**
 * Job script to compile and run all batch unit tests against a dockerized oracle database (works with mxBuildVersion > 18)
 * 
 * @param mxBuildVersion The maxenso/innowake build to test.
 * @param javaVersion The java version the test will run with
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
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
		def port
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"

		docker.image(dockerUtils.pullImage('oracle-MIGRATED', mxVersion)).withRun('-p 1521') {
			container ->
			stage('wait until DB is ready') {				
				if (dockerUtils.waitForOracle(container.id, 5)) {
					port = dockerUtils.getActualHostPort1521(container.id)
					echo "Oracle in container ${container.id} is up and running. DB is ready and listening on port ${port}. DB is running on ${miscUtils.getHostname()}."
				}
			}
			
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("--network=host -v jenkins-m2-repo:/var/m2_repo:rw") {
				def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
				buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} fullBuild=${fullBuild}"
				
				stage('prepare tests') {
					def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/batch-unit-test.git'
					gitUtils.checkoutGitProject(jobWorkspace, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
					gitUtils.getLicenseFile(mxVersionUtils.getBranchForMxBuildVersion(mxBuildVersion), '.')
					
					sh "sed -i \"s|hostname=qmsrv1.innowake.hq|hostname=localhost|g\" ${jobWorkspace}/configuration.properties"
					sh "sed -i \"s|port=1704|port=${port}|g\" ${jobWorkspace}/configuration.properties"
					if (testProjectBranch > '22.1' || testProjectBranch == '22.0' || testProjectBranch == '21.0') {
						sh "chmod 744 ${jobWorkspace}/src/test/resources/testcases/EXTERNALSCRIPT/bin/SCRIPT.sh"
					} else {
						sh "sed -i \"s,INTRDR.executable=src/test/resources/bin/script.bat,INTRDR.executable=src/test/resources/bin/script.sh,g\" ${jobWorkspace}/src/test/resources/conf/jclconfig.cfg"
						sh "chmod 744 ${jobWorkspace}/src/test/resources/bin/script.sh"
					}
				}
			
				stage('run test') {
					try {
						withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
							sh "$MVN_CMD dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=libs -Dmdep.stripVersion=true"

							sh "$MVN_CMD -Doracle.net.disableOob=true -DmvnCompilerVersion=${miscUtils.getCompilerLevel(javaVersion)} -Dinnowake.license.location=${jobWorkspace} -Dinnowake.version=${mxBuildVersion} -Dinnowake.version.full.build=${fullBuild} test"
							sh returnStatus: true, script: "mv ${jobWorkspace}/target/jacoco.exec ${jobWorkspace}/target/batch-jacoco.exec"
							archiveArtifacts allowEmptyArchive: true, artifacts: 'target/batch-jacoco.exec'
						}
					} catch (e) {
						unstable e.toString()
					}
				}
			}
		}
	}
	miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
}
