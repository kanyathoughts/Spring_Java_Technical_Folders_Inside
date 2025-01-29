@Library('TestUtils') _

/**
 * Run CAGen integration tests for Gen banking application against Oracle database.
 * @param caGenBuildVersion The caGen/hotfix version to migrate the Gen Application.
 * @param smokeTestsOnly When this parameter is selected, will execute Smoke Tests only else will execute all the tests for the Gen Application.
 * @param javaVersion The java version the test will run with
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

nodeTF('Docker-host && Region-US && !USLinux2') {
	timestamps {
		def dockerUtils = new DockerUtils()
		def gitUtils = new GitUtils()
		def mxVersionUtils = new MxVersionUtils()
		def spUtils = new SharepointUtils()
		def testProjectDir = "${pwd()}/integration-tests-oracle"
		def workDir = "${pwd()}/backend"
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, caGenBuildVersion)

		buildName "#${env.BUILD_ID} - ${caGenBuildVersion}"
		buildDescription "smokeTestsOnly=${smokeTestsOnly} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"

		stage('initialization') {
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v /data/mxJars:/data/mxJars:ro -v jenkins-m2-repo:/var/m2_repo:rw') {
				gitUtils.checkoutGitProject(testProjectDir, "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/cagen/integration-tests-oracle.git", testProjectBranch)
				spUtils.downloadJobArtifact('TF_CaGen_Java_Migration', caGenBuildVersion, 'backend.zip', pwd())
				unzip dir: './backend', zipFile: "backend.zip", quiet: true
				sh "cp -R ${testProjectDir}/src/test ${workDir}/src"
				sh "cp -R ${testProjectDir}/pom.xml ${workDir}"
			}
		}

		stage('perform tests') {
			def portsList = ['1521', '1522', '1523', '1524', '1525', '1526']
			def availablePort = dockerUtils.getAvailablePort(portsList)
			if (availablePort == 'No Port Available') {
				error 'No free port available'
			}

			docker.image(dockerUtils.pullDockerOracleImage('12.2.0.1')).withRun("-d -p ${availablePort}:1521") { dbContainer ->
				waitForDBToStart(dbContainer.id)
				sh "docker cp integration-tests-oracle/database_setup_files ${dbContainer.id}:/"
				sh "docker exec -t ${dbContainer.id} /bin/bash -c /database_setup_files/create_banking_user.sh"
				createTablesInDB(dbContainer.id)
				docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside(
						"--add-host=host.docker.internal:host-gateway -v jenkins-m2-repo:/var/m2_repo:rw") {
					dir(workDir) {
						withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
							try {
								def mvnCommand = "$MVN_CMD -Dspring.datasource.url='jdbc:oracle:thin:@host.docker.internal:${availablePort}:ORCLCDB' -Ddb.host=@host.docker.internal -Ddb.port=${availablePort} -Dcom.innowake.gen.framework.version=${caGenBuildVersion} -Dlogging.level.com.innowake=OFF -Dlogging.level.org.springframework=OFF test"
								if (Boolean.parseBoolean(smokeTestsOnly)) {
									sh "${mvnCommand} -Dtest=SmokeTests"
								} else {
									sh "${mvnCommand}"
								}
								sh returnStatus: true, script: "mv ${workDir}/target/jacoco.exec ${workDir}/target/cagen-oracle-integration-jacoco.exec"
								archiveArtifacts allowEmptyArchive: true, artifacts: 'target/cagen-oracle-integration-jacoco.exec'
							} catch (ex) {
								unstable "perform test exception: ${ex}"
							}
						}
					}
				}
			}
		}
	}
}

/**
 * Waits for the Oracle database to start.
 *
 * @param containerID The ID of the Oracle database container.
 * @throws Exception If Oracle database does not start in time.
 */
def waitForDBToStart(containerID) {
	final int SUCCESS_EXIT_CODE = 0
	final int A_FAILURE_EXIT_CODE = 1
	def messageFoundExitCode = A_FAILURE_EXIT_CODE
	String message = 'database is ready for use'
	int currentNumberOfAttempts = 0
	final int MAX_NUMBER_OF_ATTEMPTS = 20
	final int NUMBER_OF_SECONDS_TO_SLEEP= 10

	while (messageFoundExitCode != SUCCESS_EXIT_CODE && currentNumberOfAttempts < MAX_NUMBER_OF_ATTEMPTS) {
		echo "waiting for database to start ..."
		currentNumberOfAttempts++
		sleep NUMBER_OF_SECONDS_TO_SLEEP
		messageFoundExitCode = sh returnStatus: true, script: "docker logs ${containerID} | grep '${message}'"
	}
	if (messageFoundExitCode != SUCCESS_EXIT_CODE) {
		error 'The Oracle database was not ready in time.'
	}
}

/**
 * Creates tables in the Oracle database. The tables are required for the integration tests.
 *
 * @param containerID The ID of the Oracle database container.
 * @throws Exception If tables could not be created in the Oracle database.
 */
def createTablesInDB(containerID){
	int currentNumberOfAttempts = 0
	final int MAX_NUMBER_OF_ATTEMPTS = 20
	final int NUMBER_OF_SECONDS_TO_SLEEP= 10
	boolean commandSuccess = false
	while (!commandSuccess && currentNumberOfAttempts < MAX_NUMBER_OF_ATTEMPTS) {
		echo "checking if tables have been created"
		currentNumberOfAttempts++
		sleep NUMBER_OF_SECONDS_TO_SLEEP
		String output = sh returnStdout: true, script: "docker exec -t ${containerID} /bin/bash -c /database_setup_files/execute_ddl.sh"
		commandSuccess = output.contains('Index created')
	}
	if (!commandSuccess) {
    	error 'Could not create the tables in the Oracle database.'
	}
}