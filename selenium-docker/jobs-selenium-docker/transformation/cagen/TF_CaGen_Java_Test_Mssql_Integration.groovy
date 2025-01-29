@Library('TestUtils') _

/**
 * Run CAGen migration and Integration tests for Gen banking application.
 * @param caGenBuildVersion  The caGen/hotfix version to migrate the Gen Application.
 * @param smokeTestsOnly  When this parameter is selected, will execute Smoke Tests only else will execute all the tests for the Gen Application.
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

		def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(caGenBuildVersion)
		def testProjectDir = "${pwd()}/integration-tests-mssql"
		def workDir = "${pwd()}/backend"

		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, caGenBuildVersion)
		buildName "#${env.BUILD_ID} - ${caGenBuildVersion}"
		buildDescription "smokeTestsOnly=${smokeTestsOnly} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"

		stage('initialization') {
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v /data/mxJars:/data/mxJars:ro -v jenkins-m2-repo:/var/m2_repo:rw') {
				gitUtils.checkoutGitProject(testProjectDir, "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/cagen/integration-tests-mssql.git", testProjectBranch)
				spUtils.downloadJobArtifact('TF_CaGen_Java_Migration', caGenBuildVersion, 'backend.zip', pwd())                
				unzip dir: './backend', zipFile: "backend.zip", quiet: true
				sh "cp -R ${testProjectDir}/src/test ${workDir}/src"
				sh "cp -R ${testProjectDir}/pom.xml ${workDir}"
			}
		}

		stage('perform tests') {
			def portsList = ['1434', '1435', '1436', '1437', '1438', '1439']
			def availablePort = dockerUtils.getAvailablePort(portsList)
			if (availablePort == 'No Port Available') {
				error 'No free port available'
			}
			withCredentials([usernamePassword(credentialsId: 'CaGen-DB-Credentials', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
				docker.image('mcr.microsoft.com/mssql/server:2019-CU10-ubuntu-16.04').withRun("-e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=${dbPassword}' -d -i -p  ${availablePort}:1433") { dbContainer ->
					def dbReadyStatus = 1
					while (dbReadyStatus != 0) {
						dbReadyStatus = sh returnStatus: true, script: "docker logs ${dbContainer.id} | grep 'SQL Server is now ready for client connections'"
						echo 'waiting for DB ...'
						sleep 3
					}
					echo 'DB up and running'
					sh "docker cp ${testProjectDir}/INTDB_SchemaDocker.ddl ${dbContainer.id}:/home"
					sh "docker cp ${testProjectDir}/INTDB_CompatibilityLevel.ddl ${dbContainer.id}:/home"
					sh "docker exec ${dbContainer.id} /opt/mssql-tools/bin/sqlcmd -U ${dbUser} -P ${dbPassword} -i /home/INTDB_SchemaDocker.ddl"
					sh "docker exec ${dbContainer.id} /opt/mssql-tools/bin/sqlcmd -U ${dbUser} -P ${dbPassword} -i /home/INTDB_CompatibilityLevel.ddl"
					echo 'DB prepared'
					docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("--add-host=host.docker.internal:host-gateway -v jenkins-m2-repo:/var/m2_repo:rw") {
						dir(workDir) {
							withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
								try {
									def mvnCommand = "$MVN_CMD -Dcom.innowake.gen.database-type=mssql -Dcom.innowake.gen-targeting.database-type=mssql -Dspring.datasource.driver-class-name=com.microsoft.sqlserver.jdbc.SQLServerDriver -Dspring.datasource.url='jdbc:sqlserver://host.docker.internal:${availablePort};database=INTDB' -Ddb.host=host.docker.internal -Ddb.port=${availablePort} -Dcom.innowake.gen.framework.version=${caGenBuildVersion} -Dlogging.level.com.innowake=OFF -Dlogging.level.org.springframework=OFF test"
									if (Boolean.parseBoolean(smokeTestsOnly)) {
										sh "${mvnCommand} -Dtest=SmokeTests"
									} else {
										sh "${mvnCommand}"
									}
									sh returnStatus: true, script: "mv ${workDir}/target/jacoco.exec ${workDir}/target/cagen-mssql-integration-jacoco.exec"
									archiveArtifacts allowEmptyArchive: true, artifacts: 'target/cagen-mssql-integration-jacoco.exec'
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
}
