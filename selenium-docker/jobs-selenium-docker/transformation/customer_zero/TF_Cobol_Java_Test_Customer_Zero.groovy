@Library('TestUtils') _

/**
 * Runs the CobolJavaJunitTest in the Customer Zero project.
 *
 * @param mxBuildVersion The maxenso/innowake build under test.
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * 		  type: Boolean
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param javaVersion The java version the environment will run with
 * @param environmentLabel The resource label that will be locked while the job is running.
 * @param migrationBuild The build of the TF_Cobol_Java_Migration_Customer_Zero job to fetch the migrated files from
 */
timestamps {
    def mxVersionUtils = new MxVersionUtils()
    def gitUtils = new GitUtils()
    def dockerUtils = new DockerUtils()
    def miscUtils = new MiscUtils()
    withCodeCoverage = Boolean.parseBoolean(withCodeCoverage)

    if ((environmentLabel == 'perf-environment') && withCodeCoverage) {
        echo 'Exiting job...'
        error 'The code coverage measurements affect the performance therefore we should not measure the code coverage and the performance at the same time. Please choose environmentLabel=trafo-environment when withCodeCoverage is set to true'
    }

    advancedLock(environmentLabel) {
        nodeTF(env.linuxNode, environmentLabel.contains('perf')) {
            def workDir = pwd()
            def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
            def fullBuild
            docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
            }
            def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
            def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/customer-zero/customer-zero-java.git'
            def mvnFlags = "-q -Dinnowake.runtime.version=${mxBuildVersion} -Dinnowake.compiler.version=${fullBuild}"
            def pathToLogFile = "item-management-test/src/test/performance/Customer_Zero_Java_performance_report.log"
            def mvnTestFlags = "${mvnFlags} -Dinnowake.lib.core.profile.ProfilingEnabled=true " +
                    "-Dlog4j2.configurationFile=${workDir}/item-management-test/src/test/performance/log4j2.xml"
            def CZ_DB_NAME = 'customerZeroDB'

            buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
            buildDescription "withCodeCoverage=${withCodeCoverage} testProjectBranch=${testProjectBranch} javaVersion=${javaVersion} environmentLabel=${environmentLabel} migrationBuild=${migrationBuild} fullBuild=${fullBuild}"

            deleteDir()
            stage('remove old database container') {
                sh returnStatus: true, script: "docker stop ${CZ_DB_NAME}"
                sh returnStatus: true, script: "docker rm -f ${CZ_DB_NAME}"
            }

            stage('prepare') {
                docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside {
                    gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                }
                copyMigratedFiles()
            }

            // Create the Customer Zero database. To do this, we start a plain db2 container, and copy over init.sh,
            // which will create the database and tables, and load initial records into some of the tables.
            withCredentials([usernamePassword(credentialsId: 'Customer_Zero_DB_Credentials', passwordVariable: 'dbPw',
                    usernameVariable: 'dbUser')]) {
                docker.image('ibmcom/db2:11.5.6.0a').withRun("-e 'LICENSE=accept' -e 'DB2INST1_PASSWORD=${dbPw}' " +
                        "--name ${CZ_DB_NAME} --privileged=true -p 50001:50000") {
                    stage('create customer zero database') {
                        int dbReadyStatus = 1
                        final int MAX_NUMBER_OF_ATTEMPTS = 50
                        int currentNumberOfAttempts = 0
                        while (dbReadyStatus != 0 && currentNumberOfAttempts < MAX_NUMBER_OF_ATTEMPTS) {
                            dbReadyStatus = sh returnStatus: true,
                                    script: "docker logs ${CZ_DB_NAME} | grep 'Setup has completed'"
                            echo 'waiting for the database to start ...'
                            currentNumberOfAttempts++
                            sleep 3
                        }
                        if (dbReadyStatus != 0) {
                            error 'Database could not start even after max number of retries.'
                        }
                        echo "Database is running"
                        sh "docker cp item-management-test/database_setup_files ${CZ_DB_NAME}:/"
                        sh "docker exec -t ${CZ_DB_NAME} /bin/bash -c /database_setup_files/init.sh"
                    }

                    String dockerRunParameters = '-v jenkins-m2-repo:/var/m2_repo:rw'
                    if (isUnix()) {
                        dockerRunParameters += ' --add-host=host.docker.internal:host-gateway'
                    }
                    docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside(dockerRunParameters) {
                        stage('resolve dependencies') {
                            copyLicensesIntoSubprojects(mxVersion)
                            dir('item-management-parent') {
                                withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                                    sh "$MVN_CMD clean install ${mvnFlags} -DskipTests"
                                }
                            }
                        }

                        dir('item-management-test') {
                            withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
                                stage('test') {
                                    sh "$MVN_CMD clean test ${mvnTestFlags} -DskipCodeCoverage=${!withCodeCoverage}"
                                }
                            }
                        }

                        stage('finalize') {
                            sh returnStatus: true, script: "mv ${workDir}/item-management-test/target/jacoco.exec ${workDir}/item-management-test/target/customer-zero-java-jacoco.exec"
                            archiveArtifacts artifacts: "${pathToLogFile}, item-management-test/target/customer-zero-java-jacoco.exec", allowEmptyArchive: true
                        }

                        stage('reporting') {
                            when(environment.contains('perf') && currentBuild.currentResult == 'SUCCESS', 
								'Performance reporting is not executed if the job is unstable or the environment is a performance environment') {
                                def buildResult = build job: 'TF_Performance_Reporting', propagate: false, parameters: [
									string(name: 'mxBuildVersion', value: mxBuildVersion),
									string(name: 'pathToLogFile', value: pathToLogFile),
									string(name: 'buildURL', value: BUILD_URL),
									string(name: 'pageID', value: '166900506'),
									string(name: 'numDaysDataToKeep', value: '20')
								]
                                miscUtils.evaluateBuildResult(buildResult, "TF_Performance_Reporting")
                            }
                        }
                    }
                }
            }
        }
    }
}

/**
 * Gets the migrated files from the TF_Cobol_Java_Migration_Customer_Zero job, and copies them into the customer-zero-java
 * project.
 */
def copyMigratedFiles() {
    if (migrationBuild.matches('.*StatusBuildSelector.*')) {
		copyArtifacts([
				projectName: 'TF_Cobol_Java_Migration_Customer_Zero',
				filter: 'result-comparison/tmp/result-compare/actual/*',
				selector: lastSuccessful(),
				parameters: "mxBuildVersion=${mxBuildVersion},javaVersion=${javaVersion}",
				fingerprintArtifacts: true,
		])
	} else {
		copyArtifacts([
				projectName: 'TF_Cobol_Java_Migration_Customer_Zero',
				filter: 'result-comparison/tmp/result-compare/actual/*',
				selector: buildParameter(migrationBuild),
				fingerprintArtifacts: true,
		])
	}

    // Remove default java files and copy over actual migrated files from the Customer_Zero_Migration job.
    String sourceDir = 'item-management-common/src/main/java/innowake/components/source/cobol'
    sh "rm -r ${sourceDir}"
    sh 'rm -r item-management-common/src/main/java/innowake/components/itemManagement'
    sh "cp -r result-comparison/tmp/result-compare/actual ${sourceDir}"
}

/**
 * Copies an innowake license into each customer-zero-java subproject.
 *
 * @param mxVersion The maxenso/innowake branch to test
 */
def copyLicensesIntoSubprojects(String mxVersion) {
    def gitUtils = new GitUtils()
    def workDir = pwd()
    List subprojects = ['item-management-parent', 'item-management-config', 'item-management-test',
                       'item-management-cic']
    subprojects.each  {
        subproject ->
        gitUtils.getLicenseFile(mxVersion, "${workDir}/${subproject}")
        dir(subproject) {
            sh 'mv maxenso.lic innowake.lic'
        }
    }
}