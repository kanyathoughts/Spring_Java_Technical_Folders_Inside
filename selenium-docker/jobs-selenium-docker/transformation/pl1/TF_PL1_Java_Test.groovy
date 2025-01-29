@Library('TestUtils') _

 /**
 * Checkout and run the PL/I tests in Java.
 *
 * @param mxBuildVersion  The maxenso version to run the PL/I tests.
 * @param useDifferentTestProjectBranch By default the pl1-java project is checked out with branch master. 
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param javaVersion The java version the test will run with
 * @param environmentLabel The resource label that will be locked while the job is running.
 */
 
timestamps {
    def mxVersionUtils = new MxVersionUtils()
    def gitUtils = new GitUtils()
    def dockerUtils = new DockerUtils()
    def miscUtils = new MiscUtils()

    advancedLock(environmentLabel) {
        nodeTF(env.linuxNode, environmentLabel.contains('perf')) {
            def workDir = pwd()
            def testProjectDir = "${pwd()}/pl1-java"
            def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
            def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
            def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/pl1-test.git'
            def czDbName = 'customerZeroDB'
            def fullBuild

            buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
            docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
                fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
            }
            buildDescription "testProjectBranch=${testProjectBranch} javaVersion=${javaVersion} environmentLabel=${environmentLabel} fullBuild=${fullBuild}"

            deleteDir()
            stage('remove old database container') {
                sh returnStatus: true, script: "docker stop ${czDbName}"
                sh returnStatus: true, script: "docker rm -f ${czDbName}"
            }

            stage('prepare') {
                docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside {
                    gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                }
                testProjectDir = "${pwd()}/pl1-java"
                copyArtifacts projectName: 'TF_PL1_Java_Migration',
                            filter: 'eclipseWorkspace/Test-Project/src-pl1-java/**',
                            selector: lastSuccessful(),
                            parameters: "mxBuildVersion=${fullBuild}",
                            target: 'artifacts'
                            fingerprintArtifacts: true

                // Remove default java files and copy over actual migrated files from the PL1_Migration job.                
                sh "rm -rf ${testProjectDir}/pl1-java/src-pl1-java/*"	  
                sh "cp -r artifacts/eclipseWorkspace/Test-Project/src-pl1-java/* ${testProjectDir}/src-pl1-java"
            }

            // Create the Customer Zero database. To do this, we start a plain db2 container, and copy over init.sh,
            // which will create the database and tables, and load initial records into some of the tables.
            withCredentials([usernamePassword(credentialsId: 'Customer_Zero_DB_Credentials', passwordVariable: 'dbPw', usernameVariable: 'dbUser')]) {
                docker.image('ibmcom/db2:11.5.6.0a').withRun("-e 'LICENSE=accept' -e 'DB2INST1_PASSWORD=${dbPw}' " + 
                "--name ${czDbName} --privileged=true -p 50001:50000") {
                   
                    stage('create database') {
                        def dbReadyStatus = 1
                        def MAX_NUMBER_OF_ATTEMPTS = 50
                        def currentNumberOfAttempts = 0
                        while (dbReadyStatus != 0 && currentNumberOfAttempts < MAX_NUMBER_OF_ATTEMPTS) {
                            dbReadyStatus = sh returnStatus: true,
                            script: "docker logs ${czDbName} | grep 'Setup has completed'"
                            echo 'waiting for the database to start ...'
                            currentNumberOfAttempts++
                            sleep 3
                        }
                        if (dbReadyStatus != 0) {
                            error 'Database could not start even after max number of retries.'
                        }
                        echo 'Database is running'
                        sh "docker cp pl1-java/database_setup_files ${czDbName}:/"
                        sh "docker exec -t ${czDbName} /bin/bash -c /database_setup_files/init.sh"
                    }

                    docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw --add-host=host.docker.internal:host-gateway') {

                        stage('resolve dependencies') {
                            gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), testProjectDir)
                            dir(testProjectDir) {
                                withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                                    sh "$MVN_CMD -U dependency:resolve -Dinnowake.version=${mxBuildVersion}"
                                    sh "$MVN_CMD clean compile test-compile -Dinnowake.version=${mxBuildVersion}"
                                }
                            } 
                        }
                        
                        stage('test') {
                            withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
                                dir(testProjectDir) {
                                    try {
                                        sh "$MVN_CMD test -Dinnowake.version=${mxBuildVersion}"
                                    } catch (ex) {
                                        unstable "Exception occurred: ${ex}"
                                    } finally {
                                        //Archive files if deviations are found
                                        sh returnStatus: true, script: "mv ${testProjectDir}/target/jacoco.exec ${testProjectDir}/target/pl1-jacoco.exec"
                                        archiveArtifacts artifacts: 'test/res/jcl/*/tmp/*,target/pl1-jacoco.exec', allowEmptyArchive: true
						            }
                                }
                            }
                        } 
                    }
                }   
            }
        }
    }
}
