@Library('TestUtils') _
/**
 * This job runs the db-crawler test: https://iriseu.deloitte.com/browse/WQST-732
 *
 * @param executeOn		The node where to execute the db-crawler test.
 * @param buildTag		The version under test.
 */

node(executeOn) {
    def dockerUtils = new DockerUtils()
    def miscUtils = new MiscUtils()
    def gitUtils = new GitUtils()
    def jobWorkspace = pwd()
    def testDir = "${jobWorkspace}/WQST-732"
    def parserCSVCreationTime
    def applicationLogCreationTime

    buildName "#${env.BUILD_ID} - ${buildTag}"
    buildDescription "executeOn=${executeOn} buildTag=${buildTag}"
    /**
	* Check if a build is already running, if true then abort the current build
	*
	*/
    stage('Check if already running') {
        if (currentBuild.previousBuild.rawBuild.isBuilding() || currentBuild.previousBuild.previousBuild.rawBuild.isBuilding()) {
            error('A previous build is still running.')
        }
    }

    /**
    * In the US Linux servers, setup the NMSLO database. Please refer to 
    * https://confluence.innowake.hq/display/~sadey/Steps+to+setup+NMSLO+DB+in+US+Linux+servers for the steps to setup a database.
    *
    * Database setup is successful.
    *
    */  
    stage('Step 01') { 
        echo '************************************'
        echo 'Starting DB Crawler tests: Step 01'
        def buildResult = build job: 'Mod_Docker_Start_DB_NM_SLO', propagate: false, parameters: [
				[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeOn], nodeEligibility: [$class: 'AllNodeEligibility']]
			]
        if (buildResult.result == 'SUCCESS') {
            echo 'Step 01 passed'
        } else {
            error 'Step 01 failed, NMSLO DB setup failed'
        }
    }
    
    /**
	* Place the required db driver jar on the server. The <driver>.jar file can be found at
    * https://gitlab.consulting.sltc.com/appmod/qef/innowake-test-projects/modernization-test-projects/nmslo.git
	*
    * The db driver jars are present on the linux server.
    *
    */  
    stage('Step 02') {
        echo '************************************'
        echo 'Starting DB Crawler tests: Step 02'
        deleteDir()
	    def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/modernization-test-projects/nmslo.git"
        def folderContents = 'none'
		dir(jobWorkspace) {
			git branch: 'master', credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
		}
        sh 'mkdir WQST-732'
        sh "cp mssql-jdbc-9.4.0.jre8.jar ${testDir}/mssql-jdbc-9.4.0.jre8.jar"
        dir(testDir) { 
            folderContents = sh returnStdout: true, script: 'ls'
            if (folderContents.contains('mssql-jdbc-9.4.0.jre8.jar')) {
                echo 'Step 02 passed'
            } else {
                error 'Step 02 failed, MSSQL DB driver Jar not present in workspace.'
            }
        }
	}

    /**
	* Place the correct db crawler jar file (version under test) on the server using WinSCP or Putty.
	*
    * The correct jar file (version under test) is available in the package registry (nightly build artifacts).
    * https://pd-gitlab.deloitte.com/innowake/dbcutter-registry/-/packages
    */  
    stage('Step 03') {
        echo '************************************'
        echo 'Starting DB Crawler tests: Step 03'
        withCredentials([string(credentialsId: 'pd-gitlab-registry-token', variable: 'registryAccessToken')]) {
			sh "curl -k --header \"PRIVATE-TOKEN: ${registryAccessToken}\" \"https://pd-gitlab.deloitte.com/api/v4/projects/103/packages/generic/db-cutter/${buildTag}/db-crawler-${buildTag}.jar\" --output ${testDir}/db-crawler-${buildTag}.jar"
		}
        sh "sleep 10"
        def folderContents = 'none'
        dir(testDir) { 
            folderContents = sh returnStdout: true, script: 'ls'
            if (folderContents.contains("db-crawler-${buildTag}.jar")) {
                echo 'Step 03 passed'
            } else {
                error 'Step 03 failed, DB Crawler Jar not present in testDir.'
            }
        }
    }

    /**
	* Type in the command for crawling
	*
    * Crawling starts and after around 5 minutes it shows a success message "Finished crawling the database"
    * The following artifacts are created:
    * a log folder containing an application.log file
    * a file "dbcrawler_schema_nmslo_step05.json"
    * a file "parser.csv"
    * Check if each of those artifacts equal to their expected results.
    *
    */
    stage('Step 05') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 05'
                def step05 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                def schemaContent = 'none'
                try {
                    step05 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo_step05.json"
                } catch (err) {
                   unstable 'Step 05 failed - crawling is not successful'
                } finally {
                    if ((step05.contains('Finished crawling the database.')) && (step05 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_nmslo_step05.json')) {
                             folderContents = sh returnStdout: true, script: 'ls log'
                             echo 'The contents in the directory are:'
                             echo folderContents
                             if (folderContents.contains('application.log') && folderContents.contains('parser.csv')) {
                                parserCSVCreationTime = sh returnStdout: true, script: 'stat -c %z  log/parser.csv'
                                applicationLogCreationTime = sh returnStdout: true, script: 'stat -c %z  log/application.log'
                                applicationLogContent = sh returnStdout: true, script: 'cat log/application.log'
                                schemaContent = sh returnStdout: true, script: 'cat dbcrawler_schema_nmslo_step05.json'
                                def size
                                if (applicationLogContent.contains('Exception')) {
                                    applicationLogContent = applicationLogContent.split('Exception')
                                    size = applicationLogContent.size()
                                } else {
                                    size = 0
                                }
                                echo "application.log file contains ${size} exceptions"    
                                if (size > 4) {
                                    unstable 'Step 05 failed - application log contains too many exceptions'
                                } else {
                                    if(!schemaContent.contains('model.sys')) {
                                        echo 'Step 05 passed'
                                        sh 'cp log/application.log application_step05.log'
                                        sh 'cp log/parser.csv parser_step05.csv'
                                    }
                                }
                            } else {
                                unstable 'Step 05 failed - either application.log or parser.csv is not present'
                            }
                        } else {
                            unstable 'Step 05 failed - either log folder or dbcrawler_schema_nmslo_step05.json is not present'
                        }
                    } else {
                        unstable 'Step 05 failed - crawling is not successful'
                    }
                }
            }
        }
    }

    /**
	* Type in the command for crawling after removing -cp from it
	*
    * An error message "Error: Could not find or load main class dbcrawler.jar:mssql-jdbc-9.4.0.jre8.jar" is displayed.
    *
    */
    stage('Step 06') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************' 
                echo 'Starting DB Crawler tests: Step 06'
                def step06 = 'none'
                def result = 'failed'
                try {
                    step06 = sh returnStdout: true, script: "java -Xmx8g \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo.json"
                } catch (err) {
                    result = 'passed'
                } finally {
                    if (result.contains('passed')) {
                        echo 'Step 06 passed'
                    } else {
                        unstable 'Step 06 failed - crawling occurs even without -cp'
                    }
                }
            }
        }
    }

    /**
	* Revert back to the original command and add -ess false at the end to include the system tables
	*
    * Crawling starts and after a few minutes it shows a success message "Finished crawling the database"
    * The log folder gets updated and the file dbcrawler_schema_nmslo_step07.json gets generated with the expected content.
    *
    */
    stage('Step 07') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************' 
                echo 'Starting DB Crawler tests: Step 07'
                def step07 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                def schemaContent = 'none'
                try {
                    step07 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo_step07.json -ess false"
                } catch (err) {
                   unstable 'Step 07 failed - an exception is thrown'
                } finally {
                    if ((step07.contains('Finished crawling the database.')) && (step07 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        def parserCSVCreationTimeStep07
                        def applicationLogCreationTimeStep07
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_nmslo_step07.json')) {
                             folderContents = sh returnStdout: true, script: 'ls log'
                             echo 'The contents in the directory are:'
                             echo folderContents
                             if (folderContents.contains('application.log') && folderContents.contains('parser.csv')) {
                                applicationLogContent = sh returnStdout: true, script: 'cat log/application.log'
                                parserCSVCreationTimeStep07 = sh returnStdout: true, script: 'stat -c %z  log/parser.csv'
                                applicationLogCreationTimeStep07 = sh returnStdout: true, script: 'stat -c %z  log/application.log'
                                schemaContent = sh returnStdout: true, script: 'cat dbcrawler_schema_nmslo_step07.json'
                                if (parserCSVCreationTime != parserCSVCreationTimeStep07 && applicationLogCreationTime != applicationLogCreationTimeStep07) {
                                    parserCSVCreationTime = parserCSVCreationTimeStep07
                                    applicationLogCreationTime = applicationLogCreationTimeStep07
                                } else {
                                    unstable 'Step 07 failed - either application.log or parser.csv or both are not updated'
                                }
                                def size
                                if (applicationLogContent.contains('Exception')) {
                                    applicationLogContent = applicationLogContent.split('Exception')
                                    size = applicationLogContent.size()
                                } else {
                                    size = 0
                                }
                                echo "application.log file contains ${size} exceptions"
                                if (size > 4) {
                                    unstable 'Step 07 failed - application log contains too many exceptions'
                                } else {
                                    if(schemaContent.contains('"size" : 40') && schemaContent.contains('"size" : 30') && schemaContent.contains('"size" : 20') && schemaContent.contains('"size" : 10') && schemaContent.contains('model.sys')) {
                                        def content = schemaContent.split('"size"')
                                        println(content.size());
                                        if(content.size()>5170) {
                                            echo 'Step 07 passed'
                                            sh 'cp log/application.log application_step07.log'
                                            sh 'cp log/parser.csv parser_step07.csv'
                                        }
                                    }
                                }
                            } else {
                                unstable 'Step 07 failed - either application.log or parser.csv or both are not present'
                            }
                        } else {
                            unstable 'Step 07 failed - either log folder or dbcrawler_schema_nmslo_step07.json or both are not present'
                        }
                    } else {
                        unstable 'Step 07 failed - crawling is not successful'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling without specifying the mssql jdbc jar
	*
    * An error message "Application closed down unexpectedly: Could not get connection properties: 
    * Could not find a suitable JDBC driver for database connection URL, 
    * jdbc:sqlserver://localhost;databaseName=unit_test_1" is displayed.
    *
    */
    stage('Step 08') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 08'
                def step08 = 'none'
                try {
                    step08 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo.json"
                } catch (err) {
                    unstable 'Step 08 failed - an exception is thrown'
                } finally {
                    if (step08.contains('Could not find a suitable JDBC driver for database connection URL')) {
                        echo 'Step 08 passed'
                    } else {
                        unstable 'Step 08 failed - crawling occurs without specifying mssql jdbc jar parameter'
                    }
                }
            }
        }
    }

    /**
	* Delete the jdbc driver jar from the directory using the command: rm mssql-jdbc-9.4.0.jre8.jar
	*
    * The jar file gets deleted successfully.
    *
    */
    stage('Step 09') {
        def folderContents = 'none'
        echo '************************************'
        echo 'Starting DB Crawler tests: Step 09'
        dir(testDir) {
            sh 'rm mssql-jdbc-9.4.0.jre8.jar'
            folderContents = sh returnStdout: true, script: 'ls'
        }
        if (folderContents.contains('mssql-jdbc-9.4.0.jre8.jar')) {
            unstable 'Step 09 failed - file not deleted'
        } else {
            echo 'Step 09 passed'
        }
    }

    /**
	* Run the command for crawling
	*
    * An error message "Application closed down unexpectedly: Could not get connection properties: 
    * Could not find a suitable JDBC driver for database connection URL, jdbc:sqlserver://localhost;databaseName=unit_test_1"
    * is displayed as in step 9.
    *
    */
    stage('Step 10') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 10'
                def step10 = 'none'
                try {
                    step10 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo.json"
                } catch (err) {
                    unstable 'Step 10 failed - an exception is thrown'
                } finally {
                    if (step10.contains('Could not find a suitable JDBC driver for database connection URL')) {
                        echo 'Step 10 passed'
                    } else {
                        unstable 'Step 10 failed - crawling occurs without the mssql jdbc jar'
                    }
                }
            }
        }
    }

    /**
	* Place the mssql-jdbc-9.4.0.jre8.jar in the directory again
	*
    * The jar file is available in the directory again.
    *
    */
    stage('Step 11') {
        def folderContents = 'none'
        echo '************************************'
        echo 'Starting DB Crawler tests: Step 11'
        sh "cp mssql-jdbc-9.4.0.jre8.jar ${testDir}/mssql-jdbc-9.4.0.jre8.jar"
        dir(testDir) {
            folderContents = sh returnStdout: true, script: 'ls'
        }
        if (folderContents.contains('mssql-jdbc-9.4.0.jre8.jar')) {
            echo 'Step 11 passed'
        } else {
            unstable 'Step 11 failed - file not present in directory'
        }
    }

    /**
	* Run the command for crawling without specifying the dbcrawler jar
	*
    * An error message "Could not find or load main class innowake.innovationlab.dbcrawler.Main" is displayed.
    *
    */
    stage('Step 12') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 12'
                def step12 = 'none'
                def result = 'failed'
                try {
                    step12 = sh returnStdout: true, script: "java -Xmx8g -cp \"mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo.json"
                } catch (err) {
                    result ='passed'
                } finally {
                    if (result.contains('passed')) {
                        echo 'Step 12 passed'
                    } else {
                        unstable 'Step 12 failed - crawling is succesful without specifying the dbcrawler jar'
                    }
                }
            }
        }
    }

    /**
	* Delete the db crawler jar from the directory using the command: rm dbcrawler-1.2.0
	*
    * The jar file gets deleted successfully.
    *
    */
    stage('Step 13') {
        def folderContents = 'none'
        echo '************************************'
        echo 'Starting DB Crawler tests: Step 13'
        dir(testDir) {
            sh "rm db-crawler-${buildTag}.jar"
            folderContents = sh returnStdout: true, script: 'ls'
        }
        if (folderContents.contains("db-crawler-${buildTag}.jar")) {
            unstable 'Step 13 failed - file delete is not successful'
        } else {
            echo 'Step 13 passed'
        }
    }

    /**
	* Run the command for crawling
	*
    * An error message "Could not find or load main class innowake.innovationlab.dbcrawler.Main" is displayed.
    *
    */
    stage('Step 14') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 14'
                def step14 = 'none'
                def result = 'failed'
                try {
                    step14 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo.json"
                } catch (err) {
                    result ='passed'
                } finally {
                    if (result.contains('passed')) {
                        echo 'Step 14 passed'
                    } else {
                        unstable 'Step 14 failed - crawling is succesful without the dbcrawler jar'
                    }
                }
            }
        }
    }

    /**
	* Place the dbcrawler.jar in the directory again.
	*
    * The jar file is available in the directory again.
    *
    */
    stage('Step 15') {
         def folderContents = 'none'
        echo '************************************'
        echo 'Starting DB Crawler tests: Step 15'
        withCredentials([string(credentialsId: 'pd-gitlab-registry-token', variable: 'registryAccessToken')]) {
			sh "curl -k --header \"PRIVATE-TOKEN: ${registryAccessToken}\" \"https://pd-gitlab.deloitte.com/api/v4/projects/103/packages/generic/db-cutter/${buildTag}/db-crawler-${buildTag}.jar\" --output ${testDir}/db-crawler-${buildTag}.jar"
		}
		dir(testDir) {
            folderContents = sh returnStdout: true, script: 'ls'
        }
        if (folderContents.contains("db-crawler-${buildTag}.jar")) {
            echo 'Step 15 passed'
        } else {
            unstable 'Step 15 failed - file not present in the directory'
        }
     }

    /**
	* Run the command for crawling without specifying the main class.
	*
    * An error message "Unrecognized option: -c Error: Could not create the Java Virtual Machine. 
    * Error: A fatal exception has occurred. Program will exit." is displayed.
    *
    */
     stage('Step 16') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 16'
                def step16 = 'none'
                def result = 'failed'
                try {
                    step16 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo.json"
                } catch (err) {
                    result = 'passed'
                } finally {
                    if (result.contains('passed')) {
                        echo 'Step 16 passed'
                    } else {
                        unstable 'Step 16 failed - crawling is successful without specifying the main class.'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling without specifying -c
	*
    * An error message "Missing required option: c usage: dbcrawler" is displayed.
    *
    */
    stage('Step 17') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 17'
                def step17 = 'none'
                try {
                    step17 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main - \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo.json"
                } catch (err) {
                    unstable 'Step 17 failed - an exception thrown'
                } finally {
                    if (step17.contains('Missing required option: c')) {
                        echo 'Step 17 passed'
                    } else {
                        unstable 'Step 17 failed - crawling is successful without specifying parameter -c'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling without specifying u for username and p for password
	*
    * An error message "Application closed down unexpectedly: Could not connect to jdbc:sqlserver://localhost;databaseName=unit_test_1, 
    * for unspecified user, with properties {}: Login failed for user '' is displayed.
    *
    */
    stage('Step 18') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 18'
                def step18 = 'none'
                try {
                    step18 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" ${dbUser} ${dbPassword} -f dbcrawler_schema_nmslo.json"
                } catch (err) {
                    unstable 'Step 18 failed - an exception thrown'
                } finally {
                    if (step18.contains('Login failed for user')) {
                        echo 'Step 18 passed'
                    } else {
                        unstable 'Step 18 failed - crawling is successful without specifying the u and p parameters for username and password'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling without specifying the correct username and password
	*
    * An error message "Application closed down unexpectedly: Could not connect to jdbc:sqlserver://localhost;databaseName=unit_test_1, 
    * for unspecified user, with properties {}: Login failed for user '' is displayed.
    *
    */
    stage('Step 19') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 19'
                def step19 = 'none'
                try {
                    step19 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u testuser -p ${dbPassword} -f dbcrawler_schema_nmslo.json"
                } catch (err) {
                    unstable 'Step 19 failed - an excpetion thrown'
                } finally {
                    if (step19.contains('Login failed for user')) {
                        echo 'Step 19 passed'
                    } else {
                        unstable 'Step 19 failed - crawling is successful without specifying the correct username and password'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling after replacing localhost with 127.0.0.1
	*
    * Crawling starts and after a few minutes it shows a success message "Finished crawling the database" 
    * The log folder gets updated and the file dbcrawler_schema_nmslo_step20.json gets generated.
    *
    */
    stage('Step 20') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 20'
                def step20 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                try {
                    step20 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://127.0.0.1;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo_step20.json"
                } catch (err) {
                   unstable 'Step 20 failed - an exception is thrown'
                } finally {
                    if ((step20.contains('Finished crawling the database.')) && (step20 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        def parserCSVCreationTimeStep20
                        def applicationLogCreationTimeStep20
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_nmslo_step20.json')) {
                             folderContents = sh returnStdout: true, script: 'ls log'
                             echo 'The contents in the directory are:'
                             echo folderContents
                             if (folderContents.contains('application.log') && folderContents.contains('parser.csv')) {
                                parserCSVCreationTimeStep20 = sh returnStdout: true, script: 'stat -c %z  log/parser.csv'
                                applicationLogCreationTimeStep20 = sh returnStdout: true, script: 'stat -c %z  log/application.log'
                                applicationLogContent = sh returnStdout: true, script: 'cat log/application.log'
                                if (parserCSVCreationTime != parserCSVCreationTimeStep20 && applicationLogCreationTime != applicationLogCreationTimeStep20) {
                                    parserCSVCreationTime = parserCSVCreationTimeStep20
                                    applicationLogCreationTime = applicationLogCreationTimeStep20
                                } else {
                                    unstable 'Step 20 failed - either application.log or parser.csv or both are not updated'
                                }
                                def size
                                if (applicationLogContent.contains('Exception')) {
                                    applicationLogContent = applicationLogContent.split('Exception')
                                    size = applicationLogContent.size()
                                } else {
                                    size = 0
                                }
                                echo "application.log file contains ${size} exceptions"                                
                                if (size > 4) {
                                    unstable 'Step 20 failed - application log contains too many exceptions'
                                } else {
                                    echo 'Step 20 passed'
                                    sh 'cp log/application.log application_step20.log'
                                    sh 'cp log/parser.csv parser_step20.csv'
                                }
                            } else {
                                unstable 'Step 20 failed - either application.log or parser.csv or both are not present'
                            }
                        } else {
                            unstable 'Step 20 failed - either log folder or dbcrawler_schema_nmslo_step20.json or both are not present'
                        }
                    } else {
                        unstable 'Step 20 failed - crawling is not successful'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling specifying parameters username instead of u and password instead of p
	*
    * Crawling starts and after a few minutes it shows a success message "Finished crawling the database" 
    * The log folder gets updated and the file dbcrawler_schema_nmslo_step21.json gets generated.
    *
    */
    stage('Step 21') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 21'
                def step21 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                try {
                    step21 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1;username=${dbUser};password=${dbPassword}\" -f dbcrawler_schema_nmslo_step21.json"
                } catch (err) {
                   unstable 'Step 21 failed, an exception is thrown'
                } finally {
                    if ((step21.contains('Finished crawling the database.')) && (step21 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        def parserCSVCreationTimeStep21
                        def applicationLogCreationTimeStep21
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_nmslo_step21.json')) {
                             folderContents = sh returnStdout: true, script: 'ls log'
                             echo 'The contents in the directory are:'
                             echo folderContents
                             if (folderContents.contains('application.log') && folderContents.contains('parser.csv')) {
                                parserCSVCreationTimeStep21 = sh returnStdout: true, script: 'stat -c %z  log/parser.csv'
                                applicationLogCreationTimeStep21 = sh returnStdout: true, script: 'stat -c %z  log/application.log'
                                applicationLogContent = sh returnStdout: true, script: 'cat log/application.log'
                                if (parserCSVCreationTime != parserCSVCreationTimeStep21 && applicationLogCreationTime != applicationLogCreationTimeStep21) {
                                    parserCSVCreationTime = parserCSVCreationTimeStep21
                                    applicationLogCreationTime = applicationLogCreationTimeStep21
                                } else {
                                    unstable 'Step 21 failed - either application.log or parser.csv or both are not updated'
                                }
                                def size
                                if (applicationLogContent.contains('Exception')) {
                                    applicationLogContent = applicationLogContent.split('Exception')
                                    size = applicationLogContent.size()
                                } else {
                                    size = 0
                                }
                                echo "application.log file contains ${size} exceptions"
                                if (size > 4) {
                                    unstable 'Step 21 failed - application log contains too many exceptions'
                                } else {
                                    echo 'Step 21 passed'
                                    sh 'cp log/application.log application_step21.log'
                                    sh 'cp log/parser.csv parser_step21.csv'
                                }
                            } else {
                                unstable 'Step 21 failed - either application.log or parser.csv or both are not present'
                            }
                        } else {
                            unstable 'Step 21 failed - either log folder or dbcrawler_schema_nmslo_step21.json or both are not present'
                        }
                    } else {
                        unstable 'Step 21 failed - crawling is not successful'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling without specifying -Xmx8g
	*
    * Crawling starts and after a few minutes it shows a success message "Finished crawling the database" 
    * The log folder gets updated and the file dbcrawler_schema_nmslo_step22.json gets generated.
    *
    */
    stage('Step 22') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 22'
                def step22 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                try {
                    step22 = sh returnStdout: true, script: "java -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo_step22.json"
                } catch (err) {
                   unstable 'Step 22 failed, an exception is thrown'
                } finally {
                    if ((step22.contains('Finished crawling the database.')) && (step22 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        def parserCSVCreationTimeStep22
                        def applicationLogCreationTimeStep22
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_nmslo_step22.json')) {
                             folderContents = sh returnStdout: true, script: 'ls log'
                             echo 'The contents in the directory are:'
                             echo folderContents
                             if (folderContents.contains('application.log') && folderContents.contains('parser.csv')) {
                                parserCSVCreationTimeStep22 = sh returnStdout: true, script: 'stat -c %z  log/parser.csv'
                                applicationLogCreationTimeStep22 = sh returnStdout: true, script: 'stat -c %z  log/application.log'
                                applicationLogContent = sh returnStdout: true, script: 'cat log/application.log'
                                if (parserCSVCreationTime != parserCSVCreationTimeStep22 && applicationLogCreationTime != applicationLogCreationTimeStep22) {
                                    parserCSVCreationTime = parserCSVCreationTimeStep22
                                    applicationLogCreationTime = applicationLogCreationTimeStep22
                                } else {
                                    unstable 'Step 22 failed - either application.log or parser.csv or both are not updated'
                                }
                                def size
                                if (applicationLogContent.contains('Exception')) {
                                    applicationLogContent = applicationLogContent.split('Exception')
                                    size = applicationLogContent.size()
                                } else {
                                    size = 0
                                }
                                echo "application.log file contains ${size} exceptions"
                                if (size > 4) {
                                    unstable 'Step 22 failed - application log contains too many exceptions'
                                } else {
                                    echo 'Step 22 passed'
                                    sh 'cp log/application.log application_step22.log'
                                    sh 'cp log/parser.csv parser_step22.csv'
                                }
                            } else {
                                unstable 'Step 22 failed - either application.log or parser.csv or both are not present'
                            }
                        } else {
                            unstable 'Step 22 failed - either log folder or dbcrawler_schema_nmslo_step22.json or both are not present'
                        }
                    } else {
                        unstable 'Step 22 failed - crawling is not successful'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling after specifying the schemas to be blacklisted using -sbl
	*
    * Crawling starts and after a few minutes it shows a success message "Finished crawling the database". 
    * Crawling does not take place for the database schemas:
    * model.db_ddladmin
    * master.db_accessadmin
    * tempdb.db_datawriter
    * unit_test_1
    * This can be verified from dbcrawler_schema_nmslo_step23.json
    *
    */
    stage('Step 23') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 23'
                def step23 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                try {
                    step23 = sh returnStdout: true, script: "java -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo_step23.json -sbl model.db_ddladmin,master.*,unit_test_1.*"
                } catch (err) {
                   unstable 'Step 23 failed'
                } finally {
                    if ((step23.contains('Finished crawling the database.')) && (step23 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        fileContents = sh returnStdout: true, script: 'cat dbcrawler_schema_nmslo_step23.json'
                        fileContents = fileContents.split('schemes')[1]
                        def parserCSVCreationTimeStep23
                        def applicationLogCreationTimeStep23
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_nmslo_step23.json') && !fileContents.contains('model.db_ddladmin') && !fileContents.contains('master.db_accessadmin')  && !fileContents.contains('unit_test_1')) {
                             folderContents = sh returnStdout: true, script: 'ls log'
                             echo 'The contents in the directory are:'
                             echo folderContents
                             if (folderContents.contains('application.log') && folderContents.contains('parser.csv')) {
                                parserCSVCreationTimeStep23 = sh returnStdout: true, script: 'stat -c %z  log/parser.csv'
                                applicationLogCreationTimeStep23 = sh returnStdout: true, script: 'stat -c %z  log/application.log'
                                applicationLogContent = sh returnStdout: true, script: 'cat log/application.log'
                                if (parserCSVCreationTime != parserCSVCreationTimeStep23 && applicationLogCreationTime != applicationLogCreationTimeStep23) {
                                    parserCSVCreationTime = parserCSVCreationTimeStep23
                                    applicationLogCreationTime = applicationLogCreationTimeStep23
                                } else {
                                    unstable 'Step 23 failed - either application.log or parser.csv or both are not updated'
                                }
                                def size
                                if (applicationLogContent.contains('Exception')) {
                                    applicationLogContent = applicationLogContent.split('Exception')
                                    size = applicationLogContent.size()
                                } else {
                                    size = 0
                                }
                                echo "application.log file contains ${size} exceptions"
                                if (size > 4) {
                                    unstable 'Step 23 failed - application log contains too many exceptions'
                                } else {
                                    echo 'Step 23 passed'
                                    sh 'cp log/application.log application_step23.log'
                                    sh 'cp log/parser.csv parser_step23.csv'
                                }
                            } else {
                                unstable 'Step 23 failed - either application.log or parser.csv or both are not present'
                            }
                        } else {
                            unstable 'Step 23 failed - either log folder or dbcrawler_schema_nmslo_step23.json or both are not present or dbcrawler_schema_nmslo_step23.json does not contain the expected content'
                        }
                    } else {
                        unstable 'Step 23 failed - crawling is not successful'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling after specifying the schemas to be whitelisted using -swl
	*
    * Crawling starts and after a few minutes it shows a success message "Finished crawling the database".
    * Crawling takes place only for the database schema unit_test_1
    * This can be verified from dbcrawler_schema_nmslo_step24.json
    *
    */
    stage('Step 24') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 24'
                def step24 = 'failed'
                def folderContents = 'none'
                def fileContents = 'none'
                def applicationLogContent = 'none'
                try {
                    step24 = sh returnStdout: true, script: "java -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo_step24.json -swl unit_test_1.*"
                } catch (err) {
                   unstable 'Step 24 failed'
                } finally {
                    if ((step24.contains('Finished crawling the database.')) && (step24 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        fileContents = sh returnStdout: true, script: 'cat dbcrawler_schema_nmslo_step24.json'
                        def parserCSVCreationTimeStep24
                        def applicationLogCreationTimeStep24
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_nmslo_step24.json') && !fileContents.contains('model.db_ddladmin') && !fileContents.contains('master.db_accessadmin') && !fileContents.contains('tempdb.db_datawriter')) {
                             folderContents = sh returnStdout: true, script: "ls log"
                             echo 'The contents in the directory are:'
                             echo folderContents
                             if (folderContents.contains('application.log') && folderContents.contains('parser.csv')) {
                                applicationLogContent = sh returnStdout: true, script: 'cat log/application.log'
                                parserCSVCreationTimeStep24 = sh returnStdout: true, script: 'stat -c %z  log/parser.csv'
                                applicationLogCreationTimeStep24 = sh returnStdout: true, script: 'stat -c %z  log/application.log'
                                if (parserCSVCreationTime != parserCSVCreationTimeStep24 && applicationLogCreationTime != applicationLogCreationTimeStep24) {
                                    parserCSVCreationTime = parserCSVCreationTimeStep24
                                    applicationLogCreationTime = applicationLogCreationTimeStep24
                                } else {
                                    unstable 'Step 24 failed - either application.log or parser.csv or both are not updated'
                                }
                                def size
                                if (applicationLogContent.contains('Exception')) {
                                    applicationLogContent = applicationLogContent.split('Exception')
                                    size = applicationLogContent.size()
                                } else {
                                    size = 0
                                }
                                echo "application.log file contains ${size} exceptions"
                                if (size > 4) {
                                    unstable 'Step 24 failed - application log contains too many exceptions'
                                } else {
                                    echo 'Step 24 passed'
                                    sh 'cp log/application.log application_step24.log'
                                    sh 'cp log/parser.csv parser_step24.csv'
                                }
                            } else {
                                unstable 'Step 24 failed - either application.log or parser.csv or both are not present'
                            }
                        } else {
                            unstable 'Step 24 failed - either log folder or dbcrawler_schema_nmslo_step24.json or both are not present or dbcrawler_schema_nmslo_step24.json does not contain the expected content'
                        }
                    } else {
                        unstable 'Step 24 failed - crawling is not successful'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling after specifying the schemas to be whitelisted and blaclisted using -swl and -sbl together
	*
    * An error message is displayed "Either blacklist or whitelist can be used, but not both together".
    *
    */
    stage('Step 25') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 25'
                def step25 = 'none'
                try {
                    step25 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:mssql-jdbc-9.4.0.jre8.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:sqlserver://localhost;databaseName=unit_test_1\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_nmslo.json -sbl model.* -swl unit_test_1.*"
                } catch (err) {
                    unstable 'Step 25 failed - an exception is thrown'
                } finally {
                    if (step25.contains('Either blacklist or whitelist can be used, but not both together.')) {
                        echo 'Step 25 passed'
                    } else {
                        unstable 'Step 25 failed - crawling is successful with both blacklist and whitelist together.'
                    }
                }
            }
        }
    }

    stage('Archive artifacts') {
        try {
            dir(jobWorkspace) {
                archiveArtifacts "WQST-732/**"
            }
        } catch (err) {
            echo 'Artifacts not generated'
                currentBuild.result = 'UNSTABLE'
        }
    }
}
