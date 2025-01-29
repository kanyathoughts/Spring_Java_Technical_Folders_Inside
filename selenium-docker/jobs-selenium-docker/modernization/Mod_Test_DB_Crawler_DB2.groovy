@Library('TestUtils') _
/**
 * This job runs the db-crawler test against Mannheimer DB2 database: https://iriseu.deloitte.com/browse/WQST-967
 *
 * @param executeOn		The node where to execute the db-crawler test
 * @param buildTag		The version under test.
 */

node(executeOn) {
    def dockerUtils = new DockerUtils()
    def miscUtils = new MiscUtils()
    def gitUtils = new GitUtils()
    def jobWorkspace = pwd()
    def testDir = "${jobWorkspace}/WQST-967"
    def parserCSVCreationTime
    def applicationLogCreationTime
    def containerPort

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
    * Use the following Jenkins job to setup the Mannheimer DB2 database: 
    http://qef-linux1-us.deloitte.com:8085/view/modernization/job/Mod_Docker_Start_DB2/
    *
    * Database setup is successful.
    *
    */  
    stage('Step 01') { 
        echo '************************************'
        echo 'Starting DB Crawler tests: Step 01'
        def buildResult = build job: 'Mod_Docker_Start_DB2', propagate: false, parameters: [
				[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeOn], nodeEligibility: [$class: 'AllNodeEligibility']]
			]
            def buildDesc = buildResult.getDescription()
            containerPort = buildDesc.split('=')[1].trim()
            echo "${buildDesc}"
        if (buildResult.result == 'SUCCESS') {
            echo 'Step 01 passed'
        } else {
            error 'Step 01 failed, Mannheimer DB2 database setup failed'
        }
    }
    
    /**
	* Place the required db driver jars: db2jcc.jar and db2jcc_license_cu.jar on the server:
    * https://gitlab.consulting.sltc.com/appmod/qef/innowake-test-projects/modernization-test-projects/mannheimer.git
	*
    * The db driver jars are present on the linux server.
    *
    */  
    stage('Step 02') {
        echo '************************************'
        echo 'Starting DB Crawler tests: Step 02'
        deleteDir()
	    def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/modernization-test-projects/mannheimer.git"
        def folderContents = 'none'
		dir(jobWorkspace) {
			git branch: 'master', credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
		}
        sh 'mkdir WQST-967'
        sh "cp db2jcc.jar ${testDir}/db2jcc.jar"
        sh "cp db2jcc_license_cu.jar ${testDir}/db2jcc_license_cu.jar"
        dir(testDir) { 
            folderContents = sh returnStdout: true, script: 'ls'
            if (folderContents.contains('db2jcc.jar') && folderContents.contains('db2jcc_license_cu.jar')) {
                echo 'Step 02 passed'
            } else {
                error 'Step 02 failed, DB2 driver Jars not present in workspace.'
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
            sh "curl -k --header \"PRIVATE-TOKEN: ${registryAccessToken}\" \"https://pd-gitlab.deloitte.com/api/v4/projects/103/packages/generic/db-cutter/${buildTag}/db-crawler-zOs-${buildTag}.jar\" --output ${testDir}/db-crawler-zOs-${buildTag}.jar"
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
	* Run the command for crawling
	*
    * Crawling starts and after around 5 minutes it shows a success message "Finished crawling the database"
    * The log folder gets updated and the file dbcrawler_schema_mannheimer_db2_step05.json gets generated.
    *
    */
  stage('Step 05') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 05'
                def step05 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                try {
                    step05 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:db2jcc.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://localhost:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2_step05.json"
                } catch (err) {
                   unstable 'Step 05 failed - crawling is not successful'
                } finally {
                    if ((step05.contains('Finished crawling the database.')) && (step05 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_mannheimer_db2_step05.json')) {
                             folderContents = sh returnStdout: true, script: 'ls log'
                             echo 'The contents in the directory are:'
                             echo folderContents
                             if (folderContents.contains('application.log') && folderContents.contains('parser.csv')) {
                                parserCSVCreationTime = sh returnStdout: true, script: 'stat -c %z  log/parser.csv'
                                applicationLogCreationTime = sh returnStdout: true, script: 'stat -c %z  log/application.log'
                                applicationLogContent = sh returnStdout: true, script: 'cat log/application.log'
                                def size
                                if (applicationLogContent.contains('Exception')) {
                                    applicationLogContent = applicationLogContent.split('Exception')
                                    size = applicationLogContent.size()
                                } else {
                                    size = 0
                                }
                                echo "application.log file contains ${size} exceptions"    
                                if (size > 10) {
                                    unstable 'Step 05 failed - application log contains too many exceptions'
                                } else {
                                    echo 'Step 05 passed'
                                    sh 'cp log/application.log application_step05.log'
                                    sh 'cp log/parser.csv parser_step05.csv'
                                }
                            } else {
                                unstable 'Step 05 failed - either application.log or parser.csv is not present'
                            }
                        } else {
                            unstable 'Step 05 failed - either log folder or dbcrawler_schema_mannheimer_db2_step05.json is not present'
                        }
                    } else {
                        unstable 'Step 05 failed - crawling is not successful'
                    }
                }
            }
        }
    } 

    /**
	* Run the command for crawling after removing -cp from it
	*
    * An error message "Error: Could not find or load main class dbcrawler.jar:db2jcc.jar" is displayed.
    *
    */
    stage('Step 06') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************' 
                echo 'Starting DB Crawler tests: Step 06'
                def step06 = 'none'
                def result = 'failed'
                try {
                    step06 = sh returnStdout: true, script: "java -Xmx8g \"db-crawler-${buildTag}.jar:db2jcc.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://localhost:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2.json"
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
	* Run the command after removing db2jcc.jar from it
	*
    * Crawling starts and after a few minutes it shows a success message "Finished crawling the database"
    * The log folder gets updated and the file dbcrawler_schema_mannheimer_db2_step07.json gets generated.
    *
    */
    stage('Step 07') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************' 
                echo 'Starting DB Crawler tests: Step 07'
                def step07 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                def schemaContent = 'none'
                try {
                    step07 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://localhost:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2_step07.json"
                } catch (err) {
                   unstable 'Step 07 failed - an exception is thrown'
                } finally {
                    if ((step07.contains('Finished crawling the database.')) && (step07 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        def parserCSVCreationTimeStep07
                        def applicationLogCreationTimeStep07
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_mannheimer_db2_step07.json')) {
                             folderContents = sh returnStdout: true, script: 'ls log'
                             echo 'The contents in the directory are:'
                             echo folderContents
                             if (folderContents.contains('application.log') && folderContents.contains('parser.csv')) {
                                applicationLogContent = sh returnStdout: true, script: 'cat log/application.log'
                                parserCSVCreationTimeStep07 = sh returnStdout: true, script: 'stat -c %z  log/parser.csv'
                                applicationLogCreationTimeStep07 = sh returnStdout: true, script: 'stat -c %z  log/application.log'
                                schemaContent = sh returnStdout: true, script: 'cat dbcrawler_schema_mannheimer_db2_step07.json'
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
                                if (size > 10) {
                                    unstable 'Step 07 failed - application log contains too many exceptions'
                                } else {
                                   if(schemaContent.contains('"size" : 40') && schemaContent.contains('"size" : 30') && schemaContent.contains('"size" : 20') && schemaContent.contains('"size" : 10')) {
                                        def content = schemaContent.split('"size"')
                                        println(content.size());
                                        if(content.size()>4500) {
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
                            unstable 'Step 07 failed - either log folder or dbcrawler_schema_mannheimer_db2_step07.json or both are not present'
                        }
                    } else {
                        unstable 'Step 07 failed - crawling is not successful'
                    }
                }
            }
        }
    }

   /**
	* Delete the db2 driver jars from the directory using the commands: rm db2jcc.jar and rm db2jcc_license_cu.jar
	*
    * The jar files get deleted successfully.
    *
    */
    stage('Step 08') {
        def folderContents = 'none'
        echo '************************************'
        echo 'Starting DB Crawler tests: Step 08'
        dir(testDir) {
            sh 'rm db2jcc.jar'
            sh 'rm db2jcc_license_cu.jar'
            folderContents = sh returnStdout: true, script: 'ls'
        }
        if (folderContents.contains('db2jcc.jar') || folderContents.contains('db2jcc_license_cu.jar')) {
            unstable 'Step 08 failed - files not deleted'
        } else {
            echo 'Step 08 passed'
        }
    }

     /**
	* Run the command for crawling
	*
    * Crawling starts and after around 5 minutes it shows a success message "Finished crawling the database"
    * The log folder gets updated and the file dbcrawler_schema_mannheimer_db2_step07.json gets generated.
    *
    */
  stage('Step 09') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 09'
                def step05 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                try {
                    step09 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:db2jcc.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://localhost:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2_step09.json"
                } catch (err) {
                   unstable 'Step 09 failed - crawling is not successful'
                } finally {
                    if ((step09.contains('Finished crawling the database.')) && (step09 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        def parserCSVCreationTimeStep09
                        def applicationLogCreationTimeStep09
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_mannheimer_db2_step09.json')) {
                             folderContents = sh returnStdout: true, script: 'ls log'
                             echo 'The contents in the directory are:'
                             echo folderContents
                             if (folderContents.contains('application.log') && folderContents.contains('parser.csv')) {
                                parserCSVCreationTimeStep09 = sh returnStdout: true, script: 'stat -c %z  log/parser.csv'
                                applicationLogCreationTimeStep09 = sh returnStdout: true, script: 'stat -c %z  log/application.log'
                                applicationLogContent = sh returnStdout: true, script: 'cat log/application.log'
                                if (parserCSVCreationTime != parserCSVCreationTimeStep09 && applicationLogCreationTime != applicationLogCreationTimeStep09) {
                                    parserCSVCreationTime = parserCSVCreationTimeStep09
                                    applicationLogCreationTime = applicationLogCreationTimeStep09
                                } else {
                                    unstable 'Step 09 failed - either application.log or parser.csv or both are not updated'
                                }
                                def size
                                if (applicationLogContent.contains('Exception')) {
                                    applicationLogContent = applicationLogContent.split('Exception')
                                    size = applicationLogContent.size()
                                } else {
                                    size = 0
                                }
                                echo "application.log file contains ${size} exceptions"                                
                                if (size > 10) {
                                    unstable 'Step 09 failed - application log contains too many exceptions'
                                } else {
                                    echo 'Step 09 passed'
                                    sh 'cp log/application.log application_step09.log'
                                    sh 'cp log/parser.csv parser_step09.csv'
                                }
                            } else {
                                unstable 'Step 09 failed - either application.log or parser.csv or both are not present'
                            }
                        } else {
                            unstable 'Step 09 failed - either log folder or dbcrawler_schema_mannheimer_db2_step09.json or both are not present'
                        }
                    } else {
                        unstable 'Step 09 failed - crawling is not successful'
                    }
                }
            }
        }
    } 

    /**
	* Run the command for crawling without specifying the dbcrawler jar
	*
    * An error message "Could not find or load main class innowake.innovationlab.dbcrawler.Main" is displayed.
    *
    */
    stage('Step 10') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 10'
                def step10 = 'none'
                def result = 'failed'
                try {
                    step10 = sh returnStdout: true, script: "java -Xmx8g -cp \"db2jcc.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://localhost:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2.json"
                } catch (err) {
                    result ='passed'
                } finally {
                    if (result.contains('passed')) {
                        echo 'Step 10 passed'
                    } else {
                        unstable 'Step 10 failed - crawling is succesful without specifying the dbcrawler jar'
                    }
                }
            }
        }
    }

    /**
	* Delete the db crawler jar from the directory using the command: rm dbcrawler-<version>
	*
    * The jar file gets deleted successfully.
    *
    */
    stage('Step 11') {
        def folderContents = 'none'
        echo '************************************'
        echo 'Starting DB Crawler tests: Step 11'
        dir(testDir) {
            sh "rm db-crawler-${buildTag}.jar"
            folderContents = sh returnStdout: true, script: 'ls'
        }
        if (folderContents.contains("db-crawler-${buildTag}.jar")) {
            unstable 'Step 11 failed - file delete is not successful'
        } else {
            echo 'Step 11 passed'
        }
    }

      /**
	* Run the command for crawling
	*
    * An error message "Could not find or load main class innowake.innovationlab.dbcrawler.Main" is displayed.
    *
    */
    stage('Step 12') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 12'
                def step12 = 'none'
                def result = 'failed'
                try {
                    step12 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:db2jcc.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://localhost:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2.json"
                } catch (err) {
                    result ='passed'
                } finally {
                    if (result.contains('passed')) {
                        echo 'Step 12 passed'
                    } else {
                        unstable 'Step 12 failed - crawling is succesful without the dbcrawler jar'
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
    stage('Step 13') {
         def folderContents = 'none'
        echo '************************************'
        echo 'Starting DB Crawler tests: Step 13'
        withCredentials([string(credentialsId: 'pd-gitlab-registry-token', variable: 'registryAccessToken')]) {
			sh "curl -k --header \"PRIVATE-TOKEN: ${registryAccessToken}\" \"https://pd-gitlab.deloitte.com/api/v4/projects/103/packages/generic/db-cutter/${buildTag}/db-crawler-${buildTag}.jar\" --output ${testDir}/db-crawler-${buildTag}.jar"
		}
		dir(testDir) {
            folderContents = sh returnStdout: true, script: 'ls'
        }
        if (folderContents.contains("db-crawler-${buildTag}.jar")) {
            echo 'Step 13 passed'
        } else {
            unstable 'Step 13 failed - file not present in the directory'
        }
     }

     /**
	* Run the command for crawling without specifying the main class.
	*
    * An error message "Unrecognized option: -c Error: Could not create the Java Virtual Machine. 
    * Error: A fatal exception has occurred. Program will exit." is displayed.
    *
    */
     stage('Step 14') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 14'
                def step14 = 'none'
                def result = 'failed'
                try {
                    step14 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:db2jcc.jar\" -c \"jdbc:db2://localhost:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2.json"
                } catch (err) {
                    result = 'passed'
                } finally {
                    if (result.contains('passed')) {
                        echo 'Step 14 passed'
                    } else {
                        unstable 'Step 14 failed - crawling is successful without specifying the main class.'
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
    stage('Step 15') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 15'
                def step15 = 'none'
                try {
                    step15 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:db2jcc.jar\" innowake.innovationlab.dbcrawler.Main \"jdbc:db2://localhost:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2.json"
                } catch (err) {
                    unstable 'Step 15 failed - an exception thrown'
                } finally {
                    if (step15.contains('Missing required option: c')) {
                        echo 'Step 15 passed'
                    } else {
                        unstable 'Step 15 failed - crawling is successful without specifying parameter -c'
                    }
                }
            }
        }
    }

     /**
	* Run the command for crawling without specifying u for username and p for password
	*
    * An error message "Application closed down unexpectedly: Could not connect to jdbc:db2://localhost:<port>/IS05, for unspecified user, with properties {}: 
    * [jcc][t4][10205][11234][4.28.11] Null userid is not supported. ERRORCODE=-4461, SQLSTATE=42815" is displayed.
    *
    */
    stage('Step 16') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 16'
                def step16 = 'none'
                try {
                    step16 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:db2jcc.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://localhost:${containerPort}/IS05\" ${dbUser} ${dbPassword} -f dbcrawler_schema_mannheimer_db2.json"
                } catch (err) {
                    unstable 'Step 16 failed - an exception thrown'
                } finally {
                    echo step16
                    if (step16.contains('Null userid is not supported')) {
                        echo 'Step 16 passed'
                    } else {
                        unstable 'Step 16 failed - crawling is successful without specifying the u and p parameters for username and password'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling without specifying the correct username and password
	*
    * An error message "Application closed down unexpectedly: Could not connect to jdbc:db2://localhost:50009/IS05, for user 'testuser', 
    * with properties {}: [jcc][t4][2013][11249][4.28.11] Connection authorization failure occurred.  Reason: User ID or Password invalid. ERRORCODE=-4214, SQLSTATE=28000" is displayed.
    *
    */
    stage('Step 17') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 17'
                def step17 = 'none'
                try {
                    step17 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:db2jcc.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://localhost:${containerPort}/IS05\" -u testuser -p testpassword -f dbcrawler_schema_mannheimer_db2.json"
                } catch (err) {
                    unstable 'Step 17 failed - an excpetion thrown'
                } finally {
                    echo step17
                    if (step17.contains('User ID or Password invalid')) {
                        echo 'Step 17 passed'
                    } else {
                        unstable 'Step 17 failed - crawling is successful without specifying the correct username and password'
                    }
                }
            }
        }
    }

     /**
	* Run the command for crawling after replacing localhost with 127.0.0.1
	*
    * Crawling starts and after a few minutes it shows a success message "Finished crawling the database" 
    * The log folder gets updated and the file dbcrawler_schema_mannheimer_db2_step18.json gets generated.
    *
    */
    stage('Step 18') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 18'
                def step18 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                try {
                    step18 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:db2jcc.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://127.0.0.1:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2_step18.json"
                } catch (err) {
                   unstable 'Step 18 failed - an exception is thrown'
                } finally {
                    if ((step18.contains('Finished crawling the database.')) && (step18 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        def parserCSVCreationTimeStep18
                        def applicationLogCreationTimeStep18
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_mannheimer_db2_step18.json')) {
                             folderContents = sh returnStdout: true, script: 'ls log'
                             echo 'The contents in the directory are:'
                             echo folderContents
                             if (folderContents.contains('application.log') && folderContents.contains('parser.csv')) {
                                parserCSVCreationTimeStep18 = sh returnStdout: true, script: 'stat -c %z  log/parser.csv'
                                applicationLogCreationTimeStep18 = sh returnStdout: true, script: 'stat -c %z  log/application.log'
                                applicationLogContent = sh returnStdout: true, script: 'cat log/application.log'
                                if (parserCSVCreationTime != parserCSVCreationTimeStep18 && applicationLogCreationTime != applicationLogCreationTimeStep18) {
                                    parserCSVCreationTime = parserCSVCreationTimeStep18
                                    applicationLogCreationTime = applicationLogCreationTimeStep18
                                } else {
                                    unstable 'Step 18 failed - either application.log or parser.csv or both are not updated'
                                }
                                def size
                                if (applicationLogContent.contains('Exception')) {
                                    applicationLogContent = applicationLogContent.split('Exception')
                                    size = applicationLogContent.size()
                                } else {
                                    size = 0
                                }
                                echo "application.log file contains ${size} exceptions"                                
                                if (size > 10) {
                                    unstable 'Step 18 failed - application log contains too many exceptions'
                                } else {
                                    echo 'Step 18 passed'
                                    sh 'cp log/application.log application_step18.log'
                                    sh 'cp log/parser.csv parser_step18.csv'
                                }
                            } else {
                                unstable 'Step 18 failed - either application.log or parser.csv or both are not present'
                            }
                        } else {
                            unstable 'Step 18 failed - either log folder or dbcrawler_schema_mannheimer_db2_step18.json or both are not present'
                        }
                    } else {
                        unstable 'Step 18 failed - crawling is not successful'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling without specifying -Xmx8g
	*
    * Crawling starts and after a few minutes it shows a success message "Finished crawling the database" 
    * The log folder gets updated and the file dbcrawler_schema_mannheimer_db2_step19.json gets generated.
    *
    */
    stage('Step 19') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 19'
                def step19 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                try {
                    step19 = sh returnStdout: true, script: "java -cp \"db-crawler-${buildTag}.jar:db2jcc.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://localhost:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2_step19.json"
                } catch (err) {
                   unstable 'Step 19 failed, an exception is thrown'
                } finally {
                    echo step19
                    if ((step19.contains('Finished crawling the database.')) && (step19 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        def parserCSVCreationTimeStep19
                        def applicationLogCreationTimeStep19
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_mannheimer_db2_step19.json')) {
                             folderContents = sh returnStdout: true, script: 'ls log'
                             echo 'The contents in the directory are:'
                             echo folderContents
                             if (folderContents.contains('application.log') && folderContents.contains('parser.csv')) {
                                parserCSVCreationTimeStep19 = sh returnStdout: true, script: 'stat -c %z  log/parser.csv'
                                applicationLogCreationTimeStep19 = sh returnStdout: true, script: 'stat -c %z  log/application.log'
                                applicationLogContent = sh returnStdout: true, script: 'cat log/application.log'
                                if (parserCSVCreationTime != parserCSVCreationTimeStep19 && applicationLogCreationTime != applicationLogCreationTimeStep19) {
                                    parserCSVCreationTime = parserCSVCreationTimeStep19
                                    applicationLogCreationTime = applicationLogCreationTimeStep19
                                } else {
                                    unstable 'Step 19 failed - either application.log or parser.csv or both are not updated'
                                }
                                def size
                                if (applicationLogContent.contains('Exception')) {
                                    applicationLogContent = applicationLogContent.split('Exception')
                                    size = applicationLogContent.size()
                                } else {
                                    size = 0
                                }
                                echo "application.log file contains ${size} exceptions"
                                if (size > 10) {
                                    unstable 'Step 19 failed - application log contains too many exceptions'
                                } else {
                                    echo 'Step 19 passed'
                                    sh 'cp log/application.log application_step19.log'
                                    sh 'cp log/parser.csv parser_step19.csv'
                                }
                            } else {
                                unstable 'Step 19 failed - either application.log or parser.csv or both are not present'
                            }
                        } else {
                            unstable 'Step 19 failed - either log folder or dbcrawler_schema_mannheimer_db2_step19.json or both are not present'
                        }
                    } else {
                        unstable 'Step 19 failed - crawling is not successful'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling after specifying the schemas to be blacklisted using -sbl
	*
    * Crawling starts and after a few minutes it shows a success message "Finished crawling the database" 
    * Crawling does not take place for the database schemas starting with SYSIBM
    * The log folder gets updated and the file dbcrawler_schema_mannheimer_db2_step20.json gets generated.
    *
    */
    stage('Step 20') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 20'
                def step20 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                try {
                    step20 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:db2jcc.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://localhost:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2_step20.json -sbl SYSIBM*"
                } catch (err) {
                   unstable 'Step 20 failed - an exception is thrown'
                } finally {
                    if ((step20.contains('Finished crawling the database.')) && (step20 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        fileContents = sh returnStdout: true, script: 'cat dbcrawler_schema_mannheimer_db2_step20.json'
                        fileContents = fileContents.split('schemes')[1]
                        def parserCSVCreationTimeStep20
                        def applicationLogCreationTimeStep20
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_mannheimer_db2_step20.json') && !fileContents.contains('SYSIBMADM') && !fileContents.contains('SYSIBMINTERNAL')  && !fileContents.contains('SYSIBMTS')) {
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
                                if (size > 10) {
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
                            unstable 'Step 20 failed - either log folder or dbcrawler_schema_mannheimer_db2_step20.json or both are not present or dbcrawler_schema_mannheimer_db2_step20.json does not contain the expected content'
                        }
                    } else {
                        unstable 'Step 20 failed - crawling is not successful'
                    }
                }
            }
        }
    }

    /**
	* Run the command for crawling after specifying the schemas to be whitelisted using -swl
	*
    * Crawling starts and after a few minutes it shows a success message "Finished crawling the database" 
    * Crawling takes place for the database schemas IS05
    * The log folder gets updated and the file dbcrawler_schema_mannheimer_db2_step21.json gets generated.
    *
    */
    stage('Step 21') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 21'
                def step21 = 'failed'
                def folderContents = 'none'
                def applicationLogContent = 'none'
                try {
                    step21 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:db2jcc.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://localhost:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2_step21.json -swl IS05"
                } catch (err) {
                   unstable 'Step 21 failed - an exception is thrown'
                } finally {
                    if ((step21.contains('Finished crawling the database.')) && (step21 != 'failed')) {
                        folderContents = sh returnStdout: true, script: 'ls'
                        fileContents = sh returnStdout: true, script: 'cat dbcrawler_schema_mannheimer_db2_step21.json'
                        fileContents = fileContents.split('schemes')[1]
                        def parserCSVCreationTimeStep21
                        def applicationLogCreationTimeStep21
                        echo 'The contents in the directory are:'
                        echo folderContents
                        if (folderContents.contains('log') && folderContents.contains('dbcrawler_schema_mannheimer_db2_step21.json') && !fileContents.contains('SYSIBMADM') && !fileContents.contains('SYSIBMINTERNAL')  && !fileContents.contains('SYSIBMTS') && !fileContents.contains('DB2INST1')) {
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
                                if (size > 10) {
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
                            unstable 'Step 21 failed - either log folder or dbcrawler_schema_mannheimer_db2_step21.json or both are not present or dbcrawler_schema_mannheimer_db2_step21.json does not contain the expected content'
                        }
                    } else {
                        unstable 'Step 21 failed - crawling is not successful'
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
    stage('Step 22') {
        withCredentials([usernamePassword(credentialsId: 'modernization-mannheimer-db2-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
            dir(testDir) {
                echo '************************************'
                echo 'Starting DB Crawler tests: Step 22'
                def step22 = 'none'
                try {
                    step22 = sh returnStdout: true, script: "java -Xmx8g -cp \"db-crawler-${buildTag}.jar:db2jcc.jar\" innowake.innovationlab.dbcrawler.Main -c \"jdbc:db2://localhost:${containerPort}/IS05\" -u ${dbUser} -p ${dbPassword} -f dbcrawler_schema_mannheimer_db2.json -sbl SYSIBM* -swl IS05"
                } catch (err) {
                    unstable 'Step 22 failed - an exception is thrown'
                } finally {
                    if (step22.contains('Either blacklist or whitelist can be used, but not both together.')) {
                        echo 'Step 22 passed'
                    } else {
                        unstable 'Step 22 failed - crawling is successful with both blacklist and whitelist together.'
                    }
                }
            }
        }
    }

    stage('Archive artifacts') {
        try {
            dir(jobWorkspace) {
                archiveArtifacts "WQST-967/**"
            }
        } catch (err) {
            echo 'Artifacts not generated'
                currentBuild.result = 'UNSTABLE'
        }
    }
}
