@Library('TestUtils') _

/**
 * Runs the Csharp Test in the Customer Zero project.
 *
 * @param mxBuildVersion The maxenso/innowake build under test.
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param environmentLabel The resource label that will be locked while the job is running.
 * @param migrationBuild The build of the TF_Cobol_Csharp_Migration_Customer_Zero job to fetch the migrated files from
 */
timestamps {
    def mxVersionUtils = new MxVersionUtils()
    def gitUtils = new GitUtils()
    def dockerUtils = new DockerUtils()
    def miscUtils = new MiscUtils()
    def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
    def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/customer-zero/customer-zero-csharp.git'
    final String CZ_DB_NAME = 'customerZeroDbCsharp'

    advancedLock(environmentLabel) {
        nodeTF(env.linuxNode, environmentLabel.contains('perf')) {
            def workDir = pwd()
            stage('remove old database container') {
                sh returnStatus: true, script: "docker stop ${CZ_DB_NAME}"
                sh returnStatus: true, script: "docker rm -f ${CZ_DB_NAME}"
            }
            
            stage('setup DB'){
                deleteDir()
                docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside {
                       gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                }
                
                // Create the Customer Zero database. To do this, we start a plain db2 container, and copy over init.sh,
                // which will create the database and tables, and load initial records into some of the tables.
                withCredentials([usernamePassword(credentialsId: 'Customer_Zero_DB_Credentials', passwordVariable: 'dbPw', usernameVariable: 'dbUser')]) {
                   stage('create customer zero database') {
                        docker.image('ibmcom/db2:11.5.6.0a').run("-e LICENSE=accept -e DB2INST1_PASSWORD=${dbPw} --name ${CZ_DB_NAME} --privileged=true -p 50002:50000")
                        int dbReadyStatus = 1
                        final int MAX_NUMBER_OF_ATTEMPTS = 50
                        int currentNumberOfAttempts = 0
                        while (dbReadyStatus != 0 && currentNumberOfAttempts < MAX_NUMBER_OF_ATTEMPTS) {
                            dbReadyStatus = sh returnStatus: true,
                                    script: "docker logs ${CZ_DB_NAME} | grep 'Setup has completed'"
                            echo 'waiting for the database to start ...'
                            currentNumberOfAttempts++
                            sleep 30
                        }
                        if (dbReadyStatus != 0) {
                            error 'Database could not start even after max number of retries.'
                        }
                        echo "Database is running"
                        sh 'chmod +x CustomerZero.Configuration/src/Res/db2/build/init.sh'
                        sh "docker cp CustomerZero.Configuration/src/Res/db2/build ${CZ_DB_NAME}:/"
                        sh "docker exec -t ${CZ_DB_NAME} /bin/bash -c /build/init.sh"
                   }
        		}
            }
        }
        
        nodeTF(env.windowsNode, environmentLabel.contains('perf')) {
            def workDir = pwd()            
            def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
            String pathToLogFile = "Customer_Zero_Csharp_performance_report.log"
           			
            buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
            buildDescription "testProjectBranch=${testProjectBranch} environmentLabel=${environmentLabel} migrationBuild=${migrationBuild} fullBuild=${fullBuild}"

            deleteDir()
            stage('initialisation') {
		    	gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                if (migrationBuild.matches('.*StatusBuildSelector.*')) {
		            copyArtifacts   projectName: 'TF_Cobol_Csharp_Migration_Customer_Zero',
				                    filter: 'result-comparison/tmp/result-compare/actual/*',
			                    	selector: lastSuccessful(),
			                    	parameters: "mxBuildVersion=${mxBuildVersion}",
			                    	fingerprintArtifacts: true
            	} else {
	            	copyArtifacts	projectName: 'TF_Cobol_Csharp_Migration_Customer_Zero',
				                    filter: 'result-comparison/tmp/result-compare/actual/*',
		                    		selector: buildParameter(migrationBuild),
				                    fingerprintArtifacts: true
	            }
	            
	            // Remove default csharp files and copy over actual migrated files from the Customer_Zero_Migration job.
                String sourceDir = 'CustomerZero.Common\\src\\src-cobol-csharp'
                bat "rd /s /q ${sourceDir}"
				bat returnStatus: true, script: "robocopy result-comparison\\tmp\\result-compare\\actual ${sourceDir} /e"
				
				configFileProvider([configFile(fileId: 'NuGet.config', targetLocation: '.', variable: 'pathToNugetConfigFile')]) {
					// Add nexus user to NuGet.config
					withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-Nexus', passwordVariable: 'nexusPw', usernameVariable: 'nexusUser')]) {
						bat "nuget.exe sources update -name CF -username ${nexusUser} -password ${nexusPw}"
						bat "nuget.exe sources update -name Nexus -username ${nexusUser} -password ${nexusPw}"
			    	}
				}

                //Connection string reworking
				withCredentials([usernamePassword(credentialsId: 'Customer_Zero_DB_Credentials', passwordVariable: 'dbPw', usernameVariable: 'dbUser')]) {
					def content = readFile 'CustomerZero.Tests/App.config'
					content = content.replaceAll(/connectionString="Server=[^;]*/, "connectionString=\"Server=${miscUtils.getHostNameByNodeName(env.linuxNode)}:50002")
					content = content.replaceAll(/Database=[a-zA-Z][a-zA-Z]*/, "Database=CUSTZERO;UID=${dbUser};PWD=${dbPw}")
					writeFile encoding: 'UTF-8', file: 'CustomerZero.Tests/App.config', text: content
				}

				// Innowake Package update
				bat 'nuget.exe restore customer-zero-csharp.sln'
				def output = bat returnStdout: true, script: "nuget.exe update customer-zero-csharp.sln -Version ${mxBuildVersion} -Id InnoWake.NET.Dependencies-dist"
                dir('CustomerZero.Tests'){
                    bat "dotnet add CustomerZero.Tests.csproj package InnoWake.NET.Dependencies-dist --version ${mxBuildVersion}"
                    bat "dotnet add CustomerZero.Tests.csproj package innowake.runtime-dotnet-dist --version ${mxBuildVersion}"
                    bat "dotnet add CustomerZero.Tests.csproj package innowake.runtime-ims-torpedo-dotnet-dist --version ${mxBuildVersion}"
                    bat "dotnet add CustomerZero.Tests.csproj package innowake.runtime-torpedo-dotnet-dist --version ${mxBuildVersion}"                    
                }
                
                echo output
				if (output.contains('is not found')){
					error "innowake artifacts with version ${mxBuildVersion} are not available"
				}
            }
            
            stage('build') {
    		    bat 'dotnet build customer-zero-csharp.sln'
	        }
            
            dir('CustomerZero.Tests/bin/Debug/net472') {
                stage('test') {
        	        gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), pwd())
    	            catchError(message: 'Tests failed', buildResult: 'UNSTABLE', catchInterruptions: false, stageResult: 'UNSTABLE') {
    	            	retry(2) {
    	            		timeout(time: 20, unit: 'MINUTES') {
    	            			bat 'dotnet test CustomerZero.Tests.dll --platform x64 --logger:"nunit;LogFileName=TestResults.xml"'
    	            		}
    	            	}
    	            }
        			nunit testResultsPattern: 'TestResults\\TestResults.xml', failedTestsFailBuild: false
                }
	    
	    	    stage('finalize') {
		        	archiveArtifacts artifacts: pathToLogFile, allowEmptyArchive: true 
		        }
            }
        }
    }
}


