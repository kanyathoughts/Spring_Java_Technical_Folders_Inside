@Library('TestUtils') _

/**
 * Checkout and run the IMS DB performance tests in C#.
 *
 * @param mxBuildVersion  The maxenso version to run the IMS DB performance tests. 
 * @param useDifferentTestProjectBranch By default the ims-db-csharp project is checked out with branch master. 
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param environmentLabel The resource label that will be locked while the job is running.
**/

timestamps {    
    def mxVersionUtils = new MxVersionUtils()
    def gitUtils = new GitUtils()
    def miscUtils = new MiscUtils()
    def perfUtils = new PerformanceUtils()

    advancedLock(environmentLabel) { 
	    nodeTF(env.windowsNode, environmentLabel.contains('perf')) {    
            def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
            def pathToLogFile = 'ims-db-csharp/bin/Release/test/res/performance/IMS_DB_Csharp_performance_report.log'
            def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
            
            buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
            buildDescription "testProjectBranch=${testProjectBranch} environmentLabel=${environmentLabel} fullBuild=${fullBuild}"
		
            stage('initialisation') {
                deleteDir()
				gitUtils.checkoutGitProject(pwd(), "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/ims/ims-db-csharp.git", testProjectBranch)
		   
                copyArtifacts projectName: 'TF_IMS_Csharp_Migration',
                              filter: 'IMS_Migration_Csharp/eclipseWorkspace/Test-Project/src-*-csharp/db/',
                              selector: lastSuccessful(),
                              parameters: "mxBuildVersion=${fullBuild}",
                              fingerprintArtifacts: true
                bat "rd /s /q ims-db-csharp\\src-cobol-csharp\\db ims-db-csharp\\src-ims-csharp\\db"
                bat returnStatus: true, script: 'robocopy IMS_Migration_Csharp/eclipseWorkspace/Test-Project/src-ims-csharp/db ims-db-csharp/src-ims-csharp/db /e'
                bat returnStatus: true, script: 'robocopy IMS_Migration_Csharp/eclipseWorkspace/Test-Project/src-cobol-csharp/db ims-db-csharp/src-cobol-csharp/db /e'
			
                configFileProvider([configFile(fileId: 'NuGet.config', targetLocation: '.', variable: 'pathToNugetConfigFile')]) {
                    withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-Nexus', passwordVariable: 'nexusPw', usernameVariable: 'nexusUser')]) {
                        bat "nuget.exe sources update -name Nexus -username ${nexusUser} -password ${nexusPw} " 
                    }
                    bat "nuget.exe restore ims-db-csharp.sln" 
                    // Innowake Package update
                    def output = bat returnStdout: true, script: "nuget.exe update ims-db-csharp.sln -Version ${mxBuildVersion} -Id innowake.runtime-dotnet-testframework-dist"
                    echo output
                    if (output.contains('is not found')){
                        error "innowake artifacts with version ${mxBuildVersion} are not available"
                    }
                }
            }
		
		    stage('build') {
		        bat 'dotnet clean ims-db-csharp.sln --configuration Release'
	            bat 'dotnet build ims-db-csharp.sln --configuration Release'
	            gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), 'ims-db-csharp\\bin\\Release')
		    }
		
		    stage('test') {
	            dir('ims-db-csharp/bin/Release') {
	                catchError(message: 'IMS DB tests failed', buildResult: 'UNSTABLE', catchInterruptions: false, stageResult: 'UNSTABLE') {
	                	retry(2) {
	                		timeout(time: 4, unit: 'HOURS') {
	                    		bat 'dotnet test ims-db-csharp.dll --filter "FullyQualifiedName~performance" --platform x64 --logger:"nunit;LogFileName=TestResults.xml"'
	                    	}
	                    }
	                }
                    nunit testResultsPattern: 'TestResults\\TestResults.xml', failedTestsFailBuild: false
                }
	        }

            stage('finalize') {
                archiveArtifacts artifacts: pathToLogFile, allowEmptyArchive: false
            }

            stage('reporting') {
				when(currentBuild.currentResult == 'SUCCESS', 'Performance reporting is not executed if the job is unstable') {
				    def buildResult = build job: 'TF_Performance_Reporting', propagate: false, parameters: [
						string(name: 'mxBuildVersion',    value: mxBuildVersion),
						string(name: 'pathToLogFile',     value: pathToLogFile),
						string(name: 'buildURL',          value: BUILD_URL),
						string(name: 'pageID',            value: '166891901'),
						string(name: 'numDaysDataToKeep', value: '20')
					]
				    miscUtils.evaluateBuildResult(buildResult, "TF_Performance_Reporting")
				}
        	}
        }
    }
}
