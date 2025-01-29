@Library('TestUtils') _

/**
 * Run the COBOL_ASYNC_API tests in Csharp
 *
 * @param mxBuildVersion  The maxenso version to run the IMS DB tests. 
 * @param useDifferentTestProjectBranch By default the ims-db-csharp project is checked out with branch master. 
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

nodeTF('OS-Windows && Tool-msbuild') {
    timestamps {
        def jobWorkspace = pwd()
        def mxVersionUtils = new MxVersionUtils()
        def gitUtils = new GitUtils()
        def miscUtils = new MiscUtils()
        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		
        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "testProjectBranch=${testProjectBranch}"

        stage('initialisation') {
            deleteDir()
            gitUtils.checkoutGitProject(pwd(), "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/testcase-cobol-async-api-csharp.git", testProjectBranch)                              
                
            configFileProvider([configFile(fileId: 'NuGet.config', targetLocation: '.', variable: 'pathToNugetConfigFile')]) {
                withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-Nexus', passwordVariable: 'nexusPw', usernameVariable: 'nexusUser')]) {
                    bat "nuget.exe sources update -name Nexus -username ${nexusUser} -password ${nexusPw}" 
                }
                bat 'nuget.exe restore CobolAsyncAPI.sln' 
                // Innowake Package update
                def output = bat returnStdout: true, script: "nuget.exe update CobolAsyncAPI.sln -Version ${mxBuildVersion} -Id innowake.runtime-dotnet-testframework-dist"
                echo output
                if (output.contains('is not found')){
                    error "innowake artifacts with version ${mxBuildVersion} are not available"
                }                
            }
        }
		
		stage('build') {
		    bat 'dotnet clean CobolAsyncAPI.sln --configuration Debug'
	        bat 'dotnet build CobolAsyncAPI.sln --configuration Debug'
	        gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), 'CobolAsyncAPI\\bin\\Debug')
		}
		
		stage('test') {
	        dir('CobolAsyncAPI/bin/Debug') {	            	         
                catchError(message: 'Cobol Csharp tests failed', buildResult: 'UNSTABLE', catchInterruptions: false, stageResult: 'UNSTABLE') {
                    retry(2) {
                        timeout(time: 5, unit: 'MINUTES') {
                            bat 'dotnet test CobolAsyncApi.dll --logger:"nunit;LogFileName=TestResults.xml"'
                        }
                    }
                }
                nunit testResultsPattern: 'TestResults\\TestResults.xml', failedTestsFailBuild: false
	        }
		}
    }
}
