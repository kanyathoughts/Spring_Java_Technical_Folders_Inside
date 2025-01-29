@Library('TestUtils') _

/**
 * Build and run runtime natural VSAM tests 
 *
 * @param mxBuildVersion The maxenso build under test
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param dotnetFrameworkVersion The .Net Framework version the build and the test will run with
																												  
 */

timestamps {
    def mxVersionUtils = new MxVersionUtils()
    def gitUtils = new GitUtils()

    nodeTF('OS-Windows && Tool-msbuild') {    
        def projectNameCsharp = 'natural-vsam-csharp'
        def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)

        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "testProjectBranch=${testProjectBranch} dotnetFrameworkVersion=${dotnetFrameworkVersion} fullBuild=${fullBuild}"

        stage('initialisation') {
            deleteDir()
            gitUtils.checkoutGitProject(pwd(), "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/natural-vsam-csharp.git", testProjectBranch)
			copyArtifacts([
			               projectName: 'TF_Natural_Csharp_Migration_Vsam',
			               filter: '**/*.cs', 
			               selector: lastSuccessful(),
			               parameters: "mxBuildVersion=${fullBuild}",
			               target: 'natural-vsam-csharp/src-natural-csharp/vsam/test/transformed',
			               fingerprintArtifacts: true,
			               flatten: true
			               ])			
			
            configFileProvider([configFile(fileId: 'NuGet.config', targetLocation: '.', variable: 'pathToNugetConfigFile')]) {
                withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-Nexus', passwordVariable: 'nexusPw', usernameVariable: 'nexusUser')]) {
                    bat "nuget.exe sources update -name Nexus -username ${nexusUser} -password ${nexusPw}"
                }
                bat "nuget.exe restore natural-vsam-csharp.sln"
                def output = bat returnStdout: true, script: "nuget.exe update natural-vsam-csharp\\natural-vsam-csharp.csproj -Version ${mxBuildVersion} -Id innowake.runtime-dotnet-dist"
                echo output
                if (output.contains('is not found')) {
                    error "innowake artifacts with version ${mxBuildVersion} are not available"
                }
                bat "msbuild.exe natural-vsam-csharp.sln -t:Clean,Build -p:TargetFrameworkVersion=${dotnetFrameworkVersion}"
            }
        }
                
        stage ('restore database') {
            withCredentials([usernamePassword(credentialsId: 'MS_SQL_User_Windows8_Trafo', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
    		    def pathToBackupFile = pwd() + '\\vsam.bak'
    		    bat "sqlcmd -S localhost -U ${dbUser} -P ${dbPassword} -Q \"RESTORE DATABASE VSAM FROM DISK ='${pathToBackupFile}' WITH REPLACE\""
            }
	    }
	 
        stage('test') {
            dir(projectNameCsharp) {
                dir('bin/Debug') {
                    gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), '.')
                    bat 'ren maxenso.lic innowake.lic'
                    catchError(message: 'Natural VSAM Csharp tests failed', buildResult: 'UNSTABLE', catchInterruptions: false, stageResult: 'UNSTABLE') {
                        retry(2) {
                            timeout(time: 5, unit: 'MINUTES') {
                                bat 'dotnet test natural-vsam-csharp.dll --logger:"nunit;LogFileName=TestResults.xml"'
                            }
                        }
                    }
                    nunit testResultsPattern: 'TestResults\\TestResults.xml', failedTestsFailBuild: false
                }
            }
        }
    }
}
