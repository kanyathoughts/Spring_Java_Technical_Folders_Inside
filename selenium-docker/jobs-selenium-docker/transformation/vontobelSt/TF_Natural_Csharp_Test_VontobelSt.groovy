@Library('TestUtils') _

/**
 * Build and run runtime natural tests 
 *
 * @param mxBuildVersion The maxenso build under test
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param dotnetFrameworkVersion The .Net Framework version the build and the test will run with
 * @param attributeMigrationTest By default the natural_csharp_src.zip is checked out from the sharepoint.
 * 		If attributeMigrationTest is set to true the natural_csharp_src_attribute.zip is checked out for the test. 
 */

nodeTF('OS-Windows && Tool-msbuild') {

    timestamps {

        def mxVersionUtils = new MxVersionUtils()
        def svnUtils = new SvnUtils()
        def gitUtils = new GitUtils()
        def spUtils = new SharepointUtils()
        def projectNameCsharp = 'natural-csharp-vontobelSt'
        def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
        def sourceFilesForTest = 'natural_csharp_src'

        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)

        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "testProjectBranch=${testProjectBranch} dotnetFrameworkVersion=${dotnetFrameworkVersion} fullBuild=${fullBuild} attributeMigartionTest=${attributeMigrationTest}"

        stage('initialisation') {
            deleteDir()

            gitUtils.checkoutGitProject(pwd(), "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/natural-test/natural-csharp-vontobelSt.git", testProjectBranch)
			gitUtils.checkoutGitProject('dbBackup', "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/natural-test/natural-db-backup.git", 'master')																																							 
			unzip dir: 'dbBackup', quiet: true, zipFile: 'dbBackup/vontobelSt.zip'
			
			if (params.attributeMigrationTest) {
			    sourceFilesForTest = 'natural_csharp_src_attribute'
			}

            spUtils.downloadJobArtifact('TF_Natural_Csharp_Migration_VontobelSt', fullBuild, "${sourceFilesForTest}.zip", pwd())
            unzip dir: 'VS_Solution/natural-csharp-vontobelSt/src-natural-csharp', quiet: true, zipFile: "${sourceFilesForTest}.zip"
            
            dir('VS_Solution') {
                configFileProvider([configFile(fileId: 'NuGet.config', targetLocation: '.', variable: 'pathToNugetConfigFile')]) {
                    withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-Nexus', passwordVariable: 'nexusPw', usernameVariable: 'nexusUser')]) {
                        bat "nuget.exe sources update -name Nexus -username ${nexusUser} -password ${nexusPw}"
                    }
                    bat "nuget.exe restore natural-csharp-vontobelSt.sln"
                    def output = bat returnStdout: true, script: "nuget.exe update natural-csharp-vontobelSt\\natural-csharp-vontobelSt.csproj -Version ${mxBuildVersion} -Id innowake.runtime-dotnet-dist"
                    echo output
                    if (output.contains('is not found')) {
                        error "innowake artifacts with version ${mxBuildVersion} are not available"
                    }
                    bat "msbuild.exe natural-csharp-vontobelSt.sln -t:Clean,Build -p:TargetFrameworkVersion=${dotnetFrameworkVersion}"
                }
            }
        }

        stage ('restore database') {
            withCredentials([usernamePassword(credentialsId: 'MS_SQL_User_Windows8_Trafo', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
    		    dir('dbBackup') {
    			    def pathToBackupFile = pwd() + '\\vontobelSt.bak'
    			    def pathToAfterDbScript = pwd() + '\\afterDbRestore.sql'    			    
    		    	bat "sqlcmd -S localhost -U ${dbUser} -P ${dbPassword} -Q \"RESTORE DATABASE vontobelSt FROM DISK ='${pathToBackupFile}'\""
    		    	bat "sqlcmd -S localhost -U ${dbUser} -P ${dbPassword} -i ${pathToAfterDbScript}"    		    	
    		    	bat "del ${pathToBackupFile}"
    		    }
            }
	    }

        stage('test') {
            def workDir = 'VS_Solution/' + projectNameCsharp + '/bin/Debug'
            dir(workDir) {
                gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), '.')
                bat 'ren maxenso.lic innowake.lic'
                catchError(message: 'Natural Csharp tests failed', buildResult: 'UNSTABLE', catchInterruptions: false, stageResult: 'UNSTABLE') {
                    timeout(time: 20, unit: 'MINUTES') {
                        bat 'dotnet test natural-csharp-vontobelSt.dll --logger:"nunit;LogFileName=TestResults.xml"'
                    }
                }
                nunit testResultsPattern: 'TestResults\\TestResults.xml', failedTestsFailBuild: false
            }
        }
    }
}
