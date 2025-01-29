@Library('TestUtils') _

/**
 * Checkout and run the IMS WB tests in C#.
 *
 * @param mxBuildVersion  The maxenso version to run the IMS WB test. 
 * @param useDifferentTestProjectBranch By default the ims-db-csharp project is checked out with branch master. 
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
**/

nodeTF('USWindows8-Trafo') {
    timestamps {
        def mxVersionUtils = new MxVersionUtils()
        def gitUtils = new GitUtils()
        def miscUtils = new MiscUtils()
        def testProjectBranch = Boolean.parseBoolean(useDifferentTestProjectBranch) ? differentTestProjectBranch : 'master'
        def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
		
        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "testProjectBranch=${testProjectBranch}"
     
        stage('initialisation') {
            deleteDir()
            gitUtils.checkoutGitProject(pwd(), "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/ims/ims-db-csharp.git", testProjectBranch)            
            configFileProvider([configFile(fileId: 'NuGet.config', targetLocation: '.', variable: 'pathToNugetConfigFile')]) {
                withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-Nexus', passwordVariable: 'nexusPw', usernameVariable: 'nexusUser')]) {
                    bat "nuget.exe sources update -name Nexus -username ${nexusUser} -password ${nexusPw} " 
                }
                bat 'nuget.exe restore ims-db-csharp.sln' 
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
            gitUtils.getLicenseFile(mxVersion, 'ims-db-csharp\\bin\\Release')
	    }
	
	    stage('test') {
            dir('ims-db-csharp/bin/Release') {
                catchError(message: 'IMS WB tests failed', buildResult: 'UNSTABLE', catchInterruptions: false, stageResult: 'UNSTABLE') {
                    timeout(time: 1, unit: 'HOURS'){
	                		retry(1) {
	                    		bat 'dotnet test ims-db-csharp.dll --filter "FullyQualifiedName~wb_dbd|wb_sql" --platform x64 --logger:"nunit;LogFileName=TestResults.xml"'
	                    	}
	                    }
                    nunit testResultsPattern: 'TestResults\\TestResults.xml', failedTestsFailBuild: false
                }
            }
        }
        
        stage('finalize') {
			bat "copy ims-db-csharp\\bin\\Release\\ims-db-csharp.dll ims-db-csharp_${mxBuildVersion}.dll"
			archiveArtifacts artifacts: "ims-db-csharp_${mxBuildVersion}.dll", allowEmptyArchive: true
		}
    }
}