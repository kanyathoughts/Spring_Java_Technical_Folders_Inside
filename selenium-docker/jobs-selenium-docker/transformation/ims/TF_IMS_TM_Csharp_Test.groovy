@Library('TestUtils') _

/**
 * Checkout and run the IMS TM tests in C#.
 *
 * @param mxBuildVersion  The maxenso version to run the IMS TM tests. 
 * @param useDifferentTestProjectBranch By default the ims-tm-csharp project is checked out with branch master. 
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param dotnetFrameworkVersion The .Net Framework version the build and the test will run with
**/

nodeTF('USWindows8-Trafo') {
	timestamps {
		def mxVersionUtils = new MxVersionUtils()
		def gitUtils = new GitUtils()
		def miscUtils = new MiscUtils()
		
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
		
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "testProjectBranch=${testProjectBranch} dotnetFrameworkVersion=${dotnetFrameworkVersion} fullBuild=${fullBuild}"
	
		stage('initialisation') {
		    deleteDir()
			gitUtils.checkoutGitProject(pwd(), "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/ims/ims-tm-csharp.git", testProjectBranch)
		   
		    copyArtifacts projectName: 'TF_IMS_Csharp_Migration',
							  filter: 'IMS_Migration_Csharp/eclipseWorkspace/Test-Project/src-*-csharp/tm/',
							  selector: lastSuccessful(),
							  parameters: "mxBuildVersion=${fullBuild}",
							  fingerprintArtifacts: true
				bat "rd /s /q ims-tm-csharp\\src-cobol-csharp ims-tm-csharp\\src-ims-csharp"
				bat returnStatus: true, script: 'robocopy IMS_Migration_Csharp/eclipseWorkspace/Test-Project/src-ims-csharp ims-tm-csharp/src-ims-csharp /e'
			    bat returnStatus: true, script: 'robocopy IMS_Migration_Csharp/eclipseWorkspace/Test-Project/src-cobol-csharp ims-tm-csharp/src-cobol-csharp /e'
			
		    configFileProvider([configFile(fileId: 'NuGet.config', targetLocation: '.', variable: 'pathToNugetConfigFile')]) {
				withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-Nexus', passwordVariable: 'nexusPw', usernameVariable: 'nexusUser')]) {
					bat "nuget.exe sources update -name Nexus -username ${nexusUser} -password ${nexusPw}" 
				}
				bat "nuget.exe restore ims-tm-csharp.sln"
				// Innowake Package update
				def output = bat returnStdout: true, script: "nuget.exe update ims-tm-csharp.sln -Version ${mxBuildVersion} -Id innowake.runtime-dotnet-testframework-dist"
				echo output
				if (output.contains('is not found')){
					error "innowake artifacts with version ${mxBuildVersion} are not available"
				}
			}
		}
		
		stage('build') {
		    bat "msbuild ims-tm-csharp.sln -t:Clean,Build -p:TargetFrameworkVersion=${dotnetFrameworkVersion}"
	        gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), 'ims-tm-csharp\\bin\\Debug')
	    }
	   
	    stage('test') {
	        dir('ims-tm-csharp/bin/Debug') {
	            catchError(message: 'IMS TM tests failed', buildResult: 'UNSTABLE', catchInterruptions: false, stageResult: 'UNSTABLE') {
	            	retry(2) {
	            		timeout(time: 20, unit: 'MINUTES') {
	            			bat 'dotnet test ims-tm-csharp.dll --logger:"nunit;LogFileName=TestResults.xml"'
	            		}
	            	}
	            }
				nunit testResultsPattern: 'TestResults\\TestResults.xml', failedTestsFailBuild: false
            }
	    }
	    
	    stage('finalize') {
			archiveArtifacts artifacts: 'ims-tm-csharp/test/res/log.out', allowEmptyArchive: true 
		}
	}
}