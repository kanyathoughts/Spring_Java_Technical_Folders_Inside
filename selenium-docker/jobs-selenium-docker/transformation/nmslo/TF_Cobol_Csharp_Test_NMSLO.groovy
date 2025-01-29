@Library('TestUtils') _

/**
 * Builds and runs the NMSLO project
 * 
 * @param mxBuildVersion The maxenso build under test
 * @param useDifferentTestProjectBranch By default the branch of the LeanFT project is checked out based on the selected mxVersion. 
 * If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param environmentLabel The resource label that will be locked while the job is running.
 */

timestamps {
	def miscUtils = new MiscUtils()
	def mxVersionUtils = new MxVersionUtils()
	def gitUtils = new GitUtils()
	def perfUtils = new PerformanceUtils()
	def spUtils = new SharepointUtils()
	
	advancedLock(environmentLabel) {
		nodeTF(env.windowsNode, environmentLabel.contains('perf')) {
			def mxBuildVersionWithoutLeadingZerosAtLastPosition = mxVersionUtils.removeLeadingZerosAtLastPosition(mxBuildVersion)
			def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
			def pathToLogFile = 'VS_Solution/Tests/bin/Debug/logs/NMSLO_performance_report.log'
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
			
			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			buildDescription "testProjectBranch=${testProjectBranch} environmentLabel=${environmentLabel}"
			
			stage('setup') {
				retry(2) {
					deleteDir()
					gitUtils.checkoutGitProject(pwd(), "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/nmslo.git", testProjectBranch)
					def csharpSubfolders = ['Batch/Defs', 'Batch/Pgms', 'Common/Copy', 'Online/Defs', 'Online/Maps', 'Online/Pgms'].collect { subfolder -> "eclipseWorkspace/Test-Project/src-cobol-csharp/${subfolder}/" }.join(', ')
					
					spUtils.downloadJobArtifact('TF_Cobol_Csharp_Migration_NMSLO', mxVersionUtils.getFullBuildForDotnetBuildVersion(mxBuildVersion), 'csharpSources.zip', pwd())
					unzip dir: 'VS_Solution', zipFile: 'csharpSources.zip', quiet: true
		   			
					dir('VS_Solution') {
						configFileProvider([configFile(fileId: 'NuGet.config', targetLocation: '.', variable: 'pathToNugetConfigFile')]) {
							// Add nexus user to NuGet.config
							withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-Nexus', passwordVariable: 'nexusPw', usernameVariable: 'nexusUser')]) {
								bat "nuget.exe sources update -name Nexus -username ${nexusUser} -password ${nexusPw}"
							}
							def dbNodeHostname= miscUtils.getHostNameByNodeName(env.linuxNode)
							// Replace database connection information in config files
							withCredentials([usernamePassword(credentialsId: 'NMSLO_DB_User', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
								def content = readFile 'Tests/App.config'
								content = content.replaceAll(/Data Source=[^;]*/, "Data Source=${dbNodeHostname}")
								content = content.replaceAll(/Integrated Security=[a-zA-Z][a-zA-Z]*/, "User ID=${dbUser};Password=${dbPassword};Integrated Security=False")
								writeFile encoding: 'UTF-8', file: 'Tests/App.config', text: content
								def base64encodedPassword = powershell returnStdout: true, script: "[convert]::ToBase64String([Text.Encoding]::UTF8.GetBytes(\"${dbPassword}\"))"
								content = readFile 'Batch/res/jclconfig.cfg'
								content = content.replaceAll(/SOURCE_DB=.*/, "SOURCE_DB=${dbNodeHostname}")
								content = content.replaceAll(/DB_USERNAME=.*/, "DB_USERNAME=${dbUser}")
								content = content.replaceAll(/DB_PASSWORD=.*/, "DB_PASSWORD=${base64encodedPassword.trim()}")
								writeFile encoding: 'UTF-8', file: 'Batch/res/jclconfig.cfg', text: content
							}
							// Update and get dependencies  
							dir('Online') {
								def maxensoPrefix = ''
								if (mxVersionUtils.hasMaxensoInArtifactName(mxBuildVersion)) {
									maxensoPrefix = 'maxenso.maxenso-'
								}
								bat "dotnet remove package innowake.${maxensoPrefix}runtime-dotnet-dist"
								bat "dotnet remove package innowake.${maxensoPrefix}runtime-torpedo-dotnet-dist"
								bat 'dotnet remove package InnoWake.NET.Dependencies-dist'
								bat "dotnet add package innowake.${maxensoPrefix}runtime-dotnet-dist --version=${mxBuildVersionWithoutLeadingZerosAtLastPosition}"
								bat "dotnet add package innowake.${maxensoPrefix}runtime-torpedo-dotnet-dist --version=${mxBuildVersionWithoutLeadingZerosAtLastPosition}"
								bat "dotnet add package InnoWake.NET.Dependencies-dist --version=${mxBuildVersionWithoutLeadingZerosAtLastPosition}"
							}
							bat 'nuget.exe restore RAPS.sln'
							def output = bat returnStdout: true, script: "nuget.exe update RAPS.sln -Version ${mxBuildVersionWithoutLeadingZerosAtLastPosition} -Id InnoWake.NET.Dependencies-dist"
							echo output
							if (output.contains('is not found') || output.contains('has timed out')) {
								error "innowake artifacts with version ${mxBuildVersion} are not available"
							}
						}
					}
					// Start smtp server for email tests
					dir('fakesmtp') {
						withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings', publisherStrategy: 'EXPLICIT') {
							bat 'mvn dependency:copy -Dartifact=com.github.tntim96:fakesmtp:2.0 -DoutputDirectory=.'
						}
						//in this case, withJava uses default java version defined in MiscUtils
						withJava {
							// The fakesmtp server started here is stopped automatically as soon as this Jenkins job ends. This is accomplished by Windows, the "start" command and the "/b" option.
							bat 'start /b java -jar fakesmtp-2.0.jar -s -b -m'
						}
					}
				}
			}
			
			stage('build') {
				dir('VS_Solution') {
					bat 'msbuild.exe RAPS.sln -t:Build'
					//Add license file
					gitUtils.getLicenseFile(mxVersion, 'Batch\\bin\\Debug')
					gitUtils.getLicenseFile(mxVersion, 'JCLTestCase\\bin\\Debug')
					gitUtils.getLicenseFile(mxVersion, 'LimsDimsService\\bin\\Debug')
					gitUtils.getLicenseFile(mxVersion, 'Online\\bin\\Debug')
					gitUtils.getLicenseFile(mxVersion, 'Online\\LimsPublish\\bin\\Debug')
					gitUtils.getLicenseFile(mxVersion, 'Tests\\bin\\Debug')
				}
			}
				
			stage('test') {
				// Misc Tests
				executeTest('SLO.RAPS.Tests.Batch.Misc', '', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Builtins', '', env.linuxNode) // -> Success
				
				// Online Tests
				executeTest('SLO.RAPS.Tests.Online.Screens1', 'dev_3.20181022_BEFORE_ONLINE.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Online.Screens2', 'dev_3.20181021_BEFORE_ONLINE.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Online.Screens3', 'dev_3.20181128_BEFORE.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Online.Screens4', 'dev_3.20181128_AFTER_ONLINE_B.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Online.Screens5', 'dev_3.20181128_AFTER_ONLINE_C.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Online.Screens6', 'dev_3.20190104_AFTER_BATCH.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Online.Screens7', 'dev_3.20181128_AFTER_ONLINE_D.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Online.Screens8', 'dev_3.20181128_AFTER_ONLINE_F.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Online.Screens9', 'dev_3.20181128_AFTER_ONLINE_G.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Online.SIT_Roles', 'dev_3.20190514_BEFORE_QA_SIT_ROLES.bak', env.linuxNode) // -> Success
				
				// Batch Tests
				executeTest('SLO.RAPS.Tests.Batch.Jobs000a', 'dev_3.20181022_BEFORE_ONLINE.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0000', 'dev_3.20181127_BEFORE_BATCH.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0220', 'dev_3.20190220_BEFORE_BATCH.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0225', 'dev_3.20190225_BEFORE_BATCH.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0226', 'dev_3.20190226_BEFORE_BATCH.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0227a', 'dev_3.20190227_BEFORE_BATCH_A.bak', env.linuxNode) // -> Success
				//executeTest('SLO.RAPS.Tests.Batch.Jobs0227b', 'dev_3.20190227_BEFORE_BATCH_B.bak', env.linuxNode) // -> Failure, db import fails
				executeTest('SLO.RAPS.Tests.Batch.Jobs0227c', 'dev_3.20190227_BEFORE_BATCH_C.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0228', 'dev_3.20190228_BEFORE_BATCH.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0301', 'dev_3.20190301_BEFORE_BATCH.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0306', 'dev_3.20190306_BEFORE_BATCH.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0403', 'dev_3.20190403_BEFORE_BATCH.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0412', 'dev_3.20190412_BEFORE_PROD_EVPE.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0615', 'dev_3.201906015_AFTER_PROD_EVPE_B.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0628a', 'dev_3.20180628_BEFORE_RAP2.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.Jobs0628b', 'dev_3.20180628_BEFORE_RSET.bak', env.linuxNode) // -> Success
				//executeTest('SLO.RAPS.Tests.Batch.Pf9Jobs', 'dev_3.20190104_AFTER_PF9.bak', env.linuxNode) // -> Failure
				executeTest('SLO.RAPS.Tests.Batch.Jobs0918SIT', 'dev_3.20190918_SIT.bak', env.linuxNode) // -> Success
				
				// EDDR, EVDR, MDSR 
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0413', 'dev_3.7PM_Prod_April_13_20200423.bak', env.linuxNode) // -> Failure
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0414', 'dev_3.7PM_Prod_April_14_20200423.bak', env.linuxNode) // -> Failure
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0410', 'dev_3.RAPS-SQL-STG_raps_stg_FULL_20200410.bak', env.linuxNode) // -> Failure
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0415', 'dev_3.7PM_Prod_April_15_20200423.bak', env.linuxNode) // -> Failure
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0416', 'dev_3.7PM_Prod_April_16_20200423.bak', env.linuxNode) // -> Failure
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0417', 'dev_3.7PM_Prod_April_17_20200424.bak', env.linuxNode) // -> Failure
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0420', 'dev_3.7PM_Prod_April_20_20200425.bak', env.linuxNode) // -> Failure
				
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobsQa0420', 'raps_qa.Before_OG4EVDR_20200420_171157.bak', env.linuxNode) // -> Failure, INCOMPLETE database error 
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0716', 'dev_3.7PM_Prod_July_16_20200717_Updated.bak', env.linuxNode) // -> Failure, INCOMPLETE database error 
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0506', 'dev_3.7PM_Prod_May_06_20200507.bak', env.linuxNode) // -> Failure
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0706', 'dev_3.20200706_7PM_Prod_20200801.bak', env.linuxNode) // -> Failure
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0709', 'dev_3.20200709_7PM_Prod_20200802.bak', env.linuxNode) // -> Failure
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0713', 'dev_3.20200713_7PM_Prod_20200802.bak', env.linuxNode) // -> Failure
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0722', 'raps_sit.20200722_7PM_RAPS_before_EDDR.bak', env.linuxNode) // -> Failure, INCOMPLETE database error
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0723', 'dev_3.20200723_7PM_Prod_20200801.bak', env.linuxNode) // -> Failure, INCOMPLETE database error 
				
				// MNOTI
				//executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0331', 'raps_perf.pre_OG4MNOTI_for_March_Mnth_End.bak', env.linuxNode) // -> Failure
					
				// EVPE
				executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0613', 'dev_3.6AM_Prod_June_13_20200624.bak', env.linuxNode) // -> Success
				executeTest('SLO.RAPS.Tests.Batch.RegressionJobs0620', 'dev_3.6AM_Prod_June_20_20200731.bak', env.linuxNode) // -> Success
				
				if (mxVersion == '21.6') {
					setBuildStatusBasedOnNUnitReports('VS_Solution/Tests/bin/Debug/TestResults-*.xml', 64)
				} else if (mxVersion == '22.2') {
					setBuildStatusBasedOnNUnitReports('VS_Solution/Tests/bin/Debug/TestResults-*.xml', 44)
				} else {
					setBuildStatusBasedOnNUnitReports('VS_Solution/Tests/bin/Debug/TestResults-*.xml', 14)
				}
			}

			stage('finalize') {
				archiveArtifacts pathToLogFile
				zip archive: true, dir: 'VS_Solution/Tests/bin/Debug/', glob: 'TestResults-*.xml', zipFile: 'TestResults.zip'
			}

			stage('reporting') {
			    def buildResult = build job: 'TF_Performance_Reporting', quietPeriod: 0, propagate: false,
			                      parameters: [string(name: 'mxBuildVersion',    value: mxBuildVersion),
			                                   string(name: 'pathToLogFile',     value: pathToLogFile),
			                                   string(name: 'buildURL',          value: BUILD_URL),
			                                   string(name: 'pageID',            value: '166891894'),			                                   
			                                   string(name: 'numDaysDataToKeep', value: '20')]

				miscUtils.evaluateBuildResult(buildResult, "TF_Performance_Reporting")
        	}
		}
	}
}

/**
 * Starts the database, runs a test and stops the database again
 * 
 * @param nameSpace Full path to the test class
 * @param databaseSnapshot Name of the dabase snapshot that will be imported
 * @param dbNodeName Name of the node where the database will be started
 */
def executeTest(nameSpace, databaseSnapshot, dbNodeName) {
	def dbContainerName = 'nmslo_db'
	if (startDb(dbContainerName, dbNodeName, databaseSnapshot)) {
		dir('VS_Solution/Tests/bin/Debug') {
			bat "RAPSTests.exe --where=namespace=${nameSpace} --result=TestResults-${nameSpace}.xml;format=nunit2"   
		}
		stopDb(dbContainerName, dbNodeName)
	}
}

/**
 * Starts the db container and imports a database snapshot if specified
 *
 * @param dbContainerName Name of the container
 * @param dbNodeName Name of the node where the database will be started
 * @param databaseSnapshot Name of the dabase snapshot that will be imported
 * @return true if the database start was successful, false if not
 */
def startDb(dbContainerName, dbNodeName, databaseSnapshot) {
	def dockerUtils = new DockerUtils()
	def gitUtils = new GitUtils()
	def mxVersionUtils = new MxVersionUtils()
	def miscUtils = new MiscUtils()
	
	def returnStatus = false
	
	catchError(catchInterruptions: false, stageResult: 'FAILURE', message: 'Problem starting the database') {
		retry(5) {
			stopDb(dbContainerName, dbNodeName)
			nodeTF(dbNodeName, dbNodeName.contains('perf')) {
				deleteDir()
				docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside {
					def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
					gitUtils.checkoutGitProject('nmslo', "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/nmslo.git", testProjectBranch)
				}
				withCredentials([usernamePassword(credentialsId: 'NMSLO_DB_User', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
					// All database snapshot have to be available under /data/nmslo_db on the server where the test runs
					def databaseMount = "-v /data/nmslo_db/${databaseSnapshot}:/tmp/db_dump.bak:ro"
					if (databaseSnapshot == '') {
						databaseMount = "-v ${pwd()}/nmslo/VS_Solution/JCLTestCase/Test/res/Sql/tables.create.sql:/tmp/tables.create.sql"
					}
					docker.image('mcr.microsoft.com/mssql/server:2019-CU16-GDR1-ubuntu-20.04').run("--name ${dbContainerName} -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=${dbPassword}' -p 1433:1433 ${databaseMount}")
					// Wait until database is up and running
					sleep 30
					def database = 'unit_test_1'
					if (databaseSnapshot == '') {
						// If no database snapshot is imported, some databases must be created
						sh "docker exec ${dbContainerName} /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P ${dbPassword} -Q 'CREATE DATABASE ${database}'"
						sh "docker exec ${dbContainerName} /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P ${dbPassword} -Q 'CREATE DATABASE raps_dev'"
						sh "docker exec ${dbContainerName} /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P ${dbPassword} -d raps_dev -i /tmp/tables.create.sql"
					} else {
						def sourceDatabase = databaseSnapshot.split(/\./)[0]
						def stdOut = sh returnStdout: true, script: "docker exec ${dbContainerName} /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P ${dbPassword} -Q \"RESTORE DATABASE [${database}] FROM DISK='/tmp/db_dump.bak' WITH REPLACE, MOVE '${sourceDatabase}' TO '/var/opt/mssql/data/${database}.mdf', MOVE '${sourceDatabase}_log' TO '/var/opt/mssql/data/${database}_log.ldf' \""
						echo stdOut
						if (stdOut.contains('RESTORE DATABASE is terminating abnormally')) {
							error 'RESTORE DATABASE is terminating abnormally'
						}
					}
					returnStatus = true
				}
			}
		}
	}
	return returnStatus
}


/**
 * Stops the db container
 * 
 * @param dbContainerName Name of the container
 * @param dbNodeName of the node the database is running on
 */
def stopDb(dbContainerName, dbNodeName) {
	nodeTF(dbNodeName, dbNodeName.contains('perf')) {
		sh returnStatus: true, script: "docker rm -f ${dbContainerName}"
	}
}

/**
 * This method goes over all NUnit reports and checks how many test cases failed.
 * If the number of actual failing test cases does not match the expected number 
 * the method will mark the stage as unstable.
 * 
 * @param filePattern Pattern for NUnit files
 * @param expectedNumberOfFailedTestCases Number of test cases that are expected to fail
 */
def setBuildStatusBasedOnNUnitReports(filePattern, expectedNumberOfFailedTestCases) {
	catchError(catchInterruptions: false, message: 'Error while getting the number of failed test cases', stageResult: 'FAILURE') {
		def actualNumberOfFailedTestCases = 0
		def files = findFiles glob: filePattern
		files.each { file ->
			def fileContent = readFile encoding: 'UTF-8', file: file.toString()
			def errorMatcher = fileContent =~ /errors=\"(\d*)\"/
			errorMatcher.find()
			actualNumberOfFailedTestCases += errorMatcher.group(1).toInteger()
			def failureMatcher = fileContent =~ /failures=\"(\d*)\"/
			failureMatcher.find()
			actualNumberOfFailedTestCases += failureMatcher.group(1).toInteger()
		}
		if (actualNumberOfFailedTestCases != expectedNumberOfFailedTestCases) {
			unstable "The number of failed test cases was ${actualNumberOfFailedTestCases} instead of ${expectedNumberOfFailedTestCases}"
		}
	}
}