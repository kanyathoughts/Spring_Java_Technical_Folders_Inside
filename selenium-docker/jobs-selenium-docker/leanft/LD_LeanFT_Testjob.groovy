/**
 * Job script to compile and run all automated LeanFT test cases for maxenso/innowake 
 * 
 * @param mxBuildVersion The maxenso/innowake build to test.
 * @param testcase Name including the package of the test you want to run.
 * @param reuse_workspace If true project checkout and copy of the eclipse is 
 * 		skipped and the previous artifacts of project and eclipse is used.
 * @param executeOn The Jenkins node the test will run on
 * @param scEnvironmentHost The Jenkins node host name the SC_Environment is running on
 * @param javaVersion The java version the test will run with
 * @param useDifferentTestProjectBranch By default the branch of the LeanFT project is checked out based on the selected mxVersion. 
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */
@Library('TestUtils') _

node(executeOn) {
	timestamps {
		def svnUtils = new SvnUtils()
		def gitUtils = new GitUtils()
		def miscUtils = new MiscUtils()
		def mxVersionUtils = new MxVersionUtils()
		def spUtils = new SharepointUtils()
		def dockerUtils = new DockerUtils()
		def jobWorkspace = pwd()
		//def eclipseFolderpath = "${jobWorkspace}\\eclipse-windows\\eclipse.exe"
		def eclipseFolderpath = "${jobWorkspace}\\eclipse-windows"
		def eclipseDir = "${jobWorkspace}\\eclipse-windows"
		def eclipseExe = "${eclipseDir}\\eclipse.exe"
		def eclipseWorkspace = "${jobWorkspace}\\workspace"
		def eclipseRequired = true
		def svnUrlQm = svnUtils.getSvnUrlQm()
		def testProject = 'leanft-ld'
		def scEnvironmentPort = '55004'
		def runResultsDir = 'RunResults'
		def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
		def testProjectBranch
		def innowakeLicenseLocation
		def testcaseBranchVersion = (mxVersion == '99.9') ? 'master' : mxVersion
		def oraclePort
		def oracleHost
		def oracleDB = 'oracleDB'
		def oracleRequiredTestcases = ['testcases.mee.MeeDBMaintenanceToolTest', 'testcases.mee.MeeDBHealthCheckTest']
		// all elements in this list don't need innoWake's eclipse and therefore eclipse won't be downloaded
		def eclipseIsNotNeededList = ['testcases.base.EclipseInstallToolTest', 'testcases.smoketests.ControlCenterTest']

		if (Boolean.parseBoolean(useDifferentTestProjectBranch)) {
			testProjectBranch = differentTestProjectBranch
		} else if (mxVersion == '99.9') {
			testProjectBranch = 'master'
		} else {
				testProjectBranch = mxVersion
			   //testProjectBranch = 'WQS-2725'
		}
		reuse_workspace = Boolean.parseBoolean(reuse_workspace)

		buildName "#${env.BUILD_ID} - ${testcase} - ${mxBuildVersion}"
		buildDescription "executeOn=${executeOn} mxBuildVersion=${mxBuildVersion} testcase=${testcase} reuse_workspace=${reuse_workspace} scEnvironmentHost=${scEnvironmentHost} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"

		stage('Checkout test project') {
			if (reuse_workspace) {
				echo 'Skipping "Checkout test project"'
			} else {
				deleteDir()
				def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/ui-test-projects/leanft-ld.git"
				dir(testProject) {
					git branch: testProjectBranch, credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
				}
			}
		}

		stage('Copy maxenso/innowake eclipse') {
			if (reuse_workspace || testcase in eclipseIsNotNeededList) {
				echo 'Skipping "Copy maxenso/innowake eclipse"'
			} else {
				echo 'Download eclipse bundle from Sharepoint'
				spUtils.downloadIwEclipseWindows(mxBuildVersion, "${jobWorkspace}/eclipse-windows")
			}
		}

		if (testcase in oracleRequiredTestcases) {
			echo '------Starting Oracle Environment------'
			def oracleBuildResult = build job: 'LD_Docker_Start_Oracle_Env', parameters: [
				extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
				string(name: 'imageTag', value: 'oracle-MIGRATED'),
				string(name: 'javaVersion', value: javaVersion),
				[$class: 'NodeParameterValue', name: 'executeOn', labels: ['USLinux1'], nodeEligibility: [$class: 'AllNodeEligibility']]
			]
			miscUtils.evaluateBuildResult(oracleBuildResult)
			oraclePort = oracleBuildResult.buildVariables['oraclePort']
			oracleHost = oracleBuildResult.buildVariables['oracleHost']
			echo "DB is ready and listening on port ${oraclePort}. DB is running on ${oracleHost}."
			//stores oraclePort and oracleHost in the file 'config.properties' of leanft. 
			//To use both variables during testing (it can be called with PropertyProvider.getProperty("oracle.host"))
			bat "echo. >> ${jobWorkspace}/${testProject}/resources/config.properties"
			bat "echo #OracleDB >> ${jobWorkspace}/${testProject}/resources/config.properties"
			bat "echo oracle.port=${oraclePort} >> ${jobWorkspace}/${testProject}/resources/config.properties"
			bat "echo oracle.host=${oracleHost} >> ${jobWorkspace}/${testProject}/resources/config.properties"
		}

		/**
		* For this testcase it is necessary to use a fresh eclipse. 
		* Unfortunately, this fresh eclipse is stored as a *.tar.gz file in Sharepoint.
		* It was difficult to extract this file on a windows node, therefore the extraction is performed on a linux node. 
		* The extracted files/folders will be stashed and then unstashed on the windows node 
		*/
		if(testCase == 'testcases.base.EclipseInstallToolTest'){
			node('Docker-host') {
			    deleteDir()
				def workDir = pwd() 
				eclipseDir = 'eclipse'
				def linuxEclipseZip = mxVersionUtils.getEclipseZip(mxBuildVersion)
				def windowsEclipseZip = linuxEclipseZip.replaceAll("linux-gtk-x86_64.tar.gz", "win32-x86_64.tar.gz")
				echo "Eclipse ${windowsEclipseZip}"
				docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
					dir(eclipseDir){
						if (! spUtils.downloadFile("${spUtils.getBaseEclipseDir()}/${windowsEclipseZip}", './')) {
	            			error "Download of ${spUtils.getBaseEclipseDir()}/${windowsEclipseZip} failed"
	            		}
						sh "tar -xf ${windowsEclipseZip} -C ${workDir}"
						sh "rm ${windowsEclipseZip}"
					}
				}
				stash name: 'eclipse-dir', includes: "${eclipseDir}/**"
			}
		}

		stage('Run test') {
			try {
				timeout(time: 1, unit: 'HOURS') {
					def workspace = "${eclipseWorkspace}\\${testcase}"
					def splashScreenVersion = mxVersionUtils.getVersionOnAboutDialog(mxBuildVersion)
					if ( ! mxVersionUtils.isTrunk(mxBuildVersion)) {
						splashScreenVersion = splashScreenVersion.replaceFirst(' ','').replaceFirst('_', ' ')
					}
					dir(workspace) {
						deleteDir()
					}
					dir(runResultsDir) {
						deleteDir()
					}
					withJava(javaVersion) {
						if (testProjectBranch > '21.0') {
							withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
								dir(testProject) {
									def status = bat returnStatus: true, script: 'mvn clean compile exec:java -Dexec.mainClass=unittesting.session.UiSessionDetector'
									if (status != 0) {
										def errorMessage = "On the node ${executeOn} is currently no UI sessions active, therefore the build failed and no tests were executed"
										wrap([$class: 'BuildUser']) {
											if (env.BUILD_USER_ID != 'null') {
												def subj = "build ${currentBuild.number} of ${currentBuild.projectName} failed"
												def body = "${errorMessage}, see ${currentBuild.absoluteUrl}"
												def to = env.BUILD_USER_ID
												emailext subject: subj, body: body, to: to
											}
										}
										error errorMessage
									}
								}
							}
						}
						switch (testcase) {
							case 'testcases.smoketests.ControlCenterTest':
								def control_center = 'control-center'
								def cc = "${control_center}\\control-center"
								def lcm_api_client = 'lcm-api-client'
								dir(lcm_api_client) {
									deleteDir()
									spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'lcm-api-client.zip', "${jobWorkspace}/lcm-api-client")
									bat "java -jar lcm-api-client\\lib\\lcm-api-client-dist-${mxBuildVersion}.jar -host ${scEnvironmentHost} -variant ENT -port ${scEnvironmentPort} -requests 5 -project LCMTestprojekt -scriptname \"Hello World - Server\" 2>&1"
								}
								dir(control_center) {
									deleteDir()
									spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'control-center.zip', "${jobWorkspace}/control-center")
								}
									dir(cc) {
										bat 'start-en.bat'
									}
									eclipseRequired = false
									break
							case 'testcases.smoketests.NatanalyzerTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_NATANALYZER", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-natanalyzer.git", testcaseBranchVersion)
								break
							case 'testcases.smoketests.MeeclipseTest':
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								break
							case 'testcases.mee.GdaAccessFromJarTest':
								gitUtils.checkoutGitProject("${workspace}\\WMEE8814", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/wmee-8814.git", testcaseBranchVersion)
								break
							case 'testcases.mee.MeeclipseLaunchableTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_IRIS_6867", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/base/testcase-iris-6867.git", testcaseBranchVersion)
								break
							case 'testcases.mee.MeeclipseWorkspacePreferencesTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_MEE", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/testcase-mee.git", testcaseBranchVersion)
								gitUtils.getLicenseFile(mxVersion, workspace)
								break
							case 'testcases.mee.MeeclipseNaturalIncludeJCopyCodeTest':
							case 'testcases.mee.MeeclipseMeeEditorSpecificTest':
							case 'testcases.mee.MeeclipseViewDefinitionsTest':
							case 'testcases.mee.MeeclipseMeeEditorJDTTest':
							case 'testcases.mee.MeeclipseNaturalRedefineTest':
							case 'testcases.mee.MeeclipseArraysOnGroupsTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_MEE", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/testcase-mee.git", testcaseBranchVersion)
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								gitUtils.getLicenseFile(mxVersion, workspace)
								break
							case 'testcases.mee.MeeclipseCobolRedefineTest':
							case 'testcases.mee.MeeclipseAutomaticAssemblingOfJCopiesAndDataAreasOnChangeTest':
							case 'testcases.mee.MeeclipseCobolIncludeJCopyCodeTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_MEE_JCPY", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/testcase-mee-jcpy.git", testcaseBranchVersion)
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								gitUtils.getLicenseFile(mxVersion, workspace)
								break
							case 'testcases.mee.MeeclipseVaadinPrintFunctionTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_VAADIN_PRINT", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/testcase-vaadin-print.git", testcaseBranchVersion)
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								gitUtils.getLicenseFile(mxVersion, workspace)
								break
							case 'testcases.mee.MeeclipseMonitoringAppTest':
								gitUtils.checkoutGitProject("${workspace}\\Wmee4757Test", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/wmee-4757.git", testcaseBranchVersion)
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								gitUtils.getLicenseFile(mxVersion, workspace)
								break
							case 'testcases.mee.MeeclipseVaadinTestsTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_VAADIN", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/testcase-vaadin.git", testcaseBranchVersion)							
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								gitUtils.getLicenseFile(mxVersion, workspace)							
								break
							case 'testcases.mee.MeeclipseBatchclipseTest':
								gitUtils.checkoutGitProject("${workspace}\\jcl-test", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/jcl-test.git", testcaseBranchVersion)							
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								gitUtils.getLicenseFile(testcaseBranchVersion, workspace)							
								innowakeLicenseLocation = "${jobWorkspace}/eclipse-windows/innowake.lic"
								break
							case 'testcases.mee.MeeclipseNextLevelFunctionalityTest':
								gitUtils.checkoutProject(workspace, 'mmrs-m01-mmrs00b', svnUtils.getSvnUrlProject(mxVersion, 'mmrs/mmrs-m01-mmrs00b'))
								copyInnoWakeRuntimeJarsToFolder("${workspace}/lib", mxBuildVersion)
								gitUtils.getLicenseFile(mxVersion, workspace)
								dir("${workspace}/mmrs-m01-mmrs00b") {
									bat 'erase /f .classpath'
									bat 'rename .classpath-leanft .classpath'
								}
								break
							case 'testcases.mee.MeeclipseJMapEditorTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_JMAP_EDITOR", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/testcase-jmap-editor.git", testcaseBranchVersion)		
								gitUtils.checkoutGitProject("${workspace}\\Nat2Java", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/nat2java.git", 'master')
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								gitUtils.getLicenseFile(testcaseBranchVersion, workspace)		
								copyMigrationJarsToFolder("${workspace}/lib", mxBuildVersion)
								dir("${workspace}/scriptActions"){
									spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "natural-${mxBuildVersion}.script-actions", "${workspace}/scriptActions")	
								}									
								break
							case 'testcases.mee.MeeclipseCobolMeeSteplibsTest':
							case 'testcases.mee.MeeScreenReaderCompatibilityPluginTest':
							case 'testcases.ndt.CobolclipseBMSMapViewerEnhancementTest':
								gitUtils.checkoutGitProject("${workspace}\\testcase-cobol", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-cobol.git", testcaseBranchVersion)							
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								gitUtils.getLicenseFile(mxVersion, workspace)							
								break
							case 'testcases.mee.MeeclipseCICSBMSOperandsTest':
							case 'testcases.mee.MeeclipseBMSMapPreviewTest':							
								gitUtils.checkoutGitProject("${workspace}\\CICS-Testing", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/testcase-cics.git", testcaseBranchVersion)
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								gitUtils.getLicenseFile(mxVersion, workspace)
								innowakeLicenseLocation = "${jobWorkspace}/eclipse-windows/innowake.lic"
								break
							case 'testcases.mee.ParserCobolTest':
								gitUtils.checkoutGitProject("${workspace}\\testcase-cobol", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-cobol.git", testcaseBranchVersion)
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								copyMigrationJarsToFolder("${workspace}/lib", mxBuildVersion)
								dir("${workspace}/scriptActions") {
									spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "cobol-${mxBuildVersion}.script-actions", "${workspace}/scriptActions")
								}
								break
							case 'testcases.mee.MeeCobol2JavaFeaturesTest':
								gitUtils.checkoutGitProject("${workspace}\\testcase-cobol", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-cobol.git", testcaseBranchVersion)
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								copyMigrationJarsToFolder("${workspace}/lib", mxBuildVersion)
								dir("${workspace}/scriptActions") {
									spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "cobol-${mxBuildVersion}.script-actions", "${workspace}/scriptActions")
								}
								break
							case 'testcases.mee.Cobol2JavaTest':
								gitUtils.checkoutGitProject("${workspace}\\testcase-cobol", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-cobol.git", testcaseBranchVersion)
								gitUtils.checkoutGitProject("${workspace}\\wmee-7231-target", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/wmee-7231-target.git", 'master')
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								copyMigrationJarsToFolder("${workspace}/lib", mxBuildVersion)
								spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "cobol-${mxBuildVersion}.script-actions", "${workspace}/lib")
								break			
							case 'testcases.mee.SourceMigrationCobolTest':
								gitUtils.checkoutGitProject("${workspace}\\wmee-7231-src", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/wmee-7231-src.git", 'master')
								gitUtils.checkoutGitProject("${workspace}\\wmee-7231-target", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/wmee-7231-target.git", 'master')
								copyMigrationJarsToFolder("${workspace}/lib", mxBuildVersion)
								dir("${workspace}/scriptActions") {
									spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "cobol-${mxBuildVersion}.script-actions", "${workspace}/scriptActions")
								}
								break
							case 'testcases.mee.Natural2JavaTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTS_QS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/tests-qs.git", testcaseBranchVersion)
								copyMigrationJarsToFolder("${workspace}/lib", mxBuildVersion)
								dir("${workspace}/scriptActions") {
									spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "natural-${mxBuildVersion}.script-actions", "${workspace}/scriptActions")
								}
								break
							case 'testcases.mee.MeeDBMaintenanceToolTest':
								gitUtils.checkoutGitProject("${workspace}\\OracleDBConnection", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/db-oracle-connection.git", 'master')
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_DB_MAINTENANCE", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/testcase-db-maintenance.git", testcaseBranchVersion)
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								copyMigrationJarsToFolder("${workspace}/lib", mxBuildVersion)
								gitUtils.getLicenseFile(mxVersion, workspace)
								innowakeLicenseLocation = "${jobWorkspace}/eclipse-windows/innowake.lic"
								dir("${workspace}/scriptActions") {
									spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "natural-${mxBuildVersion}.script-actions", "${workspace}/scriptActions")
								}				
								break
							case 'testcases.mee.MeeDBMaintenanceToolSchemaNameTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_DB_MAINTENANCE", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/testcase-db-maintenance.git", testcaseBranchVersion)
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								copyMigrationJarsToFolder("${workspace}/lib", mxBuildVersion)
								gitUtils.getLicenseFile(mxVersion, workspace)
								innowakeLicenseLocation = "${jobWorkspace}/eclipse-windows/innowake.lic"
								dir("${workspace}/scriptActions") {
									spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "natural-${mxBuildVersion}.script-actions", "${workspace}/scriptActions")
								}				
								break
							case 'testcases.mee.PL12JavaTest':
								gitUtils.checkoutGitProject("${workspace}\\testcase-pl1", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/testcase-pl1.git", testcaseBranchVersion)
								copyMigrationJarsToFolder("${workspace}/lib", mxBuildVersion)
								dir("${workspace}/scriptActions") {
									spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "pl1-${mxBuildVersion}.script-actions", "${workspace}/scriptActions")
								}	
								break
							case 'testcases.mee.MeeDBHealthCheckTest':
								gitUtils.checkoutGitProject("${workspace}\\OracleDBConnection", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/db-oracle-connection.git", 'master')
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_DB_MAINTENANCE", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/testcase-db-maintenance.git", testcaseBranchVersion)
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								copyMigrationJarsToFolder("${workspace}/lib", mxBuildVersion)
								gitUtils.getLicenseFile(mxVersion, workspace)
								innowakeLicenseLocation = "${jobWorkspace}/eclipse-windows/innowake.lic"
								dir("${workspace}/scriptActions") {
									spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "natural-${mxBuildVersion}.script-actions", "${workspace}/scriptActions")
								}
								dir("${workspace}\\TESTCASE_DB_MAINTENANCE") {
									bat "echo 'start cmd' > helper.bat"
								}				
								break
							case 'testcases.mdd.AuthFileTest':
							case 'testcases.mdd.AutoImportTest':
							case 'testcases.mdd.BusinessRulesEditorTest':
							case 'testcases.mdd.FlowlayoutTest':
							case 'testcases.mdd.IntegrityViolationTest':
							case 'testcases.mdd.PreferencesAndPropertiesTest':
							case 'testcases.mdd.ReferenceViewTest':
							case 'testcases.mdd.ShortcutsTest':
							case 'testcases.mdd.UiEditorTest':
								gitUtils.checkoutGitProject("${workspace}\\maxenso-example-swing", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mdd/maxenso-example/maxenso-example-swing.git", testcaseBranchVersion)
								break
							case 'testcases.mdd.MDDSmokeTestsTest':
							      checkoutGitProject("${workspace}\\MaxensoExample-vaadin", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mdd/maxenso-example/maxenso-example-vaadin.git", testcaseBranchVersion)							  
								  copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								  gitUtils.getLicenseFile(mxVersion, workspace)							  
								  break
							case 'testcases.ndt.DDMEditorTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTS_QS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/tests-qs.git", testcaseBranchVersion)
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_SVN_LOCK", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-svn-lock.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.LCMAccessibilityTest':
								gitUtils.checkoutGitProject("${workspace}\\LCMTestprojekt", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/sc/lcm-testproject-for-leanft-test.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.NatCreatorCreateFromTemplateTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTS_NC_QS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natcreator/tests-nc-qs.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.NaturalCallSequenceViewTest':
							case 'testcases.ndt.NaturalEditorCodeCollapseAndCodeProtectionTest':
							case 'testcases.ndt.NaturalEditorCodeFormattingAndCodeStructuringTest':
							case 'testcases.ndt.NaturalLogicalViewTest':
							case 'testcases.ndt.NaturalRemotePreviewTest':
							case 'testcases.ndt.NaturalRulesViewTest':
							case 'testcases.ndt.ObjectCaseTest':
							case 'testcases.ndt.RemoteOperationsTest':
							case 'testcases.ndt.NaturalLCMConfigurationTest':
							case 'testcases.ndt.TeamFileTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTS_QS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/tests-qs.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.NatcreatorHyperlinkingTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_HYPERLINKING", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natcreator/testcase-hyperlinking.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.NaturalDependenciesTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_CROSSLIBRARY_DEPENDENCY", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-crosslibrary-dependency.git", testcaseBranchVersion)
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_SUBSTITUTIONSREFERENZEN", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-substitutionsreferenzen.git", testcaseBranchVersion)
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_UPLOAD_COMPILE_DEPENDENCIES", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-upload-compile-dependencies.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.NaturalEditorHyperlinkingAndProjectlinkingTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTS_QS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/tests-qs.git", testcaseBranchVersion)
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_EDITOR_SYNTAXHIGHLIGHTING", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-editor-syntaxhighlighting.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.NaturalEditorToUpperAndToLowerCaseCodeTranslationTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTS_QS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/tests-qs.git", testcaseBranchVersion)
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_ILLEGAL_CHARACTERS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-illegal-characters.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.NaturalObjectInfoViewTest':
							case 'testcases.ndt.NaturalEditorLineNumberReferencesTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTS_QS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/tests-qs.git", testcaseBranchVersion)
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_LINE_REFERENCES", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-line-references.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.NaturalTargetPropertyPageTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_TARGET_PROPERTYPAGE_READONLY", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-target-propertypage-readonly.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.NormalizationTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_IRIS_1466", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-iris-1466.git", testcaseBranchVersion)
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_SQL_STATEMENTS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-sql-statements.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.SteplibsTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_LOCAL_STEPLIBS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-local-steplibs.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.CobolclipseExtendedTest':
							case 'testcases.ndt.CobolclipseCopybookAssemblingTest':
							case 'testcases.ndt.CobolclipseOptionalSettingTest':
							case 'testcases.ndt.CobolclipsePeekDefinitionFunctionalityTest':
							case 'testcases.ndt.CobolclipseSyntaxHighlightingTest':
								gitUtils.checkoutGitProject("${eclipseWorkspace}\\${testcase}\\testcase-cobol", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-cobol.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.MapEditorTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTS_QS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/tests-qs.git", testcaseBranchVersion)
								gitUtils.checkoutGitProject("${workspace}\\ReadOnlyModules", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/read-only-modules.git", testcaseBranchVersion)
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_SVN_LOCK", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-svn-lock.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.ErrorMessageEditorTest':
								checkoutProjectWithVersionControl(workspace, 'TESTCASE_SVN_LOCK', svnUtils.getSvnUrlProject(mxVersion, 'TESTCASE_SVN_LOCK'))
								break
							case 'testcases.ndt.TargetCollisionErrorsTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTS_QS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/tests-qs.git", testcaseBranchVersion)
								gitUtils.checkoutGitProject("${workspace}\\testcase-cobol", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-cobol.git", testcaseBranchVersion)
								break
							case 'testcases.ndt.NatclipseAssembledPreviewTest':
								gitUtils.checkoutGitProject("${workspace}\\TESTCASE_SUBSTITUTIONSREFERENZEN", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-substitutionsreferenzen.git", testcaseBranchVersion)
								break
							case 'testcases.base.EclipseViewForExpertScriptsTest':
								gitUtils.checkoutGitProject("${eclipseWorkspace}\\${testcase}\\expert-view-demo", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/base/expert-view-demo.git", 'master')
								break
							case 'testcases.base.GeneralLicensingTest':
							    gitUtils.checkoutGitProject("${workspace}\\licenses", "${gitUtils.getGitUrlQef()}/infrastructure/licenses.git", 'master')
								gitUtils.checkoutGitProject("${workspace}\\Nat2Java", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/nat2java.git", 'master')
								copyMigrationJarsToFolder("${workspace}/lib", mxBuildVersion)
								dir("${workspace}/scriptActions"){
									spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "natural-${mxBuildVersion}.script-actions", "${workspace}/scriptActions")	
									}
								gitUtils.checkoutGitProject("${workspace}\\TESTS_QS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/tests-qs.git", testcaseBranchVersion)	
								break
							case 'testcases.base.GeneralLicensingSmokeTest':
							    gitUtils.checkoutGitProject("${workspace}\\licenses", "${gitUtils.getGitUrlQef()}/infrastructure/licenses.git", testcaseBranchVersion)
								gitUtils.checkoutGitProject("${workspace}\\Nat2Java", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/nat2java.git", 'master')
								gitUtils.checkoutGitProject("${workspace}\\jcl-test", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mee/jcl-test.git", testcaseBranchVersion)
								checkoutGitProject("${workspace}\\MaxensoExample-vaadin", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/mdd/maxenso-example/maxenso-example-vaadin.git", testcaseBranchVersion)
								checkoutGitProject("${workspace}\\TestcaseCode-Completion", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-code-completion.git", testcaseBranchVersion)
								copyInnoWakeRuntimeJarsToFolder("${workspace}/libs", mxBuildVersion)
								copyMigrationJarsToFolder("${workspace}/lib", mxBuildVersion)
								dir("${workspace}/scriptActions"){
									spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "natural-${mxBuildVersion}.script-actions", "${workspace}/scriptActions")	
									}
								gitUtils.checkoutGitProject("${workspace}\\TESTS_QS", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/tests-qs.git", testcaseBranchVersion)	
								break	
							case 'testcases.base.FieldTracerTest':
								gitUtils.checkoutGitProject("${workspace}\\fieldtracing", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/base/fieldtracing.git", testcaseBranchVersion)
								break
							case 'testcases.base.EclipseInstallToolTest':
								eclipseRequired = false
								dir("${workspace}\\innowake-required-plugins") {
									spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'innowake-all-plugins.zip', './')
									spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'innowake-required-plugins.zip', './')
								}
								dir(workspace) {
									spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'install-tool.zip', './')
									unstash 'eclipse-dir'
								}
								gitUtils.getLicenseFile(mxVersion, workspace)
								break
							case 'testcases.sc.ControlCenterSCTest':
								dir("${workspace}/sc") {
									spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'soa-server.zip', "${workspace}/sc")
									spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'control-center.zip', "${workspace}/sc")
									spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'workbench.zip', "${workspace}/sc")
									spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'termx-emulator.zip', "${workspace}/sc")
								}
								gitUtils.getLicenseFile(mxVersion, "${workspace}\\sc\\workbench")
								gitUtils.getLicenseFile(mxVersion, "${workspace}\\sc\\soa-server")
								dir("${workspace}/batfile") {
									bat "echo 'start cmd' > startCMD.bat"
								}
								break
							case 'testcases.sc.SoaConnectorDataStructureTest':
								gitUtils.checkoutGitProject("${workspace}\\qs-project", "${gitUtils.getGitUrlQef()}/innowake-test-projects/legacy-devops-test-projects/sc/soa-connector-open-system", testcaseBranchVersion)
								dir("${workspace}/sc") {
									spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'soa-server.zip', "${workspace}/sc")
									spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'control-center.zip', "${workspace}/sc")
								}
								gitUtils.getLicenseFile(mxVersion, "${workspace}\\sc\\workbench")
								gitUtils.getLicenseFile(mxVersion, "${workspace}\\sc\\soa-server")
								copyInnowakeEmfineBrokerJarToFolder("${workspace}/lib", mxBuildVersion)
								dir("${workspace}/batfile") {
									bat "echo 'start cmd' > startCMD.bat"
								}
								withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
									dir("${workspace}\\qs-project") {
										bat "mvn dependency:resolve"
									}
								}
								break
							default:
								break
						}
						if (eclipseRequired) {
						//Pushd sets the working dir for the .bat scope, so that eclipse uses the correct eclipse.ini file
						def startEclipseCmd = "Pushd ${eclipseFolderpath}\nstart ${eclipseExe} -data ${workspace}"
						// The command is written to a bat file that enables us to start the eclipse on a server with a test case specific 									workspace 
						writeFile file: "${jobWorkspace}\\openWorkspace_${testcase}.bat", text: startEclipseCmd
						bat startEclipseCmd
					}

						echo "run test ${testcase}"
						withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
							dir(testProject) {
								bat "mvn clean -DscEnvironmentHost=${scEnvironmentHost} -DsplashScreenVersion=\"${splashScreenVersion}\" -DmxBuildVersion=${mxBuildVersion} -DmxJarDirectory=${workspace}\\lib -DworkspaceDir=${workspace} -DjobWorkspaceDir=${jobWorkspace} -Dm2Repo=E:/m2_repo/repository -DsettingsXml=E:\\m2_repo\\settings.xml -DjavaVersion=${javaVersion} -Dtest=${testCase} test"
							}
						}
					}
				}
			} catch (e) {
				unstable e.toString()
			}
		}

		if (testcase in oracleRequiredTestcases) {
			echo '------Stopping Oracle Environment------'
			build job: 'LD_Docker_Stop_Oracle_Env',
			parameters: [
				[$class: 'NodeParameterValue', name: 'executeOn', labels: ['USLinux1'], nodeEligibility: [$class: 'AllNodeEligibility']]
			]
		}

		stage('Archive run results') {
			dir(testProject) {
				archiveArtifacts "${runResultsDir}/**/*"
				publishHTML allowMissing: false, alwaysLinkToLastBuild: true, keepAll: true, reportDir: runResultsDir, reportFiles: 'runresults.html', reportName: testcase, reportTitles: "#${env.BUILD_ID} - ${mxBuildVersion}: ${testcase}"
			}
		}
	}
}

/**
 * method to copy the innowake runtime JARs to a specific folder inside the Jenkins workspace in the servers
 *
 * @param folder The folder where the JARs will be copied to
 * @param mxBuildVersion The innowake/maxenso build to be tested
 */
def copyInnoWakeRuntimeJarsToFolder(folder, mxBuildVersion) {
	def mxVersionUtils = new MxVersionUtils()
	def artifactPrefix = 'innowake'
	def groupId = 'innowake.bundle'
	if (mxVersionUtils.hasMaxensoInArtifactName(mxBuildVersion)) {
		artifactPrefix = 'innowake-maxenso'
		groupId = 'innowake.maxenso'
	}
	withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}-custom-default:${mxBuildVersion} -DoutputDirectory=${folder}"
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}-deps-general:${mxBuildVersion} -DoutputDirectory=${folder}"
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}-deps-hibernate:${mxBuildVersion} -DoutputDirectory=${folder}"
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}-deps-jasper:${mxBuildVersion} -DoutputDirectory=${folder}"
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}-deps-jta:${mxBuildVersion} -DoutputDirectory=${folder}"
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}-deps-logging:${mxBuildVersion} -DoutputDirectory=${folder}"
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}-deps-test:${mxBuildVersion} -DoutputDirectory=${folder}"
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}-deps-vaadin:${mxBuildVersion} -DoutputDirectory=${folder}"
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}-deps-vaadin-push:${mxBuildVersion} -DoutputDirectory=${folder}"
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}-runtime-dist:${mxBuildVersion} -DoutputDirectory=${folder}"
	}
	dir(folder) {
		bat "ren ${artifactPrefix}-custom-default-${mxBuildVersion}.jar ${artifactPrefix}-custom-default.jar"
		bat "ren ${artifactPrefix}-deps-hibernate-${mxBuildVersion}.jar ${artifactPrefix}-deps-hibernate.jar"
		bat "ren ${artifactPrefix}-deps-general-${mxBuildVersion}.jar ${artifactPrefix}-deps-general.jar"
		bat "ren ${artifactPrefix}-deps-jasper-${mxBuildVersion}.jar ${artifactPrefix}-deps-jasper.jar"
		bat "ren ${artifactPrefix}-deps-jta-${mxBuildVersion}.jar ${artifactPrefix}-deps-jta.jar"
		bat "ren ${artifactPrefix}-deps-logging-${mxBuildVersion}.jar ${artifactPrefix}-deps-logging.jar"
		bat "ren ${artifactPrefix}-deps-test-${mxBuildVersion}.jar ${artifactPrefix}-deps-test.jar"
		bat "ren ${artifactPrefix}-deps-vaadin-push-${mxBuildVersion}.jar ${artifactPrefix}-deps-vaadin-push.jar"
		bat "ren ${artifactPrefix}-deps-vaadin-${mxBuildVersion}.jar ${artifactPrefix}-deps-vaadin.jar"
		bat "ren ${artifactPrefix}-runtime-dist-${mxBuildVersion}.jar ${artifactPrefix}-runtime-dist.jar"
	}
}

/**
 * method to copy the expert mee-source-migration JARs to a specific folder inside the Jenkins workspace in the servers
 *
 * @param folder The folder where the JARs will be copied to
 * @param mxBuildVersion The innowake/maxenso build to be tested
 */
def copyMigrationJarsToFolder(folder, mxBuildVersion) {
	def mxVersionUtils = new MxVersionUtils()
	def artifactPrefix = 'mee-source-migration'
	def groupId = 'innowake.products.mee.source.migration'

	withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}-dist:${mxBuildVersion} -DoutputDirectory=${folder}"
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}-deps-general:${mxBuildVersion} -DoutputDirectory=${folder}"
	}
	dir(folder) {
		bat "ren ${artifactPrefix}-dist-${mxBuildVersion}.jar ${artifactPrefix}-dist.jar"
		bat "ren ${artifactPrefix}-deps-general-${mxBuildVersion}.jar ${artifactPrefix}-deps-general.jar"
	}
}

/**
 * method to copy innowake-mee-udf-oracle5 JAR to a specific folder inside the Jenkins workspace in the servers
 *
 * @param folder The folder where the JAR will be copied to
 * @param mxBuildVersion The innowake/maxenso build to be tested
 */
def copyInnowakeMeeUDFOracle5JarToFolder(folder, mxBuildVersion){
	def artifactPrefix = 'innowake-mee-udf-oracle5'
	def groupId = 'innowake.products.mee.runtime.natural.datastore' 
	withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}:${mxBuildVersion} -DoutputDirectory=${folder}"
	}
	dir(folder) {
		bat "ren ${artifactPrefix}-${mxBuildVersion}.jar ${artifactPrefix}.jar"
	}
}

/**
 * method to copy emfine-broker-java-dist JAR to a specific folder inside the Jenkins workspace in the servers
 *
 * @param folder The folder where the JAR will be copied to
 * @param mxBuildVersion The innowake/maxenso build to be tested
 */
def copyInnowakeEmfineBrokerJarToFolder(folder, mxBuildVersion){
	def artifactPrefix = 'emfine-broker-java-dist'
	def groupId = 'innowake.products.emfine.broker.java' 
	withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
		bat "mvn dependency:copy -Dartifact=${groupId}:${artifactPrefix}:${mxBuildVersion} -DoutputDirectory=${folder}"
	}
	dir(folder) {
		bat "ren ${artifactPrefix}-${mxBuildVersion}.jar ${artifactPrefix}.jar"
	}
}

/**
 * method to checkout projects from SVN
 *
 * @param localFolder The workspace where the project will be checked out in the servers
 * @param localProjectName The folder name where the project will be checked out inside the Jenkins workspace in the servers
 * @param remoteProjectLocation The name of the project in SVN
 */
def checkoutProject(localFolder, localProjectName, remoteProjectLocation) {
	dir("${localFolder}/${localProjectName}") {
		withCredentials([
			usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'svnPw', usernameVariable: 'svnUser')
		]) {
			bat "svn --no-auth-cache --username ${svnUser} --password ${svnPw} --depth infinity --force export ${remoteProjectLocation} ."
		}
	}
}

/**
 * method to checkout projects from SVN with revision control
 *
 * @param localFolder The workspace where the project will be checked out in the servers
 * @param localProjectName The folder name where the project will be checked out inside the Jenkins workspace in the servers
 * @param remoteProjectLocation The name of the project in SVN
 */
def checkoutProjectWithVersionControl(localFolder, localProjectName, remoteProjectLocation) {
	dir("${localFolder}/${localProjectName}") {
		withCredentials([
			usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'svnPw', usernameVariable: 'svnUser')
		]) {
			bat "svn --no-auth-cache --username ${svnUser} --password ${svnPw} --depth infinity --force checkout ${remoteProjectLocation} ."
		}
	}
}

/**
 * method to checkout projects from Gitlab
 *
 * @param localFolder The folder name where the project will be checked out inside the Jenkins workspace in the servers
 * @param url The URL of the Gitlab repository of the test project
 * @param branch The branch which will be checked out
 */
def checkoutGitProject(localFolder, url, branch) {
	dir(localFolder) {
		git branch: branch, credentialsId: 'USAppModQMUserSVC-User-PW', url: url
	}
}
