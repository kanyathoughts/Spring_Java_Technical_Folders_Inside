/**
 * Compile and run all automated LeanFT test cases for mining 
 * 
 * @param mxBuildVersion The maxenso/innowake build to test.
 * @param testcase  Name including the package of the test you want to run.
 * @param reuse_workspace If true project checkout and copy of the eclipse is skipped and the previous artifacts of project and eclipse are used.
 * @param executeOn The Jenkins node the test will run on
 * @param miningServerURL URL where the mining environment is running that will be used
 * @param runWithoutKeycloak flag for the smoketest, if it shall run with or without keycloak
 * @param useDifferentTestProjectBranch By default the branch of the LeanFT project is checked out based on the selected mxVersion. 
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param useDifferentDiscoveryJumpstartProjectBranch By default the branch of the LeanFT project is checked out based on the selected mxVersion. 
 * 		If useDifferentDiscoveryJumpstartProjectBranch is set to true the branch is overridden by the parameter differentDiscoveryJumpstartProjectBranch
 * @param differentDiscoveryJumpstartProjectBranch The branch that will be used if useDifferentDiscoveryJumpstartProjectBranch is set.
 */
@Library('TestUtils') _

node(executeOn) {

	timestamps {
		
		def svnUtils = new SvnUtils()
		def gitUtils = new GitUtils()
		def spUtils = new SharepointUtils()
		def svnUrlQm = svnUtils.getSvnUrlQm()
		def mxVersionUtils = new MxVersionUtils()
		def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
		def eclipseVersion = mxBuildVersion
		def jobWorkspace = pwd()
		def eclipseRequired = true
		def testProject = 'EclipseTestProject'
		def eclipseDir = "${jobWorkspace}\\eclipse-windows"
		def eclipseExe = "${eclipseDir}\\eclipse.exe"
		def eclipseWorkspace = "${jobWorkspace}\\workspace"
		def eclipseFolderpath = "${jobWorkspace}\\eclipse-windows"
		def runResultsDir = 'RunResults'
		def testProjectBranch
		def featureToEnable
		def switchOnOrOff
		def miningFeatureToggleJobExecuted = false
		def metaDataTestCases = [
			'testcases.mining.webui.ConsistentAnnotationEditorTest',
			'testcases.mining.webui.CustomPropertiesTest',
			'testcases.mining.webui.AnnotationTaggingTest',
	    	'testcases.mining.webui.EclipseDeeplinkingTest',
			'testcases.mining.webui.ModuleDetailsTest',
			'testcases.mining.webui.ModifiedModulesTest',
			'testcases.mining.webui.EditDescriptionsTest',
			'testcases.mining.webui.SwaggerAPIAndAnnotationUITest',
			'testcases.mining.webui.CodeViewerTest',
			'testcases.mining.webui.MetricsUIUtilitiesInterfacesSqlTest',
			'testcases.mining.webui.MetricsUIDecompositionCodeQualityCandidatesTest',
			'testcases.mining.webui.UserCustomizableModuleTableTest',
			'testcases.mining.webui.UserCustomizableAnnotationTableTest',
			'testcases.mining.webui.UserCustomizableDataDictionaryTableTest',
			'testcases.mining.webui.DNAVisualizationTest',
			'testcases.mining.webui.SharedTaxonomyEditorTest',
			'testcases.mining.webui.TaxonomyConfigurationTest',
			'testcases.mining.webui.SharedModuleDetailsViewTest',
			'testcases.mining.webui.MetricsUITaxonomyFilterTest',
			'testcases.mining.webui.JobListTest',
			'testcases.mining.plugin.SmokeTest',
			'testcases.mining.plugin.AnnotationCobolclipseBMSTest',
			'testcases.mining.plugin.AnnotationsCobolSourceEditorTest',
			'testcases.mining.plugin.AnnotationsEclipseJavaEditorTest',
			'testcases.mining.plugin.AnnotationsMeeclipseJavaEditorTest',
			'testcases.mining.plugin.AnnotationBatchclipseBatchEditorTest',
			'testcases.mining.plugin.AnnotationsNaturalSourceEditorTest',
			'testcases.mining.plugin.DataDictionaryCobolTest',
			'testcases.mining.plugin.DataDictionaryNaturalTest',
			'testcases.mining.plugin.IdentifyCandidatesTest',
			'testcases.mining.plugin.IdentifyBusinessRuleCandidatesTest',
			'testcases.mining.plugin.IdentifyBusinessVariablesTest',
			'testcases.mining.plugin.TaxonomyTest',
			'testcases.discovery.IncrementalSourceCodeUploadTest',
			'testcases.discovery.UnchangedSourceFilesTest'
		]
		
		if (Boolean.parseBoolean(useDifferentTestProjectBranch)) {
			testProjectBranch = differentTestProjectBranch
		} else if (mxVersion == '99.9') {
			testProjectBranch = 'master'
		} else {
			testProjectBranch = mxVersion
		}
		if (Boolean.parseBoolean(useDifferentDiscoveryJumpstartProjectBranch)) {
			discoveryJumpstartProjectBranch = differentDiscoveryJumpstartProjectBranch
		} else {
			discoveryJumpstartProjectBranch = 'master'
		}
		reuse_workspace = Boolean.parseBoolean(reuse_workspace)
		
		buildName "#${env.BUILD_ID} - ${testcase} - ${mxBuildVersion}"
		buildDescription "mxBuildVersion=${mxBuildVersion} testcase=${testcase} reuse_workspace=${reuse_workspace} executeOn=${executeOn} miningServerURL=${miningServerURL} runWithoutKeycloak=${runWithoutKeycloak} testProjectBranch=${testProjectBranch}"
		
		def versionPattern = ~/(.*-20\d+)(.*)/
		def versionMatch = (mxBuildVersion =~ versionPattern)
		if (versionMatch && versionMatch[0][2].contains('-')) {
			eclipseVersion = mxBuildVersion.substring(0, mxBuildVersion.lastIndexOf('-'))
		}
		versionMatch = null
		
		stage('Checkout test project') {
			if (reuse_workspace) {
				echo 'Skipping "Checkout test project"'
			} else {
				deleteDir()
				def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/ui-test-projects/leanft-dm.git"
				dir(testProject) {
					git branch: testProjectBranch, credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
				}
			}
		}
		
		stage('Copy innowake eclipse') {
			if (reuse_workspace || testcase == 'testcases.discovery.UICSVImportTest'
					|| testcase ==  'testcases.mining.webui.ClientProjectManagementTest'
					|| testcase == 'testcases.mining.webui.DBCutterExportExtensionTest') {
				echo 'Skipping "Copy maxenso/innowake eclipse"'
			} else {
				echo 'Copy eclipse bundle from SharePoint'
				spUtils.downloadIwEclipseWindows(mxBuildVersion, eclipseDir)
			}
		}
		
		stage('Run test') {
			try {
				timeout(time: 4, unit: 'HOURS') {
					def workspace = "${eclipseWorkspace}\\${testcase}"
					dir(workspace) {
						deleteDir()
					}
					dir(runResultsDir) {
						deleteDir()
					}
					// Get fresh license before starting a test
					dir(eclipseDir) {
						bat 'erase /f innowake.lic'						
						gitUtils.getSingleFileOnWindows('infrastructure/licenses', 'mining/innowake.lic', mxVersion, '.')
					}
					switch (testcase) {
						case 'testcases.mining.plugin.LicensingCobolclipseTest':
							runEclipseWithEnableAllLicense(workspace, eclipseExe, eclipseVersion, testProject, runResultsDir)
							prepareLicense(eclipseDir, 'cobolclipse.lic', mxVersion)
							break
						case 'testcases.mining.plugin.LicensingMiningCobolclipseTest':
							runEclipseWithEnableAllLicense(workspace, eclipseExe, eclipseVersion, testProject, runResultsDir)
							prepareLicense(eclipseDir, 'cobolclipse-mining.lic', mxVersion)
							break
						case 'testcases.mining.plugin.LicensingMiningWithTrunkVersionTest':
							runEclipseWithEnableAllLicense(workspace, eclipseExe, eclipseVersion, testProject, runResultsDir)
							prepareLicense(eclipseDir, 'mining-with-trunk-version.lic', mxVersion)
							break
						case 'testcases.mining.plugin.LicensingMiningWrongVersionTest':
							runEclipseWithEnableAllLicense(workspace, eclipseExe, eclipseVersion, testProject, runResultsDir)
							prepareLicense(eclipseDir, 'mining-wrong-version.lic', mxVersion)
							break
						case 'testcases.mining.plugin.LicensingWithoutLicenseTest':
							runEclipseWithEnableAllLicense(workspace, eclipseExe, eclipseVersion, testProject, runResultsDir)
							bat "erase /f ${eclipseDir}\\innowake.lic"
							break
						case 'testcases.mining.webui.LargeDependencyGraphTest':
							cloneProject("${eclipseWorkspace}\\${testcase}\\txdmv_discovered", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/txdmv_discovered.git", 'master')
							break
						case 'testcases.mining.webui.CallChainCustomConfigTest':
							cloneProject("${eclipseWorkspace}\\${testcase}\\discovery-jumpstart", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/discovery-jumpstart.git", discoveryJumpstartProjectBranch)
							cloneProject("${eclipseWorkspace}\\${testcase}\\mining-custom-config", "${gitUtils.getGitUrlQef()}/infrastructure/mining-custom-config.git", 'master')
							break
						case 'testcases.mining.plugin.LargeSourceCodeFileUploadTest':
							cloneProject("${eclipseWorkspace}\\${testcase}\\large-files-project", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/large-files-project.git", 'master')
							break
						case 'testcases.mining.plugin.ImportDiscoveryExpertExcelTest':
						case 'testcases.mining.plugin.VersionHandshakeTest':
							cloneProject("${eclipseWorkspace}\\${testcase}\\DemoDez18", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/demodez18.git", 'master')
							break
						case 'testcases.mining.plugin.IdentifyBusinessRuleCandidatesTest':
							cloneProject("${eclipseWorkspace}\\${testcase}\\discovery-jumpstart", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/discovery-jumpstart.git", discoveryJumpstartProjectBranch)
							featureToEnable = ['stageTwoRuleIdentification', 'improvedBusinessRuleIdentification']
							miningFeatureToggle(featureToEnable, 'On')
							miningFeatureToggleJobExecuted = true
							break
						case 'testcases.discovery.TxDMVTest':
							cloneProject("${eclipseWorkspace}\\${testcase}\\txdmv-test-project", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/txdmv-test-project.git", 'master')
							break	
						case 'testcases.discovery.CancelSourceUploadTest':
							cloneProject("${eclipseWorkspace}\\${testcase}\\txdmv-test-project", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/txdmv-test-project.git", 'master')
							cloneProject("${eclipseWorkspace}\\${testcase}\\discovery-jumpstart", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/discovery-jumpstart.git", discoveryJumpstartProjectBranch)
							break
						case 'testcases.discovery.IncrementalModulesTest':
						case 'testcases.discovery.IncrementalSourceCodeUploadTest':
							// only trigger feature toggle for non-AWS environment
							if (! isCausedByAWS()) {
								featureToEnable = ['incrementalScan']
								miningFeatureToggle(featureToEnable, 'On')
								miningFeatureToggleJobExecuted = true
							}
							cloneProject("${eclipseWorkspace}\\${testcase}\\discovery-jumpstart", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/discovery-jumpstart.git", discoveryJumpstartProjectBranch)
							break
						case 'testcases.discovery.UnchangedUndiscoveredEntitesTest':
							cloneProject("${eclipseWorkspace}\\${testcase}\\discovery-jumpstart", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/discovery-jumpstart.git", discoveryJumpstartProjectBranch)
							try {
								bat "robocopy ${eclipseWorkspace}\\${testcase} ${eclipseWorkspace}\\${testcase}-copy /s /e"
							} catch (Exception e){
								echo e.toString()
							}
							break
						case 'testcases.discovery.UICSVImportTest':						
						case 'testcases.mining.webui.ModuleSearchTest':
						case 'testcases.mining.webui.DBCutterExportExtensionTest':
						case 'testcases.mining.webui.JobListTest':
							eclipseRequired = false
							break
						case 'testcases.mining.webui.EclipseDeeplinkingTest':
						case 'testcases.mining.webui.SharedModuleDetailsViewTest':
							featureToEnable = ['eclipseDeepLink']
							miningFeatureToggle(featureToEnable, 'On')
							miningFeatureToggleJobExecuted = true
							cloneProject("${eclipseWorkspace}\\${testcase}\\discovery-jumpstart", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/discovery-jumpstart.git", discoveryJumpstartProjectBranch)
							break
						case 'testcases.mining.webui.UserCustomizableAnnotationTableTest':
							featureToEnable = ['improvedBusinessRuleIdentification']
							miningFeatureToggle(featureToEnable, 'On')
							miningFeatureToggleJobExecuted = true
							cloneProject("${eclipseWorkspace}\\${testcase}\\discovery-jumpstart", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/discovery-jumpstart.git", discoveryJumpstartProjectBranch)
							break
						case 'testcases.mining.webui.ControlFlowGraphJCLTest':
							featureToEnable = ['controlFlowGraphJclSidePanel']
							miningFeatureToggle(featureToEnable, 'On')
							miningFeatureToggleJobExecuted = true
							cloneProject("${eclipseWorkspace}\\${testcase}\\discovery-jumpstart", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/discovery-jumpstart.git", discoveryJumpstartProjectBranch)
							break
						case 'testcases.mining.performance.QATestprojectsDiscoveryTest':
							cloneProject("${eclipseWorkspace}\\${testcase}\\QATestprojects", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/QATestprojects.git", discoveryJumpstartProjectBranch)
							break
						default:
							cloneProject("${eclipseWorkspace}\\${testcase}\\discovery-jumpstart", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/discovery-jumpstart.git", discoveryJumpstartProjectBranch)
							break
					}
					if (eclipseRequired) {
						// solves WQM-4506: Pushd sets the working dir for the .bat scope, so that eclipse uses the correct eclipse.ini file
						def startEclipseCmd = "Pushd ${eclipseFolderpath}\nstart ${eclipseExe} -data ${workspace}"
						// The command is written to a bat file that enables us to start the eclipse on a server with a test case specific workspace 
						writeFile file: "${jobWorkspace}\\openWorkspace_${testcase}.bat", text: startEclipseCmd
						bat startEclipseCmd
					}
					echo "run test ${testcase}"
					withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
						dir(testProject) {
							bat "mvn clean -DmxBuildVersion=${eclipseVersion} -DworkspaceDir=${workspace} -DminingServerURL=${miningServerURL} -DrunWithoutKeycloak=${runWithoutKeycloak} -Dtest=${testCase} test"
						}
					}
					if (miningFeatureToggleJobExecuted) {
						miningFeatureToggle(featureToEnable, 'Off')
					}
				}
			} catch (e) {
				unstable e.toString()
			}
		}
		
		stage('Archive run results') {
			dir(testProject) {
				archiveArtifacts allowEmptyArchive: true, artifacts: "${runResultsDir}/**/*"
				publishHTML allowMissing: true, alwaysLinkToLastBuild: true, keepAll: true, reportDir: runResultsDir, reportFiles: 'runresults.html', reportName: testcase, reportTitles: "#${env.BUILD_ID} - 	${mxBuildVersion}: ${testcase}"	
			}
			if (testcase == 'testcases.discovery.MinificationTest') {
				archiveArtifacts allowEmptyArchive: true, artifacts: 'workspace\\testcases.discovery.MinificationTest\\discovery-jumpstart\\discover-code-*'
				archiveArtifacts allowEmptyArchive: true, artifacts: 'workspace\\testcases.discovery.MinificationTest\\discovery-jumpstart\\discover-metrics-*'
				archiveArtifacts allowEmptyArchive: true, artifacts: 'workspace\\testcases.discovery.MinificationTest\\discovery-jumpstart\\discovery_*'
			} else if (testcase == 'testcases.discovery.TxDMVTest') {
				archiveArtifacts allowEmptyArchive: true, artifacts: 'workspace\\testcases.discovery.TxDMVTest\\txdmv-test-project\\discover-code-*'
				archiveArtifacts allowEmptyArchive: true, artifacts: 'workspace\\testcases.discovery.TxDMVTest\\txdmv-test-project\\discover-metrics-*'
				archiveArtifacts allowEmptyArchive: true, artifacts: 'workspace\\testcases.discovery.TxDMVTest\\txdmv-test-project\\discovery_*'
			} 
		}
		
		if (metaDataTestCases.contains(testcase) && ! isCausedByAWS()) {
			stage('trigger DM_Docker_Restore_DB_Backup') {
				def miningServer = getMiningServer(miningServerURL)
				build job: 'DM_Docker_Restore_DB_Backup', parameters: [ booleanParam(name: 'withMetadata', value: true),
				[$class: 'NodeParameterValue', name: 'executeOn', labels: [miningServer], nodeEligibility: [$class: 'AllNodeEligibility']]]
			}
		}
	}
}

/**
 * Returns the mining server of a given mining server URL. 
 * 
 * @param miningServerURL The URL to the mining server.
 * @return miningServer The server name from the given URL.
 */
def getMiningServer(miningServerURL) {
	switch(miningServerURL) {
		case 'http://qef-linux1-us.deloitte.com:8080/':
			return 'USLinux1'
		case 'http://qef-linux3.deloitte.com:8080/':
			return 'USLinux3'
		case 'http://qef-linux5-us-dm.deloitte.com:8080/':
			return 'USLinux5-DM'
		case 'http://qef-linux6-us-dm.deloitte.com:8080/':
			return 'USLinux6-DM'
		case 'http://qef-linux7-us-dm.deloitte.com:8080/':
			return 'USLinux7-DM'
		case 'http://qef-linux8-us-dm.deloitte.com:8080/':
			return 'USLinux8-DM'
	}
}

/**
 * When starting the eclipse without an enable-all license on the server an exception occurs.
 * To avoid this exception the method runEclipseWithEnableAllLicense needs to be called first. 
 * The method will start the eclipse with an enable-all license and closes it after a short test.
 * Afterwards it is possible to launch the eclipse without a license or a license that is not an enable-all license.
 * 
 * @param workspace Workspace of the current test case
 * @param eclipseVersion The maxenso/innowake build to test
 * @param testProject Name of the test project
 * @param runResultsDir Name of the LeanFT run results folder
 */
def runEclipseWithEnableAllLicense(workspace, eclipseExe, eclipseVersion, testProject, runResultsDir) {
	bat returnStatus: true, script: "erase ${workspace}_temp"
	bat "start ${eclipseExe} -data ${workspace}_temp"
	withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
		dir(testProject) {
			bat "mvn -DmxBuildVersion=${eclipseVersion} -Dtest=testcases.mining.plugin.LicensingMiningAvailableTest test"
		}
	}
	dir("${testProject}\\${runResultsDir}") {
		deleteDir()
	}
}

/**
 * Delete the current innowake license, checkout the new one
 * rename it to innowake.lic
 * 
 * @param workingDirectory The directory with the license
 * @param licenseName The name of the license
 * @param mxVersion Innowake branch e.g. 21.0
 */
def prepareLicense(workingDirectory, licenseName, mxVersion) {
	def gitUtils = new GitUtils()
	dir(workingDirectory) {
		bat 'erase /f innowake.lic'
		gitUtils.getSingleFileOnWindows('infrastructure/licenses', "mining/${licenseName}", mxVersion, '.')
		bat "rename ${licenseName} innowake.lic"
	}
}

/**
 * Clones the specified project from git
 * 
 * @param localFolder The folder the project will be checked out to
 * @param url URL of the project repository
 * @param branch The branch to be cloned
 */
def cloneProject(localFolder, url, branch) {
	dir(localFolder) {
		git branch: branch, credentialsId: 'USAppModQMUserSVC-User-PW', url: url
	}
}

/**
 * Execute DM_Feature_Toggle Job to enable/disable a feature
 * 
 * @param feature            The feature that needs to be enabled/disable
 * @param switchOnOrOff      The On or Off action on the feature 
 */
def miningFeatureToggle(featureToEnable, switchOnOrOff) {
	def miningServer
	switch (miningServerURL) {
		case 'http://qef-linux1-us.deloitte.com:8080/':
			miningServer = 'USLinux1'
			break
		case 'http://qef-linux3.deloitte.com:8080/':
			miningServer = 'USLinux3'
			break
		case 'http://qef-linux5-us-dm.deloitte.com:8080/':
			miningServer = 'USLinux5-DM'
			break
		case 'http://qef-linux6-us-dm.deloitte.com:8080/':
			miningServer = 'USLinux6-DM'
			break
		case 'http://qef-linux7-us-dm.deloitte.com:8080/':
			miningServer = 'USLinux7-DM'
			break
		case 'http://qef-linux8-us-dm.deloitte.com:8080/':
			miningServer = 'USLinux8-DM'
			break
	}
	retry(count: 3) {
		sleep 10
		featureToEnable.each{
		feature->
		build job: 'DM_Feature_Toggle', parameters: [[$class: 'NodeParameterValue', name: 'executeOn', labels: [miningServer], nodeEligibility: [$class: 'AllNodeEligibility']], string(name: 'feature', value: "${feature}"), string(name: 'switchOnOrOff', value: "${switchOnOrOff}")]
		echo "${feature} feature switched ${switchOnOrOff} in ${miningServer}"
		}
	}
}

@NonCPS
def isCausedByAWS() {
	def upstream = currentBuild.rawBuild.getCause(hudson.model.Cause$UpstreamCause)
	return (upstream && upstream.toString().contains("DM_Tests_AWS_ASG"))
}
