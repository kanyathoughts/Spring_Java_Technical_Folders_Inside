/**
 * Compile and run all automated selenium test cases for mining UI
 * 
 * @param mxBuildVersion The maxenso/innowake build to test.
 * @param testcase  Name including the package of the test you want to run.
 * @param reuse_workspace If true project checkout and copy of the eclipse is skipped and the previous artifacts of project and eclipse are used.
 * @param executeOn The Jenkins node the test will run on
 * @param miningServerURL URL where the mining environment is running that will be used
 * @param useDifferentTestProjectBranch By default, the master branch of selenium_dm test project is checkout. If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */
@Library('TestUtils') _

node(executeOn) {

	timestamps {
		def gitUtils = new GitUtils()
		def runReportDir = 'report'
		def jobWorkspace = pwd()
		def testProject = 'EclipseTestProject'
		def testProjectBranch
		def featureToEnable
		def switchOnOrOff
		def miningFeatureToggleJobExecuted = false
		def metaDataTestCases = [
		'testcases.webui.TaxonomyPropagationTest',
		'testcases.webui.UserCustomizableModuleTableTest',
		'testcases.webui.ExportTablesTest',
		'testcases.webui.BulkExecutionOfMiningActionsTest',
		'testcases.webui.GeneralTest',
		'testcases.webui.CustomDataDictionaryPropertiesTest',
		'testcases.webui.GraphFileTest',
		'testcases.webui.ModuleCustomPropertiesTest',		
		'testcases.webui.AnnotationCategoryTest',
		'testcases.webui.ClientProjectManagementTest',		
		'testcases.webui.ManageTaxonomiesTest',
		'testcases.iam.ProjectEditorTest',
		'testcases.iam.ProjectViewerTest',
		'testcases.webui.CallChainIgnoreModulesAndDataFlowTest'
		]
	
		if (Boolean.parseBoolean(useDifferentTestProjectBranch)) {
			testProjectBranch = differentTestProjectBranch
		} else {
			testProjectBranch = 'master'
		}
		reuse_workspace = Boolean.parseBoolean(reuse_workspace)
		
		buildName "#${env.BUILD_ID} - ${testcase}"
		buildDescription "mxBuildVersion=${mxBuildVersion} testcase=${testcase} reuse_workspace=${reuse_workspace} executeOn=${executeOn} miningServerURL=${miningServerURL} testProjectBranch=${testProjectBranch}"
		
		stage('Checkout test project') {
			if (reuse_workspace) {
				echo 'Skipping "Checkout test project"'
			} else {
				deleteDir()
				def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/ui-test-projects/selenium_dm.git"
				dir(testProject) {
					git branch: testProjectBranch, credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
				}
			}
		}
		
		stage('Run test') {
			switch(testcase) {
				case 'testcases.webui.ExportTablesTest':
					featureToEnable=['enhancedGraphMlExport','confluenceExport']
					miningFeatureToggle(featureToEnable, 'On')
					miningFeatureToggleJobExecuted = true
					break
			}
			echo "run test ${testcase}"
			withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
				dir(testProject) {
					bat "powershell -Command \"(gc testng.xml) -replace \'test to run\', \'${testcase}\' | Out-File -encoding ASCII testng.xml\""
					try {
						bat "mvn clean -DmxBuildVersion=${mxBuildVersion} -DexecuteOn=${executeOn} -DminingServerURL=${miningServerURL} test"
					} catch (e) {
						unstable e.toString()
					}
					finally {
						bat "powershell -Command \"(gc testng.xml) -replace \'${testcase}\', \'test to run\' | Out-File -encoding ASCII testng.xml\""
					}
				}
			}
			if (miningFeatureToggleJobExecuted) {
				miningFeatureToggle(featureToEnable, 'Off')
			}
		}

		stage('Archive run results') {
			dir(testProject) {
				archiveArtifacts "${runReportDir}/**/*"
				publishHTML allowMissing: false, alwaysLinkToLastBuild: true, keepAll: true, reportDir: runReportDir, reportFiles: 'index.html', reportName: testcase, reportTitles: "#${env.BUILD_ID}: ${testcase}"
			}
		}
		
		if (metaDataTestCases.contains(testcase)) {
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
