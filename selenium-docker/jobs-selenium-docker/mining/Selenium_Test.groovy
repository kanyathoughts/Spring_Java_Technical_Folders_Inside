@Library('TestUtils') _

/**
 * Script to run a Selenium test case against the Innowake mining application
 *
 * @param executeOn The Jenkins node the test will run on
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

timestamps {
	def gitUtils = new GitUtils()
	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def featureToEnable
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

	node(executeOn) {
		def testProjectBranch = Boolean.parseBoolean(useDifferentTestProjectBranch) ? differentTestProjectBranch : 'docker-check'
		def webDriverPort = dockerUtils.getAvailablePort([
			'4444',
			'4445',
			'4446',
			'4447',
			'4448',
			'4449'
		])
		def user = miscUtils.getUserID()
		def group = miscUtils.getGroupID()
		if (webDriverPort == 'No Port Available') {
			error 'No free port for web driver available'
		}
		def workDir = pwd()
		def seleniumDockerRunParams = "-p ${webDriverPort}:4444 --shm-size='8g' -v ${workDir}/Downloads:/home/seluser/Downloads --add-host=host.docker.internal:host-gateway --name IW_DM-qa-test -e SCREEN_WIDTH=1936 -e SCREEN_HEIGHT=1078"
		def jenkinsEnvDockerRunParams = "-v jenkins-m2-repo:/var/m2_repo:rw --add-host=host.docker.internal:host-gateway -u=0"

		buildName "#${env.BUILD_ID} - ${testCase}"
		buildDescription "testcase=${testcase} testProjectBranch=${testProjectBranch} executeOn=${executeOn} mxBuildVersion=${mxBuildVersion} miningServerURL=${miningServerURL}"

		deleteDir()
		docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside('-u=0') {
			sh "mkdir Downloads"
			sh "groupadd -g 1201 seluser"
			sh "useradd seluser -u 1200 -g 1201"
			sh "chown seluser:seluser Downloads"
			sh "chmod 777 -R Downloads"
		}
		docker.image("selenium/standalone-chrome:98.0-chromedriver-98.0-grid-4.1.2-20220217").withRun(seleniumDockerRunParams) {
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside(jenkinsEnvDockerRunParams) {
				stage('init') {
					def remoteProjectLocation = 'innowake-test-projects/ui-test-projects/selenium_dm.git'
					gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
				}

				stage('test') {
					switch(testcase) {
						case 'testcases.webui.ExportTablesTest':
						featureToEnable=['enhancedGraphMlExport','confluenceExport']
						miningFeatureToggle(featureToEnable, 'On')
						miningFeatureToggleJobExecuted = true
						break
					}
					catchError(buildResult: 'UNSTABLE', stageResult: 'UNSTABLE') {
						withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
							sh "sed -i 's,test to run,${testCase},g' testng.xml"
							sh "$MVN_CMD -DminingServerURL=${miningServerURL} -DremoteWebDriver=true -DexecuteOn=${executeOn} -DmxBuildVersion=${mxBuildVersion} test"
						}
					}
					sh "chown -hR ${user}:${group} *"
					sh "chown -hR ${user}:${group} .settings .project .gitignore .classpath"
					if (miningFeatureToggleJobExecuted) {
						miningFeatureToggle(featureToEnable, 'Off')
					}
				}

				stage('report') {
					publishHTML allowMissing: false, alwaysLinkToLastBuild: true, keepAll: true,
					reportDir: 'report', reportFiles: 'index.html',
					reportName: testCase, reportTitles: "${testCase}"
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
