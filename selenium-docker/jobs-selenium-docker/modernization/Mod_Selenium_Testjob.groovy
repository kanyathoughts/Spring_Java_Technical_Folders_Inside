/**
 * Job script to compile and run all automated Selenium tests for db-cutter
 * @param buildTag The db cutter version/tag to use.
 * @param testcase Name including the package of the test you want to run.
 * @param executeOn The Jenkins node the test will run on
 * @param useDifferentTestProjectBranch By default the master branch of the selenium-testng project is checked out.
 * If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */
@Library('TestUtils') _

node(executeOn) {
	def gitUtils = new GitUtils()
	def miscUtils = new MiscUtils()
	def jobWorkspace = pwd()
	def testProject = 'DBCutterTestProject'
	def runReportDir = 'artifact/reports'
	def nmsloArtifactsDir = 'DBCutterTestProject/NMSLOArtifacts'
    def libertyMutualArtifactsDir = 'DBCutterTestProject/LibertyMutualArtifacts'
	def testProjectBranch

	if (Boolean.parseBoolean(useDifferentTestProjectBranch)) {
		testProjectBranch = differentTestProjectBranch
	} else {
		testProjectBranch = 'master'
	}

	buildName "#${env.BUILD_ID} - ${testcase}"
	buildDescription "buildTag=${buildTag} executeOn=${executeOn} executeTestAgainst=${executeTestAgainst} testcase=${testcase} testProjectBranch=${testProjectBranch}"

	stage('Checkout test project') {
		deleteDir()
		def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/ui-test-projects/selenium-testng.git"
		dir(testProject) {
			git branch: testProjectBranch, credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
		}
	}

	stage('Checkout Artifacts') {
		def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/modernization-test-projects/nmslo.git"
		dir(nmsloArtifactsDir) {
			git branch: 'master', credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
		}

        gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/modernization-test-projects/liberty-mutual.git"
		dir(libertyMutualArtifactsDir) {
			git branch: 'master', credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
		}
		
		if (testcase == 'testcases.dbcutter.SmokeTestCrawler') {
			dir(nmsloArtifactsDir) {
				copyArtifacts(projectName: 'Mod_Test_DB_Crawler', filter: '**/*step05.json')
			}
		}

		if (testcase == 'testcases.dbcutter.MiningExtensionsImportTest') {
            copyArtifacts(projectName: 'Mod_Selenium_Testjob', parameters: 'testcase=testcases.dbcutter.MiningExtensionsExportTest',  filter: '**/resources/mining-export.csv', target: nmsloArtifactsDir)
			copyArtifacts(projectName: 'Mod_Selenium_Testjob', parameters: 'testcase=testcases.dbcutter.MiningExtensionsExportTest',  filter: '**/resources/mining-module-relations.csv', target: nmsloArtifactsDir)
		} 
	}

	stage('Run test') {
		echo "run test ${testcase}"
		withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
			dir(testProject) {
				bat "powershell -Command \"(gc testng.xml) -replace \'test to run\', \'${testcase}\' | Out-File -encoding ASCII testng.xml\""
				bat "mvn clean -DbuildTag=${buildTag} -DexecuteOn=${executeOn} -DexecuteTestAgainst=${executeTestAgainst} test"
			}
		}
	}

	stage('Archive run results') {
		dir(testProject) {
			archiveArtifacts "${runReportDir}/**/*"
			archiveArtifacts "resources/**/*"
			publishHTML allowMissing: false, alwaysLinkToLastBuild: true, keepAll: true, reportDir: runReportDir, reportFiles: 'index.html', reportName: testcase, reportTitles: "#${env.BUILD_ID}: ${testcase}"
		}
		
	}
}
