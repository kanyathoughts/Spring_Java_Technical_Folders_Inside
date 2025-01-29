/**
 * Run the mining - plugin version handshake test (WQST-590)
 * 
 * @param mxBuildVersion The innowake version to test.
 * @param executeOn The Jenkins node the test will run on.
 */
@Library('TestUtils') _

node(executeOn) {
	def gitUtils = new GitUtils()
	def spUtils = new SharepointUtils()
	def mxVersionUtils = new MxVersionUtils()
	def eclipseVersion = mxBuildVersion
	def jobWorkspace = pwd()
	def eclipseDir = "${jobWorkspace}\\eclipse-windows"
	def testProject = 'leanft'
	def miningArtefactsDir = 'mining-artifacts'
	def runResultsDir = 'RunResults'
	def testCase = 'testcases.mining.plugin.VersionHandshakeTest'
	def miningServerURL = 'http://localhost:8080'
	buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	buildDescription "mxBuildVersion=${mxBuildVersion} executeOn=${executeOn}"
	
	def versionPattern = ~/(.*-20\d+)(.*)/
	def versionMatch = (mxBuildVersion =~ versionPattern)
	if (versionMatch && versionMatch[0][2].contains('-')) {
		eclipseVersion = mxBuildVersion.substring(0, mxBuildVersion.lastIndexOf('-'))
	}
	versionMatch = null
	
	stage('checkout test project') {
		deleteDir()
		def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/ui-test-projects/leanft.git"
		dir(testProject) {
			git branch: 'master', credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
		}
	}
	
	stage ('checkout mining artifacts') {
		def gitlabProject = "${gitUtils.getGitUrlQef()}/infrastructure/mining-test-artefacts.git"
		dir(miningArtefactsDir) {
			git branch: 'master', credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
		}
	}
	
	stage ('start database') {
		dir(miningArtefactsDir) {
			unzip zipFile: 'orientdb-mining-21.1.0-alpha-202103180543.zip'
			bat "start /min bin\\server.bat"
			sleep 30
		}
	}
	
	stage ('start api-server') {
		dir(miningArtefactsDir) {
			bat "start /min \"Mining-Api-Server\" java -jar mining-api-server-dist-21.1.0-alpha-202103180543.jar"
			sleep 30
		}
	}
	
	stage('copy eclipse') {
		echo 'Copy eclipse bundle from SharePoint'
		spUtils.downloadIwEclipseWindows(mxBuildVersion, eclipseDir)
	}
	
	stage ('run test') {
		def eclipseExe = "${eclipseDir}\\eclipse.exe"
		def eclipseWorkspace = "${jobWorkspace}\\workspace"
		try {
			timeout(time: 1, unit: 'HOURS') {
				def workspace = "${eclipseWorkspace}\\${testCase}"
				dir(workspace) {
					deleteDir()
				}
				dir(runResultsDir) {
					deleteDir()
				}
				// Get fresh license before starting a test
				dir(eclipseDir) {
					bat 'erase /f innowake.lic'
					gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), '.')
					bat "rename maxenso.lic innowake.lic"
				}
				def startEclipseCmd = "start ${eclipseExe} -data ${workspace}"
				// The command is written to a bat file that enables us to start the eclipse on a server with a test case specific workspace 
				writeFile file: "${jobWorkspace}\\openWorkspace_${testCase}.bat", text: startEclipseCmd
				bat startEclipseCmd
				echo "run test ${testCase}"
				withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
					dir(testProject) {
						bat "mvn clean -DmxBuildVersion=${eclipseVersion} -DworkspaceDir=${workspace} -DminingServerURL=${miningServerURL} -Dtest=${testCase} test"
					}
				}
			}
		} catch (e) {
			unstable e.toString()
		}
	}
	
	stage ('archive run results') {
		dir(testProject) {
			archiveArtifacts allowEmptyArchive: true, artifacts: "${runResultsDir}/**/*"
			publishHTML allowMissing: true, alwaysLinkToLastBuild: true, keepAll: true, reportDir: runResultsDir, reportFiles: 'runresults.html', reportName: testCase, reportTitles: "#${env.BUILD_ID} - 	${mxBuildVersion}: ${testCase}"
		}
	}
	
	stage ('stop api-server') {
		bat "taskkill /F /FI \"WINDOWTITLE eq Mining-Api-Server\" /T"
	}
	
	stage ('stop database') {
		dir(miningArtefactsDir) {
			bat 'bin\\shutdown.bat'
		}
	}
}