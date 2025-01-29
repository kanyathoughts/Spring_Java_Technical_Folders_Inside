/**
 * Compile and run all automated LeanFT test cases for mining 
 * 
 * @param mxBuildVersion The innowake build to test.
 * @param testcase  Name including the package of the test you want to run.
 * @param reuse_workspace If true project checkout and copy of the eclipse is skipped and the previous artifacts of project and eclipse are used.
 * @param executeOn The Jenkins node the test will run on
 * @param useDifferentTestProjectBranch By default the branch of the LeanFT project is checked out based on the selected mxVersion. 
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param usedifferentQATestProjectBranch By default the branch of the LeanFT project is checked out based on the selected mxVersion. 
 * 		If usedifferentQATestProjectBranch is set to true the branch is overridden by the parameter differentQATestProjectBranch
 * @param differentQATestProjectBranch The branch that will be used if usedifferentQATestProjectBranch is set.
 */
@Library('TestUtils') _

node(executeOn) {
	def gitUtils = new GitUtils()
	def spUtils = new SharepointUtils()
	def mxVersionUtils = new MxVersionUtils()
	def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
	def eclipseVersion = mxBuildVersion
	def jobWorkspace = pwd()
	def testProject = 'EclipseTestProject'
	def eclipseDir = "${jobWorkspace}\\eclipse-windows"
	def eclipseFolderpath = ""
	def eclipseExe = ""
	def eclipseWorkspace = "${jobWorkspace}\\workspace"
	def runResultsDir = 'RunResults'
	def testProjectBranch
	def parallelStageExit = false
	// all elements in this list don't need innoWake's eclipse and therefore eclipse won't be downloaded
	def eclipseIsNotNeededList = [
		'testcases.mining.performance.ModuleSearchTest'
	]
	def eclipseNotRequired = testcase in eclipseIsNotNeededList

	def versionPattern = ~/(.*-20\d+)(.*)/
	def versionMatch = (mxBuildVersion =~ versionPattern)
	if (versionMatch && versionMatch[0][2].contains('-')) {
		eclipseVersion = mxBuildVersion.substring(0, mxBuildVersion.lastIndexOf('-'))
	}
	versionMatch = null

	if (Boolean.parseBoolean(useDifferentTestProjectBranch)) {
		testProjectBranch = differentTestProjectBranch
	} else if (mxVersion == '99.9') {
		testProjectBranch = 'master'
	} else {
		testProjectBranch = mxVersion
	}
	if (Boolean.parseBoolean(usedifferentQATestProjectBranch)) {
		QATestProjectBranch = differentQATestProjectBranch
	} else {
		QATestProjectBranch = 'master'
	}

	eclipseFolderpath = "${jobWorkspace}\\innowake-eclipse-complete-${mxBuildVersion}-en-win-min\\eclipse"
	eclipseExe = "${eclipseFolderpath}\\eclipse.exe"
	reuse_workspace = Boolean.parseBoolean(reuse_workspace)

	buildName "#${env.BUILD_ID} - ${testcase} - ${mxBuildVersion}"
	buildDescription "mxBuildVersion=${mxBuildVersion} testcase=${testcase} reuse_workspace=${reuse_workspace} executeOn=${executeOn} testProjectBranch=${testProjectBranch} QATestProjectBranch=${QATestProjectBranch}"

	stage('Checkout test project') {
		if (reuse_workspace) {
			echo 'Skipping "Checkout test project"'
			dir(testProject) {
				bat "git pull origin ${testProjectBranch}"
			}
		} else {
			deleteDir()
			def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/ui-test-projects/leanft-dm.git"
			dir(testProject) {
				git branch: testProjectBranch, credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
			}
		}
	}

	stage('Copy innowake eclipse') {
		if (reuse_workspace || eclipseNotRequired) {
			echo 'Skipping "Copy maxenso/innowake eclipse"'
		} else {
			echo 'Copy eclipse bundle from SharePoint'
			// spUtils.downloadIwEclipseWindows(mxBuildVersion, eclipseDir)
			spUtils.downloadFile("${spUtils.getInnowakeDir()}/innowake-eclipse-complete-${mxBuildVersion}-en-win-min.zip", './')
			// Extract the innowake-eclipse bundle zip file
			def sevenZipExePath = "C:\\Progra~1\\7-Zip\\7z.exe"
			def innowakeBundleZipFile = "${jobWorkspace}\\innowake-eclipse-complete-${mxBuildVersion}-en-win-min.zip"
			def innowakeBundleFolderPath = "${jobWorkspace}\\innowake-eclipse-complete-${mxBuildVersion}-en-win-min"
			bat "mkdir ${innowakeBundleFolderPath}"
			bat "${sevenZipExePath} x ${innowakeBundleZipFile} -o${innowakeBundleFolderPath}"
		}
	}

	stage('Run Test') {
		try {
			timeout(time: 18, unit: 'HOURS') {
				def workspace = "${eclipseWorkspace}\\${testcase}"
				dir(runResultsDir) {
					deleteDir()
				}

				// Get fresh license before starting a test
				dir(eclipseFolderpath) {
					bat 'erase /f innowake.lic'
					gitUtils.getSingleFileOnWindows('infrastructure/licenses', 'mining/innowake.lic', mxVersion, '.')
				}
				switch (testcase) {
					case 'testcases.mining.performance.QATestprojectsDiscoveryTest':
					case 'testcases.mining.performance.DiscoverCodePerformanceTest':
						if (reuse_workspace) {
							def qatestprojectFolderPath = "${eclipseWorkspace}\\${testcase}\\QATestprojects"
							echo 'Reseting local git branch'
							dir(qatestprojectFolderPath) {
								bat """
									git restore .
									git clean -fd -d
									git reset --hard
									git submodule foreach --recursive git reset --hard
									git submodule update --init --recursive
									git pull origin ${QATestProjectBranch}
									"""
							}
						} else {
							def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/QATestprojects.git"
							dir(workspace) {
								deleteDir()
							}
							cloneProject("${eclipseWorkspace}\\${testcase}\\QATestprojects", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/QATestprojects.git", QATestProjectBranch)
						}
						break
					case 'testcases.mining.performance.DiscoverMetricsIncrementalScanTest':
						if (reuse_workspace) {
							def qatestprojectFolderPath = "${eclipseWorkspace}\\${testcase}\\QATestprojects"
							echo 'Reseting local git branch'
							dir(qatestprojectFolderPath) {
								bat """
									git restore .
									git clean -fd -d
									git reset --hard
									git submodule foreach --recursive git reset --hard
									git submodule update --init --recursive
									git pull origin 'QATestprojects-discovered'
									"""
							}
						} else {
							def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/QATestprojects.git"
							dir(workspace) {
								deleteDir()
							}
							cloneProject("${eclipseWorkspace}\\${testcase}-1\\QATestprojects", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/QATestprojects.git", 'QATestprojects-discovered1')
							cloneProject("${eclipseWorkspace}\\${testcase}-2\\QATestprojects", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/QATestprojects.git", 'QATestprojects-discovered')				
						}
						break
				}	
				if (testcase == 'testcases.mining.performance.DiscoverCodePerformanceTest') {
					try {
					    bat "robocopy ${eclipseWorkspace}\\${testcase} ${eclipseWorkspace}\\${testcase}-copy /s /e"
				    } catch (Exception e){
                        echo e.toString()
                    }
					try {
					    bat "robocopy ${eclipseWorkspace}\\${testcase} ${eclipseWorkspace}\\${testcase}-copy2 /s /e"
				    } catch (Exception e){
                        echo e.toString()
                    }
				}
				if (! eclipseNotRequired) {
					// solves WQM-4506: Pushd sets the working dir for the .bat scope, so that eclipse uses the correct eclipse.ini file
					def startEclipseCmd = "Pushd ${eclipseFolderpath}\nstart ${eclipseExe} -data ${workspace}"
					// The command is written to a bat file that enables us to start the eclipse on a server with a test case specific workspace
					writeFile file: "${jobWorkspace}\\openWorkspace_${testcase}.bat", text: startEclipseCmd
					bat startEclipseCmd
				}
				echo "run test ${testcase}"
				withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
					dir(testProject) {
						bat "mvn clean -DmxBuildVersion=${eclipseVersion} -DworkspaceDir=${workspace} -Dtest=${testcase} -DminingServerURL=http://localhost:2580/ test"
					}
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
		if (testcase == 'testcases.mining.performance.QATestprojectsDiscoveryTest') {
			archiveArtifacts allowEmptyArchive: true, artifacts: 'workspace\\testcases.mining.performance.QATestprojectsDiscoveryTest\\QATestprojects\\discover-code-*'
			archiveArtifacts allowEmptyArchive: true, artifacts: 'workspace\\testcases.mining.performance.QATestprojectsDiscoveryTest\\QATestprojects\\discover-metrics-*'
			archiveArtifacts allowEmptyArchive: true, artifacts: 'workspace\\testcases.mining.performance.QATestprojectsDiscoveryTest\\QATestprojects\\discovery_*'
			archiveArtifacts allowEmptyArchive: true, artifacts: 'workspace\\testcases.mining.performance.QATestprojectsDiscoveryTest\\QATestprojects\\effort-summary**'
			archiveArtifacts allowEmptyArchive: true, artifacts: '${mxBuildVersion}\\mining\\server\\logs\\mining-api-server.log'
		}
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
