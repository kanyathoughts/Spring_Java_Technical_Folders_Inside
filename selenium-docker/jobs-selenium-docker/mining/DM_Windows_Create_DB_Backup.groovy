/**
 * Compile and run automated LeanFT tests for creation of DB backup. 
 * 
 * @param mxBuildVersion The innowake build to test.
 * @param testcase  Name including the package of the test you want to run.
 * @param reuse_workspace If true project checkout and copy of the eclipse is skipped and the previous artifacts of project and eclipse are used.
 * @param executeOn The Jenkins node the test will run on
 * @param useDifferentTestProjectBranch By default the branch of the LeanFT project is checked out based on the selected mxVersion. 
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */
@Library('TestUtils') _

node(executeOn) {
	def gitUtils = new GitUtils()
	def spUtils = new SharepointUtils()
	def mxVersionUtils = new MxVersionUtils()
	def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
	def eclipseVersion = mxBuildVersion
	def jobWorkspace = pwd()
	def eclipseRequired = true
	def testProject = 'EclipseTestProject'
	def eclipseDir = "${jobWorkspace}\\eclipse-windows"
	def eclipseFolderpath = ""
	def eclipseExe = ""
	def eclipseWorkspace = "${jobWorkspace}\\workspace"
	def runResultsDir = 'RunResults'
	def testProjectBranch

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

	eclipseFolderpath = "${jobWorkspace}\\innowake-eclipse-complete-${mxBuildVersion}-en-win-min\\eclipse"
	eclipseExe = "${eclipseFolderpath}\\eclipse.exe"
	reuse_workspace = Boolean.parseBoolean(reuse_workspace)
	
	buildName "#${env.BUILD_ID} - ${testcase} - ${mxBuildVersion}"
	buildDescription "mxBuildVersion=${mxBuildVersion} testcase=${testcase} reuse_workspace=${reuse_workspace} executeOn=${executeOn} testProjectBranch=${testProjectBranch}"
	
	stage('Checkout test project') {
		if (reuse_workspace) {
			echo 'Skipping "Checkout test project"'
			dir(testProject) {
				bat "git pull origin ${testProjectBranch}"
			}
		} else {
			deleteDir()
			def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/ui-test-projects/leanft.git"
			dir(testProject) {
				git branch: testProjectBranch, credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
			}
		}
	}
	
	stage('Copy innowake eclipse') {
		if (reuse_workspace) {
			echo 'Skipping "Copy maxenso/innowake eclipse"'
		} else {
			echo 'Copy eclipse bundle from SharePoint'
			// spUtils.downloadIwEclipseWindows(mxBuildVersion, eclipseDir)
			spUtils.downloadFile("/Shared Documents/delivery/archive/innowake/innowake-eclipse-complete-${mxBuildVersion}-en-win-min.zip", './')
			// Extract the innowake-eclipse bundle zip file
			def sevenZipExePath = "C:\\Progra~1\\7-Zip\\7z.exe"
			def innowakeBundleZipFile = "${jobWorkspace}\\innowake-eclipse-complete-${mxBuildVersion}-en-win-min.zip"
			def innowakeBundleFolderPath = "${jobWorkspace}\\innowake-eclipse-complete-${mxBuildVersion}-en-win-min"
			bat "mkdir ${innowakeBundleFolderPath}"
			bat "${sevenZipExePath} x ${innowakeBundleZipFile} -o${innowakeBundleFolderPath}"
		}
	}
	
	stage('Copy mining bundle') {
		if (reuse_workspace) {
			echo 'Skipping "Copy mining bundle"'
		} else {
			def bundlePath = '/Shared Documents/delivery/archive/innowake/'
			spUtils.downloadFile("${bundlePath}${mxBuildVersion}.zip", './')
		
			// Extract the mining bundle zip file
			def sevenZipExePath = "C:\\Progra~1\\7-Zip\\7z.exe"
			def miningBundleZipFile = "${jobWorkspace}\\${mxBuildVersion}.zip"
			def miningBundleFolderPath = "${jobWorkspace}\\${mxBuildVersion}"
			bat "mkdir ${miningBundleFolderPath}"
			bat "${sevenZipExePath} x ${miningBundleZipFile} -o${miningBundleFolderPath}"
		
			// Extract the orientdb zip file
			def orientdbZipFile = "${jobWorkspace}\\${mxBuildVersion}\\mining\\orientdb\\orientdb-mining-${mxBuildVersion}.zip"
			def orientdbFolderPath = "${jobWorkspace}\\${mxBuildVersion}\\mining\\orientdb\\orientdb-mining-${mxBuildVersion}"
			bat "mkdir ${orientdbFolderPath}"
			bat "${sevenZipExePath} x ${orientdbZipFile} -o${orientdbFolderPath}"
			dir(miningBundleFolderPath + "\\mining\\server") {
				gitUtils.getSingleFileOnWindows('infrastructure/licenses', 'mining/innowake.lic', mxVersion, '.')
			}
		}
	}
		
	stage('Run Test') {
		try {
			timeout(time: 8, unit: 'HOURS') {
				def workspace = "${eclipseWorkspace}\\${testcase}"
				dir(runResultsDir) {
					deleteDir()
				}
						
				// Get fresh license before starting a test
				dir(eclipseFolderpath) {
					bat 'erase /f innowake.lic'
					gitUtils.getSingleFileOnWindows('infrastructure/licenses', 'mining/innowake.lic', mxVersion, '.')
				}
				
				dir(workspace) {
					deleteDir()
				}
				cloneProject("${eclipseWorkspace}\\${testcase}\\discovery-jumpstart", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/discovery-jumpstart.git", "master")
				cloneProject("${eclipseWorkspace}\\${testcase}\\txdmv-test-project", "${gitUtils.getGitUrlQef()}/innowake-test-projects/mining-test-projects/txdmv-test-project.git", 'master')
				
				// solves WQM-4506: Pushd sets the working dir for the .bat scope, so that eclipse uses the correct eclipse.ini file
				def startEclipseCmd = "Pushd ${eclipseFolderpath}\nstart ${eclipseExe} -data ${workspace}"
				// The command is written to a bat file that enables us to start the eclipse on a server with a test case specific workspace 
				writeFile file: "${jobWorkspace}\\openWorkspace_${testcase}.bat", text: startEclipseCmd
				bat startEclipseCmd

				echo "run test ${testcase}"
				withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
					dir(testProject) {
						bat "mvn clean -DmxBuildVersion=${eclipseVersion} -DworkspaceDir=${workspace} -Dtest=${testcase} test"
					}
				}
			}
		} catch (e) {
			unstable e.toString()
		}
	}
	
	stage('Create DB Dump') {	
		def orientdbFolderPath = "${jobWorkspace}\\${mxBuildVersion}\\mining\\orientdb\\orientdb-mining-${mxBuildVersion}\\bin"
		dir(orientdbFolderPath) {
			if (testcase == 'mining.dbdumps.CreateDBBackupForQATestEnv') {
				bat "console.bat \"connect plocal:${jobWorkspace}\\${mxBuildVersion}\\mining\\orientdb\\orientdb-mining-${mxBuildVersion}\\databases\\mining admin admin;backup database backup_Txdmv.zip\""
			} else if (testcase == 'mining.dbdumps.CreateDBBackupForDMStagingServer') {
				bat "console.bat \"connect plocal:${jobWorkspace}\\${mxBuildVersion}\\mining\\orientdb\\orientdb-mining-${mxBuildVersion}\\databases\\mining admin admin;backup database stagingServerBackup.zip\""
			}			
		}
	}

	stage('Archive run results') {
		dir(testProject) {
			archiveArtifacts allowEmptyArchive: true, artifacts: "${runResultsDir}/**/*"
			publishHTML allowMissing: true, alwaysLinkToLastBuild: true, keepAll: true, reportDir: runResultsDir, reportFiles: 'runresults.html', reportName: testcase, reportTitles: "#${env.BUILD_ID} - 	${mxBuildVersion}: ${testcase}"	
		}
		def orientdbFolderPath = "${mxBuildVersion}\\mining\\orientdb\\orientdb-mining-${mxBuildVersion}\\bin"
		archiveArtifacts allowEmptyArchive: true, artifacts: "${orientdbFolderPath}/**/*"
		publishHTML allowMissing: true, alwaysLinkToLastBuild: true, keepAll: true, reportDir: runResultsDir, reportFiles: 'runresults.html', reportName: testcase, reportTitles: "#${env.BUILD_ID} - 	${mxBuildVersion}: ${testcase}"	
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
