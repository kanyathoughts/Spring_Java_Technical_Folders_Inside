/**
 * Starts or Stops the Mining Environment local on a Windows node.
 * --Start--
 * It downloads the Mining bundle and extract the required folders to the workspace.
 * Two batch scripts for orientDB and Mining API will be created and added to Windows' Task Schedular.
 * The execution of the batch scripts with the Task Schedular keeps the environment alive after
 * successful job execution.
 * 
 * --Stop--
 * Gets the Process ID of the started batch scripts and terminates the processes.
 * It removes the added tasks from Windows' Task Schedular.
 * 
 * @param mxBuildVersion The innowake build to test.
 * @param executeOn The Jenkins node the "environment" will run on
 * @param envAction "Start", "Stop" or "Backup"
 */

@Library('TestUtils') _
import java.text.SimpleDateFormat

node(executeOn) {
	def gitUtils = new GitUtils()
	def spUtils = new SharepointUtils()
	def mxVersionUtils = new MxVersionUtils()
	def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
	def jobWorkspace = pwd()
	def log4jPath = "${jobWorkspace}\\log4j"

	buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	buildDescription "mxBuildVersion=${mxBuildVersion} executeOn=${executeOn} envAction=${envAction}"

	stage('Stop running environment') {
		if (envAction == 'Start' || envAction == 'Backup') {
			def orientDBTask = 'OrientDBTask'
			def miningTask = 'MiningTask'
			dir(jobWorkspace){
				bat 'IF EXIST miningPID.txt DEL /F miningPID.txt'
				bat 'IF EXIST orientPID.txt DEL /F orientPID.txt'
			}

			createOrientPidScript()		
			getPidAndKillProcess("${jobWorkspace}\\getOrientPid.bat", "${jobWorkspace}\\orientPID.txt")
			createMiningPidScript()
			getPidAndKillProcess("${jobWorkspace}\\getMiningPid.bat", "${jobWorkspace}\\miningPID.txt")
			if (fileExists('${jobWorkspace}\\orientPID.txt')) {
				bat "SCHTASKS /DELETE /TN ${orientDBTask} /F"
			}
			if (fileExists('${jobWorkspace}\\getMiningPid.txt')) {
				createMiningPidScript()
				getPidAndKillProcess("${jobWorkspace}\\getMiningPid.bat", "${jobWorkspace}\\miningPID.txt")
				bat "SCHTASKS /DELETE /TN ${miningTask} /F"
			}
		}
	}

	stage('copy mining bundle') {	
		if (envAction == 'Start') {
			deleteDir()
			spUtils.downloadFile("${spUtils.getInnowakeDir()}/${mxBuildVersion}.zip", './')

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

			def miningApiServerPath = "${jobWorkspace}\\${mxBuildVersion}\\mining\\server"
			dir(miningApiServerPath) {
				gitUtils.getSingleFileOnWindows('infrastructure/licenses', 'mining/innowake.lic', mxVersion, '.')
			}

			dir(log4jPath) {
				gitUtils.getSingleFileOnWindows('infrastructure/mining-job-artifacts', 'log4j2-performance.xml', 'master', log4jPath)
				bat "ren log4j2-performance.xml log4j2.xml"
			}

			// Increase the heap size in server.bat
			def serverBatFile = "${orientdbFolderPath}\\bin\\server.bat"
			powershell "((Get-Content -path ${serverBatFile} -Raw) -replace 'set MAXHEAP=-Xms2G -Xmx4G','set MAXHEAP=-Xms4G -Xmx32G') | Set-Content -Path ${serverBatFile}"
		}
	}

	stage('restore mining environment') {
		if (envAction == 'Backup') {
			dir(jobWorkspace){
				bat 'IF EXIST miningPID.txt DEL /F miningPID.txt'
				bat 'IF EXIST orientPID.txt DEL /F orientPID.txt'
			}
			gitUtils.checkoutGitProject(jobWorkspace, "${gitUtils.getGitUrlQef()}/infrastructure/mining_qatestproject_dump.git", 'master')
			def orientdbBinFolder = "${jobWorkspace}\\${mxBuildVersion}\\mining\\orientdb\\orientdb-mining-${mxBuildVersion}\\bin"
			def miningDatabasePath = "${jobWorkspace}\\${mxBuildVersion}\\mining\\orientdb\\orientdb-mining-${mxBuildVersion}\\databases\\mining"
			dir(orientdbBinFolder) {
				bat "console.bat \"connect plocal:${miningDatabasePath} admin admin;DROP DATABASE\""
				bat "IF EXIST ${miningDatabasePath} RMDIR /S /Q ${miningDatabasePath}"
				bat "console.bat \"CREATE DATABASE plocal:${miningDatabasePath} admin admin\""
				try {
					bat "console.bat \"connect plocal:${miningDatabasePath} admin admin;RESTORE DATABASE ${jobWorkspace}\\QATestprojectBackup.zip;CREATE INDEX Module_name_ft ON Module(name) FULLTEXT ENGINE LUCENE METADATA { \"allowLeadingWildcard\": true, \"default\": \"innowake.mining.data.plugin.lucene.LowercasingKeywordAnalyzer\" };\""
				} catch(e) {
					echo 'Index Module_name_ft already exists.'
				}
			}
		}

	}

	stage('start/stop orientdb') {
		echo "---${envAction} OrientDB---"
		def orientdbFolderPath = "${jobWorkspace}\\${mxBuildVersion}\\mining\\orientdb\\orientdb-mining-${mxBuildVersion}"
		def orientDBTask = 'OrientDBTask'
		if (envAction == 'Start') {
			def startingTime = getCurrentTime()
			bat "echo set ORIENTDB_HOME=${orientdbFolderPath}> startOrient.bat"
			bat "echo start ${orientdbFolderPath}\\bin\\server.bat >> startOrient.bat"
			bat "echo exit>> startOrient.bat"
			try {
				bat "SCHTASKS /CREATE /SC ONCE /ST ${startingTime}  /TN ${orientDBTask} /TR ${jobWorkspace}\\startOrient.bat"
			} catch(e) {
				echo "The task name ${orientDBTask} already exists."
			}
			bat "SCHTASKS /RUN /TN ${orientDBTask}"
			createOrientPidScript()
			bat "start /b ${jobWorkspace}\\getOrientPid.bat"
		}

		if (envAction == 'Stop') {
			getPidAndKillProcess("${jobWorkspace}\\getOrientPid.bat", "${jobWorkspace}\\orientPID.txt")
			bat "SCHTASKS /DELETE /TN ${orientDBTask} /F"
		}

		if (envAction == 'Backup') {
			def startingTime = getCurrentTime()
			try {
				bat "SCHTASKS /CREATE /SC ONCE /ST ${startingTime}  /TN ${orientDBTask} /TR ${jobWorkspace}\\startOrient.bat"
			} catch(e) {
				echo "The task name ${orientDBTask} already exists."
			}
			bat "SCHTASKS /RUN /TN ${orientDBTask}"
		}
	}

	stage('start/stop mining API') {
		echo "---${envAction} Mining Environment---"
		def miningApiServerPath = "${jobWorkspace}\\${mxBuildVersion}\\mining\\server"
		def miningTask = 'MiningTask'
		if (envAction == 'Start') {
			powershell "Start-Sleep -s 30"
			dir(miningApiServerPath){
				def miningApiServerJarFile = "${miningApiServerPath}\\mining-api-server-dist-${mxBuildVersion}.jar"
				bat "echo java -Dlogging.config=${log4jPath}\\log4j2.xml -Dinnowake.license.location=${miningApiServerPath}\\innowake.lic -jar ${miningApiServerJarFile} --server.port=2580 --mining.cookieId=DISABLED --spring.profiles.active=profiling,no-authorization> mining.bat"
			}
			bat "echo E:> start_miningEnv.bat"
			bat "echo cd ${miningApiServerPath}>> start_miningEnv.bat"
			bat "echo start mining.bat>> start_miningEnv.bat"
			bat "echo exit>> start_miningEnv.bat"
			def startingTime = getCurrentTime()
			try {
				bat "SCHTASKS /CREATE /SC ONCE /ST ${startingTime} /TN ${miningTask} /TR ${jobWorkspace}\\start_miningEnv.bat"
			} catch(e) {
				echo "The task name ${miningTask} already exists."
			}
			bat "SCHTASKS /RUN /TN ${miningTask}"
			powershell "Start-Sleep -s 60"
			createMiningPidScript()
			bat "start /b ${jobWorkspace}\\getMiningPid.bat"
		}
		
		if (envAction == 'Stop') {
			getPidAndKillProcess("${jobWorkspace}\\getMiningPid.bat", "${jobWorkspace}\\miningPID.txt")
			bat "SCHTASKS /DELETE /TN ${miningTask} /F"
		}

		if (envAction == 'Backup') {
			powershell "Start-Sleep -s 30"
			def startingTime = getCurrentTime()
			try {
				bat "SCHTASKS /CREATE /SC ONCE /ST ${startingTime} /TN ${miningTask} /TR ${jobWorkspace}\\start_miningEnv.bat"
			} catch(e) {
				echo "The task name ${miningTask} already exists."
			}
			bat "SCHTASKS /RUN /TN ${miningTask}"
			powershell "Start-Sleep -s 120"
		}
	}

	stage('Archive run results') {
	    if (envAction == 'Stop') {
	        archiveArtifacts allowEmptyArchive: true, artifacts: "${mxBuildVersion}/mining/server/logs/*"
			dir("${mxBuildVersion}/mining/server/logs") {
				deleteDir();
			}
    	}
	}

}

/**
 * Returns the current time ('HH:mm')
 */
def getCurrentTime() {
	Date date = new Date()
	return date.format("HH:mm")
}

/**
 * Creates a batch file that can be used to get the Process ID of the running process 'server.bat' (orientDB).
 */
def createOrientPidScript() {
	def scriptFile = '''for /f \"tokens=2 delims=,\" %%P in ('tasklist /v /fo csv ^| findstr /i \"server.bat\"') do set pid=%%~P
echo %pid%> orientPID.txt'''
	writeFile file: 'getOrientPid.bat', text: scriptFile
}

/**
 * Creates a batch file that can be used to get the Process ID of the running process 'mining.bat'.
 */
def createMiningPidScript() {
	def scriptFile = '''for /f \"tokens=2 delims=,\" %%P in ('tasklist /v /fo csv ^| findstr /i \"mining.bat\"') do set pid=%%~P
echo %pid%> miningPID.txt'''
	writeFile file: 'getMiningPid.bat', text: scriptFile
}

/**
 * Starts the scriptFile that stores the Process ID (PID) of a specific process in a file.
 * The stored PID will be used to kill the process. 
 * Unfortunetaly, the termination of orientDB and mining must be performed twice.
 */
def getPidAndKillProcess(scriptFile, pidFile) {
	int i = 0;
	def pid
	while(i < 2) {
		bat "start /b ${scriptFile}"
		sleep 2
		if (fileExists(pidFile)) {
			pid = readFile pidFile
			if (! pid.isEmpty() && pid.isNumber()) {
				bat "taskkill /F /PID ${pid}"
			}
		}
		i++
	}
}

