/**
 * Starts or Stops the Mining Environment on a Windows node and validate 
 * if the environment starts only with valid mining-core enabled license.
 * 
 * @param executeOn The Jenkins node the test will run on
 * @param mxBuildVersion The innowake build to test.
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
	buildDescription "mxBuildVersion=${mxBuildVersion} executeOn=${executeOn}"

	stage('Stop running environment') {
		bat "npx kill-port 2480"
		bat "npx kill-port 2580"
	}
	
	stage('clean up workspace') {
		deleteDir()
	}

	stage('copy mining bundle') {		
		dir('mining') {
			spUtils.downloadJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "mining-api-server-dist-${mxBuildVersion}.jar", './')
			gitUtils.getSingleFileOnWindows('infrastructure/licenses', 'mining/mining-with-mining-core-disabled.lic', mxVersion, '.')
			bat "ren mining-with-mining-core-disabled.lic innowake.lic"
		}
		
		dir('orientDB'){
			spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, "orientdb-mining-${mxBuildVersion}.zip", './')
		}

		dir(log4jPath) {
			gitUtils.getSingleFileOnWindows('infrastructure/mining-job-artifacts', 'log4j2-performance.xml', 'master', log4jPath)
			bat "ren log4j2-performance.xml log4j2.xml"
		}

		// Increase the heap size in server.bat
		def serverBatFile = "${jobWorkspace}\\orientDB\\bin\\server.bat"
		powershell "((Get-Content -path ${serverBatFile} -Raw) -replace 'set MAXHEAP=-Xms2G -Xmx4G','set MAXHEAP=-Xms4G -Xmx16G') | Set-Content -Path ${serverBatFile}"
	}

	stage("start orientdb") {
		def orientdbFolderPath = "${jobWorkspace}\\orientDB"
		def orientDBTask = 'OrientDBTask'
		def startingTime = getCurrentTime()
		bat "echo set ORIENTDB_HOME=${orientdbFolderPath}> startOrient.bat"
		bat "echo start ${orientdbFolderPath}\\bin\\server.bat >> startOrient.bat"
		bat "exit>> startOrient.bat"
		try {
			bat "SCHTASKS /CREATE /SC ONCE /ST ${startingTime}  /TN ${orientDBTask} /TR ${jobWorkspace}\\startOrient.bat"
		} catch(e) {
			echo "The task name ${orientDBTask} already exists."
		}
		bat "SCHTASKS /RUN /TN ${orientDBTask}"
	}

	stage('start mining API without mining-core') {
		def miningApiServerPath = "${jobWorkspace}\\mining"
		def miningTask = 'MiningTask'
		powershell "Start-Sleep -s 30"
		dir(miningApiServerPath){
			def miningApiServerJarFile = "${miningApiServerPath}\\mining-api-server-dist-${mxBuildVersion}.jar"
			bat "echo cd ${miningApiServerPath} > mining.bat"
			bat "echo java -Dlogging.config=${log4jPath}\\log4j2.xml -Dinnowake.license.location=${miningApiServerPath}\\innowake.lic -jar ${miningApiServerJarFile} --server.port=2580 --mining.cookieId=DISABLED --spring.profiles.active=profiling,no-authorization ^> output.txt>> mining.bat"
			bat "echo exit>> mining.bat"
		}
		bat "echo start ${miningApiServerPath}\\mining.bat> start_miningEnv.bat"
		bat "echo exit>> start_miningEnv.bat"
		bat "start ${jobWorkspace}\\start_miningEnv.bat"
		powershell "Start-Sleep -s 60"
		def outputData = readFile(file: "${miningApiServerPath}\\output.txt")
        if (! outputData.contains("The innoWake mining-mining-core is not licensed")) {
            error("License check expected to fail, but was started successfully")
        } else {
            echo "License check failed due to missing mining-core"
        }
	}
	
	stage('start mining API with mining-core') {
		def miningApiServerPath = "${jobWorkspace}\\mining"
		def miningTask = 'MiningTask'		
		powershell "Start-Sleep -s 30"
		dir(miningApiServerPath){
			gitUtils.getSingleFileOnWindows('infrastructure/licenses', 'mining/mining-with-mining-core-enabled.lic', mxVersion, '.')
			bat 'erase /f innowake.lic'
			bat "ren mining-with-mining-core-enabled.lic innowake.lic"
			def miningApiServerJarFile = "${miningApiServerPath}\\mining-api-server-dist-${mxBuildVersion}.jar"
			bat "echo cd ${miningApiServerPath} > mining.bat"
			bat "echo java -Dlogging.config=${log4jPath}\\log4j2.xml -Dinnowake.license.location=${miningApiServerPath}\\innowake.lic -jar ${miningApiServerJarFile} --server.port=2580 --mining.cookieId=DISABLED --spring.profiles.active=profiling,no-authorization ^> output.txt>> mining.bat"
			bat "exit>> mining.bat"
		}
		bat "echo start ${miningApiServerPath}\\mining.bat> start_miningEnv.bat"
		def startingTime = getCurrentTime()
		bat "start ${jobWorkspace}\\start_miningEnv.bat"
		powershell "Start-Sleep -s 60"
		def outputData = readFile(file: "${miningApiServerPath}\\output.txt")
		if (outputData.contains("The innoWake mining-mining-core is not licensed")) {
            error("License check failed, unable to start mining-api-server")
        } else {
            echo "License check succeeded and mining-api-server has been started successfully"
        }
	}
	
	stage("stop orientdb") {
		bat "npx kill-port 2480"
	}

	stage('stop mining API') {
		bat "npx kill-port 2580"
	}
	
	stage('Archive run results') {
		dir(jobWorkspace) {
			archiveArtifacts allowEmptyArchive: true, artifacts: "mining\\output.txt"
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
		pid = readFile pidFile
		bat "taskkill /F /PID ${pid}"
		i++
	}
}

