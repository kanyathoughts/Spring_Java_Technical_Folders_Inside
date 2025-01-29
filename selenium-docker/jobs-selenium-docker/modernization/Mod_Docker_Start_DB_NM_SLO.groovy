@Library('TestUtils') _
/**
 * This job sets up a MSSQL docker container with data from NMSLO. Therefore, following parameter have to be specified.
 *
 * @param executeOn		The node where to setup the NMSLO database.
 */

node(executeOn) {
	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def initialDBPassword
	def containerID
	def containerPort
	def containerHostname

	buildName "#${env.BUILD_ID} - ${executeOn}"

	stage('Stop and remove running MSSQL Docker container') { 
		sh "docker stop sqlContainer1 || true"
		sleep 5
		sh "docker rm -f sqlContainer1 || true"
		sleep 5
	}

	stage('Start MSSQL Docker container') {
		withCredentials([usernamePassword(credentialsId: 'modernization-mssql-initial-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
			initialDBPassword = dbPassword
			sh "docker run -d --name sqlContainer1 -e ACCEPT_EULA=Y -e ${dbUser}_PASSWORD=\"${dbPassword}\" -p 1433:1433 -v /data/nmslo_db/dev_3.20180628_BEFORE_RSET_MODERNIZATION_QA_compressed.bak:/tmp/db_dump.bak:ro mcr.microsoft.com/mssql/server:2017-latest"
			sleep 10
		}
	}

	stage ('Setup MSSQL Docker container with NMSLO data') {
		withCredentials([usernamePassword(credentialsId: 'modernization-mssql-initial-login', passwordVariable: 'initialDBPassword', usernameVariable: 'dbUser')]) {
			withCredentials([usernamePassword(credentialsId: 'modernization-mssql-updated-login', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
				sh "docker exec -t sqlContainer1 /opt/mssql-tools/bin/sqlcmd -S localhost -U ${dbUser} -P \"${initialDBPassword}\" -Q \"ALTER LOGIN SA WITH PASSWORD=\'${dbPassword}\'\""
				sh "docker exec sqlContainer1 /opt/mssql-tools/bin/sqlcmd -S localhost -U ${dbUser} -P ${dbPassword} -Q \"RESTORE DATABASE [unit_test_1] FROM DISK='/tmp/db_dump.bak' WITH REPLACE, MOVE 'dev_3' TO '/var/opt/mssql/data/unit_test_1.mdf', MOVE 'dev_3_log' TO '/var/opt/mssql/data/unit_test_1_log.ldf' \""
				containerID = sh returnStdout: true, script: "docker ps -aqf 'name=sqlContainer1'"
				containerPort = sh returnStdout: true, script: "docker port ${containerID}"
				containerPort = containerPort.split(':')[1].trim()
				containerHostname = miscUtils.getHostname()
				echo "DB is ready and listening on port ${containerPort}. DB is running on ${containerHostname}."
			}
		}
	}
}