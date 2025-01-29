@Library('TestUtils') _

/**
 * Imports a time-tracking database dump and sets up the application in a dockerized environment.
 * To stop the job you have to click on "Proceed" in the console log.
 * 
 * !!! IMPORTANT !!!
 * The job get's the database dump and the application war file from the /tmp folder. 
 * - The file name of the database dump must have the following structure: Backup_Database_innowake_zeiterfassung_*.backup
 * - The war file must have the name "time-tracking.war"
 */

node('DELinux2') {
	
	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def webappsDir = 'webapps'
	def warFile = 'time-tracking.war'
	def dbDump = 'time-tracking.dump'
	
	stage('prepare environment') {
		deleteDir()
		docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside('-v /tmp:/tmp:ro') {
			try {
				sh "cp /tmp/Backup_Database_innowake_zeiterfassung_*.backup ${dbDump}"
			} catch (Exception e) {
				error('Either there is no backup file or there are multiple files with the structure "Backup_Database_innowake_zeiterfassung_*.backup" in the /tmp folder.')
			}
			dir(webappsDir) {
				try {
					sh "cp /tmp/${warFile} ${warFile}"
				} catch (Exception e) {
					error("There ist no file with the name \"${warFile}\" in the /tmp folder.")
				}
			}
		}
	}
	
	stage('run environment') {
		def jobWorkspace = pwd()
		def appPort = '7070'
		def dbContainerName = 'time-tracking_test_db'
		def dbName = 'innowake_zeiterfassung'
		def dbVolumeName = "time-tracking_test_db_volume"
		
		sh returnStatus: true, script: "docker volume rm ${dbVolumeName}"
		sh "docker volume create ${dbVolumeName}"
		withCredentials([usernamePassword(credentialsId: 'time-tracking_DB_user', passwordVariable: 'dbPw', usernameVariable: 'dbUser')]) {
			docker.image("postgres:10.5").withRun("--name ${dbContainerName} -v ${jobWorkspace}/${dbDump}:/backups/${dbDump} -v ${dbVolumeName}:/var/lib/postgresql/data -e POSTGRES_PASSWORD=${dbPw} -e POSTGRES_DB=${dbName} -e POSTGRES_USER=${dbUser} -p 5432:5432 ") {
				// Wait until db is started
				sleep 10
				sh "docker exec -t ${dbContainerName} psql -U postgres -c 'CREATE ROLE iw_releasemanager WITH LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT NOREPLICATION CONNECTION LIMIT -1;'"
				sh "docker exec -t ${dbContainerName} psql -U postgres -c 'CREATE ROLE iw_reporter WITH LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT NOREPLICATION CONNECTION LIMIT -1;\'"
				sh "docker exec -t ${dbContainerName} psql -U postgres -c 'CREATE ROLE \"plutos.batchjob\" WITH LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT NOREPLICATION CONNECTION LIMIT -1;\'"
				sh "docker exec ${dbContainerName} pg_restore -U postgres -d ${dbName} --no-owner /backups/${dbDump}"
				docker.image("tomcat:9.0.37-jdk8-openjdk").withRun("--name time-tracking_test_app -p ${appPort}:8080 -v ${jobWorkspace}/${webappsDir}/${warFile}:/usr/local/tomcat/webapps/${warFile}") {
					// Wait until application is started
					sleep 10
					def hostname = miscUtils.getHostname()
					echo "time-tracking running under http://${hostname}:${appPort}/time-tracking"
					input 'Click on proceed when you do not longer need the environment'
				}
			}
		}
		sh returnStatus: true, script: "docker volume rm ${dbVolumeName}"
	}
}