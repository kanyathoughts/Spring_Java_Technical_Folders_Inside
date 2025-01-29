/**
 * Job to stop running SC_Environment containers
 *
 * @param executeOn The Jenkins node the job will run on
 */
	
node(executeOn) {
	
	stage('remove old docker containers') {
	    sh returnStatus: true, script: 'docker rm -f sc_environment_db'
	    sh returnStatus: true, script: 'docker rm -f sc_environment_soa'
		sh returnStatus: true, script: "docker volume rm sc_environment_db_volume"
	}
}
