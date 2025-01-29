@Library('TestUtils') _

/*
* Stops and remove the Oracle DB container
* @param executeOn 		The Jenkins node the job will run on
*/

node(executeOn) {
    timestamps {
        buildName "#${env.BUILD_ID}"
        def oracleDB = 'oracleDB'
        def dockerUtils = new DockerUtils()
		stage('Stop Oracle DB') {
		    sh "docker stop ${oracleDB}"
			sleep 20
			sh "docker rm -f ${oracleDB}"
			echo "DB has been stopped and container has been removed"
		}
	}
}