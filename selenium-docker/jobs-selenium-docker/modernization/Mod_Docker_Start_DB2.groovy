@Library('TestUtils') _

/**
 * Starts a docker container with a DB2 database
 * 
 * @param executeOn The node where to setup the DB2 database.
 */

node(executeOn) {
	
	timestamps {
		
        def dockerUtils = new DockerUtils()
        def miscUtils = new MiscUtils()
        def containerPort
        
        buildName "#${env.BUILD_ID} - ${executeOn}"

        stage('Stop and remove running DB2 Docker container') { 
		sh "docker stop mannheimerDB2-DB2Container || true"
		sleep 5
		sh "docker rm -f mannheimerDB2-DB2Container || true"
		sleep 5
	}

        stage('Start DB2 Docker container') {
            dockerUtils.startDockerDb2Multiple('mannheimerDB2', 'DB2Container')
            sleep 10
            def containerID = sh returnStdout: true, script: "docker ps -aqf 'name= mannheimerDB2-DB2Container'"
            dockerUtils.waitForDb2(containerID, 5)
            containerPort = sh returnStdout: true, script: "docker port ${containerID}"
			containerPort = containerPort.split(':')[1].trim()
            containerPort = containerPort.split('\n')[0].trim()
            def hostname = miscUtils.getHostname()
            echo "DB is ready and listening on port ${containerPort}. DB is running on ${hostname}."
        }

        buildDescription "containerPort=${containerPort}"
	}
}