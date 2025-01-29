@Library('TestUtils') _

/**
 * Starts a docker container with an Oracle database, waits for user confirmation and afterwards stops the database.
 * 
 * @param imageTag  The tag of the docker image
 */

nodeTF('Docker-host && Region-EU') {
	
	timestamps {
		
        def dockerUtils = new DockerUtils()
        def miscUtils = new MiscUtils()
        
        buildName "#${env.BUILD_ID} - ${imageTag}"
        docker.image(dockerUtils.pullDockerOracleImage(imageTag)).withRun('-p 1521') {
            container ->
            stage('Wait until DB is ready') {                
				dockerUtils.waitForOracle(container.id, 5)
            }
            
            stage('wait-for-user') {
                def port = dockerUtils.getActualHostPort1521(container.id)
                def hostname = miscUtils.getHostname()
                input "DB is ready and listening on port ${port}. DB is running on ${hostname}. Proceed here to shut it down."
            }
        }
	}
}
