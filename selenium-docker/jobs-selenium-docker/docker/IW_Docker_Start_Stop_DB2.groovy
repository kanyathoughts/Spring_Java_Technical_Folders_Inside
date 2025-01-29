@Library('TestUtils') _

/**
 * Starts a docker container with a DB2 database, waits for user confirmation and afterwards stops the database.
 * 
 * @param imageTag  The tag of the docker image
 */

nodeTF('Docker-host && Region-EU') {
	
	timestamps {
		
        def dockerUtils = new DockerUtils()
        def miscUtils = new MiscUtils()
        
        buildName "#${env.BUILD_ID} - ${imageTag}"
        docker.image(dockerUtils.pullDockerDb2Image(imageTag)).withRun('-p 50000') {
            container ->
            stage('Wait until DB is ready') {
                dockerUtils.waitForDb2(container.id, 5)
            }
            
            stage('wait-for-user') {
                def port = dockerUtils.getActualHostPort50000(container.id)
                def hostname = miscUtils.getHostname()
                input "DB is ready and listening on port ${port}. DB is running on ${hostname}. Proceed here to shut it down."
            }
        }
	}
}