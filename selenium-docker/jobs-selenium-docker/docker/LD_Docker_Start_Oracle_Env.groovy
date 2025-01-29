@Library('TestUtils') _

/**
* Creates and starts a docker container with an Oracle database. 
* It also copies the UDF-jar file from mxJars and uploads to the oracle container
*
* @param mxBuildVersion 	The maxenso/innowake build to test.
* @param imageTag           The tag of the docker image
* @param javaVersion        The java version the test will run with
* @param executeOn 		    The Jenkins node the job will run on
*/

node(executeOn) {
    timestamps{
        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "mxBuildVersion=${mxBuildVersion}"

        def miscUtils = new MiscUtils()
        def dockerUtils = new DockerUtils()
        def jobWorkspace = pwd()
	    def mxJarsDir = "/data/mxJars/${mxBuildVersion}"
        def euDockerRegistryHostAndPort = dockerUtils.getEuDockerRegistryHostAndPort()
        def oracleDB = 'oracleDB'

        stage('clean-up-workspace') {
            sh returnStatus: true, script: "docker rm -f ${oracleDB}"
            deleteDir()
        }

        try{
           docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                    stage('copy-UDF-jar') {
                        def artifactPrefix = 'innowake-mee-udf-oracle5'
	                    def groupId = 'innowake.products.mee.runtime.natural.datastore' 
	                    withMaven(maven: 'Default', mavenSettingsConfig: 'linux_maven_settings') {
		                    sh "$MVN_CMD dependency:copy -Dartifact=${groupId}:${artifactPrefix}:${mxBuildVersion} -DoutputDirectory=${jobWorkspace}"
	                    }                   
                        dir(jobWorkspace) {
		                    sh "mv ${artifactPrefix}-${mxBuildVersion}.jar ${artifactPrefix}.jar"
	                    }
                    }
            }
            
            stage('Start Oracle Container') {
                sh "docker run -d --name ${oracleDB} -p 1521 " + dockerUtils.pullDockerOracleImage(imageTag)
                sleep 60
                env.oraclePort = getActualHostPort1521(oracleDB)
                env.oracleHost = miscUtils.getHostname()
                echo "DB is ready and listening on port ${env.oraclePort}. DB is running on ${env.oracleHost}."
            }

            stage('upload-UDF-jar') {
                sh "docker cp ${jobWorkspace}/innowake-mee-udf-oracle5.jar ${oracleDB}:/home/oracle"
            
                def credentials = dockerUtils.getCredentials(imageTag)
                def user = credentials.get(0)
                def pass = credentials.get(1)
                def dbConnectString = "${user}/${pass}@ORCLPDB1"
                    
				sh "docker exec ${oracleDB} sh dropjava -user ${dbConnectString} -jarsasdbobjects /home/oracle/innowake-mee-udf-oracle5.jar"
                sh "docker exec ${oracleDB} sh loadjava -user ${dbConnectString} -jarsasdbobjects /home/oracle/innowake-mee-udf-oracle5.jar"
            }
        } catch (ex) {
			miscUtils.errorWithStackTrace(ex)
        }
    }
}


/**
 * Gets the host port of a running container for Oracle port 1521.
 * 
 * @param containerId The ID of the container
 * @return the port number
 */
def getActualHostPort1521(containerId) {
	echo 'Trying to find out on which port the docker container listens'
	def portCmd = "docker port ${containerId} 1521"
	def stdout = sh returnStdout: true, script: portCmd
	def port = stdout.split('\n')[0].split(':')[1].trim()
	echo "Docker Container listens on port ${port}"
	return port
}