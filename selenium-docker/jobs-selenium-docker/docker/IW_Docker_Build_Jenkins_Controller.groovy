@Library('TestUtils') _

/**
 * Build the docker image "jenkins-controller" from a Dockerfile and upload it to the QM registry.
 * 
 * @param jenkinsLevel  	what kind of Jenkins (dev or operative) shall be built
 * @param jenkinsVersion	which Jenkins version shall be used
 */
node('Docker-host') {
	def gitUtils = new GitUtils()
    def relativeGitProjectPath = 'infrastructure/environment/jenkins-controller.git'
	def dockerUtils = new DockerUtils()
	
	timestamps {
		stage('build-and-push') {
			deleteDir()
			buildName "#${BUILD_NUMBER} - ${jenkinsLevel} - ${jenkinsVersion}"
			docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside {
				withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
					sh "git clone --branch ${jenkinsVersion} ${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/${relativeGitProjectPath}"
				}
			}

			dir('jenkins-controller') {
				sh "docker-compose -f ${jenkinsLevel}/docker-compose.yml build"
				docker.withRegistry(dockerUtils.getEuDockerRegistryUrl(), 'EU-Docker-Registry-User') {
					docker.image(dockerUtils.getJenkinsControllerImageName(jenkinsLevel, jenkinsVersion)).push()
				}
			}
		}
	}
}