@Library('TestUtils') _

/**
 * Build the docker image "jenkins-environment" from a Dockerfile and upload it to the QM registry.
 * 
 * @branch The jenkins-environment branch
 * @javaVersion The Java version to build a Docker image for
 * @imageSuffix Suffix of the image to build
 */

node('Docker-host') {
    timestamps {
		def gitUtils = new GitUtils()
        def dockerUtils = new DockerUtils()
        def imageName = dockerUtils.getJenkinsEnvironmentImageName(javaVersion) + imageSuffix + (branch != 'master' ? "-${branch}" : '')
		def relativeGitProjectPath = 'infrastructure/environment/jenkins-environment.git'
		
		stage('checkout and build jenkins environment') {
            buildName "#${BUILD_NUMBER} - ${branch} - ${javaVersion}"
			buildDescription "imageSuffix=${imageSuffix}"
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside {
                withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
                    sh "git -c http.sslVerify=false clone --branch ${branch} ${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/${relativeGitProjectPath}"
                }
            }

			dir('jenkins-environment') {
				docker.build(imageName, "-f ${javaVersion}/Dockerfile${imageSuffix} .")
                sh 'docker image ls'
            }
		}
		
        stage('push') {
			docker.withRegistry(dockerUtils.getEuDockerRegistryUrl(), 'EU-Docker-Registry-User') {
				docker.image(imageName).push()
			}
		}
	}
}