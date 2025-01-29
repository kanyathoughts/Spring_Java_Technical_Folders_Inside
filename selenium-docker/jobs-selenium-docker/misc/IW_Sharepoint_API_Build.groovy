@Library('TestUtils') _

/**
 * Build the sharepoint API
 * @param branch  The branch of the Sharepoint API that will be built and uploaded to Nexus. Default: master.
 */
 
node('Docker-host') {
	
	timestamps {
		def gitUtils = new GitUtils()
		def dockerUtils = new DockerUtils()
		def spUtils = new SharepointUtils()
	    
        def remoteProjectLocation = 'infrastructure/sharepoint-api.git'
        
   	    buildName "#${env.BUILD_ID} - ${branch}"
   		
		deleteDir()
		docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
			def workDir = pwd()
			
			stage('init') {
                gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", branch)
			}
			
			stage('build') {
				withMaven(maven: 'Default') {
					sh "$MVN_CMD -U dependency:resolve"
					withCredentials([usernamePassword(credentialsId: 'USAppModQMUserSVC-at-deloitte-com-User-PW', passwordVariable: 'password', usernameVariable: 'username')]) {
						def commonParams = "-Dusername=\"${username}\" -Dpassword=\"${password}\" -Ddomain=\"${spUtils.getDomain()}\" -DsharepointSiteUrl=\"${spUtils.getSiteUrl()}\""
						sh "$MVN_CMD clean deploy ${commonParams}"
					}
				}
			}
		}
	}
	
}
