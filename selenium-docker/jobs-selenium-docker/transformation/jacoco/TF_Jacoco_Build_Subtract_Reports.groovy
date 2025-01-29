@Library('TestUtils') _

/**
 * Builds an executable jar file from the Jacoco-tools
 */
 
nodeTF('Docker-host') {
	
	timestamps {
		def svnUtils = new SvnUtils()
		def dockerUtils = new DockerUtils()
	    
		try {
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
				stage('init') {
					withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'svnPw', usernameVariable: 'svnUser')]) {
						svnUtils.svnExportRecursive("${svnUtils.getSvnUrlQa()}/tools/jacoco-tools", '.', svnUser, svnPw)
					}
				}
				
				stage('build') {
					withMaven(maven: 'Default') {
						sh '$MVN_CMD --debug -U dependency:resolve'
					}
					withAnt(installation: 'Default') {
						sh 'ant -buildfile build.xml build-jar'
					}
				}
			}
		} finally {
			stage('Archive artifacts') {
				archiveArtifacts '*.jar'
			}
		}

	}

}
