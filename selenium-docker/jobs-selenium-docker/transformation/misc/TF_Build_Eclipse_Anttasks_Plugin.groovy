@Library('TestUtils') _

/**
 * Build the tool-innowake-eclipse-anttasks-plugin
 * 
 * @param branchName The project branch that will be cloned
 *        type: git branch
 * @param withComSunXmlBindLibrary If checked the job will add com.sun.xml.bind.v2 to the MANIFEST.MF file. This is required for version 19 and higher.
 *        type: Boolean
 */
 
nodeTF('Docker-host') {
	
	timestamps {
		
		def svnUtils = new SvnUtils()
		def gitUtils = new GitUtils()
		def dockerUtils = new DockerUtils()
   		def workDir 
   		
   		buildName "#${env.BUILD_ID} - branchName=${branchName}"
		buildDescription "withComSunXmlBindLibrary=${withComSunXmlBindLibrary}"
   		
		deleteDir()
		docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside {
			workDir = pwd()
			
			stage('setup-workspace') {
				def remoteProjectLocation = 'infrastructure/transformation/tool-innowake-eclipse-anttasks-plugin.git'
				gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", branchName)
				withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'svnPw', usernameVariable: 'svnUser')]) {
					svnUtils.svnExportFiles("${svnUtils.getSvnUrlQaBaseLib()}/eclipse-SDK-4.15-linux-gtk-x86_64.tar.gz", workDir, svnUser, svnPw)
				}
				if (Boolean.parseBoolean(withComSunXmlBindLibrary)) {
					sh "sed -i \"s|org.eclipse.debug.ui|org.eclipse.debug.ui,\\n com.sun.xml.bind.v2|g\" META-INF/MANIFEST.MF"
				}
			}
			
			stage('build-plugin') {
				withAnt(installation: 'Default') {
					sh 'ant -buildfile build.xml plugin'
				}
			}
		}
		
		stage('copy-artifacts') {
			archiveArtifacts 'plugin/*'
		}
	}
}