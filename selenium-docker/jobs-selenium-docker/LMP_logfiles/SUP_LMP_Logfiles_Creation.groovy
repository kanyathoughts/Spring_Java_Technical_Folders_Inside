@Library('TestUtils') _

/**
 * Job script to generate logfiles for LMP Prod instance.
 */
node('USLinux1') {
	def miscUtils = new MiscUtils()
	 
	stage('cleanup') {
		deleteDir()
	}
    
	stage('generate-logfiles') {
		try {
			sh "docker logs lmp_web_prod > lmp_web_logfile"
			sh "docker logs lmp_ui_prod > lmp_ui_logfile"
			archiveArtifacts artifacts: 'lmp_web_logfile'
			archiveArtifacts artifacts: 'lmp_ui_logfile'
			echo "Log files copied"
		} catch(e) {
			unstable "Create_LMP_Logfiles job is unstable, logfiles not copied from LMP"
			def subj = "build ${currentBuild.number} of ${currentBuild.projectName} is unstable"
			def body = "Create_LMP_Logfiles job is unstable, the LMP containers are not running so please restart the "Start_LMP_Prod" job. Logfiles not generated see ${currentBuild.absoluteUrl}"
			def to = miscUtils.getMailReceivers(currentBuild.projectName)
			emailext subject: subj, body: body, to: to
		}
	}
}