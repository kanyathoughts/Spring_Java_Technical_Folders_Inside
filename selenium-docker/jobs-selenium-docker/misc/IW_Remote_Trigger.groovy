 @Library('TestUtils') _
/**
 * This job is intended to be triggered from a remote Jenkins instance
 * This happens every night if the PD build was successfull.
 *
 * The job is used to copy the build the eclipse bundle from the mxBuildVersion
 * we get from the calling job. After that any available test can be triggered.
 *
 * @param mxBuildVersion maxenso/innowake version. Passed by the calling job
 * @param rebuild_number only used in case of mining rebuild. Passed by the calling job
 * @param skipCopyEclipse If you want to trigger all tests again, but save time, you can 
 * check this option and skip building the eclipse once again.
 */
 
node('OS-Linux' && 'Docker-host') {
	
	buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
    buildDescription "mxBuildVersion=${mxBuildVersion} rebuild_number=${rebuild_number} skipCopyEclipse=${skipCopyEclipse}"

    def mxVersionUtils = new MxVersionUtils()
	def miscUtils = new MiscUtils()
	def spUtils = new SharepointUtils()
	def dockerUtils = new DockerUtils()
	def artifactDir = spUtils.getJenkinsArtifactsDir()
	def artifactVersion = ''
	def javaVersion = 'java11'
	if (! rebuild_number.isEmpty()) {
		artifactVersion = "${mxBuildVersion}-${rebuild_number}"
	}
	skipCopyEclipse = Boolean.parseBoolean(skipCopyEclipse)
	artifactDir = "${artifactDir}/IW_Copy_MxBuildVersion/${artifactVersion ?: mxBuildVersion}"
	def mailList = miscUtils.getMailReceivers(currentBuild.projectName)
	def artifactPrefix = mxVersionUtils.hasMaxensoInArtifactName(mxBuildVersion) ? 'maxenso' : 'innowake'

	stage('copy build') {
		if (! skipCopyEclipse) {
			def buildResult = build job:'IW_Copy_MxBuildVersion', propagate: false, parameters:[string(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'rebuild_number', value: rebuild_number)]
			miscUtils.evaluateBuildResult(buildResult)
		}
	}

	docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
		if (mxBuildVersion.contains('alpha')) {
			echo 'trunk (alpha) version detected'
			def eclipseArtifactsExist = spUtils.fileExists("${artifactDir}/${artifactPrefix}-all-plugins.zip")
			def miningArtifactsExist = spUtils.fileExists("${artifactDir}/mining-api-server-dist-${artifactVersion ?: mxBuildVersion}.jar")

			if (eclipseArtifactsExist) {
				if (rebuild_number.isEmpty()) {
					parallel DM_Test_Suite: {
						if(miningArtifactsExist) {
							miscUtils.startTestSuite('DM_Test_Suite', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'deploy', value: true), string(name: 'rebuild_number', value: rebuild_number)])
						}	
					}, LD_Test_Suite: {
						miscUtils.startTestSuite('LD_Test_Suite', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name:'javaVersion', value: 'java11')])
					}, TF_Test_Suite: {
						if (! miscUtils.isJobAlreadyRunningWithVersion('TF_Test_Suite', mxBuildVersion)) {
							miscUtils.startTestSuite('TF_Test_Suite', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: 'java11')])
						}
					}, SUP_Test_Suite: {
						miscUtils.startTestSuite('SUP_Test_Suite', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion)])
					}
				} else {
					if (miningArtifactsExist) { 
						miscUtils.startTestSuite('DM_Test_Suite', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'deploy', value: false), string(name: 'rebuild_number', value: rebuild_number)])
					}
				}
			} else {
				echo 'eclipse artifacts do not exist, triggering TF_Test_Suite anyways'
				if (! miscUtils.isJobAlreadyRunningWithVersion('TF_Test_Suite', mxBuildVersion)) {
					miscUtils.startTestSuite('TF_Test_Suite', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: 'java11')])
				}
			}
		} else if (mxBuildVersion.contains('beta')) {
			echo 'beta version detected'
		} else {
			echo 'hotfix version detected'
			def subj = "build ${currentBuild.number} of ${currentBuild.projectName}"
			def body = "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion} Hotfix tests were triggered by the Remote_trigger job."
			emailext subject: subj, body: body, to: mailList
			def branchVar = mxBuildVersion.tokenize('\\.')[0] as Integer
			if (branchVar >= 21) {
				miscUtils.startTestSuite('LD_Complete_Hotfix_Execution', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: 'java11')])
			} else {
				miscUtils.startTestSuite('LD_Complete_Hotfix_Execution', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: 'java8')])
			}
    	}
	}
}
