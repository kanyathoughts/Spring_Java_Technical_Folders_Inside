@Library('TestUtils') _

/**
 * Job script to prepare a soa-connector environment that can be used for testing soa-connector funtionalities.
 * 
 * @param mxBuildVersion 	The maxenso/innowake build to test.
 * @param javaVersion 		The java version the environment will run with
 * @param executeOn 		The Jenkins node the job will run on
 */

node(executeOn) {
	def svnUtils = new SvnUtils()
	def gitUtils = new GitUtils()
	def mxVersionUtils = new MxVersionUtils()
	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def spUtils = new SharepointUtils()
	
	def jobWorkspace = pwd()
	def dbPort = '5444'
	def dbContainerName = 'sc_environment_db'
	def scContainerName = 'sc_environment_soa'
	def dbVolumeName = "sc_environment_db_volume"
	// Save host name in the environment variables
	env.scEnvironmentHost = miscUtils.getHostname()
	env.scEnvironmentNode = env.NODE_NAME
	
	buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	buildDescription "mxBuildVersion=${mxBuildVersion} javaVersion=${javaVersion} scEnvironmentNode=${env.scEnvironmentNode}"
	
	stage('Remove old docker containers and cleanup workspace') {
		sh returnStatus: true, script: "docker rm -f ${dbContainerName}"
		sh returnStatus: true, script: "docker rm -f ${scContainerName}"
		sh returnStatus: true, script: "docker volume rm ${dbVolumeName}"
		deleteDir()
	}
	
	docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
		stage('Checkout and copy files') {
			dir('request-server'){
				spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'request-server.zip', "${jobWorkspace}/request-server")
			}
			spUtils.downloadAndExtractJobArtifact('IW_Copy_MxBuildVersion', mxBuildVersion, 'soa-server.zip', jobWorkspace)
			
			withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'svnPw', usernameVariable: 'svnUser')]) {
				svnUtils.svnExportRecursive("${svnUtils.getSvnUrlQa()}/projects/sc/sc_environment", jobWorkspace, svnUser, svnPw)
			}
			gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), "${jobWorkspace}/soa-server")
		}
		
		stage('Copy eclipse bundle') {
			spUtils.downloadIwEclipseLinux(mxBuildVersion, "${jobWorkspace}/deployment-server/eclipse")
		}
		
		stage('Configure environment') {
			sh 'tar -zxvf workspace.tar.gz'
			sh 'chmod ugo+x *.sh'
			
			// Remove com.collabnet folders and jars to prevent svn exception. See https://confluence.innowake.hq/display/KB/LCM+Troubleshooting
			// -> SVNException by revert Function of SubclipseTeamProvider
			sh "rm -f ${jobWorkspace}/deployment-server/eclipse/plugins/com.collabnet.subversion.merge_4.1.0.jar"
			sh "rm -rf ${jobWorkspace}/deployment-server/eclipse/features/com.collabnet.subversion.merge.feature_4.1.0"
			// For Eclipse 4.9 and above following files have to be deleted
			sh "rm -f ${jobWorkspace}/deployment-server/eclipse/plugins/org.tigris.subversion.subclipse.ui_4.2.4.201804261743.jar"
			sh "rm -f ${jobWorkspace}/deployment-server/eclipse/plugins/com.collabnet.subversion.merge_4.2.0.1.jar"
			sh "rm -f ${jobWorkspace}/deployment-server/eclipse/plugins/org.tigris.subversion.subclipse.ui_1.10.11.jar"
			
			//Replace classpath, working directory, hostname, port in SC configuration
			def strEquinoxJar = sh(script: "sed -nr '/org.eclipse.equinox.launcher_/p' ${workspace}/deployment-server/eclipse/eclipse.ini", returnStdout: true).trim()
			sh "sed -i \"s|plugins/org.eclipse.equinox.launcher_.*.jar|${strEquinoxJar}|g\" ${jobWorkspace}/soa-server/data/config/ServiceDef.dat"
			sh "rm -f ${jobWorkspace}/request-server/lib/lcm-core-hypersonic-${mxBuildVersion}.jar"
			sh "rm -f ${jobWorkspace}/request-server/lib/lcm-core-mysql-${mxBuildVersion}.jar"
			sh "rm -f ${jobWorkspace}/request-server/lib/lcm-core-oracle-${mxBuildVersion}.jar"
			sh "rm -f ${jobWorkspace}/request-server/lib/lcm-core-sqlserver-${mxBuildVersion}.jar"
			def changeCPoutput = ''
			def lsOutput= sh returnStdout: true, script: "ls  ${jobWorkspace}/request-server/lib/*.jar"
			def files = lsOutput.split("\\r?\\n")
			for (f in files) {
				f = f.split("lib/")[1]
				if (changeCPoutput.isEmpty()) {
					changeCPoutput="${changeCPoutput}./lib/${f}"
				} else {
					changeCPoutput="${changeCPoutput}:./lib/${f}"
				}
			}

			sh "sed -i \"s|-ClasspathToReplace|${changeCPoutput}|g\" ${jobWorkspace}/soa-server/data/config/ServiceDef.dat"
			sh "sed -i \"s|/var/lib/jenkins/workspace/SC_Environment|${jobWorkspace}|g\" ${jobWorkspace}/soa-server/data/config/ServiceDef.dat"
			
			sh "sed -i \"s|qmsrv1.innowake.hq|${env.scEnvironmentHost}|g\" ${jobWorkspace}/soa-server/data/config/ServiceDef.dat"
			sh "sed -i \"s|qmsrv1.innowake.hq|${env.scEnvironmentHost}|g\" ${jobWorkspace}/soa-server/data/config/Server.dat"
			sh "sed -i \"s|qmsrv1.innowake.hq|${env.scEnvironmentHost}|g\" ${jobWorkspace}/workspace/.metadata/.plugins/org.eclipse.core.runtime/.settings/innowake.ndt.lcm.prefs"
			sh "sed -i \"s|5432|${dbPort}|g\" ${jobWorkspace}/soa-server/data/config/ServiceDef.dat"
		}
	}
	
	stage('Start docker containers') {
		sh "docker volume create ${dbVolumeName}"
		withCredentials([usernamePassword(credentialsId: 'SC_Environment_DB_user', passwordVariable: 'dbPw', usernameVariable: 'dbUser')]) {
			docker.image('postgres:9.6.10').run("--name ${dbContainerName} -e POSTGRES_PASSWORD=${dbPw} -e POSTGRES_DB=postgres -e POSTGRES_USER=${dbUser} -p ${dbPort}:5432 -v ${jobWorkspace}/init.sql:/docker-entrypoint-initdb.d/init.sql -v ${dbVolumeName}:/var/lib/postgresql/data")
		}
		def userId = miscUtils.getUserID()
		def groupId = miscUtils.getGroupID()
		docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).run("--user ${userId}:${groupId} --name ${scContainerName} -p 55004:55004 -p 55005:55005 -p 55007:55007 -p 55008:55008 -v ${jobWorkspace}:${jobWorkspace} --entrypoint ${jobWorkspace}/manage-soa-server.sh", "start ${jobWorkspace}")
	}
}
