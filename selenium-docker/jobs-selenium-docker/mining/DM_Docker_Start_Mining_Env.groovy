import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
@Library('TestUtils') _
/**
 * This job creates a dockerized Mining environment. Therefore, following parameters have to be specified.
 *
 * @param mxBuildVersion     	The build to use.
 * @param executeOn          	The node where to run the environment.
 * @param APIServer         	The port where the API-Server should listen to.
 * @param OrientDBStudio   		The port where the OrientDB Studio should listen to.
 * @param OrientDBbin    		The port where the OrientDB should listen to (binary).
 * @param withKeycloak    		Specifies whether the environment shall be started without keycloak connection
 * @param keyCloakPort       	The port on which the keycloak container runs.
 * @param javaVersion			The java version the test will run with
 * @param orientdbHeapSizeArgs  The default heap size arguments of OrientDB inside server.bat
 */

node(executeOn) {

 timestamps {
    def jobWorkspace = pwd()
    def gitUtils = new GitUtils()
    def spUtils = new SharepointUtils()
    def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def dockerDir = "${jobWorkspace}/docker"
	def gitDirLicenses = "${jobWorkspace}/gitLicenses"
    def originalBuildVersion = mxBuildVersion 
    def artifactDir = "${spUtils.getJenkinsArtifactsDir()}/IW_Copy_MxBuildVersion/${mxBuildVersion}"
    def featuresToBeEnabled = ['eclipseDeepLink']
    def orientDbName = 'orient-db'
    def apiServerName = 'api-server'
	def serverIPToRunOn = miscUtils.getIpByNodeName(executeOn)
	
	withKeycloak = Boolean.parseBoolean(withKeycloak)
    buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
    buildDescription "mxBuildVersion=${mxBuildVersion} executeOn=${executeOn} APIServer=${APIServer} OrientDBStudio=${OrientDBStudio} OrientDBbin=${OrientDBbin} withKeycloak = ${withKeycloak}"

    stage('stop and remove running containers') {
        sh returnStatus: true, script: "docker rm -f ${orientDbName}"
        sh returnStatus: true, script: "docker rm -f ${apiServerName}"
    }

    stage('create keycloak instance') {
	    if (withKeycloak) {
            build job: 'DM_Docker_Start_Keycloak_Instance', wait: true, propagate: false, parameters: [
				string(name: 'mxBuildVersion', value: originalBuildVersion),
				[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeOn], nodeEligibility: [$class: 'AllNodeEligibility']]
			]
        } else {
        	Utils.markStageSkippedForConditional('create keycloak instance')
        }
    }

    stage('get selected build and checkout docker files') {
        deleteDir()
       	def gitlabProjectDbDumps
		def gitlabProjectLicense
		docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside {
            withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
       	        gitlabProjectDbDumps = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/mining-db-dumps.git"
            	gitlabProjectLicense = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/licenses.git"
            }
			dir(dockerDir) {
			    sh "git clone --depth 1 --branch master ${gitlabProjectDbDumps} ${dockerDir}"
			}
			dir(gitDirLicenses) {
				sh "git clone --depth 1 --branch master ${gitlabProjectLicense} ${gitDirLicenses}"
			}			
			if (! spUtils.downloadFile("${artifactDir}/orientdb-mining-${mxBuildVersion}.zip", './')) {
	                error "Download of ${artifactDir}/orientdb-mining-${mxBuildVersion}.zip failed"
	        }
	        if (! spUtils.downloadFile("${artifactDir}/mining-api-server-dist-${mxBuildVersion}.jar", './')) {
	            error "Download of ${artifactDir}/mining-api-server-dist-${mxBuildVersion}.jar failed"
	        }
		}
    }

    stage('get orientDB binaries from latest build') {
        sh "unzip ./orientdb-mining-${mxBuildVersion}.zip -d ${jobWorkspace}/orientDb"
        // Add test-data statement from job workspace to /bin which will be mounted into the oriendtb container
        sh "cp ${jobWorkspace}/docker/backup_Txdmv.zip ${jobWorkspace}/orientDb/bin"
        sh "chmod -R 777 ${jobWorkspace}/orientDb"
        sh "chmod -R 777 ${jobWorkspace}/docker"
        //copy the db dump to /tmp for the DM_Docker_Restore_DB_Backup job
        sh "cp ${jobWorkspace}/docker/backup_Txdmv.zip /tmp/backup_Txdmv.zip"
    }

    stage('orientDB Container setup and restore data') {
        sh "docker create --name ${orientDbName} -p ${OrientDBbin}:2424 -p ${OrientDBStudio}:2480 -e \"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/orientDb/bin\" --entrypoint /orientDb/bin/server.sh " + dockerUtils.pullJenkinsEnvironmentImage(javaVersion)
        sh "./orientDb/bin/console.sh \"connect plocal:orientDb/databases/mining admin admin; DROP DATABASE; CREATE DATABASE plocal:orientDb/databases/mining admin admin; RESTORE DATABASE ${jobWorkspace}/orientDb/bin/backup_Txdmv.zip; CREATE INDEX Module_name_ft ON Module(name) FULLTEXT ENGINE LUCENE METADATA { \"allowLeadingWildcard\": true, \"default\": \"innowake.mining.data.plugin.lucene.LowercasingKeywordAnalyzer\" };\""
        sh "docker cp ${jobWorkspace}/orientDb ${orientDbName}:/orientDb"
		sleep 60
    }

    stage('start orientdb') {
        if (orientdbHeapSizeArgs != 'MAXHEAP=-Xms2G -Xmx4G') {
		    echo "${jobWorkspace}/orientDb/bin/server.bat"
			sh "sed -i 's/MAXHEAP=-Xms2G -Xmx4G/MAXHEAP=-Xms4G -Xmx16G/g' ${jobWorkspace}/orientDb/bin/server.bat"
		}
        sh "docker start ${orientDbName}"
		sleep 60
    }

    stage('setup and run api server') {
        def startCommand = "nohup java -jar -Dserver.port=8080 -Djava.awt.headless=true -Dspring.datasource.url=jdbc:orient:remote:${serverIPToRunOn}:${OrientDBbin}/mining ./mining-api-server-dist.jar --mining.cookieId=DISABLED"
        if (withKeycloak) {
            startCommand = startCommand + " --spring.profiles.active=authorized-access --keycloak.auth-server-url=http://${serverIPToRunOn}:8180/auth migrateToKeycloak --username=first --password=first --debug"
        }
        sh "cp ./mining-api-server-dist-${mxBuildVersion}.jar ${jobWorkspace}/mining-api-server-dist.jar"
        sh "chmod 777 ${jobWorkspace}/mining-api-server-dist.jar"
        
        docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).run("--name ${apiServerName} -p ${APIServer}:8080 -v ${jobWorkspace}/mining-api-server-dist.jar:/mining-api-server-dist.jar -v ${gitDirLicenses}/mining/innowake.lic:/innowake.lic", "bash -c \"${startCommand}\"")
        sleep 120
		echo '-----Check if api-server container was launched correctly-----'
		checkAPIServer()
    }

	stage('create user data') {
	    if (withKeycloak) {
			sh 'docker exec keycloak-server sh /opt/jboss/tools/insertUserData.sh'
		} else {
        	Utils.markStageSkippedForConditional('create user data')
        }
	}
		
	stage('enable features') {
		featuresToBeEnabled.push ('legacyWebViewsInEclipse')
			featuresToBeEnabled.each {
				feature ->
				echo "Enabling '${feature}'"
				build job: 'DM_Feature_Toggle', wait: true, propagate: false, parameters: [
					string(name: 'feature', value: feature), 
					string(name: 'switchOnOrOff', value: 'On'), 
					[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeOn], nodeEligibility: [$class: 'AllNodeEligibility']]
				]
				sleep 10
			}
	}
	
	stage('create metadata') {
		def miningServer = getMiningServerURL(executeOn)
        build job: 'DM_Metadata_Creation', wait: true, propagate: false, parameters: [string(name: 'miningServerURL', value: miningServer)]
    }
 }
}

/**
 * Returns the mining server URL of a given mining server. 
 * 
 * @param miningServerURL The URL to the mining server.
 * @return miningServer The server name from the given URL.
 */
def getMiningServerURL(executeOn) {
	switch(executeOn) {
		case 'USLinux1':
			return 'http://qef-linux1-us.deloitte.com:8080/'
		case 'USLinux3':
			return 'http://qef-linux3.deloitte.com:8080/'
		case 'USLinux5-DM':
			return 'http://qef-linux5-us-dm.deloitte.com:8080/'
		case 'USLinux6-DM':
			return 'http://qef-linux6-us-dm.deloitte.com:8080/'
		case 'USLinux7-DM':
			return 'http://qef-linux7-us-dm.deloitte.com:8080/'
		case 'USLinux8-DM':
			return 'http://qef-linux8-us-dm.deloitte.com:8080/'
	}
}

/**
* Returns a list of regex matches, or an empty list if no matches are found
*/
def List getRegexMatch(String line, java.util.regex.Pattern pattern) {
	return (line =~ pattern) ? (line =~ pattern)[0] : []
}

/**
* checks if the api-server container was launched correctly
*/
def checkAPIServer() {
	def linuxServer = getMiningServerURL(executeOn)
	def match = getRegexMatch(linuxServer, ~/\/\/(.*)\//)
	def host = match.get(1).tokenize(':')[0].trim()
	def port = match.get(1).tokenize(':')[1].trim()
	/**
	* nc : command runs Netcat, a utility for sending raw data over a network connection
	* -d : do not attempt to read from stdin
	* -z : only listen, don't send data
	* -w : timeout. Connections that cannot be established or are idle timeout after timeout seconds
	* &> /dev/null : don't print out any output of the command
	*/
	def status = sh returnStatus: true, script: "nc -d -z -w 10 ${host} ${port}"
	if(status != 0) {
		error "!!!The api-server container was not launched correctly or took more time than expected!!!"
	}
}