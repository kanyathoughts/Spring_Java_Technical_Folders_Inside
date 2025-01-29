@Library('TestUtils') _
/**
 * This job restores the mining db backup into a specified mining environment. 
 * This will override all existing mining meta data (e.g. Annotations and Data Dictionary entries).
 *
 * @param executeOn		The node where to import the mining db restore.
 * @withMetadata        The boolean parameter if set then db dump with metadata restored
 */

node(executeOn) {
	
	timestamps {
	
    buildName "#${env.BUILD_ID} - ${executeOn}"
	withMetadata = Boolean.parseBoolean(withMetadata)
    buildDescription "executeOn=${executeOn} withMetadata=${withMetadata}"
	
    def jobWorkspace = pwd()
    def miscUtils = new MiscUtils()
	def dockerUtils = new DockerUtils()
	def gitUtils = new GitUtils()
	def executeEnv = executeOn
	def env = "/data/jenkins/prod/${executeEnv}"
	def jobDir = "${env}/workspace/DM_Docker_Start_Mining_Env"
	def dockerDir = "${jobDir}/docker"
    def orientDbName = 'orient-db'
    def apiServerName = 'api-server'	
  
    stage('stop server') {
        sh returnStatus: true, script: "docker stop ${apiServerName}"
    }

    stage('copy db backup and remove /orientDb') {
        deleteDir()
        sh "cp /tmp/backup_Txdmv.zip ${jobWorkspace}/backup_Txdmv.zip"
		sh "cp ${dockerDir}/backup_Txdmv_metadata.gz ${jobWorkspace}/backup_Txdmv_metadata.gz"
        sh "docker cp ${orientDbName}:/orientDb ${jobWorkspace}/orientDb"
        sh 'docker exec orient-db rm -rf /orientDb'
		sh returnStatus: true, script: "docker stop ${orientDbName}"
        sh "chmod -R 777 ${jobWorkspace}"
    }

    stage('restore mining db') {
        sh './orientDb/bin/console.sh \"connect plocal:orientDb/databases/mining admin admin;DROP DATABASE\"'
        sh 'rm -rf ./orientDb/databases/mining'
        sh './orientDb/bin/console.sh \"CREATE DATABASE plocal:orientDb/databases/mining admin admin\"'
		if (!withMetadata){
			sh "./orientDb/bin/console.sh \"connect plocal:orientDb/databases/mining admin admin;RESTORE DATABASE ${jobWorkspace}/backup_Txdmv.zip;CREATE INDEX Module_name_ft ON Module(name) FULLTEXT ENGINE LUCENE METADATA { \"allowLeadingWildcard\": true, \"default\": \"innowake.mining.data.plugin.lucene.LowercasingKeywordAnalyzer\" };\"" 
        } else {
			sh "./orientDb/bin/console.sh \"connect plocal:orientDb/databases/mining admin admin;RESTORE DATABASE ${jobWorkspace}/backup_Txdmv_metadata.gz;\"" 
 
		}
		sh "chmod -R 777 ${jobWorkspace}"
        sh "docker cp ${jobWorkspace}/orientDb/ ${orientDbName}:/orientDb/"
    }

    stage('restart orientdb and start api server') {
        sh "docker restart ${orientDbName}"
        sleep 60
        sh "docker start ${apiServerName}"
        sleep 50
    }

    stage('create metadata') {   
        build job: 'DM_Feature_Toggle', wait: true, propagate: false, parameters: [
			string(name: 'feature', value: 'legacyWebViewsInEclipse'), 
			string(name: 'switchOnOrOff', value: 'On'), 
			[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeOn], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
		if (!withMetadata){
			def miningServer = getMiningServerURL(executeOn)
			build job: 'DM_Metadata_Creation', wait: true, propagate: false, parameters: [string(name: 'miningServerURL', value: miningServer)]
		}
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
