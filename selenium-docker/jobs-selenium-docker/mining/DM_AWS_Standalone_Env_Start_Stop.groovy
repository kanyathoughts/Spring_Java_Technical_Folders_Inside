import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
@Library('TestUtils') _
/*
 * This job spins up or shuts down a standalone AWS instance for Mining.
 *
 * @param awsAction         "Start" or "Stop"
 * @param mxBuildVersion    The build to use.
 * @param importBackupTxdmv The database dump to import
 * @param largeDbInstance	Boolean to spin up larger instance launch template
 * @param startApiServer	Boolean to start the api server service
 */

node('USLinux1') {
    def jobWorkspace = pwd()
	def miscUtils = new MiscUtils()
	def spUtils = new SharepointUtils()
	def dockerUtils = new DockerUtils()
    def gitUtils = new GitUtils()
    def licenseDir = "${jobWorkspace}/licenses"
    def backupDir = "${jobWorkspace}/backups"
	def artifactDir = "${spUtils.getJenkinsArtifactsDir()}/IW_Copy_MxBuildVersion/${mxBuildVersion}"
	def awsCmd = "docker run --rm -v aws_cli_config:/root/.aws amazon/aws-cli --no-verify-ssl"
	def awsCmdProfile = "${awsCmd} --profile AssumeRole"
	def orientDbIp
	def orientDbId
	def orientDbDns
	def apiServerIp
	def apiServerId
	def apiServerDns
	def remote
	
    buildName "#${env.BUILD_ID} - ${awsAction}"
    buildDescription "mxBuildVersion=${mxBuildVersion} importBackupTxdmv=${importBackupTxdmv}"

	def cleanUpAwsEnv = {
	    echo 'Environment Cleanup triggered'
		try {			
			// Terminate orientdb server if it was started
			if (orientDbId) {
				// Clean up EFS, delete api server jar
				if (remote) {
					withCredentials([sshUserPrivateKey(credentialsId: 'AWS-ssh-key', keyFileVariable: 'identity', passphraseVariable: '', usernameVariable: 'userName')]) {
						remote.user = userName
								remote.identityFile = identity
								sshRemove remote: remote, path: "/mnt/efs/server/standalone/mining-api-server-dist-${mxBuildVersion}.jar"
					}
				}
				terminateInstance(orientDbId, awsCmdProfile)
			}
			
			// Terminate API server if it was started
			if (apiServerId) {		
				terminateInstance(apiServerId, awsCmdProfile)
			}
			
			// Verify all EC2 instances have been shut down
			if (orientDbId) {
				echo 'Wait for OrientDb shutdown'
				waitForInstanceTermination(orientDbId, awsCmdProfile)
			}
			if (apiServerId) {
				echo 'Wait for API Server shutdown'
				waitForInstanceTermination(apiServerId, awsCmdProfile)
			}
			
		} catch (e) {
			error("AWS cleanup failed: ${e}")
		}
	}
	
	stage('Configure AWS CLI') {
		// Clean up previous environment
		deleteDir()
		
		// Initialize dockerized aws
		echo 'Initialize AWS CLI'
		echo sh(script: 'docker run --rm amazon/aws-cli --version', returnStdout: true).trim()
		
		// Create volume for AWS config
		// command does nothing if volume already exists
		sh 'docker volume create aws_cli_config'
		
		// Attempt to access profile AssumeRole, configure AWS if role doesn't exist
		try {
			sh "${awsCmd} configure --profile AssumeRole get region"
		} catch (e) {
			echo 'Configuring AWS profile'
			// Configure AWS profile
			withCredentials([usernamePassword(credentialsId: 'AWS-service-user', passwordVariable: 'awsSecretKey', usernameVariable: 'awsAccessKey')]) {
				sh "${awsCmd} configure set aws_access_key_id ${awsAccessKey}"
				sh "${awsCmd} configure set aws_secret_access_key ${awsSecretKey}"
			}
			sh "${awsCmd} configure set default.region eu-central-1"
			sh "${awsCmd} configure set default.output json"
			
			sh "${awsCmd} configure --profile AssumeRole set role_arn arn:aws:iam::068757897087:role/QEF-TestingRole"
			sh "${awsCmd} configure --profile AssumeRole set source_profile default"
			sh "${awsCmd} configure --profile AssumeRole set region eu-central-1"
		}
		
		echo sh(script: "${awsCmdProfile} sts get-caller-identity", returnStdout: true).trim()
	}
    
    stage('Check out Files') {
		if(awsAction == "Start") {
			def gitlabProject
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside {
				withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
		   	        gitlabProject = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/licenses.git"
	   	        }
			    dir(jobWorkspace) {
				    sh "git clone --depth 1 --branch master ${gitlabProject} ${licenseDir}"
				}
				if (importBackupTxdmv.toBoolean()) {			
			        withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
			   	        gitlabProject = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/mining-db-dumps.git"
			        }
					dir(jobWorkspace) {
					    sh "git clone --depth 1 --branch master ${gitlabProject} ${backupDir}"
					}
				}
				if (! spUtils.downloadFile("${artifactDir}/orientdb-mining-${mxBuildVersion}.zip", "${jobWorkspace}")) {
		            error "Download of ${artifactDir}/orientdb-mining-${mxBuildVersion}.zip failed"
		        }
				if (! spUtils.downloadFile("${artifactDir}/mining-api-server-dist-${mxBuildVersion}.jar", "${jobWorkspace}")) {
		            error "Download of ${artifactDir}/mining-api-server-dist-${mxBuildVersion}.jar failed"
		        }
			}
		} else {
			Utils.markStageSkippedForConditional('Check out DB Dumps')
		}
	}
	
    stage('Start OrientDB EC2 instance') {
        if(awsAction == "Start") {
    		try {
    			def orientDbLaunchTemplateId = 'lt-07aee3cc919a5726c'
    			if (largeDbInstance.toBoolean()) {
    				orientDbLaunchTemplateId='lt-0d71eab221e1a9d35'
    			}
    			def instanceDetailsJson = sh(script: "${awsCmdProfile} ec2 run-instances --launch-template LaunchTemplateId=${orientDbLaunchTemplateId} --no-verify-ssl", returnStdout: true).trim()
    	    	echo "OrientDB Server Details: ${instanceDetailsJson}"
    			
    	    	def orientDbJson = readJSON text: instanceDetailsJson
    			def orientDbInstances = orientDbJson.Instances[0]
    			orientDbIp = orientDbInstances.PrivateIpAddress
    			orientDbId = orientDbInstances.InstanceId
    			orientDbDns = orientDbInstances.PrivateDnsName
    			  			 
    			waitForInstanceInitialization(orientDbId, awsCmdProfile)
    		} catch (e) {
    			cleanUpAwsEnv()
    			error("OrientDB EC2 instance startup failed: ${e}")
    		}
        } else {
			Utils.markStageSkippedForConditional('Start OrientDB EC2 instance')
		}
	}
	
	stage('Configure and Start OrientDB Server (SSH)') {
		// SSH Steps available commands
        //   sshCommand remote: remote, command: 'ls -lrt'
        //   sshScript remote: remote, script: 'test.sh'
        //   sshPut remote: remote, from: 'test.sh', into: '.'
        //   sshGet remote: remote, from: 'test.sh', into: 'test_new.sh', override: true
        //   sshRemove remote: remote, path: 'test.sh'		
		if(awsAction == "Start") {
    		try {
    			remote = [:]
    			remote.name = "${orientDbDns}"
    			remote.host = "${orientDbIp}"
    			remote.allowAnyHosts = true
    			remote.timeoutSec = 30
    					
    			withCredentials([sshUserPrivateKey(credentialsId: 'AWS-ssh-key', keyFileVariable: 'identity', passphraseVariable: '', usernameVariable: 'userName')]) {
    				remote.user = userName
    				remote.identityFile = identity
    	
    				// root location ./ expected to be /home/ec2-user after login
    				
    				// configure and start orientDb
    				sshPut remote: remote, from: "${jobWorkspace}/orientdb-mining-${mxBuildVersion}.zip", into: '.'
    				sshCommand remote: remote, command: "unzip -qq orientdb-mining-${mxBuildVersion}.zip -d ./orientdb"
    				sshCommand remote: remote, command: "sed -i 's/2480-2480/8080-8080/g' ./orientdb/config/orientdb-server-config.xml"
    				sshCommand remote: remote, command: 'chmod 744 ./orientdb/bin/server.sh'
    				sshCommand remote: remote, command: 'sudo systemctl start orientDbStart.service'
    				sleep 30
    				
					// import test data if selected
					if (importBackupTxdmv.toBoolean()) {
	    				echo 'Importing test data'
	    				sshCommand remote: remote, command: 'chmod 744 ./orientdb/bin/console.sh'
						sshPut remote: remote, from: "${backupDir}/backup_Txdmv.zip", into: './orientdb/bin'
						sshCommand remote: remote, command: "./orientdb/bin/console.sh \"CONNECT plocal:./orientdb/databases/mining admin admin; RESTORE DATABASE ./orientdb/bin/backup_Txdmv.zip; CREATE INDEX Module_name_ft ON Module(name) FULLTEXT ENGINE LUCENE METADATA { \"allowLeadingWildcard\": true, \"default\": \"innowake.mining.data.plugin.lucene.LowercasingKeywordAnalyzer\" };\" > /dev/null"
					}
				}
    		} catch (e) {
    			cleanUpAwsEnv()
    			error("Orient DB server startup failed: ${e}")
    		}
	    } else {
			Utils.markStageSkippedForConditional('Configure and Start OrientDB Server (SSH)')
		}
	}

	stage('Configure Mining API Server Version') {
	    if(awsAction == "Start") {
    		try {
    			withCredentials([sshUserPrivateKey(credentialsId: 'AWS-ssh-key', keyFileVariable: 'identity', passphraseVariable: '', usernameVariable: 'userName')]) {
    				remote.user = userName
    				remote.identityFile = identity
    				
    				sshPut remote: remote, from: "${jobWorkspace}/mining-api-server-dist-${mxBuildVersion}.jar", into: '/mnt/efs/server/standalone/'
    				sshCommand remote: remote, command: "sed -i 's/jdbc:orient:remote:.*\\/mining/jdbc:orient:remote:${orientDbIp}:2424\\/mining/g' /mnt/efs/server/standalone/application-default.yml"
					sshCommand remote: remote, command: "sed -i 's/java .*/java -Dlogging.config=log4j2.xml -jar mining-api-server-dist-${mxBuildVersion}.jar --spring.config.additional-location=application-default.yml --mining.cookieId=DISABLED/g' /mnt/efs/server/standalone/start.sh"
    			}
    		} catch (e) {
    			cleanUpAwsEnv()
    			error("Mining API server config failed: ${e}")
    		}
	    } else {
			Utils.markStageSkippedForConditional('Configure Mining API Server Version')
		}
	}
	
	stage('Start Mining API Server') {
	    if(awsAction == "Start") {
    	    try {
    			def apiServerLaunchTemplateId = 'lt-07206c5eed7b1a9e0'
    			def instanceDetailsJson = sh(script: "${awsCmdProfile} ec2 run-instances --launch-template LaunchTemplateId=${apiServerLaunchTemplateId},Version=6 --no-verify-ssl", returnStdout: true).trim()
    	    	echo "Api Server Details: ${instanceDetailsJson}"
    			
    	    	def apiServerJson = readJSON text: instanceDetailsJson
    			def apiServerInstances = apiServerJson.Instances[0]
    			apiServerIp = apiServerInstances.PrivateIpAddress
    			apiServerId = apiServerInstances.InstanceId
    			apiServerDns = apiServerInstances.PrivateDnsName
    			  			 
    			waitForInstanceInitialization(apiServerId, awsCmdProfile)
    			
    			if (startApiServer.toBoolean()) {
	    			startApiServer(apiServerId, apiServerIp, awsCmdProfile)
    			}
    			
    			buildDescription "mxBuildVersion=${mxBuildVersion} importBackupTxdmv=${importBackupTxdmv} orientDbIp=${orientDbIp} orientDbId=${orientDbId} apiServerIp=${apiServerIp} apiServerId=${apiServerId}"
    	    } catch (e) {
    			//cleanUpAwsEnv()
    			error("API Server instance startup failed: ${e}")
    		}
	    } else {
			Utils.markStageSkippedForConditional('Start Mining API Server')
		}
	}
	
	stage('Clean Up AWS environment') {
	    if(awsAction == "Stop") {
	    	// parse build description of start job
	    	def buildDesc = currentBuild.previousBuild.getDescription()
	    	if (buildDesc && buildDesc.contains('orientDbIp')) {
		    	if (buildDesc.contains('orientDbIp')) {
		    		mxBuildVersion = (buildDesc =~ /mxBuildVersion=([A-Za-z0-9\.\-]+)/)[0][1]
				    orientDbId = (buildDesc =~ /orientDbId=([A-Za-z0-9\.\-]+)/)[0][1]
				    orientDbIp = (buildDesc =~ /orientDbIp=([A-Za-z0-9\.\-]+)/)[0][1]
				    apiServerId = (buildDesc =~ /apiServerId=([A-Za-z0-9\.\-]+)/)[0][1]
				    
				    echo "mxBuildVersion=${mxBuildVersion} orientDbIp=${orientDbIp} orientDbId=${orientDbId} apiServerId=${apiServerId}"
				} else {
					echo 'server info could not be found in previous run build description'
				}
	    	}
	    
	        if(orientDbIp) {
    	        orientDbDns = "ip-${orientDbIp.replaceAll('\\.','-')}.eu-central-1.compute.internal"
    	        remote = [:]
    			remote.name = "${orientDbDns}"
    			remote.host = "${orientDbIp}"
    			remote.allowAnyHosts = true
    			remote.timeoutSec = 30
    			
		        cleanUpAwsEnv()
	        } else {
	            error('orientDbInstanceId not set, required for Stop action.')
	        }
	    } else {
			Utils.markStageSkippedForConditional('Clean Up AWS environment')
		}
	}
}

def startApiServer(serverId, serverIp, awsCmdProfile) {
	echo 'Starting API Server'
	def remoteApiServer = [:]
	remoteApiServer.name = "ip-${serverIp.replaceAll('\\.','-')}.eu-central-1.compute.internal"
	remoteApiServer.host = "${serverIp}"
	remoteApiServer.allowAnyHosts = true
	remoteApiServer.timeoutSec = 30
			
	withCredentials([sshUserPrivateKey(credentialsId: 'AWS-ssh-key', keyFileVariable: 'identity', passphraseVariable: '', usernameVariable: 'userName')]) {
		remoteApiServer.user = userName
		remoteApiServer.identityFile = identity

		// root location ./ expected to be /home/ec2-user after login
		echo "Moving license to /home/ec2-user/"
		sshPut remote: remoteApiServer, from: "${pwd()}/licenses/mining/innowake.lic", into: '.'
		// start mining api server
		echo "Starting api server on ${serverIp}"
		sshCommand remote: remoteApiServer, command: 'chmod 744 ./server/start.sh'
		sshCommand remote: remoteApiServer, command: 'sudo systemctl start miningApiStart.service'
	}
}

def terminateInstance(instanceId, awsCmdProfile) {
	sh "${awsCmdProfile} ec2 terminate-instances --instance-ids ${instanceId}"
}

def waitForInstanceInitialization(instanceId, awsCmdProfile) {
	echo "EC2 instance ${instanceId} started - waiting for initialization"
	sh "${awsCmdProfile} ec2 wait instance-status-ok --instance-ids ${instanceId}"
}

def waitForInstanceTermination(instanceId, awsCmdProfile) {
	echo 'EC2 instance shut down - waiting for termination: '
	sh "${awsCmdProfile} ec2 wait instance-terminated --instance-ids ${instanceId}"
}