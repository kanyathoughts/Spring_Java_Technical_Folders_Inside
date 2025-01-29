import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
@Library('TestUtils') _
/*
 * This job lets you start and stop AWS Mining test environments with or without Keycloak using an AWS Autoscaling Group for the API server.
 *
 * @param awsAction         "Start" or "Stop"
 * @param mxBuildVersion    The build to use.
 * @param executeSetupOn 	The Jenkins Linux node the AWS environment will be set up from
 * @param importBackupTxdmv The database dump to import	
 * @param withoutKeycloak   Boolean to determine if a Keycloak is used
 * @param clusterSize	    The initial amount of API servers started
 * @orientDbInstanceId      The OrientDb instance id to be terminated
 * @orientDbInstanceIp      The OrientDb instance ip to be terminated
 * @keycloakInstanceId      The Keycloak instance id to be terminated
 */

node(executeSetupOn) {
    def gitUtils = new GitUtils()
    def spUtils = new SharepointUtils()
	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
    def jobWorkspace = pwd()
    def licenseDir = "${jobWorkspace}/licenses"
    def backupDir = "${jobWorkspace}/backups"
    def artifactDir = "${spUtils.getJenkinsArtifactsDir()}/IW_Copy_MxBuildVersion/${mxBuildVersion}"
	def awsCmd = "docker run --rm -v aws_cli_config:/root/.aws amazon/aws-cli --no-verify-ssl"
	def awsCmdProfile = "${awsCmd} --profile AssumeRole"
	def apiServerAmiId='ami-005a1109b4e38365f'
	def autoscalingGroupName = 'QEF_Api-Server'
	def autoscalingGroupTriggered = false
	def elbTargetGroupArn = 'arn:aws:elasticloadbalancing:eu-central-1:068757897087:targetgroup/QEF-Api-Server-Target/cd6471449e72c8c4'
	def apiServerInstanceMap
	def orientDbIp
	def orientDbId
	def orientDbDns
	def keycloakIp
	def keycloakId
	def keycloakDns
	def remote
	
    buildName "#${env.BUILD_ID} - ${awsAction}"
    buildDescription "mxBuildVersion=${mxBuildVersion} clusterSize=${clusterSize} withoutKeycloak=${withoutKeycloak} importBackupTxdmv=${importBackupTxdmv}"

	def scaleApiServerGroup = { size ->
		// check current Autoscaling Group size
		def asgDetailsJson = sh(script: "${awsCmdProfile} autoscaling describe-auto-scaling-groups --auto-scaling-group-name  ${autoscalingGroupName}", returnStdout: true).trim()
		def asgJson = readJSON text: asgDetailsJson
		def apiServerInstances = asgJson.AutoScalingGroups.Instances[0]
		def initialInstanceCount = apiServerInstances.size()
		
		// scale up/down if needed
		if (initialInstanceCount != size) {
			sh "${awsCmdProfile} autoscaling update-auto-scaling-group --auto-scaling-group-name ${autoscalingGroupName} --desired-capacity ${size} --min-size ${size} --max-size ${size}"
			autoscalingGroupTriggered = true // flag for final cleanup stage
			
			// wait for ASG to create instance from AMI
			echo 'AutoScaling Group capacity updated - waiting for instance update'
			sleep 30
			// update asg info
			asgDetailsJson = sh(script: "${awsCmdProfile} autoscaling describe-auto-scaling-groups --auto-scaling-group-name  ${autoscalingGroupName}", returnStdout: true).trim()
			asgJson = readJSON text: asgDetailsJson
			apiServerInstances = asgJson.AutoScalingGroups.Instances[0]
			
			apiServerInstanceMap = [:]
			
			if (initialInstanceCount < size) {
				echo 'Scaling up'
				// wait for additional instances to start
				apiServerInstances.each {
					instance ->
					def instanceId = instance.InstanceId
					waitForInstanceInitialization(instanceId, awsCmdProfile)
					
					def instanceDetailsJson = sh(script: "${awsCmdProfile} ec2 describe-instances --instance-ids ${instanceId}", returnStdout: true).trim()
					def instanceJson = readJSON text: instanceDetailsJson
					def instanceInfo = instanceJson.Reservations.Instances[0]					
					apiServerInstanceMap.put(instanceId, instanceInfo.PrivateIpAddress[0])
				}
				
				echo "API Server Instances: ${apiServerInstanceMap}"
				startApiServers(apiServerInstanceMap, awsCmdProfile, elbTargetGroupArn)
						
			} else if (initialInstanceCount > size) {
				echo 'Scaling down'
				// wait for superfluous instances to terminate
				apiServerInstances.each {
					instance ->
					def instanceId = instance.InstanceId
	
					switch (instance.LifecycleState) {
						case 'Terminating':
							waitForInstanceTermination(instanceId, awsCmdProfile)
							break
						case 'InService':
							def instanceDetailsJson = sh(script: "${awsCmdProfile} ec2 describe-instances --instance-ids ${instanceId}", returnStdout: true).trim()
							def instanceJson = readJSON text: instanceDetailsJson
							def instanceInfo = instanceJson.Reservations.Instances[0]
							apiServerInstanceMap.put(instanceId, instanceInfo.PrivateIpAddress[0])
							break
						default:
							unstable "Unexpected ASG Instance state: ${instance.LifecycleState}"
							break
					}
				}
				echo "API Server Instances: ${apiServerInstanceMap}"
			}
		} else {
			echo 'Autoscaling Group already at requested cluster size.'
		}
	}

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
								sshRemove remote: remote, path: "/mnt/efs/server/mining-api-server-dist-${mxBuildVersion}.jar"
					}
				}
				terminateInstance(orientDbId, awsCmdProfile)
			}
			
			// Terminate keycloak server if it was started
			if (keycloakId) {		
				terminateInstance(keycloakId, awsCmdProfile)
			}
			
			// Verify all EC2 instances have been shut down
			if (orientDbId) {
				echo 'Wait for OrientDb shutdown'
				waitForInstanceTermination(orientDbId, awsCmdProfile)
			}
			if (keycloakId) {
				echo 'Wait for Keycloak shutdown'
				waitForInstanceTermination(keycloakId, awsCmdProfile)
			}
			
			// Scale down Autoscaling group
			if (autoscalingGroupTriggered) {
				echo 'Wait for Autoscaling Group shutdown'
				scaleApiServerGroup(0)
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
		
		if(awsAction == "Start") {
    		// check current Autoscaling Group size
    		def asgDetailsJson = sh(script: "${awsCmdProfile} autoscaling describe-auto-scaling-groups --auto-scaling-group-name  ${autoscalingGroupName}", returnStdout: true).trim()
    		def asgJson = readJSON text: asgDetailsJson
    		def apiServerInstances = asgJson.AutoScalingGroups.Instances[0]
    		def initialInstanceCount = apiServerInstances.size()
    		
    		// only execute pipeline if it is not running already
    		if ( initialInstanceCount != 0 ) {
    			error("Cannot start Pipeline, Autoscaling Group already in use with instance count ${initialInstanceCount}")
    		}
		}
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
		        if (!(withoutKeycloak.toBoolean())) {
			        if (! spUtils.downloadFile("${artifactDir}/mining-keycloak-extension-${mxBuildVersion}.jar", "${jobWorkspace}")) {
			            error "Download of ${artifactDir}/mining-keycloak-extension-${mxBuildVersion}.jar failed"
			        }
					if (! spUtils.downloadFile("${artifactDir}/mining-keycloak-theme-${mxBuildVersion}.jar", "${jobWorkspace}")) {
			            error "Download of ${artifactDir}/mining-keycloak-theme-${mxBuildVersion}.jar failed"
			        }
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
					
					buildDescription "mxBuildVersion=${mxBuildVersion} importBackupTxdmv=${importBackupTxdmv} orientDbIp=${orientDbIp} orientDbId=${orientDbId}"
				}
    		} catch (e) {
    			cleanUpAwsEnv()
    			error("Orient DB server startup failed: ${e}")
    		}
	    } else {
			Utils.markStageSkippedForConditional('Configure and Start OrientDB Server (SSH)')
		}
	}
	
	stage('Start Keycloak EC2 instance') {
		if(awsAction == "Start" && !(withoutKeycloak.toBoolean())) {
			try {
				def keycloakLaunchTemplateId = 'lt-0f70153c21ab885d6'
				def instanceDetailsJson = sh(script: "${awsCmdProfile} ec2 run-instances --launch-template LaunchTemplateId=${keycloakLaunchTemplateId} --no-verify-ssl", returnStdout: true).trim()
				echo "Keycloak Server Details: ${instanceDetailsJson}"
				
				def keycloakJson = readJSON text: instanceDetailsJson
				def keycloakInstances = keycloakJson.Instances[0]
				keycloakIp = keycloakInstances.PrivateIpAddress
				keycloakId = keycloakInstances.InstanceId
				keycloakDns = keycloakInstances.PrivateDnsName
				
				waitForInstanceInitialization(keycloakId, awsCmdProfile)
			} catch (e) {
				cleanUpAwsEnv()
				error("Keycloak EC2 instance startup failed: ${e}")
			}
		} else {
			Utils.markStageSkippedForConditional('Start Keycloak EC2 instance')
		}
	}
	
	stage('Configure Keycloak Instance') {
		if(keycloakIp) {
			try {
				def remoteKc = [:]
				remoteKc.name = "${keycloakDns}"
				remoteKc.host = "${keycloakIp}"
				remoteKc.allowAnyHosts = true
				remoteKc.timeoutSec = 30
				
				withCredentials([sshUserPrivateKey(credentialsId: 'AWS-ssh-key', keyFileVariable: 'identity', passphraseVariable: '', usernameVariable: 'userName')]) {
					remoteKc.user = userName
					remoteKc.identityFile = identity
					
					sshCommand remote: remoteKc, command: 'rm -rf ./keycloak-15.1.1/standalone/deployments/mining-keycloak-*'
					sshPut remote: remoteKc, from: "${jobWorkspace}/mining-keycloak-extension-${mxBuildVersion}.jar", into: './keycloak-15.1.1/standalone/deployments/'
					sshPut remote: remoteKc, from: "${jobWorkspace}/mining-keycloak-theme-${mxBuildVersion}.jar", into: './keycloak-15.1.1/standalone/deployments/'
					sshCommand remote: remoteKc, command: 'sudo systemctl start keycloak.service'
				}
				
				buildDescription "mxBuildVersion=${mxBuildVersion} importBackupTxdmv=${importBackupTxdmv} orientDbIp=${orientDbIp} orientDbId=${orientDbId} keycloakId=${keycloakId}"				
			} catch (e) {
				cleanUpAwsEnv()
				error("Keycloak config failed: ${e}")
			}
		} else {
			Utils.markStageSkippedForConditional('Configure Keycloak Instance')
		}
	}

	stage('Configure Mining API Server Version') {
	    if(awsAction == "Start") {
    		try {
    			withCredentials([sshUserPrivateKey(credentialsId: 'AWS-ssh-key', keyFileVariable: 'identity', passphraseVariable: '', usernameVariable: 'userName')]) {
    				remote.user = userName
    				remote.identityFile = identity
    				
    				sshPut remote: remote, from: "${jobWorkspace}/mining-api-server-dist-${mxBuildVersion}.jar", into: '/mnt/efs/server/'
					if (withoutKeycloak.toBoolean()) {
						sshCommand remote: remote, command: "sed -i 's/jdbc:orient:remote:.*\\/mining/jdbc:orient:remote:${orientDbIp}:2424\\/mining/g' /mnt/efs/server/application-default.yml"
						sshCommand remote: remote, command: "sed -i 's/java .*/java -Dlogging.config=log4j2.xml -jar mining-api-server-dist-${mxBuildVersion}.jar --spring.config.additional-location=application-default.yml --mining.cookieId=DISABLED/g' /mnt/efs/server/start.sh"
					} else {
						sshCommand remote: remote, command: "sed -i 's/jdbc:orient:remote:.*\\/mining/jdbc:orient:remote:${orientDbIp}:2424\\/mining/g' /mnt/efs/server/application-default-keycloak.yml"
						sshCommand remote: remote, command: "sed -i 's,auth-server-url: http://.*/auth,auth-server-url: http://${keycloakIp}:8080/auth,g' /mnt/efs/server/application-default-keycloak.yml"
						sshCommand remote: remote, command: "sed -i 's/java .*/java -Dlogging.config=log4j2.xml -jar mining-api-server-dist-${mxBuildVersion}.jar --spring.config.additional-location=application-default-keycloak.yml --mining.cookieId=DISABLED/g' /mnt/efs/server/start.sh"
					}
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
    			scaleApiServerGroup(clusterSize)
    	    } catch (e) {
    			//cleanUpAwsEnv()
    			error("Auto Scaling group capacity increase failed: ${e}")
    		}
	    } else {
			Utils.markStageSkippedForConditional('Start Mining API Server')
		}
	}
	
	stage('Clean Up AWS environment') {
	    if(awsAction == "Stop") {
	        if(orientDbInstanceId) {
    	        orientDbId = orientDbInstanceId
    	        orientDbIp = orientDbInstanceIp
    	        orientDbDns = "ip-${orientDbInstanceIp.replaceAll('\\.','-')}.eu-central-1.compute.internal"
    	        keycloakId = keycloakInstanceId
    	        autoscalingGroupTriggered = true
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

def switchFeatureToggle(apiServerInstanceMap, feature, setState) {
	apiServerInstanceMap.each {
		serverId, serverIp ->
		def remote = [:]
		remote.name = "ip-${serverIp.replaceAll('\\.','-')}.eu-central-1.compute.internal"
		remote.host = "${serverIp}"
		remote.allowAnyHosts = true
		remote.timeoutSec = 30
		
		try {
			withCredentials([sshUserPrivateKey(credentialsId: 'AWS-ssh-key', keyFileVariable: 'identity', passphraseVariable: '', usernameVariable: 'userName')]) {
				remote.user = userName
				remote.identityFile = identity
				
				echo "Switching ${feature} to ${setState} on ${serverIp}"
				sshCommand remote: remote, command: "curl -X POST 'http://localhost:8080/api/v1/features/${feature}/toggle?state=${setState}' -H 'Authorization: Bearer 6c2000c1-d9a8-4d87-92f0-0a11139a037e'"
			}
		} catch (e) {
			error("switching feature toggle ${feature} failed on ${serverIp}: ${e}")
		}
	}
}

def startApiServers(apiServerInstanceMap, awsCmdProfile, elbTargetGroupArn) {
	echo 'Starting API Server(s)'
	apiServerInstanceMap.each {
		serverId, serverIp ->
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
		
		// Returns exit code 255 if waiter reaches max attempts (40), repeat wait 3 times if needed
		def exitCode
		def attempts = 0
		while (exitCode != 0) {
			if (attempts >= 3) {
				error ("Load balancer target not in service, exit code ${exitCode}")
				break
			} else {
				attempts++
				echo "Load balancer wait attempt ${attempts}"
				exitCode = sh(script: "${awsCmdProfile} elbv2 wait target-in-service --target-group-arn ${elbTargetGroupArn} --targets Id=${serverId},Port=8080", returnStatus: true)
			}
		}
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
