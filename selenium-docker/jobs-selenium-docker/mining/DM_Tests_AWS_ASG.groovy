import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
import hudson.tasks.test.*
@Library('TestUtils') _
/*
 * This job executes the AWS Autoscaling Mining Test described in WQST-580.
 *
 * @param mxBuildVersion    The build to use.
 * @param executeSetupOn 	The Jenkins Linux node the AWS environment will be set up from
 * @param executeTestOn 	The Jenkins LeanFT node the test will run on
 * @param withoutKeycloak	Boolean to determine if a Keycloak is used
 * @param clusterSize		The initial amount of API servers started
 * @param testcases			The testcases that should be run
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
	def perfData
	def unstableStagesString = ''
	def failedStagesString = ''
	def totalTestAmount = 0
	
	buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	buildDescription "mxBuildVersion=${mxBuildVersion} clusterSize=${clusterSize} nodes=${executeSetupOn},${executeTestOn}"

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
			clusterSize = size // update clusterSize
			
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
	
	def runTestJob = { testname ->
		totalTestAmount++
		def buildResult = build job: 'DM_LeanFT_Mining_Testjob', propagate: false, parameters: [
			extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
			string(name: 'testcase', value: testname),
			booleanParam(name: 'reuse_workspace', value: false),
			[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestOn], nodeEligibility: [$class: 'AllNodeEligibility']],
			booleanParam(name: 'runWithoutKeycloak', value: withoutKeycloak),
			string(name: 'miningServerURL', value: 'http://internal-qef-api-server-lb-1942844143.eu-central-1.elb.amazonaws.com:8080/')
		]
		
		miscUtils.evaluateBuildResult(buildResult)
		
		if (buildResult.result.equals('UNSTABLE')) {
			unstableStagesString += "${testname}:${buildResult.number},"
		} else if (buildResult.result.equals('FAILURE')) {
			failedStagesString += "${testname}:${buildResult.number},"
		}
		
		def stepData = ''
		if (testname.contains('TxDMVTest')) {
			echo 'Analyzing test results...'
			try {
				def testResultAction =  buildResult.rawBuild.getAction(AbstractTestResultAction.class)
				if (testResultAction != null) {
					echo "Tests: ${testResultAction.getFailCount()} failures of ${testResultAction.getTotalCount()}."
					def passedTestResults = testResultAction.getPassedTests()
					if (passedTestResults != null) {
						passedTestResults.each {
							passedTest ->
							if(testname.contains('TxDMVTest')) {
								if (passedTest.getTitle().contains('step13')) {
									echo "Discover Code : ${passedTest.getDuration()}s"
									stepData += "${passedTest.getDuration()},"
								}
								if (passedTest.getTitle().contains('step15')) {
									echo "Discover Metrics : ${passedTest.getDuration()}s"
									stepData += "${passedTest.getDuration()}"
								}
							}
						}
					}
				}
			} catch (e) {
				unstable "Test result eval failed: ${e}"
			}
		}
		return stepData
	}
	
	def cleanUpAwsEnv = {
		echo 'AWS Cleanup procedure triggered'
		try {
			// Scale down Autoscaling group
			if (autoscalingGroupTriggered) {
				sh "${awsCmdProfile} autoscaling update-auto-scaling-group --auto-scaling-group-name ${autoscalingGroupName} --desired-capacity 0 --min-size 0 --max-size 0"
			}
			
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
			if (autoscalingGroupTriggered) {
				echo 'Wait for Autoscaling Group shutdown'
				apiServerInstanceMap.each {
					serverId, serverIp ->
					waitForInstanceTermination(serverId, awsCmdProfile)
				}
			}
		} catch (e) {
			error("AWS cleanup failed: ${e}")
		}
	}
	
	stage('Configure AWS CLI') {
		// only execute pipeline if it is not running already
		if(currentBuild.previousBuild) {
			if ( currentBuild.previousBuild.rawBuild.isBuilding() ) {
				error("Previous build ${currentBuild.previousBuild.number} has not yet finished.")
			}
		}
		
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
	
	stage('Check out Files') {
		def gitlabProject
		docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside {
			withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
	   	        gitlabProject = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/licenses.git"
   	        }
		    dir(jobWorkspace) {
			    sh "git clone --depth 1 --branch master ${gitlabProject} ${licenseDir}"
			}
	        withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
	   	        gitlabProject = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/mining-db-dumps.git"
	        }
			dir(jobWorkspace) {
			    sh "git clone --depth 1 --branch master ${gitlabProject} ${backupDir}"
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
	}
	
    stage('Start OrientDB EC2 instance') {
		try {
			def orientDbLaunchTemplateId = 'lt-07aee3cc919a5726c'
			def instanceDetailsJson = sh(script: "${awsCmdProfile} ec2 run-instances --launch-template LaunchTemplateId=${orientDbLaunchTemplateId}", returnStdout: true).trim()
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
	}
	
	stage('Configure and Start OrientDB Server (SSH)') {
		// SSH Steps available commands
        //   sshCommand remote: remote, command: 'ls -lrt'
        //   sshScript remote: remote, script: 'test.sh'
        //   sshPut remote: remote, from: 'test.sh', into: '.'
        //   sshGet remote: remote, from: 'test.sh', into: 'test_new.sh', override: true
        //   sshRemove remote: remote, path: 'test.sh'		
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
				
			}
		} catch (e) {
			cleanUpAwsEnv()
			error("Orient DB server startup failed: ${e}")
		}
	}
	
	stage('Start Keycloak EC2 instance') {
		if (!(withoutKeycloak.toBoolean())) {
			try {
				def keycloakLaunchTemplateId = 'lt-0f70153c21ab885d6'
				def instanceDetailsJson = sh(script: "${awsCmdProfile} ec2 run-instances --launch-template LaunchTemplateId=${keycloakLaunchTemplateId}", returnStdout: true).trim()
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
		if (!(withoutKeycloak.toBoolean()) && keycloakIp) {
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
			} catch (e) {
				cleanUpAwsEnv()
				error("Keycloak config failed: ${e}")
			}
		} else {
			Utils.markStageSkippedForConditional('Configure Keycloak Instance')
		}
	}

	stage('Configure Mining API Server Version') {
		try {
			withCredentials([sshUserPrivateKey(credentialsId: 'AWS-ssh-key', keyFileVariable: 'identity', passphraseVariable: '', usernameVariable: 'userName')]) {
				remote.user = userName
				remote.identityFile = identity
				
				sshCommand remote: remote, command: "rm -r -f /mnt/efs/aws-mining*"
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
	}
	
	stage('Start Mining API Server') {
		try {
			scaleApiServerGroup(clusterSize)
		} catch (e) {
			cleanUpAwsEnv()
			error("Auto Scaling group capacity increase failed: ${e}")
		}
	}
	
	stage('Smoke Test') {
		if (testcases.contains('SmokeTest')) {
			try {
				echo 'Start Smoke Test'
				runTestJob ('testcases.mining.plugin.SmokeTest')
			} catch (e) {
				cleanUpAwsEnv()
				error("SmokeTest execution failed: ${e}")
			}
		} else {
			Utils.markStageSkippedForConditional('Smoke Test')
		}
	}
	
	stage('Discovery Minification') {
		if (testcases.contains('DiscoveryMinification')) {
			try {
				echo 'Start DiscoveryMinification Test'		
				runTestJob ('testcases.discovery.MinificationTest')
			} catch (e) {
				cleanUpAwsEnv()
				error("DiscoveryMinification test execution failed: ${e}")
			}
		} else {
			Utils.markStageSkippedForConditional('Discovery Minification')
		}
	}
	
	stage('Code Viewer Test') {
		if (testcases.contains('CodeViewer')) {
			try {
				echo 'Code Viewer Test'			
				runTestJob ('testcases.mining.webui.CodeViewerTest')
			} catch (e) {
				cleanUpAwsEnv()
				error("CodeViewer test execution failed: ${e}")
			}
		} else {
			Utils.markStageSkippedForConditional('Code Viewer Test')
		}
	}
	
	stage('TxDMV - 1 node') {
		if (testcases.contains('TxDMV-1node')) {
			try {
				echo 'Start TxDMV Test - 1 node'
				scaleApiServerGroup (1)
				def result = runTestJob ('testcases.discovery.TxDMVTest')
				if (!result.isEmpty()) {
					perfData += "\ndiscovery.TxDMVTest,${mxBuildVersion},${currentBuild.number},${clusterSize},${result}"
				}
			} catch (e) {
				cleanUpAwsEnv()
				error("TxDMV-1node test execution failed: ${e}")
			}
		} else {
			Utils.markStageSkippedForConditional('TxDMV - 1 node')
		}
	}
	
	stage('TxDMV - 2 nodes') {
		if (testcases.contains('TxDMV-2nodes')) {
			try {
				echo 'Start TxDMV Test - 2 nodes'
				scaleApiServerGroup (2)
				def result = runTestJob ('testcases.discovery.TxDMVTest')
				if (!result.isEmpty()) {
					perfData += "\ndiscovery.TxDMVTest,${mxBuildVersion},${currentBuild.number},${clusterSize},${result}"
				}
			} catch (e) {
				cleanUpAwsEnv()
				error("TxDMV-2nodes test execution failed: ${e}")
			}
		} else {
			Utils.markStageSkippedForConditional('TxDMV - 2 nodes')
		}
	}
	
	stage('Switch to TxDMV backup') {
		// skip stage if ModuleSearch is the only testcase, TxDMV has already been loaded instead of DemoDez
		// otherwise reset DB to TxDMV if other testcases were executed before on DemoDez
		if (!testcases.startsWith('ModuleSearchTest') && testcases.contains('ModuleSearchTest')) {
			try{
			    // stop Api servers
				stopApiServers(apiServerInstanceMap)
				
				// reconfigure orientDb server			
				withCredentials([sshUserPrivateKey(credentialsId: 'AWS-ssh-key', keyFileVariable: 'identity', passphraseVariable: '', usernameVariable: 'userName')]) {
					remote.user = userName
					remote.identityFile = identity
			
					// stop mining api server
					echo "Stopping OrientDb server"
					sshCommand remote: remote, command: 'sudo systemctl stop orientDbStart.service'
		
					// delete orientDb
					sshCommand remote: remote, command: 'rm -r ./orientdb'
					// unzip fresh copy and import minig-quickstart
					sshCommand remote: remote, command: "unzip -qq orientdb-mining-${mxBuildVersion}.zip -d ./orientdb"
					sshCommand remote: remote, command: "sed -i 's/2480-2480/8080-8080/g' ./orientdb/config/orientdb-server-config.xml"
					sshCommand remote: remote, command: 'chmod 744 ./orientdb/bin/server.sh'
					sshCommand remote: remote, command: 'sudo systemctl start orientDbStart.service'
					sleep 30
					
					echo 'Restoring test data'
					sshCommand remote: remote, command: 'chmod 744 ./orientdb/bin/console.sh'
					sshPut remote: remote, from: "${backupDir}/backup_Txdmv.zip", into: './orientdb/bin'
					sshCommand remote: remote, command: "./orientdb/bin/console.sh \"CONNECT plocal:./orientdb/databases/mining admin admin; DROP DATABASE; CREATE DATABASE plocal:./orientdb/databases/mining admin admin; RESTORE DATABASE ./orientdb/bin/backup_Txdmv.zip; CREATE INDEX Module_name_ft ON Module(name) FULLTEXT ENGINE LUCENE METADATA { \"allowLeadingWildcard\": true, \"default\": \"innowake.mining.data.plugin.lucene.LowercasingKeywordAnalyzer\" };\" > /dev/null"
				}
				
				// restart Api Servers
				startApiServers(apiServerInstanceMap, awsCmdProfile, elbTargetGroupArn)
			} catch (e) {
				cleanUpAwsEnv()
				error("Switch to TxDMV backup failed: ${e}")
			}
		} else {
			Utils.markStageSkippedForConditional('Switch to TxDMV backup')
		}
	}

	stage('Check log directories') {
		try {
			withCredentials([sshUserPrivateKey(credentialsId: 'AWS-ssh-key', keyFileVariable: 'identity', passphraseVariable: '', usernameVariable: 'userName')]) {
				remote.user = userName
				remote.identityFile = identity
				
				def lsOutputAPI = sshCommand remote: remote, command: 'ls /mnt/efs/logs/mining-api-server.log'
				echo "lsOutputAPI = ${lsOutputAPI}"
			}
		} catch (e) {
			unstable("Checking log directories failed: ${e}")
		}
	}
	
	stage('Clean Up AWS environment') {
		if (perfData) {
			def csvText = 'Testcase,Mining Version,Build,Nodes,Dicover Code,Discover Metrics'
			csvText += perfData.replace('null','')
			writeFile file: 'PerformanceMeasures.csv', text: csvText
			archiveArtifacts 'PerformanceMeasures.csv'
		}
		
		def unstableStages = []
		def failedStages = []
		def logOutput = ''
		if (unstableStagesString) {
			logOutput += 'Unstable stages:\n'
			unstableStages = unstableStagesString.split(',')
			unstableStages.each {
				unstableStage ->
				def stageInfo = unstableStage.split(':')
				logOutput += "${stageInfo[0]}\n"
			}
		}
		if (failedStagesString) {
			logOutput += 'Failed stages:\n'
			failedStages = failedStagesString.split(',')
			failedStages.each {
				failedStage ->
				def stageInfo = failedStage.split(':')
				logOutput += "${stageInfo[0]}\n"
			}
		}
		if (logOutput) {
			echo "${logOutput}"
		}
		
		def unstablePercentage = ((unstableStages.size()+failedStages.size()) / totalTestAmount) * 100 
		def stablePercentage = 100 - unstablePercentage
		
		echo "\nTotal AWS tests: ${totalTestAmount}"
		echo "\nPercentage of unstable tests: ${unstablePercentage}%"
		echo "\nPercentage of stable tests: ${stablePercentage}%"		
		
		buildDescription "${currentBuild.getDescription()} totalTestCount=${totalTestAmount} ${unstableStagesString ? 'unstableStages='+unstableStagesString : ''} ${failedStagesString ? 'failedStages='+failedStagesString : ''}"
		
		cleanUpAwsEnv()
	}
}

def terminateInstance(instanceId, awsCmdProfile) {
	sh(script: "${awsCmdProfile} ec2 terminate-instances --instance-ids ${instanceId}", returnStdout: true)
}

def waitForInstanceInitialization(instanceId, awsCmdProfile) {
	echo "EC2 instance ${instanceId} started - waiting for initialization"
	sh(script: "${awsCmdProfile} ec2 wait instance-status-ok --instance-ids ${instanceId}", returnStdout: true)
}

def waitForInstanceTermination(instanceId, awsCmdProfile) {
	echo 'EC2 instance shut down - waiting for termination: '
	def exitCode
	def attempts = 0
	while (exitCode != 0) {
		if (attempts >= 3) {
			error("EC2 instance not terminated in time, exit code ${exitCode}")
			break
		} else {
			attempts++
			echo "Load balancer wait attempt ${attempts}"
			exitCode = sh(script: "${awsCmdProfile} ec2 wait instance-terminated --instance-ids ${instanceId}", returnStatus: true)
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
				error("Load balancer target not in service, exit code ${exitCode}")
				break
			} else {
				attempts++
				echo "Load balancer wait attempt ${attempts}"
				exitCode = sh(script: "${awsCmdProfile} elbv2 wait target-in-service --target-group-arn ${elbTargetGroupArn} --targets Id=${serverId},Port=8080", returnStatus: true)
			}
		}
	}
}

def stopApiServers(apiServerInstanceMap) {
	echo 'Stopping API Server(s)'
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

			// stop mining api server
			echo "Stopping api server on ${serverIp}"
			sshCommand remote: remoteApiServer, command: 'chmod 744 ./server/start.sh'
			sshCommand remote: remoteApiServer, command: 'sudo systemctl stop miningApiStart.service'
		}
	}
}

def switchFeatureToggle(apiServerInstanceMap, feature, setState, keycloakIp) {
	def accessToken
		
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
				
				// set access token the first time around
				if (!accessToken) {
					if (keycloakIp) {
						echo "Getting Keycloak token"
						def response = sshCommand remote: remote, command: "curl -d 'client_id=backend' -d 'username=first' -d 'password=first' -d 'grant_type=password' 'http://${keycloakIp}:8080/auth/realms/mining/protocol/openid-connect/token'"
						def responseJson = readJSON text: response
						if (responseJson.access_token) {
							accessToken = responseJson.access_token
						} else {
							error "Could not retrieve access token from Keycloak: ${responseJson}"
						}
					} else {
						accessToken = 'ff93340a-af6c-4fcd-90bb-426448fa1597'
					}
				}
				
				echo "Switching ${feature} to ${setState} on ${serverIp}"
				sshCommand remote: remote, command: "curl -X POST 'http://localhost:8080/api/v1/features/${feature}/toggle?state=${setState}' -H 'Authorization: Bearer ${accessToken}'"
			}
		} catch (e) {
			error("switching feature toggle ${feature} failed on ${serverIp}: ${e}")
		}
	}
}

