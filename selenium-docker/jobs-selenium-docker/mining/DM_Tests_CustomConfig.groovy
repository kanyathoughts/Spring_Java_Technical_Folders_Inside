import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
@Library('TestUtils') _

/**
 * Compile and run all available test cases for mining
 * 
 * @param mxBuildVersion	 	the trunk build version to use
 * @param executeTestsOn	 	the node where to execute the tests
 * @param executeEnvironmentOn	the node where to execute the environment
 */

node(executeEnvironmentOn) {

	timestamps {
	     
	    buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	    buildDescription "mxBuildVersion=${mxBuildVersion} executeTestsOn=${executeTestsOn} executeEnvironmentOn=${executeEnvironmentOn}"
	
		def miscUtils = new MiscUtils()
		def gitUtils = new GitUtils()
		def dockerUtils = new DockerUtils()
	    def workDir = pwd()
		def executeEnvironment = executeEnvironmentOn
		def env = "/data/jenkins/prod/${executeEnvironment}"
		def jobWorkspace = "${env}/workspace/DM_Docker_Start_Mining_Env"
		def gitDirLicenses = "${jobWorkspace}/licenses"
        def dockerDir = "${jobWorkspace}/docker"
		def keycloak = 'keycloak-server'
		def orientDbName = 'orient-db'
        def apiServerName = 'api-server'
		
		def startMiningEnvironmentResult
	    def testCases = [
    	    'testcases.mining.webui.CallChainCustomConfigTest'
		]
		def unstableStagesString = ''
		def failedStagesString = ''
		def totalTestAmount = 0
		def backupFile = 'backup_Txdmv_metadata.gz'
		
		if (executeEnvironmentOn == "USLinux8-DM") {
			miningServerUrl = 'http://qef-linux8-us-dm.deloitte.com:8080/'
		} else if (executeEnvironmentOn == "USLinux5-DM") {
			miningServerUrl = 'http://qef-linux5-us-dm.deloitte.com:8080/'
		} else if (executeEnvironmentOn == "USLinux6-DM") {
			miningServerUrl = 'http://qef-linux6-us-dm.deloitte.com:8080/'
		} else if (executeEnvironmentOn == "USLinux7-DM") {
			miningServerUrl = 'http://qef-linux7-us-dm.deloitte.com:8080/'
		} else if (executeEnvironmentOn == "USLinux1") {
			miningServerUrl = 'http://qef-linux1-us.deloitte.com:8080/'
		} else if (executeEnvironmentOn == "USLinux3") {
			miningServerUrl = 'http://qef-linux3.deloitte.com:8080/'
		}
		
		stage ('check if already running') {
			if(currentBuild.previousBuild) {
				if (currentBuild.previousBuild.rawBuild.isBuilding() || currentBuild.previousBuild.previousBuild.rawBuild.isBuilding()) {
					error("A previous build is still running.")
				}
			}
		}
			
	    stage ('clean up workspace') {
	        deleteDir()
	    }
	    		
	    startMiningEnvironmentResult = startMiningEnvironmentWithCustomConfig(mxBuildVersion, true, executeEnvironmentOn)
		
	    echo startMiningEnvironmentResult.result
		if (startMiningEnvironmentResult.result == 'SUCCESS') {
			stage('copy db backup and create db dump') {
				def gitlabProjectDbDumps
				def gitlabProjectLicense
			   dir(jobWorkspace) {
				sh returnStatus: true, script: "docker stop ${keycloak}"
				sh returnStatus: true, script: "docker stop ${apiServerName}"
		        sh returnStatus: true, script: "docker stop ${orientDbName}"	
				sh "docker cp ${orientDbName}:/orientDb ."
				sh "./orientDb/bin/console.sh \"connect plocal:${jobWorkspace}/orientDb/databases/mining admin admin;BACKUP DATABASE ${dockerDir}/${backupFile}\"" 
				sh "chmod -R 777 ${dockerDir}/${backupFile}"
				docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside {
				dir(dockerDir) {
					withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
					 gitlabProjectDbDumps = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/mining-db-dumps.git"
					 gitlabProjectLicense = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/licenses.git"
					}
					sh "git config user.name 'USAppModQMUserSVC'"
					sh "git config user.email 'USAppModQMUserSVC@deloitte.com'"
					sh "git add ${backupFile}"
					sh "git commit -a -m 'WQADM-7:Backup file'"
					sh "git push -f"
				}
			 }
				sh "docker start ${keycloak}"
				sh "docker start ${orientDbName}"
				sleep 60
				sh "docker start ${apiServerName}"
				sleep 50
				}
			}

			def reuseWorkspace = false
			testCases.each {
				testCase ->
				
				def stageName = testCase.replace('testcases.mining.','').replace('testcases.','')
	
				stage (stageName) {
					def buildResult = build job: 'DM_LeanFT_Mining_Testjob', propagate: false, parameters: [
						string(name: 'mxBuildVersion', value: mxBuildVersion), 
						string(name: 'testcase', value: testCase), 
						booleanParam(name: 'reuse_workspace', value: reuseWorkspace), 
						[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']],
						string(name: 'miningServerURL', value: miningServerUrl)
					]
					miscUtils.evaluateBuildResult(buildResult)
					
					if (buildResult.result.equals('UNSTABLE')) {
						unstableStagesString += "${stageName}:${buildResult.number},"
					} else if (buildResult.result.equals('FAILURE')) {
						failedStagesString += "${stageName}:${buildResult.number},"
					}
					
					totalTestAmount++
					
					reuseWorkspace = true
					echo "Build result test case ${testCase}: ${buildResult.result}"
					
					copyArtifacts filter: 'RunResults/*/**', optional: true, projectName: 'DM_LeanFT_Mining_Testjob', selector: specific(buildResult.number.toString()), target: "${workDir}/${testCase}"	
				}
			}
		} else {
			error 'Mining Environment was not launched correctly.'
		}
		
		stage('Archive run results') {
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
			
			echo "\nTotal Web UI & IAM tests: ${totalTestAmount}"
			echo "\nPercentage of unstable tests: ${unstablePercentage}%"
			echo "\nPercentage of stable tests: ${stablePercentage}%"		
			
			buildDescription "${currentBuild.getDescription()} totalTestCount=${totalTestAmount} ${unstableStagesString ? 'unstableStages='+unstableStagesString : ''} ${failedStagesString ? 'failedStages='+failedStagesString : ''}"			
		}
	}
 }


def startMiningEnvironmentWithCustomConfig(String mxBuildVersion, boolean withKeycloak, String executeEnvironmentOn) {
	def miscUtils = new MiscUtils()
	stage ("start mining environment keycloak = ${withKeycloak}") {
		startMiningEnvironmentResult = build job: 'DM_Docker_Start_Mining_Env_Custom_Config', propagate: false, parameters: [
			extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
			booleanParam(name: 'withKeycloak', value: withKeycloak),
			[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeEnvironmentOn], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
		miscUtils.evaluateBuildResult(startMiningEnvironmentResult)
	}
	if (startMiningEnvironmentResult.result != 'SUCCESS') {
		stage ("restart mining environment keycloak = ${withKeycloak}") {
			echo 'first start of mining environment without keycloak did not work -> trigger restart'
			startMiningEnvironmentResult = build job: 'DM_Docker_Start_Mining_Env_Custom_Config', propagate: true, parameters: [
			extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
			booleanParam(name: 'withKeycloak', value: withKeycloak),
			[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeEnvironmentOn], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
		miscUtils.evaluateBuildResult(startMiningEnvironmentResult)
		}
	}
	return startMiningEnvironmentResult
}
