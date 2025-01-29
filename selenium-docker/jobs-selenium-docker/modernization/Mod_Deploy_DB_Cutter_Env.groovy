@Library('TestUtils') _
/**
 * This job deploys the specified db cutter version to USLinux2.
 * 
 * @param buildTag		The db cutter version/tag to use.
 */
node('USLinux2') {
	def jobWorkspace = pwd()
	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def gitUtils = new GitUtils()
	def pdDockerRegistryUrl = "https://pd-gitlab.deloitte.com:8000"
	def serverIPToRunOn = miscUtils.getIpByNodeName('USLinux2')
	def licensesDir = "${jobWorkspace}/licenses"
	
	buildName "#${env.BUILD_ID} - ${buildTag}"
	buildDescription "buildTag=${buildTag}"
	
	stage('clean docker images') {
		//sh "docker image prune -a -f"
	}

	stage('stop and remove running containers') {
		def user = miscUtils.getUserID()
		def group = miscUtils.getGroupID()
		sh "docker compose -f ${jobWorkspace}/docker-compose.yaml down"
		sh 'docker compose rm -s -v --force'
		docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside('-u=0') {
			sh "chown -hR ${user}:${group} *"
		}
		deleteDir()
	}
	
	stage('pull licenses') {
		def gitProject = "${gitUtils.getGitUrlQef()}/infrastructure/licenses.git"
		sh "mkdir ${licensesDir}"
		dir(licensesDir) {
			git branch: 'master', credentialsId: 'USAppModQMUserSVC-User-PW', url: gitProject
		}
		sh "mv ${licensesDir}/modernization/enable-all-modernization-qa-with-db-cutter.lic ${jobWorkspace}/innowake.lic"
	}
	
	stage('pull docker-compose.yaml') {
		withCredentials([string(credentialsId: 'pd-gitlab-registry-token', variable: 'registryAccessToken')]) {
			sh "curl -k --header \"PRIVATE-TOKEN: ${registryAccessToken}\" \"https://pd-gitlab.deloitte.com//api/v4/projects/103/packages/generic/db-cutter/${buildTag}/docker-compose.yaml\" --output ${jobWorkspace}/docker-compose.yaml"
		}
	}
	
	if (buildTag.contains("+")) {
		buildTag = buildTag.replaceAll("\\+", "_")
		echo "recognized rebuild version: converted buildTag to ${buildTag}"  
	}

	stage('pull db-cutter-docs') {
		dockerUtils.pullImageFromCustomRegistry(pdDockerRegistryUrl, "pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry/db-cutter-docs:${buildTag}", 'USAppModQMUserSVC-User-PW')
	}
	
	stage('pull db-cutter-services') {
		dockerUtils.pullImageFromCustomRegistry(pdDockerRegistryUrl, "pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry/db-cutter-service:${buildTag}", 'USAppModQMUserSVC-User-PW')
	}
	
	stage('pull db-cutter-ui') {
		dockerUtils.pullImageFromCustomRegistry(pdDockerRegistryUrl, "pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry/db-cutter-ui:${buildTag}", 'USAppModQMUserSVC-User-PW')
	}
	
	stage('pull phaidon-keycloak') {
		dockerUtils.pullImageFromCustomRegistry(pdDockerRegistryUrl, "pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry/phaidon-keycloak:1.1.0", 'USAppModQMUserSVC-User-PW')
	}
	
	stage('run docker-compose.yaml') {
		sh "sed -i 's/localhost/${serverIPToRunOn}/g' ${jobWorkspace}/docker-compose.yaml"
		sh "docker compose -f ${jobWorkspace}/docker-compose.yaml up -d"
		echo "DB Cutter environment started on: http://${serverIPToRunOn}:5001"
		sleep 60
	}
	
	stage('create demo data') {
		def buildResult = build job: 'Mod_Selenium_Testjob', propagate: false, parameters: [
			string(name: 'buildTag', value: buildTag), 
			string(name: 'testcase', value: 'testcases.dbcutter.StagingDemoData'),
			[$class: 'NodeParameterValue', name: 'executeTestAgainst', labels: ['USLinux2'], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
		miscUtils.evaluateBuildResult(buildResult)
	}
}
