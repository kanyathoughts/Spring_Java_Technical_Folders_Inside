@Library('TestUtils') _
/**
 * This job creates a db cutter environment of a specific version on the selected node.
 * 
 * @param buildTag			The db cutter version/tag to use.
 * @param executeOn			Node where to create the db cutter environment
 * @param invalidLicense	Starts the environment with an invalid license (used for the LicensingInvalidTest)
 */
node(executeOn) {
	def jobWorkspace = pwd()
	def licensesDir = "${jobWorkspace}/licenses"
	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def gitUtils = new GitUtils()
	def pdDockerRegistryUrl = "https://pd-gitlab.deloitte.com:8000"
	def serverIPToRunOn = miscUtils.getIpByNodeName(executeOn)
	
	buildName "#${env.BUILD_ID} - ${buildTag}"
	buildDescription "buildTag=${buildTag} executeOn=${executeOn} invalidLicense=${invalidLicense} useRedisAsCache=${useRedisAsCache}"
	
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
		
		if (invalidLicense == 'true') {
			sh "mv ${licensesDir}/modernization/modernization-qa-without-db-cutter.lic ${jobWorkspace}/innowake.lic"
		} else {
			sh "mv ${licensesDir}/modernization/enable-all-modernization-qa-with-db-cutter.lic ${jobWorkspace}/innowake.lic"
		}
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
        if (useRedisAsCache == 'false') {
			sh "sed -i 's/UseRedisAsCache=true/UseRedisAsCache=false/g' ${jobWorkspace}/docker-compose.yaml"
            sh "sed '62d;63d;64d;65d;66d' <  ${jobWorkspace}/docker-compose.yaml > ${jobWorkspace}/docker-compose1.yaml"
            sh "rm -f ${jobWorkspace}/docker-compose.yaml"
            sh "mv ${jobWorkspace}/docker-compose1.yaml ${jobWorkspace}/docker-compose.yaml"
            sh "sed -i 's/localhost/${serverIPToRunOn}/g' ${jobWorkspace}/docker-compose.yaml"
		    sh "docker compose -f ${jobWorkspace}/docker-compose.yaml up -d"
		    echo "DB Cutter environment started on: http://${serverIPToRunOn}:5001"
		    sleep 10
        } else {
            sh "sed -i 's/localhost/${serverIPToRunOn}/g' ${jobWorkspace}/docker-compose.yaml"
		    sh "docker compose -f ${jobWorkspace}/docker-compose.yaml up -d"
		    echo "DB Cutter environment started on: http://${serverIPToRunOn}:5001"
		    sleep 10
        }
	}
}
