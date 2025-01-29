@Library('TestUtils') _
/**
 * This job publishes the db cutter artifacts of the specified version from the PD GitLab to the public Consulting GitLab at "/public/delivery/modernization/db-cutter".
 * 
 * https://gitlab.consulting.sltc.com/appmod/public/delivery/modernization/db-cutter/container_registry
 *
 * @param buildTag		The db cutter version/tag to publish. ONLY APPROVED RELEASE BUILDS!
 */
node('USLinux10-Mod') {
	def jobWorkspace = pwd()
	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def pdDockerRegistryUrl = "https://pd-gitlab.deloitte.com:8000"
	def pdDockerRegistryLocation = "pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry"
	def deliveryDockerRegistryUrl = "https://gitlab.consulting.sltc.com:8000"
	def deliveryDockerRegistryLocation = "gitlab.consulting.sltc.com:8000/appmod/public/delivery/modernization/db-cutter"
	def buildTagUnderscore
	
	buildName "#${env.BUILD_ID} - ${buildTag}"
	buildDescription "buildTag=${buildTag}"
	
	stage('pull docker-compose.yaml') {
		deleteDir()
		withCredentials([string(credentialsId: 'pd-gitlab-registry-token', variable: 'registryAccessToken')]) {
			sh "curl -k --header \"PRIVATE-TOKEN: ${registryAccessToken}\" \"https://pd-gitlab.deloitte.com//api/v4/projects/103/packages/generic/db-cutter/${buildTag}/docker-compose.yaml\" --output ${jobWorkspace}/docker-compose.yaml"
		}
	}
	
	if (buildTag.contains("+")) {
		buildTagUnderscore = buildTag.replaceAll("\\+", "_")
		echo "recognized rebuild version: created buildTagUnderscore ${buildTagUnderscore}"  
	} else {
		buildTagUnderscore = buildTag
	}

	stage('pull images') {
		dockerUtils.pullImageFromCustomRegistry(pdDockerRegistryUrl, "pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry/db-cutter-docs:${buildTagUnderscore}", 'USAppModQMUserSVC-User-PW')
		dockerUtils.pullImageFromCustomRegistry(pdDockerRegistryUrl, "pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry/db-cutter-service:${buildTagUnderscore}", 'USAppModQMUserSVC-User-PW')
		dockerUtils.pullImageFromCustomRegistry(pdDockerRegistryUrl, "pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry/db-cutter-ui:${buildTagUnderscore}", 'USAppModQMUserSVC-User-PW')
		dockerUtils.pullImageFromCustomRegistry(pdDockerRegistryUrl, "pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry/phaidon-keycloak:1.1.0", 'USAppModQMUserSVC-User-PW')
	}
	
	stage('re-tag images') {
		sh "docker tag pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry/db-cutter-docs:${buildTagUnderscore} gitlab.consulting.sltc.com:8000/appmod/public/delivery/modernization/db-cutter/db-cutter-docs:${buildTagUnderscore}"
		sh "docker tag pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry/db-cutter-service:${buildTagUnderscore} gitlab.consulting.sltc.com:8000/appmod/public/delivery/modernization/db-cutter/db-cutter-service:${buildTagUnderscore}"
		sh "docker tag pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry/db-cutter-ui:${buildTagUnderscore} gitlab.consulting.sltc.com:8000/appmod/public/delivery/modernization/db-cutter/db-cutter-ui:${buildTagUnderscore}"
		sh "docker tag pd-gitlab.deloitte.com:8000/innowake/dbcutter-registry/phaidon-keycloak:1.1.0 gitlab.consulting.sltc.com:8000/appmod/public/delivery/modernization/db-cutter/phaidon-keycloak:1.1.0"
	}
	
	stage('push images') {
		docker.withRegistry(deliveryDockerRegistryUrl, 'USAppModQMUserSVC-User-PW') {
			docker.image("gitlab.consulting.sltc.com:8000/appmod/public/delivery/modernization/db-cutter/db-cutter-docs:${buildTagUnderscore}").push()
			docker.image("gitlab.consulting.sltc.com:8000/appmod/public/delivery/modernization/db-cutter/db-cutter-service:${buildTagUnderscore}").push()
			docker.image("gitlab.consulting.sltc.com:8000/appmod/public/delivery/modernization/db-cutter/db-cutter-ui:${buildTagUnderscore}").push()
			docker.image("gitlab.consulting.sltc.com:8000/appmod/public/delivery/modernization/db-cutter/phaidon-keycloak:1.1.0").push()
		}
	}
	
	stage('modify docker-compose.yaml') {
		sh "sed -i 's,${pdDockerRegistryLocation},${deliveryDockerRegistryLocation},g' ${jobWorkspace}/docker-compose.yaml"
	}
	
	stage('archive docker-compose.yaml') {
		dir(jobWorkspace) {
        	archiveArtifacts "docker-compose.yaml"
		}
	}
	
	stage('pull package registry content') {
		dir(jobWorkspace) {
			withCredentials([string(credentialsId: 'pd-gitlab-registry-token', variable: 'registryAccessToken')]) {
				sh "curl -k --header \"PRIVATE-TOKEN: ${registryAccessToken}\" \"https://pd-gitlab.deloitte.com//api/v4/projects/103/packages/generic/db-cutter/${buildTag}/db-crawler-${buildTag}.jar\" --output ${jobWorkspace}/db-crawler-${buildTag}.jar"
			}
			withCredentials([string(credentialsId: 'pd-gitlab-registry-token', variable: 'registryAccessToken')]) {
				sh "curl -k --header \"PRIVATE-TOKEN: ${registryAccessToken}\" \"https://pd-gitlab.deloitte.com//api/v4/projects/103/packages/generic/db-cutter/${buildTag}/db-cutter-mining-extensions-${buildTag}.jar\" --output ${jobWorkspace}/db-cutter-mining-extensions-${buildTag}.jar"
			}
            withCredentials([string(credentialsId: 'pd-gitlab-registry-token', variable: 'registryAccessToken')]) {
				sh "curl -k --header \"PRIVATE-TOKEN: ${registryAccessToken}\" \"https://pd-gitlab.deloitte.com//api/v4/projects/103/packages/generic/db-cutter/${buildTag}/db-cutter-service-${buildTag}.jar\" --output ${jobWorkspace}/db-cutter-service-${buildTag}.jar"
			}
            withCredentials([string(credentialsId: 'pd-gitlab-registry-token', variable: 'registryAccessToken')]) {
				sh "curl -k --header \"PRIVATE-TOKEN: ${registryAccessToken}\" \"https://pd-gitlab.deloitte.com//api/v4/projects/103/packages/generic/db-cutter/${buildTag}/db-crawler-zOs-${buildTag}.jar\" --output ${jobWorkspace}/db-crawler-zOs-${buildTag}.jar"
			}
            withCredentials([string(credentialsId: 'pd-gitlab-registry-token', variable: 'registryAccessToken')]) {
				sh "curl -k --header \"PRIVATE-TOKEN: ${registryAccessToken}\" \"https://pd-gitlab.deloitte.com//api/v4/projects/103/packages/generic/db-cutter/${buildTag}/db-cutter-ui-${buildTag}.zip\" --output ${jobWorkspace}/db-cutter-ui-${buildTag}.zip"
			}
		}
	}
	
	stage('push package registry content') {
		dir(jobWorkspace) {
			withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'consultingRegistryAccessToken')]) {
				sh "curl -k --header \"PRIVATE-TOKEN: ${consultingRegistryAccessToken}\" --upload-file \"${jobWorkspace}/docker-compose.yaml\" \"https://gitlab.consulting.sltc.com//api/v4/projects/1397/packages/generic/db-cutter/${buildTag}/docker-compose.yaml\""
			}
			withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'consultingRegistryAccessToken')]) {
				sh "curl -k --header \"PRIVATE-TOKEN: ${consultingRegistryAccessToken}\" --upload-file \"${jobWorkspace}/db-crawler-${buildTag}.jar\" \"https://gitlab.consulting.sltc.com//api/v4/projects/1397/packages/generic/db-cutter/${buildTag}/db-crawler-${buildTag}.jar\""
			}
			withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'consultingRegistryAccessToken')]) {
				sh "curl -k --header \"PRIVATE-TOKEN: ${consultingRegistryAccessToken}\" --upload-file \"${jobWorkspace}/db-cutter-mining-extensions-${buildTag}.jar\" \"https://gitlab.consulting.sltc.com//api/v4/projects/1397/packages/generic/db-cutter/${buildTag}/db-cutter-mining-extensions-${buildTag}.jar\""
			}
            withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'consultingRegistryAccessToken')]) {
				sh "curl -k --header \"PRIVATE-TOKEN: ${consultingRegistryAccessToken}\" --upload-file \"${jobWorkspace}/db-cutter-service-${buildTag}.jar\" \"https://gitlab.consulting.sltc.com//api/v4/projects/1397/packages/generic/db-cutter/${buildTag}/db-cutter-service-${buildTag}.jar\""
			}
            withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'consultingRegistryAccessToken')]) {
				sh "curl -k --header \"PRIVATE-TOKEN: ${consultingRegistryAccessToken}\" --upload-file \"${jobWorkspace}/db-crawler-zOs-${buildTag}.jar\" \"https://gitlab.consulting.sltc.com//api/v4/projects/1397/packages/generic/db-cutter/${buildTag}/db-crawler-zOs-${buildTag}.jar\""
			}
            withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'consultingRegistryAccessToken')]) {
				sh "curl -k --header \"PRIVATE-TOKEN: ${consultingRegistryAccessToken}\" --upload-file \"${jobWorkspace}/db-cutter-ui-${buildTag}.zip\" \"https://gitlab.consulting.sltc.com//api/v4/projects/1397/packages/generic/db-cutter/${buildTag}/db-cutter-ui-${buildTag}.zip\""
			}
		}
	}
}
