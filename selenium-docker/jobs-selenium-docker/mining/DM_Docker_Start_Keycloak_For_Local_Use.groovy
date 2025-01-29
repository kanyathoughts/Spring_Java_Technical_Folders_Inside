@Library('TestUtils') _
/**
 * This job creates a dockerized Keycloak instance to connect a local api-server. Therefore, following parameters have to be specified.
 * 
 * @param miningBuildVersion    The build to use for the mining keycloak plugin.
 * @param executeOn          	The node where to run the instance.
 */

node(executeOn) {
    def jobWorkspace = pwd()
    def svnUtils = new SvnUtils()
    def spUtils = new SharepointUtils()
	def artifactDir = "${spUtils.getJenkinsArtifactsDir()}/IW_Copy_MxBuildVersion/${miningBuildVersion}"
	def miscUtils = new MiscUtils()
    def dockerUtils = new DockerUtils()
	def hostname = miscUtils.getHostname()
	def keycloakName = 'keycloak_local_use'
	buildName "#${env.BUILD_ID} - ${miningBuildVersion}"	
	buildDescription "miningBuildVersion=${miningBuildVersion} executeOn=${executeOn}"
	
	stage('stop and remove docker container') {
		sh returnStatus: true, script: "docker rm -f ${keycloakName}"
	}

    stage('checkout and copy') {
    	deleteDir()
    	docker.image(dockerUtils.pullJenkinsEnvironmentImage('java11')).inside {
    		withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'svnPw', usernameVariable: 'svnUser')]) {
	    		svnUtils.svnExportRecursive("${svnUtils.getSvnUrlQa()}/projects/mining_keycloak_for_local_use", "${jobWorkspace}/keycloak", svnUser, svnPw)
    		}
    		if (! spUtils.downloadFile("${artifactDir}/mining-keycloak-theme-${miningBuildVersion}.jar", './')) {
	            error "Download of ${artifactDir}/mining-keycloak-theme-${miningBuildVersion}.jar failed"
	        }
	        if (! spUtils.downloadFile("${artifactDir}/mining-keycloak-extension-${miningBuildVersion}.jar", './')) {
	            error "Download of ${artifactDir}/mining-keycloak-extension-${miningBuildVersion}.jar failed"
	        }
    	}
        sh "sed -i 's/VARIP:8180/${hostname}:8181/g' ${jobWorkspace}/keycloak/insertUserData.sh"
        sh "sed -i 's/VARIP:8180/${hostname}:8181/g' ${jobWorkspace}/keycloak/selectTheme.sh"
        sh "sed -i 's/APISERVERIP/localhost/g' ${jobWorkspace}/keycloak/mining-realm.json"
        //copy and unzip theme of the specified build into the job workspace
    	sh "unzip ./mining-keycloak-theme-${miningBuildVersion}.jar"
    	sh "mv ${jobWorkspace}/theme/innowake-theme ${jobWorkspace}/keycloak/innowake-theme"
        //copy extension plugin of the specified build into the job workspace
    	sh "cp ./mining-keycloak-extension-${miningBuildVersion}.jar ${jobWorkspace}/keycloak/mining-keycloak-extension.jar"
    }
    
    stage('create and run container') {    
        def environment = '-e KEYCLOAK_USER=admin -e KEYCLOAK_PASSWORD=Worx2000 -e KEYCLOAK_IMPORT=/tmp/mining-realm.json'
		def image = 'qef-linux1-de.deloitte.com:5000/jboss/keycloak:15.1.1'
		def ports = '-p 8181:8080'
		def volumes = "-v ${jobWorkspace}/keycloak/insertUserData.sh:/opt/jboss/tools/insertUserData.sh " + 
						"-v ${jobWorkspace}/keycloak/selectTheme.sh:/opt/jboss/tools/selectTheme.sh " +
						"-v ${jobWorkspace}/keycloak/mining-realm.json:/tmp/mining-realm.json " +
						"-v ${jobWorkspace}/keycloak/innowake-theme:/opt/jboss/keycloak/themes/innowake-theme " +
						"-v ${jobWorkspace}/keycloak/mining-keycloak-extension.jar:/opt/jboss/tools/mining-keycloak-extension.jar"		
		docker.withRegistry(dockerUtils.getEuDockerRegistryUrl(), 'EU-Docker-Registry-User') {
        	docker.image(image).run("--name ${keycloakName} ${environment} ${ports} ${volumes}")
		}
    }
    
    stage('create user data') {
		retry(5) {
			sleep 30
			sh "docker exec ${keycloakName} sh /opt/jboss/tools/insertUserData.sh"
		}
    }
    
    stage('install extension plugin') {    
        sh "docker exec ${keycloakName} /opt/jboss/keycloak/bin/jboss-cli.sh --command=\"module add --name=innowake.products.mining.mining-keycloak-extension --resources=/opt/jboss/tools/mining-keycloak-extension.jar --dependencies=org.keycloak.keycloak-core,org.keycloak.keycloak-services,org.keycloak.keycloak-model-jpa,org.keycloak.keycloak-server-spi,org.keycloak.keycloak-server-spi-private,javax.ws.rs.api,javax.persistence.api,org.hibernate,org.javassist,org.liquibase\""
        sleep 5
        //replace the existing "standalone-ha.xml" in the container with the modified one from SVN which contains the mining-keycloak-extension plugin as authentication provider
        sh "docker cp ${jobWorkspace}/keycloak/standalone-ha.xml ${keycloakName}:/opt/jboss/keycloak/standalone/configuration"
        //run the migration script
        sh "docker exec ${keycloakName} /opt/jboss/keycloak/bin/jboss-cli.sh --file=/opt/jboss/keycloak/bin/migrate-standalone-ha.cli"
    }
    
    stage('restart container') {
        sh "docker restart ${keycloakName}"
    }
    
    stage('select innoWake theme') {
		retry(5) {
			sleep 30
			sh "docker exec ${keycloakName} sh /opt/jboss/tools/selectTheme.sh"
		}
        sleep 10
        echo "Keycloak instance running on http://${hostname}:8181"
    }
}