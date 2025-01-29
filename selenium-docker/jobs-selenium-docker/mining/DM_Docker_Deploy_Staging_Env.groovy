@Library('TestUtils') _
/**
 * Deploy a specified mining version to the official Mining Staging Server (USLinux2).
 * This may only take place after the smoke-tests of the specified version were successful.
 * 
 * See https://amiconfluence.deloitte.com/display/KB/Mining+staging-server
 * 
 * @param miningBuildVersion	The mining build to deploy.
 * @param APIServer				The port where the api-server should run.
 * @param OrientDBStudio		The port where OrientDB Studio should run.
 * @param OrientDBbin			The port where the OrientDB should run.
 * @param javaVersion			Sets the java version which shall be used.
 */

node('USLinux2') {
	def jobWorkspace = pwd()
	def gitUtils = new GitUtils()
	def spUtils = new SharepointUtils()
	def dockerUtils = new DockerUtils()
	def dockerDir = "${jobWorkspace}/docker"
	def artifactDir = "${spUtils.getJenkinsArtifactsDir()}/IW_Copy_MxBuildVersion/${miningBuildVersion}"
	def gitDirLicenses = "${jobWorkspace}/gitLicenses"
	def usTestLinux2IP = '10.241.173.106'
	def orientDbName = 'orient-db'
	def apiServerName = 'api-server'
	def featuresToBeEnabled = ['eclipseDeepLink', 'incrementalScan']
	def originalBuildVersion = miningBuildVersion
		
	buildName "#${env.BUILD_ID} - ${miningBuildVersion}"

	stage('stop and remove running containers') {
		sh returnStatus: true, script: "docker rm -f ${orientDbName}"
        sh returnStatus: true, script: "docker rm -f ${apiServerName}"
	}
	
	stage('start keycloak server') {
		def buildResult = build job: 'DM_Docker_Start_Keycloak_Instance', wait: true, propagate: false, parameters: [
			string(name: 'mxBuildVersion', value: originalBuildVersion),
			[$class: 'NodeParameterValue', name: 'executeOn', labels: ['USLinux2'], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
	}
	
	stage('get selected build and checkout files') {
		deleteDir()
		def gitlabProject
		def gitlabProjectLicense
		docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside {
            withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
       	        gitlabProject = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/mining-job-artifacts.git"
       	        gitlabProjectLicense = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/licenses.git"
            }
			dir(dockerDir) {
			    sh "git clone --depth 1 --branch master ${gitlabProject} ${dockerDir}"
			}
			dir(gitDirLicenses) {
				sh "git clone --depth 1 --branch master ${gitlabProjectLicense} ${gitDirLicenses}"
			}
			if (! spUtils.downloadFile("${artifactDir}/orientdb-mining-${miningBuildVersion}.zip", './')) {
	            error "Download of ${artifactDir}/orientdb-mining-${miningBuildVersion}.zip failed"
	        }
			if (! spUtils.downloadFile("${artifactDir}/mining-api-server-dist-${miningBuildVersion}.jar", './')) {
	            error "Download of ${artifactDir}/mining-api-server-dist-${miningBuildVersion}.jar failed"
	        }
		}
	}
	
	stage('get orientDB binaries from build') {
        sh "unzip ./orientdb-mining-${miningBuildVersion}.zip -d ${jobWorkspace}/orientDb"
        // Add test-data statement from job workspace to /bin which will be mounted into the oriendtb container
        sh "cp ${jobWorkspace}/docker/resources/stagingServerBackup.zip ${jobWorkspace}/orientDb/bin"
        sh "chmod -R 777 ${jobWorkspace}/orientDb"
	}
	
	stage('orientDB container run') {
		sh "docker create --name ${orientDbName} -p ${OrientDBbin}:2424 -p ${OrientDBStudio}:2480 -e \"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/orientDb/bin\" --entrypoint /orientDb/bin/server.sh " + dockerUtils.pullJenkinsEnvironmentImage(javaVersion)
        sh "docker cp ${jobWorkspace}/orientDb ${orientDbName}:/orientDb"
        sh "docker start ${orientDbName}"
		sleep 60
	}
	
	stage('import mining-staging data') {
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; DROP DATABASE; CREATE DATABASE plocal:orientDb/databases/mining admin admin; RESTORE DATABASE /orientDb/bin/stagingServerBackup.zip\""
		sleep 30	
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; CREATE INDEX Module_name_ft ON Module(name) FULLTEXT ENGINE LUCENE METADATA { \"allowLeadingWildcard\": true, \"default\": \"innowake.mining.data.plugin.lucene.LowercasingKeywordAnalyzer\" };\""
	}
	
	stage('create custom properties') {
		//preparation
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; CREATE CLASS AnnotationCustomProperties IF NOT EXISTS EXTENDS CustomProperties;\""
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; UPDATE Project SET customPropertyClasses[\\\"Annotation\\\"] = [\\\"AnnotationCustomProperties\\\"];\""
		//Input/String properties
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; CREATE PROPERTY AnnotationCustomProperties.testPropertyString IF NOT EXISTS String;\""
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; ALTER PROPERTY AnnotationCustomProperties.testPropertyString CUSTOM label=\\\"Custom text\\\";\""
		//Repeater properties
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; CREATE PROPERTY AnnotationCustomProperties.myRepeater IF NOT EXISTS EMBEDDEDLIST STRING;\""
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; ALTER PROPERTY AnnotationCustomProperties.myRepeater CUSTOM label = \\\"Custom list\\\";\""
		//Tag properties
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; CREATE CLASS ProjectCustomProperties IF NOT EXISTS EXTENDS CustomProperties;\""
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; UPDATE Project SET customPropertyClasses[\\\"Project\\\"] = [\\\"ProjectCustomProperties\\\"];\""
		
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; CREATE PROPERTY ProjectCustomProperties.autoCompletionMap IF NOT EXISTS EMBEDDEDMAP EMBEDDEDLIST;\""
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; ALTER PROPERTY ProjectCustomProperties.autoCompletionMap CUSTOM label=\\\"Known Annotation Tags\\\";\""
		
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; CREATE PROPERTY AnnotationCustomProperties.colorTags IF NOT EXISTS EMBEDDEDLIST STRING;\""		
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; ALTER PROPERTY AnnotationCustomProperties.colorTags CUSTOM label=\\\"Custom tag\\\";\""
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; ALTER PROPERTY AnnotationCustomProperties.colorTags CUSTOM fieldType=\\\"TAG\\\";\""
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; ALTER PROPERTY AnnotationCustomProperties.colorTags CUSTOM autoCompletionKey=\\\"colorTags\\\";\""
		
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; CREATE PROPERTY AnnotationCustomProperties.ruleTags IF NOT EXISTS EMBEDDEDLIST STRING;\""
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; ALTER PROPERTY AnnotationCustomProperties.ruleTags CUSTOM label=\\\"Another tag example\\\";\""
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; ALTER PROPERTY AnnotationCustomProperties.ruleTags CUSTOM fieldType=\\\"TAG\\\";\""
		sh "docker exec ${orientDbName} sh /orientDb/bin/console.sh \"connect plocal:/orientDb/databases/mining admin admin; ALTER PROPERTY AnnotationCustomProperties.ruleTags CUSTOM autoCompletionKey=\\\"ruleTags\\\";\""
		
		sleep 20
	}
	
	stage('api-server container run') {
		def startCommand = "nohup java -jar -Dserver.port=8080 -Djava.awt.headless=true -Dspring.datasource.url=jdbc:orient:remote:${usTestLinux2IP}:${OrientDBbin}/mining ./mining-api-server-dist.jar --spring.profiles.active=authorized-access --mining.cookieId=DISABLED --keycloak.auth-server-url=http://${usTestLinux2IP}:8180/auth migrateToKeycloak --username=first --password=first --debug"
        sh "cp ./mining-api-server-dist-${miningBuildVersion}.jar ${jobWorkspace}/mining-api-server-dist.jar"
        sh "chmod 777 ${jobWorkspace}/mining-api-server-dist.jar"
        docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).run("--name ${apiServerName} -p ${APIServer}:8080 -v ${jobWorkspace}/mining-api-server-dist.jar:/api-server/mining-api-server-dist.jar -v ${gitDirLicenses}/mining/innowake.lic:/api-server/innowake.lic -w /api-server", "bash -c \"${startCommand}\"")
        sleep 60
	}
	
	stage('create user data') {
		sh 'docker exec keycloak-server sh /opt/jboss/tools/insertUserData.sh'
	}
		
	stage('enable features') {   	
		def body = 'grant_type=password&client_id=backend&username=first&password=first'
		def http = new URL("http://${usTestLinux2IP}:8180/auth/realms/mining/protocol/openid-connect/token").openConnection()
		http.setRequestMethod('POST')
		http.setDoOutput(true)
		http.setRequestProperty('Accept', 'application/json')
		http.setRequestProperty('Content-Type', 'application/x-www-form-urlencoded')
		http.outputStream.write(body.getBytes('UTF-8'))
		http.connect()
		def response = [:]
		def accessToken = ''
		if (http.responseCode == 200) {
			response = http.inputStream.getText('UTF-8')
			http = null
			def accessString = readJSON text: response
			accessToken = accessString['access_token']
		} else {
			response = http.errorStream.getText('UTF-8')
			http = null
			def accessString = readJSON text: response
			echo "response: ${accessString}"
			unstable 'Could not retrieve access token from keycloak.'
		}
		try {
			featuresToBeEnabled.each {
				feature ->
				echo "Enabling '${feature}'"
				def post = new URL("http://${usTestLinux2IP}:${APIServer}/api/v1/features/${feature}/toggle?state=true").openConnection();
				post.setRequestMethod("POST")
				post.setDoOutput(true)
				post.setRequestProperty('Authorization', "Bearer ${accessToken}")
				def responseCode = post.getResponseCode()
				if (responseCode.equals(204)) {
					echo '204 - OK'
				} else {
					echo responseCode
					unstable "Response code for enabling feature '${feature}' in the feature console is not as expected, please verify if the feature actually exists in the feature console of ${executeOn}."
				}
			}
		} catch (e) {
			unstable "Could not set feature toggles: ${e}"
		}
	}
}
