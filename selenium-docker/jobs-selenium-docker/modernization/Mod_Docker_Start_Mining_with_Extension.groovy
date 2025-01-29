@Library('TestUtils') _
/**
 * This job creates a dockerized Mining environment. Therefore, following parameters have to be specified.
 *
 * @param mxBuildVersion     	The build to use, if not specified it will take the latest available build
 * @param executeOn          	The node where to run the environment.
 * @param artifactVersion    	The trunk version used within the mining artifacts (jars). This is because of the mining rebuild mechanism.
 * @param javaVersion			The java version the test will run with
 * @param buildTag				The build tag for the DB Cutter export extension
 */

node(executeOn) {
    def jobWorkspace = pwd()
    def gitUtils = new GitUtils()
    def spUtils = new SharepointUtils()
    def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def dockerDir = "${jobWorkspace}/docker"
	def gitDirLicenses = "${jobWorkspace}/gitLicenses"
	def nmsloDir = "${jobWorkspace}/nmslo"
    def originalBuildVersion 
 	def artifactDir 
    def orientDbName = 'orient-db'
    def apiServerName = 'api-server'
	def serverIPToRunOn = miscUtils.getIpByNodeName(executeOn)
	
    stage('stop and remove running containers') {
        sh returnStatus: true, script: "docker rm -f ${orientDbName}"
        sh returnStatus: true, script: "docker rm -f ${apiServerName}"
    }

    stage('create keycloak instance') {
	    docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {

            if(!mxBuildVersion && !artifactVersion) {
                def miningArtifactDir = "${spUtils.getJenkinsArtifactsDir()}/IW_Copy_MxBuildVersion"
                echo miningArtifactDir

                def artifacts = spUtils.listFolders("${miningArtifactDir}/").findAll {
                    it.startsWith("99")
                }
                artifacts = artifacts.sort().reverse()
                for (int i = 0; i < artifacts.size(); i++) {
                    def contents = spUtils.listFiles("${miningArtifactDir}/${artifacts.get(i)}/").findAll {
                        it.startsWith("orientdb-mining")
                    }
                    if(!contents.isEmpty()) {
                        mxBuildVersion = artifacts.get(i)
                        break
                    }
                }
               
                echo mxBuildVersion
            }

	        if (artifactVersion) {
                mxBuildVersion = artifactVersion
                buildDescription "${currentBuild.description} artifactVersion=${artifactVersion}"
            }

            buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
            buildDescription "mxBuildVersion=${mxBuildVersion} buildTag=${buildTag} executeOn=${executeOn}"

            originalBuildVersion = mxBuildVersion

            build job: 'DM_Docker_Start_Keycloak_Instance', wait: true, propagate: false, parameters: [
				string(name: 'mxBuildVersion', value: originalBuildVersion),
				[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeOn], nodeEligibility: [$class: 'AllNodeEligibility']]
			]
    } 
    }
    

    stage('get selected build and checkout docker files') {
        deleteDir()
       	def gitlabProject
       	def gitlabProjectLicense
        def nmsloProject
        docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {

            artifactDir = "${spUtils.getJenkinsArtifactsDir()}/IW_Copy_MxBuildVersion/${mxBuildVersion}"

            withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
       	        gitlabProject = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/mining-db-dumps.git"
       	        gitlabProjectLicense = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/licenses.git"
                nmsloProject = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/innowake-test-projects/modernization-test-projects/nmslo_discovered.git"
            }
           
			dir(dockerDir) {
			    sh "git clone --branch master ${gitlabProject} ${dockerDir}"
			}
			
			dir(nmsloDir) {
                sh "git clone --branch master ${nmsloProject} ${nmsloDir}"
			}
			
			dir(gitDirLicenses) {
				sh "git clone --depth 1 --branch master ${gitlabProjectLicense} ${gitDirLicenses}"
			}	

			if (! spUtils.downloadFile("${artifactDir}/orientdb-mining-${mxBuildVersion}.zip", './')) {
				error "Download of ${artifactDir}/orientdb-mining-${mxBuildVersion}.zip failed"
			}
	        
	        if (! spUtils.downloadFile("${artifactDir}/mining-api-server-dist-${mxBuildVersion}.jar", './')) {
	        	error "Download of ${artifactDir}/mining-api-server-dist-${mxBuildVersion}.jar failed"	
	        }
			
			dir(jobWorkspace) {
				withCredentials([string(credentialsId: 'pd-gitlab-registry-token', variable: 'registryAccessToken')]) {
					sh "curl -k --header \"PRIVATE-TOKEN: ${registryAccessToken}\" \"https://pd-gitlab.deloitte.com//api/v4/projects/103/packages/generic/db-cutter/${buildTag}/db-cutter-mining-extensions-${buildTag}.jar\" --output ${jobWorkspace}/db-cutter-mining-extensions-${buildTag}.jar"
				}
			}
		}
    }

    stage('get orientDB binaries from latest build') {
		sh "unzip ./orientdb-mining-${mxBuildVersion}.zip -d ${jobWorkspace}/orientDb"
        // Add test-data statement from job workspace to /bin which will be mounted into the oriendtb container
        sh "cp ${jobWorkspace}/docker/backup_Txdmv.zip ${jobWorkspace}/orientDb/bin"
        sh "chmod -R 777 ${jobWorkspace}/orientDb"
        sh "chmod -R 777 ${jobWorkspace}/docker"
        //copy the db dump to /tmp for the DM_Docker_Restore_DB_Backup job
       // sh "cp ${jobWorkspace}/docker/backup_Txdmv.zip /tmp/backup_Txdmv.zip"
        sh "cp ${jobWorkspace}/nmslo/backup_txdmv_nmslo.zip /tmp/backup_txdmv_nmslo.zip"
    }

    stage('orientDB Container setup and restore data') {
        sh "docker create --name ${orientDbName} -p 2424:2424 -p 2480:2480 -e \"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/orientDb/bin\" --entrypoint /orientDb/bin/server.sh " + dockerUtils.pullJenkinsEnvironmentImage(javaVersion)
        sh "./orientDb/bin/console.sh \"connect plocal:orientDb/databases/mining admin admin; DROP DATABASE; CREATE DATABASE plocal:orientDb/databases/mining admin admin; RESTORE DATABASE ${jobWorkspace}/nmslo/backup_txdmv_nmslo.zip; CREATE INDEX Module_name_ft ON Module(name) FULLTEXT ENGINE LUCENE METADATA { \"allowLeadingWildcard\": true, \"default\": \"innowake.mining.data.plugin.lucene.LowercasingKeywordAnalyzer\" };\""
       // sh "./orientDb/bin/console.sh \"connect plocal:orientDb/databases/mining admin admin; DROP DATABASE; CREATE DATABASE plocal:orientDb/databases/mining admin admin; RESTORE DATABASE ${jobWorkspace}/nmslo/nmslo_discovered.zip; CREATE INDEX Module_name_ft ON Module(name) FULLTEXT ENGINE LUCENE METADATA { \"allowLeadingWildcard\": true, \"default\": \"innowake.mining.data.plugin.lucene.LowercasingKeywordAnalyzer\" };\""
        sh "docker cp ${jobWorkspace}/orientDb ${orientDbName}:/orientDb"
		sleep 60
    }

    stage('start orientdb') {
        sh "docker start ${orientDbName}"
		sleep 60
    }

    stage('setup and run api server') {
    	def startCommand = "nohup java -Dloader.path=./db-cutter-mining-extensions-${buildTag}.jar -Dserver.port=8080 -Djava.awt.headless=true -Dspring.datasource.url=jdbc:orient:remote:${serverIPToRunOn}:2424/mining -cp ./mining-api-server-dist.jar org.springframework.boot.loader.PropertiesLauncher --mining.cookieId=DISABLED --spring.profiles.active=authorized-access --keycloak.auth-server-url=http://${serverIPToRunOn}:8180/auth migrateToKeycloak --username=first --password=first --debug"
        sh "cp ./mining-api-server-dist*.jar ${jobWorkspace}/mining-api-server-dist.jar"
        sh "chmod -R 777 ${jobWorkspace}"
        docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).run("--name ${apiServerName} -p 8080:8080 -v ${jobWorkspace}/mining-api-server-dist.jar:/mining-api-server-dist.jar -v ${jobWorkspace}/db-cutter-mining-extensions-${buildTag}.jar:/db-cutter-mining-extensions-${buildTag}.jar -v ${gitDirLicenses}/mining/innowake.lic:/innowake.lic", "bash -c \"${startCommand}\"")
        sleep 120
    }
    
	stage('create metadata') {
		def miningServer = getMiningServerURL(executeOn)
		build job: 'DM_Metadata_Creation', wait: true, propagate: false, parameters: [string(name: 'miningServerURL', value: miningServer)]
	} 
}

/**
 * Returns the mining server URL of a given Jenkins node 
 * 
 * @param executeOn the Jenkins node
 * @return miningServerUrl the URL of the Jenkins node
 */
def getMiningServerURL(executeOn) {
	switch(executeOn) {
		case 'USLinux10-Mod':
			return 'http://qef-linux10-us-mod.deloitte.com:8080/'
		case 'USLinux8-DM':
			return 'http://qef-linux8-us-dm.deloitte.com:8080/'
	}
}
