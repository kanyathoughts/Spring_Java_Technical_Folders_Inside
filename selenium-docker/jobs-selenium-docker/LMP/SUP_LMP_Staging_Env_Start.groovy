@Library('TestUtils') _

/**
 * Job script to build and deploy the Production License Management Portal on the selected server.
 *
 * @param productiveEnvironment The Jenkins node the test will run on
 * @param branchName The branch from which the LMP should be pulled
 */
node('USLinux1') {
    timestamps {
        productiveEnvironment = Boolean.parseBoolean(productiveEnvironment)
        buildName "#${env.BUILD_ID} - LMP Stage"
        buildDescription "productiveEnvironment=${productiveEnvironment}, branchName=${branchName}"
        def dockerUtils = new DockerUtils()
        def miscUtils = new MiscUtils()
        def gitUtils = new GitUtils()
        def workDir = pwd()
        def metaInfFolderName = 'META-INF'
        def metaInfFolderPath = "${workDir}/LMPWeb/target/${metaInfFolderName}"
        def libFolderName = 'WEB-INF/lib'
        def libFolderPath = "${workDir}/LMPWeb/target/${libFolderName}"
        def keysFolder = "${workDir}/sslKeystore"
        def tempFolder = '/tmp'
        def sourcePathLicenseAPI = "${tempFolder}/licenseapi.jar"
        def hostname = miscUtils.getHostname()
        def mySQLDBTable = 'licensing'
        // order is important
        def ctrNames = [
                gitCtr      : 'lmp_gitclone_stage',
                prepareWSCtr: 'lmp_preparews_stage',
                npmCtr      : 'lmp_providenpm_stage',
                webImageCtr : 'lmp_web_stage',
                uiImageCtr  : 'lmp_ui_stage'
        ]
        def networkName = 'lmp_network_stage'

        def webPort = '8007'
        def uiPort = '7007'

        stage('clean up') {
            // no licenses jar, no docker purge in prod env otherwise always purge
            if (productiveEnvironment && !miscUtils.fileExists(sourcePathLicenseAPI)) {
                error("There ist no file with the name licenseapi.jar in the ${tempFolder} folder!")
            }
            dockerPurge(ctrNames, networkName)
            deleteDir()
        }

        stage('initialize') {
            docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside("--name ${ctrNames["gitCtr"]} -v /data/mxJars:/data/mxJars:ro -v jenkins-m2-repo:/var/m2_repo") {
                withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
                    //pull inside image to assure correct file ownership. Otherwise (sometimes) sslKeystore folder would have incorrect ownership
                    sh "git clone --quiet --branch ${branchName} ${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/license-management/license-management-portal.git ."
                }
            }
            if (productiveEnvironment) {
                dir('addlicensejar') {
                    sh "cp ${sourcePathLicenseAPI} ."
                }
                sh "sudo rm -f ${sourcePathLicenseAPI}"
            }
            sh "docker network create -d bridge ${networkName}"
            dir(keysFolder) {
                withCredentials([sshUserPrivateKey(credentialsId: 'ussltc8434CertificatePrivateKey', keyFileVariable: 'ussltc8434CertificatePrivateKey', passphraseVariable: '', usernameVariable: 'Private key for the certificate on ussltc8434')]) {
                    writeFile encoding: 'utf-8', file: 'ussltc8434v_consulting_sltc_com.key', text: readFile(ussltc8434CertificatePrivateKey)
                }
            }
        }

        docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside("--name ${ctrNames["prepareWSCtr"]} --network ${networkName} -v /data/mxJars:/data/mxJars:ro -v jenkins-m2-repo:/var/m2_repo") {
            stage('customize LMP') {
                dir('LMPModifiers') {
                    withMaven(maven: 'Default') {
                        sh "$MVN_CMD -U dependency:resolve"
                    }
                }
                dir('LMPService/src/main/resources') {
                    def filename = 'application.yaml'
                    def yamlContent = readYaml file: filename
                    yamlContent.url = "jdbc:mysql://${hostname}:3307/${mySQLDBTable}"
                    withCredentials([usernamePassword(credentialsId: 'LMP_MySQL_User_Stage', passwordVariable: 'mySQLPw', usernameVariable: 'mySQLUser')]) {
                        yamlContent.dbUserName = mySQLUser
                        yamlContent.dbPassword = mySQLPw
                    }
                    sh "rm ${filename}"
                    writeYaml file: filename, data: yamlContent
                }
                dir('LMPUI/public/css') {
                    sh "sed -i 's,9816f4,000000,g' vendors.bundle.css"
                }

                //WAR file generation and base license.jar
                withMaven(maven: 'Default') {
                    dir('LMPModifiers') {
                        sh "$MVN_CMD -U clean install"
                    }
                    dir('LMPApp') {
                        sh "$MVN_CMD -U clean install"
                        sh "$MVN_CMD dependency:copy"
                        sh "cp ./baselicjar/base-license-management-dist-99.9.99-TRUNK-BASE-SNAPSHOT.jar ${workDir}/LMPWeb/target"
                    }
                }

                //modify war file
                dir("${workDir}/LMPWeb/target") {
                    dir(metaInfFolderPath) {
                        sh "cp ${workDir}/LMPWeb/persistence.xml persistence.xml"
                        sh "sed -i 's,docker-mysql:3306/licensing,${hostname}:3307/${mySQLDBTable},g' persistence.xml"
                        withCredentials([usernamePassword(credentialsId: 'LMP_MySQL_User_Stage', passwordVariable: 'mySQLPw', usernameVariable: 'mySQLUser')]) {
                            sh "sed -i 's,dummyun,${mySQLUser},g' persistence.xml"
                            sh "sed -i 's,dummypw,${mySQLPw},g' persistence.xml"
                        }
                    }
                    sh "zip -u base-license-management-dist-99.9.99-TRUNK-BASE-SNAPSHOT.jar ./${metaInfFolderName}/persistence.xml"
                    dir(libFolderPath) {
                        sh "cp ${workDir}/LMPWeb/target/base-license-management-dist-99.9.99-TRUNK-BASE-SNAPSHOT.jar ."
                    }
                    sh "zip -d LMPWeb.war ./${libFolderName}/base-license-management-dist-99.9.99-TRUNK-BASE-SNAPSHOT.jar"
                    sh "zip -u LMPWeb.war ./${libFolderName}/base-license-management-dist-99.9.99-TRUNK-BASE-SNAPSHOT.jar"
                    if (productiveEnvironment) {
                        dir(libFolderPath) {
                            sh "cp ${workDir}/addlicensejar/licenseapi.jar ."
                            sh "rm -rf ${workDir}/addlicensejar"
                        }
                        sh "zip -u LMPWeb.war ./${libFolderName}/licenseapi.jar"
                        sh "rm ./${libFolderName}/licenseapi.jar"
                    }
                }
            }
        } //java container

        stage('build LMP') {
            dir('LMPUI/public') {
                sh "sed -i 's,http://localhost:8080,https://${hostname}:${webPort}/LMPWeb,g' config.json"
            }
            dir('LMPUI') {
                def user = miscUtils.getUserID()
                def group = miscUtils.getGroupID()
                docker.image('node:8.9.4').inside("--name ${ctrNames["npmCtr"]} --network ${networkName} -u=0 -v ${workDir}/LMPUI/package.json:/package.json -v ${workDir}/LMPUI/package-lock.json:/package-lock.json -v jenkins-m2-repo:/var/m2_repo:rw") {
                    sh 'node -v'
                    sh 'npm -v'
                    sh 'npm set strict-ssl false'
                    sh 'npm install'
                    sh 'npm run build'
                    //set ownership of files created inside container to user outside of container so that we can delete this files without PermissionDeniedException
                    sh "chown -hR ${user}:${group} *"
                }
            }
        }

        stage('start LMP') {
            docker.image('tomcat:9.0.37-jdk8-openjdk').withRun("--name ${ctrNames["webImageCtr"]} --restart always --network ${networkName} -v ${workDir}/LMPWeb/target/LMPWeb.war:/usr/local/tomcat/webapps/LMPWeb.war -v ${workDir}/LMPWeb/config/server.xml:/usr/local/tomcat/conf/server.xml -v ${workDir}/LMPWeb/config/web.xml:/usr/local/tomcat/conf/web.xml -v ${keysFolder}:/usr/local/keystore -p ${webPort}:8443") {
                docker.image('nginx:1.17.1-alpine').withRun("--name ${ctrNames["uiImageCtr"]} --restart always --network ${networkName} --link ${ctrNames["webImageCtr"]} -v ${workDir}/LMPUI/dist:/usr/share/nginx/html -v ${workDir}/LMPUI/nginx_conf:/etc/nginx/conf.d -v ${keysFolder}:/etc/nginx/certs -p ${uiPort}:443 -v /etc/nginx") {
                    sleep(2)
                    try {
                        echo "LMP with Dev environment is running on https://${hostname}:${uiPort}"
                        input "Proceed when LMP was accessible on\nhttps://${hostname}:${uiPort}"
                    } catch (InterruptedException e) {
                        unstable 'Abort was used which means LMP was not accessible'
                        echo '-------------------------web-LOG-------------------------'
                        sh "docker logs ${ctrNames["webImageCtr"]}"
                        echo '-------------------------UI-LOG-------------------------'
                        sh "docker logs ${ctrNames["uiImageCtr"]}"
                    }
                }
            }
        }

        stage('stop LMP') {
            dockerPurge(ctrNames, networkName)
        }
    }
}

/*
 * Stops and removes every container and network which is related to this job.
 */

def dockerPurge(ctrNames, networkName) {
    ctrNames.each { ctr ->
        sh returnStatus: true, script: "docker stop ${ctr.getValue()}"
        sh returnStatus: true, script: "docker rm -f ${ctr.getValue()}"
    }
    sh returnStatus: true, script: "docker network rm ${networkName}"
}