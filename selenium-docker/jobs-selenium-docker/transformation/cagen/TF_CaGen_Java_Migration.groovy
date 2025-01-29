@Library('TestUtils') _

/**
 * Run CAGen migration for Gen banking application. 
 * @param caGenBuildVersion The caGen/hotfix version to migrate the Gen Application.
 * @param javaVersion The java version the test will run with
 */

nodeTF('Docker-host && Region-US') {
    timestamps {
        def dockerUtils = new DockerUtils()
        def gitUtils = new GitUtils()
        def miscUtils = new MiscUtils()
        def mxVersionUtils = new MxVersionUtils()
        def spUtils = new SharepointUtils()
        def user = miscUtils.getUserID()
        def group = miscUtils.getGroupID()

        buildName "#${env.BUILD_ID} - ${caGenBuildVersion}"
        buildDescription "javaVersion=${javaVersion}"

        deleteDir()
        docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
            stage('setup') {
                withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                    sh "$MVN_CMD dependency:copy -Dartifact=com.innowake.gen:javagen-dist-spring-boot:${caGenBuildVersion} -DoutputDirectory=."
                    sh "$MVN_CMD dependency:copy -Dartifact=org.jacoco:org.jacoco.agent:0.8.8 -DoutputDirectory=."
                }
                gitUtils.getLicenseFile(mxVersionUtils.getBranchForMxBuildVersion(caGenBuildVersion), pwd())
                sh "unzip -q ./org.jacoco.agent-0.8.8.jar -d ./org.jacoco.agent-0.8.8"
            }
        }
        docker.image(dockerUtils.pullJenkinsEnvironmentCaGenImage(javaVersion)).inside('-u=0 ' +
                '-v /etc/ssl/certs/pd-nexus.deloitte.com.pem:/etc/ssl/certs/pd-nexus.deloitte.com.pem') {
            stage('migration') {
                withEnv(["NODE_EXTRA_CA_CERTS=/etc/ssl/certs/pd-nexus.deloitte.com.pem"]) {
                    miscUtils.npmCliLogin()
                    withCredentials([usernamePassword(credentialsId: 'CaGen-DB-Credentials', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
                        sh "java -javaagent:${pwd()}/org.jacoco.agent-0.8.8/jacocoagent.jar -jar javagen-dist-spring-boot-${caGenBuildVersion}.jar \
                            --com.innowake.gen.model-name='FULL END TO END WITH HELP' \
                            --com.innowake.gen.destination-directory=. \
                            --com.innowake.gen.bootstrap-timeout=5000 \
                            --com.innowake.gen.bootstrap-frontend=true \
                            --com.innowake.gen.integration-test=true \
                            --com.innowake.gen.split-app=true \
                            --com.innowake.gen.migrate-frontend=true \
                            --com.innowake.gen.migrate-backend=true \
                            --com.innowake.gen.migrate-gui=false \
                            --com.innowake.gen.preserve-symlinks=false \
                            --com.innowake.gen.disable-db=false \
                            --com.innowake.gen.database-ip=localhost \
                            --com.innowake.gen.database-port=1521 \
                            --com.innowake.gen.database-name=ORCLCDB \
                            --com.innowake.gen.database-un=${dbUser} \
                            --com.innowake.gen.database-pw=${dbPassword} \
                            --com.innowake.gen.database-driver=oracle.jdbc.OracleDriver \
                            --com.innowake.gen.database-url=jdbc:oracle:thin:@127.0.0.1:1521:ORCLCDB \
                            --com.innowake.gen.database-type=oracle \
                            --com.innowake.gen.backend-base-url=http://localhost:8088 \
                            --server.port=8088 \
                            --spring.datasource.url='jdbc:sqlserver://10.118.62.105:1433;database=DBCSE' \
                            --spring.datasource.username=dbcse \
                            --spring.datasource.password=dbcse \
                            --spring.datasource.driver-class-name=com.microsoft.sqlserver.jdbc.SQLServerDriver \
                            --com.innowake.gen.use-dist-jar=true \
                            --com.innowake.gen.version-id=${caGenBuildVersion}"
                    }
                    sh "chown -hR ${user}:${group} *"
                }
            }

            stage('archive') {
                sh returnStatus: true, script: "mv ${pwd()}/jacoco.exec ${pwd()}/cagen-migration-jacoco.exec"
                archiveArtifacts allowEmptyArchive: true, artifacts: '**/cagen-migration-jacoco.exec'
                zip zipFile: 'backend.zip', archive: false, dir: 'backend'
                spUtils.uploadJobArtifact(caGenBuildVersion, 'backend.zip')
                zip zipFile: 'frontend.zip', archive: false, dir: 'frontend'
                spUtils.uploadJobArtifact(caGenBuildVersion, 'frontend.zip')
            }
        }
    }
}