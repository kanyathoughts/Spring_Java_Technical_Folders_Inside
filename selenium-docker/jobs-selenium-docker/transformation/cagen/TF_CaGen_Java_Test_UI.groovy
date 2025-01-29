import org.jenkinsci.plugins.workflow.steps.FlowInterruptedException

@Library('TestUtils') _

/**
 * Run CAGen GUI tests.
 * @param caGenBuildVersion The caGen/hotfix version to migrate the Gen Application.
 * @param javaVersion The java version the test will run with
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param browser The web browser the tests will run with
 */

nodeTF('Docker-host && Region-US && !USLinux2') {
    timestamps {
        def dockerUtils = new DockerUtils()
        def gitUtils = new GitUtils()
        def miscUtils = new MiscUtils()
        def mxVersionUtils = new MxVersionUtils()
        def spUtils = new SharepointUtils()
        def workDir = pwd()
        def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/cagen/cagen-ui.git'
        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, caGenBuildVersion)
        def user = miscUtils.getUserID()
        def group = miscUtils.getGroupID()
        String defaultCagenBackendPort = '9000'
        String defaultWebDriverPort = '4444'
        String extentTestReportsDir = 'artifact/reports'
        String defaultBrowser = 'chrome'
        def modelNamesWithUnderscores = getTestPackageNames()


        buildName "#${env.BUILD_ID} - ${caGenBuildVersion}"
        buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} browser=${browser}"

        stage('initialization') {
            deleteDir()
            docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
            }
        }

        modelNamesWithUnderscores.each { modelNameWithUnderscores ->
            stage(modelNameWithUnderscores) {
                def sharepointDownloadSuccessful = true
                try {
                    docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                        withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
                            spUtils.downloadJobArtifact('TF_CaGen_Java_Migration_UI', caGenBuildVersion, "${modelNameWithUnderscores}_backend.zip", workDir)
                        }
                    }
                    sh "unzip -q ${modelNameWithUnderscores}_backend.zip -d ${modelNameWithUnderscores}_backend"
                } catch (Exception e) {
                    sharepointDownloadSuccessful = false
                    unstable 'Encountered an exception while trying to download artifacts for the migrated model' +
                            " ${modelNameWithUnderscores}:\n${e.toString()}\n"
                }
                when(sharepointDownloadSuccessful, 'The artifacts for this model could not be successfully downloaded' +
                        ' so we cannot execute tests against it.\n') {
                    String cagenBackendPort = dockerUtils.getAvailablePort([defaultCagenBackendPort, '9001', '9002', '9003', '9005', '9005'])
                    String webDriverPort = dockerUtils.getAvailablePort([defaultWebDriverPort, '4445', '4446', '4447', '4448', '4449'])
                    if ([cagenBackendPort, webDriverPort].contains('No Port Available')) {
                        error 'No free port available'
                    }
                    sh "find . -type f -name data.properties -exec sed -i 's/${defaultCagenBackendPort}/${cagenBackendPort}/g' {} +"
                    sh "find . -type f -name data.properties -exec sed -i 's/${defaultBrowser}/${browser}/g' {} +"
                    sh "sed -i 's/${defaultWebDriverPort}/${webDriverPort}/g' src/main/java/base/Base.java"
                    sh """sed -i 's/testcases.*"/testcases.${modelNameWithUnderscores}"/g' testng.xml"""
                    String seleniumDockerRunParams = "-p ${webDriverPort}:4444 --shm-size='2g' --add-host=host.docker.internal:host-gateway"
                    String jenkinsEnvDockerRunParams = "-v root-jenkins-m2-repo:/var/m2_repo:rw -u=0 -p ${cagenBackendPort}:8080 --add-host=host.docker.internal:host-gateway"
                    docker.image("selenium/standalone-${browser}:98.0-${browser}driver-98.0-grid-4.1.2-20220217").withRun(seleniumDockerRunParams) {
                        if (modelNameWithUnderscores.equals('NN_DELTA_LLOYD_JRA_DEV_V5')) {
                            runJRATests()
                        } else {
                            docker.image(dockerUtils.pullJenkinsEnvironmentCaGenUiImage(javaVersion)).inside(jenkinsEnvDockerRunParams) {
                                try {
                                    dir("${modelNameWithUnderscores}_backend") {
                                        sh "cp -r ../src/main/resources/images src/main/resources/static/assets"
                                        sh "sed -i 's/localhost:8080/host.docker.internal:${cagenBackendPort}/g' src/main/resources/static/main*.js"
                                        withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                                            sh "$MVN_CMD dependency:resolve"
                                            sh "$MVN_CMD spring-boot:run > spring_boot_output 2>&1 &"
                                            miscUtils.waitForMessageInLogFile('Starting application', 'spring_boot_output')
                                        }
                                    }
                                    // Execute the tests.
                                    timeout(time: 15) {
                                        withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                                            sh "$MVN_CMD clean test"
                                            sh returnStatus: true, script: "mv target/jacoco.exec target/${modelNameWithUnderscores}-jacoco.exec"
                                        }
                                    }
                                } catch (FlowInterruptedException fie) {
                                    if (fie.causes.size() != 0 && fie.causes[0].toString().contains('ExceededTimeout')) {
                                        unstable 'Exceeded timeout'
                                    } else {
                                        throw fie
                                    }
                                } catch (Exception e) {
                                    unstable 'Encountered the following exception while running tests against migrated model ' +
                                            "${modelNameWithUnderscores}\n" +
                                            "${e.toString()}\nWe will skip this model's remaining tests, and continue."
                                } finally {
                                    try {
                                        junit allowEmptyResults: true, skipMarkingBuildUnstable: true, testResults: 'target/surefire-reports/*.xml'
                                        if (miscUtils.directoryExists(extentTestReportsDir)) {
                                            sh "mv ${extentTestReportsDir} artifact/${modelNameWithUnderscores}_report"
                                            sh "mkdir ${extentTestReportsDir}"
                                            publishReportToJenkins(modelNameWithUnderscores)
                                            uploadReportToSharepoint(modelNameWithUnderscores)
                                        }
                                    } finally {
                                        sh "chown -hR ${user}:${group} *"
                                        archiveArtifacts allowEmptyArchive: true, artifacts: "**/*.exec"
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        echo 'Done running tests against all migrated models.'
        stage('finalize') {
            archiveArtifacts "artifact/**"
        }
    }
}

/**
 * Rus the NN_DELTA_LLOYD_JRA_DEV_V5 model's tests.
 */
def runJRATests() {
    def dockerUtils = new DockerUtils()
    def miscUtils = new MiscUtils()
    def user = miscUtils.getUserID()
    def group = miscUtils.getGroupID()
    final String JRA_DB_NAME = 'jraDB'
    String extentTestReportsDir = 'artifact/reports'
    String defaultCagenBackendPort = '9000'
    String defaultJRADBPort = '1521'
    def modelNameWithUnderscores = 'NN_DELTA_LLOYD_JRA_DEV_V5'
    def jraDBPort = dockerUtils.getAvailablePort([defaultJRADBPort, '1522', '1523', '1524', '1525', '1526'])
    if (jraDBPort == 'No Port Available') {
        error 'No free port available'
    }

    docker.image(dockerUtils.pullDockerOracleImage('12.2.0.1')).withRun("-p ${jraDBPort}:1521 --name ${JRA_DB_NAME}") {
        miscUtils.waitForMessageInDockerLogs('database is ready for use', JRA_DB_NAME)
        sh "docker cp src/main/resources/${modelNameWithUnderscores}/database_setup_files ${JRA_DB_NAME}:/"
        miscUtils.runScriptWithRetry("docker exec -t ${JRA_DB_NAME} /bin/bash -c /database_setup_files/execute_sql.sh", 'Not connected')
        String cagenBackendPort = dockerUtils.getAvailablePort([defaultCagenBackendPort, '9001', '9002', '9003', '9005', '9005'])
        if (cagenBackendPort == 'No Port Available') {
            error 'No free port available'
        }
        String jenkinsEnvDockerRunParams = "-v root-jenkins-m2-repo:/var/m2_repo:rw -u=0 -p ${cagenBackendPort}:8080 --add-host=host.docker.internal:host-gateway"
        docker.image(dockerUtils.pullJenkinsEnvironmentCaGenUiImage(javaVersion)).inside(jenkinsEnvDockerRunParams) {
            try {
                dir("${modelNameWithUnderscores}_backend") {
                    sh """find . -type f -name AppConfig.java -exec sed -i 's/new Codepage.*;/new Codepage(java.nio.charset.Charset.forName("UTF-8"));/g' {} +"""
                    sh "find . -type f -name AppConfig.java -exec sed -i 's/DefaultNullSqlStatementEnvironment/DefaultSqlStatementEnvironment/g' {} +"
                    sh "find . -type f -name Ain0e080ErrorInit.java -exec sed -i 's/run() {/run(){exports.expSbds1Returncode.returnCode.setValue(1);exports.expSbds1Reasoncode.reasonCode.setValue(0);/g' {} +"
                    sh """find . -type f -name Avb0j100CheckAutorisatie.java -exec sed -i 's/getUserId()/TextAttribute.of("XS45MQ")/g' {} +"""
                    sh "find . -type f -name Avb0j102ReaMedewAutorisatie.java -exec sed -i 's/.and(that.*and(desired(dbMedewerkerAutorisatie).relatesAs(MedewerkerAutorisatie.geldtVoorMaatschappij())/).and(desired(dbMedewerkerAutorisatie).relatesAs(MedewerkerAutorisatie.geldtVoorMaatschappij())/g' {} +"
                    sh "find . -type f -name application.properties -exec sed -i 's|localhost:1521/ORCLPDB1|host.docker.internal:${jraDBPort}/ORCLPDB1.localdomain|g' {} +"
                    sh "sed -i 's/localhost:8080/host.docker.internal:${cagenBackendPort}/g' src/main/resources/static/main*.js"
                    withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                        sh "$MVN_CMD dependency:resolve"
                        sh "$MVN_CMD spring-boot:run > spring_boot_output 2>&1 &"
                        miscUtils.waitForMessageInLogFile('Starting application', 'spring_boot_output')
                    }
                }
                // Execute the tests.
                timeout(time: 20) {
                    withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                        sh "$MVN_CMD clean test"
                        sh returnStatus: true, script: "mv target/jacoco.exec target/${modelNameWithUnderscores}-jacoco.exec"
                    }
                }
            } catch (FlowInterruptedException fie) {
                if (fie.causes.size() != 0 && fie.causes[0].toString().contains('ExceededTimeout')) {
                    unstable 'Exceeded timeout'
                } else {
                    throw fie
                }
            } catch (Exception e) {
                unstable 'Encountered the following exception while running tests against migrated model ' +
                        "${modelNameWithUnderscores}\n" +
                        "${e.toString()}\nWe will skip this model's remaining tests, and continue."

            } finally {
                try {
                    junit allowEmptyResults: true, skipMarkingBuildUnstable: true, testResults: 'target/surefire-reports/*.xml'
                    if (miscUtils.directoryExists(extentTestReportsDir)) {
                        sh "mv ${extentTestReportsDir} artifact/${modelNameWithUnderscores}_report"
                        sh "mkdir ${extentTestReportsDir}"
                        publishReportToJenkins(modelNameWithUnderscores)
                        uploadReportToSharepoint(modelNameWithUnderscores)
                    }
                } finally {
                    sh "chown -hR ${user}:${group} *"
                    archiveArtifacts allowEmptyArchive: true, artifacts: "**/*.exec"
                }
            }
        }
    }
}

/**
 * Returns the names of the packages in the cagen-ui project that contain
 * tests, as a list.
 *
 * @return packageNames The list of test package names
 */
def getTestPackageNames() {
    def packageNames = []
    dir('src/test/java/testcases') {
        def fileWrappers = findFiles()
        fileWrappers.each { fileWrapper ->
            if (fileWrapper.directory) {
                packageNames.add(fileWrapper.name)
            }
        }
    }
    return packageNames
}

/**
 * Publishes a model's Extent report to Jenkins.
 */
def publishReportToJenkins(String modelNameWithUnderscores) {
    try {
        publishHTML allowMissing: false, alwaysLinkToLastBuild: true, keepAll: false,
                reportDir: "artifact/${modelNameWithUnderscores}_report", reportFiles: 'index.html',
                reportName: "testcases.${modelNameWithUnderscores}",
                reportTitles: "#${env.BUILD_ID} - ${caGenBuildVersion}: testcases.${modelNameWithUnderscores}"
    } catch (Exception e) {
        unstable 'Encountered the following exception while publishing an Extent report for migrated model ' +
                "${modelNameWithUnderscores} to Jenkins\n" +
                "${e.toString()}\nWe will continue with testing the remaining models."
    }
}

/**
 * Creates a zip file from a model's Extent report, uploads the file to Sharepoint, and
 * then deletes the file locally.
 */
def uploadReportToSharepoint(String modelNameWithUnderscores) {
    try {
        def spUtils = new SharepointUtils()
        zip zipFile: "${modelNameWithUnderscores}_report.zip", archive: false, dir: "artifact/${modelNameWithUnderscores}_report"
        spUtils.uploadJobArtifact("${caGenBuildVersion}_${browser}", "${modelNameWithUnderscores}_report.zip")
        sh "rm ${modelNameWithUnderscores}_report.zip"
    } catch (Exception e) {
        unstable 'Encountered the following exception while uploading an Extent report for the migrated model ' +
                "${modelNameWithUnderscores} to Sharepoint\n" +
                "${e.toString()}\nWe will continue with testing the remaining models."
    }

}