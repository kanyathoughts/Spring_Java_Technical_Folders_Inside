@Library('TestUtils') _

/**
 * Collects the Jacoco report files (.exec) from every CaGen runtime job executed with code coverage.
 *
 * @param caGenBuildVersion The build. To be selected from the available builds.
 *		  				 type: Extensible Choice -> File Choice Parameter
 */

nodeTF('Docker-host') {

    timestamps {

        def dockerUtils = new DockerUtils()
        def gitUtils = new GitUtils()
        def mxVersionUtils = new MxVersionUtils()
        def spUtils = new SharepointUtils()
        def workDir
        def antProperties
        def productionAntFile

        echo caGenBuildVersion
        buildName "#${BUILD_NUMBER} - ${caGenBuildVersion}"

        try {
            def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, caGenBuildVersion)
            deleteDir()
            docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                workDir = pwd()
                stage('initialization') {
                    gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/infrastructure/transformation/test-coverage.git", testProjectBranch)
                    productionAntFile = "${workDir}/build-cagen.xml"
                    antProperties = "-DworkDir=${workDir} -DwithCodeCoverage='true' -DcaGenBuildVersion=${caGenBuildVersion} -DtestProjectDir=${workDir}"

                    //copy dependencies
                    def groupId = 'com.innowake.gen'
                    def runtimeArtifactPrefix = 'framework-dist'
                    def migrationArtifactPrefix = 'javagen-dist'
                    withMaven(maven: 'Default', mavenSettingsConfig: 'linux_maven_settings') {
                        sh "$MVN_CMD dependency:copy -Dartifact=${groupId}:${runtimeArtifactPrefix}:${caGenBuildVersion} -DoutputDirectory=${workDir} -Dmdep.stripVersion=true"
                        sh "$MVN_CMD dependency:copy -Dartifact=${groupId}:${migrationArtifactPrefix}:${caGenBuildVersion} -DoutputDirectory=${workDir} -Dmdep.stripVersion=true"
                    }
                }

                stage('Copy artifacts') {
                    //array with the names of the jobs
                    def jobs = ['TF_CaGen_Java_Test_Mssql_Integration','TF_CaGen_Java_Test_Mssql_Functional','TF_CaGen_Java_Test_Oracle_Functional',
                                        'TF_CaGen_Java_Test_Oracle_Integration','TF_CaGen_Java_Test_UI','TF_CaGen_Java_Migration','TF_CaGen_Java_Migration_UI']

                    jobs.each {
                        try{
                            def parameter
                            parameter = "caGenBuildVersion=${caGenBuildVersion}"
                            copyArtifacts([
                                    projectName: it,
                                    filter: '**/*.exec',
                                    selector: lastSuccessful(),
                                    parameters: parameter,
                                    target: workDir,
                                    fingerprintArtifacts: true,
                                    flatten: true
                            ])
                        } catch (Exception e) {
                            unstable "There is problematic build due to missing artifacts for the job for build version ${caGenBuildVersion}: " + it
                        }
                    }
                }

                stage('Test coverage reports') {
                    withAnt(installation: 'Default') {
                        sh "ant -buildfile ${productionAntFile} ${antProperties} completeReport"
                    }
                }

                stage('Publish reports'){
                    publishHTML (target : [allowMissing: false, alwaysLinkToLastBuild: true, keepAll: true,
                                           reportDir: "${workDir}/coverage/report/completeReport/", reportFiles: 'index.html',
                                           reportName: 'completeCodeCoverageReport'])
                }

                stage('Reports upload') {
                    zip zipFile: 'report.zip', archive: false, dir: "${workDir}/coverage/report"
                    spUtils.uploadJobArtifact(caGenBuildVersion, 'report.zip')
                }
            }
        } catch (Exception e) {
            unstable 'There are problematic builds ' + e.toString()
        }
    }
}
