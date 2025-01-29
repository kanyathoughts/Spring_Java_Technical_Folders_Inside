@Library('TestUtils') _

/**
 * Collects the Jacoco report files (.exec) from every runtime job which can be executed with code coverage.
 *
 * @param mxBuildVersion The build. To be selected from the available builds.
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

        echo mxBuildVersion
        buildName "#${BUILD_NUMBER} - ${mxBuildVersion}"

        try {
            def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
            deleteDir()
            docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                workDir = pwd()
                stage('initialization') {
                    gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/infrastructure/transformation/test-coverage.git", testProjectBranch)
                    productionAntFile = "${workDir}/build.xml"
                    antProperties = "-DworkDir=${workDir} -DwithCodeCoverage='true' -DmxBuildVersion=${mxBuildVersion} -DtestProjectDir=${workDir}"

                    //copy innowake-runtime-dist dependencies
                    def artifactPrefix = 'innowake'
                    def groupId = 'innowake.bundle'
                    def meeArtifactPrefix = 'mee-source-migration'
                    def meeGroupId = 'innowake.products.mee.source.migration'
                    withMaven(maven: 'Default', mavenSettingsConfig: 'linux_maven_settings') {
                        sh "$MVN_CMD dependency:copy -Dartifact=${groupId}:${artifactPrefix}-runtime-dist:${mxBuildVersion} -DoutputDirectory=${workDir} -Dmdep.stripVersion=true"
                        sh "$MVN_CMD dependency:copy -Dartifact=${meeGroupId}:${meeArtifactPrefix}-dist:${mxBuildVersion} -DoutputDirectory=${workDir} -Dmdep.stripVersion=true"
                    }
                }

                stage('Copy artifacts') {
                    //array with the names of the jobs
                    def jobs = ['TF_Natural_Java_Migration_KIDICAP_184','TF_Natural_Java_Migration_KIDICAP_Mini','TF_Natural_Java_Linux_Migration_Mannheimer','TF_Cobol_Java_Migration_Customer_Zero',
                                        'TF_IMS_Java_Migration','TF_Cobol_Java_Migration_License_Validation','TF_PL1_Java_Migration','TF_Natural_Java_Migration_KIDICAP_165',
                                        'TF_Natural_Java_Migration_VSAM','TF_Natural_Csharp_Migration_VontobelSt','TF_Cobol_Csharp_Migration_NMSLO','TF_Cobol_Csharp_Migration_Customer_Zero',
                                        'TF_IMS_Csharp_Migration','TF_Cobol_Csharp_Migration_License_Validation','TF_Migration_Object_Filter',
                                        'TF_Cobol_Java_Test_Batch','TF_Cobol_Java_Test_CICS','TF_Cobol_Java_Test_Cobol_Async_API','TF_Cobol_Java_Test_ExecSQL',
                                        'TF_Cobol_Java_Test_License_Validation','TF_PL1_Java_Test','TF_Cobol_Java_Test_Customer_Zero','TF_IMS_DB_Java_Test',
                                        'TF_IMS_TM_Java_Test','TF_IMS_DB_Java_Test_Performance','TF_Natural_Java_Test_KIDICAP_Mini_AGTV',
                                        'TF_Natural_Java_Test_KIDICAP_Mini_OFDAN','TF_Natural_Java_Test_KIDICAP_Mini_OFDVE',
                                        'TF_Natural_Java_Test_KIDICAP_Mini_REGRESSION','TF_Natural_Java_Test_Mannheimer','TF_Natural_Java_Test_KIDICAP_184_AGTV',
                                        'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part0103','TF_Natural_Java_Test_KIDICAP_165_AGTV2Part0409',
                                        'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part1020','TF_Natural_Java_Test_KIDICAP_165_AGTV',
                                        'TF_Natural_Java_Test_KIDICAP_165_OFDAN','TF_Natural_Csharp_Migration_VSAM','TF_Cobol_Java_Migration','TF_Cobol_Java_Test']

                    jobs.each {
                        try{
                            def parameter
                            if (it == "TF_Cobol_Java_Migration_License_Validation" || it == "TF_PL1_Java_Migration" || it == "TF_Natural_Java_Migration_VSAM"
                                    || it == "TF_Natural_Csharp_Migration_VSAM" || it == "TF_Natural_Csharp_Migration_VontobelSt" || it == "TF_Cobol_Csharp_Migration_License_Validation"
                                    || it == "TF_Migration_Object_Filter" || it == "TF_Cobol_Java_Test_Cobol_Async_API" || it == "TF_Cobol_Java_Test_Batch" || it == "TF_Cobol_Java_Test_CICS"
                                    || it == "TF_Cobol_Java_Test_ExecSQL" || it == "TF_Cobol_Java_Test_License_Validation" || it == "TF_PL1_Java_Test" || it == "TF_Cobol_Java_Migration" || it == "TF_Cobol_Java_Test") {
                                parameter = "mxBuildVersion=${mxBuildVersion}"
                            } else {
                                parameter = "withCodeCoverage=true,mxBuildVersion=${mxBuildVersion}"
                            }
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
                            unstable "There is problematic build due to missing artifacts for the job for build version ${mxBuildVersion}: " + it
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
                    spUtils.uploadJobArtifact(mxBuildVersion, 'report.zip')
                }
            }
        } catch (Exception e) {
            unstable 'There are problematic builds ' + e.toString()
        }
    }
}
