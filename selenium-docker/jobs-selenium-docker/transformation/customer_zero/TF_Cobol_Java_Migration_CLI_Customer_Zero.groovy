@Library('TestUtils') _

/**
 * Transforms the Cobol sources of the customer zero application to Java, using the Cobol2Java CLI.
 *
 * @param mxBuildVersion The maxenso build under test
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param javaVersion The java version the environment will run with
 */

nodeTF('Docker-host') {
    timestamps {

        def mxVersionUtils = new MxVersionUtils()
        def gitUtils = new GitUtils()
        def miscUtils = new MiscUtils()
        def dockerUtils = new DockerUtils()
        def resultComparisonUtils = new ResultComparisonUtils()

        try {
            def workDir = pwd()
            def migrationProjectDir = "${workDir}/item-management-mig-automation/Test-Project"
            def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
            def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
            def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/customer-zero/customer-zero-java.git'

            buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
            buildDescription "testProjectBranch=${testProjectBranch} javaVersion=${javaVersion}"

            deleteDir()
            docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                stage('prepare') {
                    gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                    gitUtils.getLicenseFile(mxVersion, migrationProjectDir)
                    withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                        sh "$MVN_CMD dependency:copy -Dartifact=innowake.products.mee.source.migration.cobol:" +
                                "mee-source-migration-cobol-standalone-dist:${mxBuildVersion} -DoutputDirectory=${migrationProjectDir}"
                    }
                }

                stage('cobol2java') {
                    dir(migrationProjectDir) {
                        sh "java -jar mee-source-migration-cobol-standalone-dist*.jar -p cobol2java.properties " +
                                "${workDir}/item-management-mig/resources/source/cobol"
                    }
                }

                stage('result-comparison') {
                    def actualDir = "${workDir}/item-management-mig-automation/Test-Project/src-cobol-java/innowake/components/source/cobol"
                    def expectedDir = "${workDir}/item-management-mig-automation/Test-Project/expected/src-cobol-java"
                    def logDir = "${workDir}/result-comparison/log"
                    def tmpDir = "${workDir}/result-comparison/tmp"
                    sh "mkdir -p ${logDir}"
                    sh "mkdir -p ${tmpDir}"
                    def compareResult = resultComparisonUtils.resultCompare(actualDir, expectedDir, [], [], logDir, tmpDir)
                    if (compareResult != 0) {
                        unstable 'Deviations in file comparison'
                    }
                }
            }
        } catch (ex) {
            miscUtils.errorWithStackTrace(ex)
        } finally {
            stage('finalize') {
                archiveArtifacts allowEmptyArchive: true, artifacts: '**/*.log, result-comparison/**/*'
            }
        }
    }
}