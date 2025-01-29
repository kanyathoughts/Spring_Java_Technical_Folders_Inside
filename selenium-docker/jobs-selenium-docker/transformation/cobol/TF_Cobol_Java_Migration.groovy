@Library('TestUtils') _

/**
 * Transforms the Cobol sources to Java, using a headless Eclipse.
 * 
 * @param mxBuildVersion  The maxenso build under test
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param javaVersion The java version the test will run with
 * type: Choice Parameter
 */
timestamps {
    def mxVersionUtils = new MxVersionUtils()
    def gitUtils = new GitUtils()
    def miscUtils = new MiscUtils()
    def resultComparisonUtils = new ResultComparisonUtils()
    def dockerUtils = new DockerUtils()
    def spUtils = new SharepointUtils()

    nodeTF('Docker-host') {
        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
        def gitlabRootDir = 'innowake-test-projects/transformation-test-projects'
        def remoteProjectLocation = gitlabRootDir + '/cobol-java-test.git'
        def customMigrationProjectLocation =  gitlabRootDir  + '/custom-migration.git'
        def customMigrationProjectBranch = 'master'
        def withCodeCoverage = true 
        def buildProperties
        def workDir
        def migDir
        def fullBuild

        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        
        try {
            deleteDir()
            docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {

                fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
                buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} fullBuild=${fullBuild}"           
                workDir = pwd()
                migDir = "${workDir}/migration"

                stage('initialisation') {
                    // WQATF-1421: Use custom-migration Test-Project and update related source and migration files
                    dir(migDir) {
                        gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/${customMigrationProjectLocation}", customMigrationProjectBranch)

                        dir('eclipseWorkspace/Test-Project/tempFolderName') {
                            gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                            sh 'cp -R cobol-java/src-cobol ../src-cobol'
                            sh 'cp -R cobol-java/src-cobol-java ../expected/src-cobol-java'
                            sh 'cp -R cobol-java/cobol2java.properties ../.'

                            deleteDir()
                        }

                    buildProperties = miscUtils.readPropertyFile('build.properties', ['mxBuildVersion': mxBuildVersion, 'workDir': pwd()])
                        spUtils.downloadIwEclipseLinux(mxBuildVersion, "${migDir}/eclipse/eclipse")
                            withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                                sh "$MVN_CMD -f ${migDir}/pom.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
                        }
                    }
                }
                
                stage('cobol2java') {
                    miscUtils.prepareAndRunMigration('cobol2java', migDir, javaVersion, withCodeCoverage)
                    archiveArtifacts 'migration/eclipseWorkspace/Test-Project/src-cobol-java/**/*'
                }
                
                stage('result-comparison') {
                    def actualDir = "${migDir}/eclipseWorkspace/Test-Project/src-cobol-java"
                    def expectedDir = "${migDir}/eclipseWorkspace/Test-Project/expected/src-cobol-java"
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
                sh returnStatus: true, script: "mv ${migDir}/coverage/migration.exec ${migDir}/coverage/cobol-java-migration.exec"
                archiveArtifacts allowEmptyArchive: true, artifacts: 'eclipseWorkspace/Test-Project/src-cobol-java/**/*,**/*?.log,**/*.exec', excludes: 'eclipseWorkspace/Test-Project/expected/**/', fingerprint: true
            }
        }
    }
}
