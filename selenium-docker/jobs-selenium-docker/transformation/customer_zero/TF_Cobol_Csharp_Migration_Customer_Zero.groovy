@Library('TestUtils') _

/**
 * Transforms the Cobol sources of the customer zero application to Csharp, using a headless Eclipse.
 *
 * @param mxBuildVersion The maxenso build under test
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param javaVersion The java version the environment will run with
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 */
timestamps {
    def mxVersionUtils = new MxVersionUtils()
    def gitUtils = new GitUtils()
    def miscUtils = new MiscUtils()
    def dockerUtils = new DockerUtils()
    def resultComparisonUtils = new ResultComparisonUtils()
    def spUtils = new SharepointUtils()

    nodeTF('Docker-host') {
        def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
        def gitlabRootDir = 'innowake-test-projects/transformation-test-projects'
        def remoteProjectLocation = gitlabRootDir + '/customer-zero/customer-zero-java.git'
        def customMigrationProjectLocation =  gitlabRootDir  + '/custom-migration.git'
        def customMigrationProjectBranch = 'master'  

        def workDir
        def buildProperties

        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "testProjectBranch=${testProjectBranch} javaVersion=${javaVersion} withCodeCoverage=${withCodeCoverage}"

        try {
            deleteDir()
            docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                workDir = pwd()
                
                stage('initialisation') {
					gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${customMigrationProjectLocation}", customMigrationProjectBranch)
					spUtils.downloadIwEclipseLinux(mxBuildVersion, "${workDir}/eclipse/eclipse")

                    // WQATF-1417: Use custom-migration Test-Project and update related build/migration files
					def tempFolderName = 'tempFolder'
                    sh 'rm -rf eclipseWorkspace/Test-Project/expected/.gitkeep'
					dir("${workDir}/eclipseWorkspace/Test-Project/${tempFolderName}") {
						gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                        //prepare expected files for cobol2csharp
                        sh 'mkdir item-management-mig/expected'
						sh 'mv item-management-mig-automation/Test-Project/expected/src-cobol-csharp item-management-mig/expected/.'

                        //prepare properties files
                        sh 'mv item-management-mig-automation/Test-Project/*.properties item-management-mig/.'

                        //prepare csharp source files
                        sh 'mkdir item-management-mig/src-cobol'
                        sh 'mv item-management-mig/resources/source/cobol/ item-management-mig/src-cobol'
                        //current Test-Prjoject has this files, too but in a more recent version. As a consequence we have to delete the outdated files
                        sh 'rm item-management-mig/build.xml item-management-mig/.project item-management-mig/.cobol-path'
                        
                        //move everthing to Test-Project
                        sh 'mv item-management-mig/* ../.'
						deleteDir()
					}

                    buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
                    withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                        sh "$MVN_CMD -f pom.xml dependency:copy-dependencies -Dinnowake.runtime.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
                    }  
                }

                stage('cobol2csharp') {
                   miscUtils.prepareAndRunMigration('cobol2csharp', workDir, javaVersion, withCodeCoverage)
                }

                stage('patch') {
                    dir("${workDir}/eclipseWorkspace/Test-Project/src-cobol-csharp/CustomerZero/Common") {
                        /*
                        .withHold() workaround
                        Currently, the SqlDeclareCursor#withHold() method throws an InvalidOperationException.
                        This is by design, because withHold() is not support by Sql Server.
                        More about https://amiconfluence.deloitte.com/x/Yzjy
                        */
                        sh "sed -i \"s|.withHold()| |g\" CUSTINVC.def.cs"
                    }
                }

                stage('result-comparison') {
                    def actualDir = "${workDir}/eclipseWorkspace/Test-Project/src-cobol-csharp/CustomerZero/Common"
                    def expectedDir = "${workDir}/eclipseWorkspace/Test-Project/expected/src-cobol-csharp"
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
                archiveArtifacts allowEmptyArchive: true, artifacts: '**/*.log,result-comparison/**/*,**/*.exec'
            }
        }
    }
}
