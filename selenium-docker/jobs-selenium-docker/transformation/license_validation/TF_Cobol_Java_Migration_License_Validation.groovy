@Library('TestUtils') _

/**
 * Transforms the Cobol sources of the license validation check to Java, using a headless Eclipse.
 * 
 * @param mxBuildVersion  The maxenso build under test
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
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
        def remoteProjectLocation = gitlabRootDir + '/license-validation.git'
        def customMigrationProjectLocation =  gitlabRootDir  + '/custom-migration.git'
        def customMigrationProjectBranch = 'master'
        def withCodeCoverage = true 
        def buildProperties
        def workDir
        def migDir

        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "testProjectBranch=${testProjectBranch} javaVersion=${javaVersion}"
        
        try {
			deleteDir()
            docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                workDir = pwd()
                migDir = "${workDir}/migration"

                stage('initialisation') {
                    // WQATF-1421: Use custom-migration Test-Project and update related source and migration files
                    dir(migDir) {
                        gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/${customMigrationProjectLocation}", customMigrationProjectBranch)

                        dir('eclipseWorkspace/Test-Project/tempFolderName') {
                            gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                            sh 'mv eclipseWorkspace/Test-Project/* ../.'

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
            }
        } catch (ex) {
			miscUtils.errorWithStackTrace(ex)
        } finally {
            stage('finalize') {
                archiveArtifacts allowEmptyArchive: true, artifacts: '**/*?.log,**/*.exec'
            }
        }
    }
}