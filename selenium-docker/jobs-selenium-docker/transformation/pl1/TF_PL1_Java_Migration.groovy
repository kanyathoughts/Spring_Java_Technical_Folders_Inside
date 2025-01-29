@Library('TestUtils') _

/**
 * Transforms PL/I to Java, using a headless Eclipse.
 *
 * @param mxBuildVersion  The maxenso build under test
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 */
timestamps {
    def mxVersionUtils = new MxVersionUtils()
    def gitUtils = new GitUtils()
    def miscUtils = new MiscUtils()
    def resultComparisonUtils = new ResultComparisonUtils()
    def dockerUtils = new DockerUtils()
    def spUtils = new SharepointUtils()

    nodeTF('Docker-host') {
		def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)

        def gitlabRootDir = 'innowake-test-projects/transformation-test-projects'
        def remoteProjectLocation = gitlabRootDir + '/pl1-test.git'
        def customMigrationProjectLocation =  gitlabRootDir  + '/custom-migration.git'
        def customMigrationProjectBranch = 'master' 
        def withCodeCoverage = false

        def buildProperties
		def workDir

		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "testProjectBranch=${testProjectBranch} javaVersion=${javaVersion}"

		try {
			deleteDir()
			 docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
				workDir = pwd()

				stage('initialisation') {
                    gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${customMigrationProjectLocation}", customMigrationProjectBranch)
					spUtils.downloadIwEclipseLinux(mxBuildVersion, "${workDir}/eclipse/eclipse")

                    // WQATF-1419: Use custom-migration Test-Project and update related build, source and migration files
                    sh 'rm -rf eclipseWorkspace/Test-Project/expected'
					dir("${workDir}/eclipseWorkspace/Test-Project/tempFolder}") {
						gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                        sh 'rm pl1-java/expected/.gitkeep'
                        sh 'mv pl1-java/src-pl1-java pl1-java/expected/.'

                        sh 'mv pl1-java/* ../.'
                        deleteDir()
					}

                    buildProperties = miscUtils.readPropertyFile('build.properties', ['mxBuildVersion': mxBuildVersion, 'workDir': workDir])
                     withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                        sh "$MVN_CMD -f ${workDir}/pom.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
                    }
                }

                stage('pli2java') {
                    miscUtils.prepareAndRunMigration('pli2java', workDir, javaVersion, withCodeCoverage)
                }

                stage('result-comparison') {
                    sh "mkdir -p ${workDir}/result-comparison/log"
                    sh "mkdir -p ${workDir}/result-comparison/tmp"
                    def actualDir = "${workDir}/eclipseWorkspace/Test-Project/src-pl1-java"
                    def expectedDir = "${workDir}/eclipseWorkspace/Test-Project/expected/src-pl1-java"
                    def compareResult = resultComparisonUtils.resultCompare(actualDir, expectedDir, [], [], "${workDir}/result-comparison/log", "${workDir}/result-comparison/tmp")
                    if (compareResult != 0) {
                    	unstable 'Deviations in file comparison'
                    }
                }
            }
        } catch (ex) {
			miscUtils.errorWithStackTrace(ex)
		} finally {
			stage('finalize') {
				archiveArtifacts allowEmptyArchive: true, artifacts: 'eclipseWorkspace/Test-Project/src-pl1-java/**/*,**/*?.log,**/*.exec', excludes: 'pl1-java/test/res/jcl/**/*', fingerprint: true
            }
        }
    }
}
