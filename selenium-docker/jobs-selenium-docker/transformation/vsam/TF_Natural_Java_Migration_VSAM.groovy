@Library('TestUtils') _

/**
 * Run the nat2java on the VSAM project against a certain maxenso build.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter 
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

nodeTF('Docker-host') {

	timestamps {
		
		def mxVersionUtils = new MxVersionUtils()
		def gitUtils = new GitUtils()
		def miscUtils = new MiscUtils()
		def resultComparisonUtils = new ResultComparisonUtils()
		def dockerUtils = new DockerUtils()
		def spUtils = new SharepointUtils()
		
		def migrationProjectLocation = 'innowake-test-projects/transformation-test-projects/custom-migration.git'
		def testProjectLocation = 'innowake-test-projects/transformation-test-projects/natural-vsam.git'
		def migrationProjectBranch = 'master'
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)

		def workDir
	    def testProjectWorkDir
		/* The properties from the file build.properties */
		def buildProperties

		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"
		
		try {
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
				workDir = pwd() + '/Migration'
				testProjectWorkDir = pwd() + '/VSAMProject'
				stage('initialisation') {
					gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${migrationProjectLocation}", migrationProjectBranch)
					gitUtils.checkoutGitProject(testProjectWorkDir, "${gitUtils.getGitUrlQef()}/${testProjectLocation}", testProjectBranch)
					spUtils.downloadIwEclipseLinux(mxBuildVersion, "${workDir}/eclipse/eclipse")
					dir(workDir) {
					    buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
					    echo buildProperties.toString()
					    withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                            sh "$MVN_CMD -f pom.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
				        }
					}
				}
				
				stage('copy files') {																																											   
				    sh "cp -r ${testProjectWorkDir}/src-natural ${workDir}/eclipseWorkspace/Test-Project "
				    sh "cp -r ${testProjectWorkDir}/src-natural-java ${workDir}/eclipseWorkspace/Test-Project/expected "
				    sh "cp -r ${testProjectWorkDir}/nat2java.properties ${workDir}/eclipseWorkspace/Test-Project"		
				}
				
                stage('nat2java') {
					miscUtils.prepareAndRunMigration('nat2java', workDir, javaVersion, true)
				}

				stage('result-comparison') {
                    sh "mkdir -p ${workDir}/result-comparison/log"
                    sh "mkdir -p ${workDir}/result-comparison/tmp"
                    def actualDir = "${workDir}/eclipseWorkspace/Test-Project/src-natural-java/vsam/test/transformed"
                    def expectedDir = "${workDir}/eclipseWorkspace/Test-Project/expected/src-natural-java/vsam/test/transformed"
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
				sh returnStatus: true, script: "mv ${workDir}/coverage/migration.exec ${workDir}/coverage/vsam-custom-migration-jacoco.exec"
                archiveArtifacts allowEmptyArchive: true, artifacts: '**/*.log, **/eclipseWorkspace/Test-Project/src-natural-java/**/*,**/*.exec', excludes: '**/.metadata/**'
            }
		}		
	}	
}
