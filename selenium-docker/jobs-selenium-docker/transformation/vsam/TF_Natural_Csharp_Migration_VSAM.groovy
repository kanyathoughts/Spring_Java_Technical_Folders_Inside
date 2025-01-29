@Library('TestUtils') _

/**
 * Run the nat2csharp on the VSAM project against a certain maxenso build.
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
		def testProjectLocation = 'innowake-test-projects/transformation-test-projects/natural-vsam-csharp.git'
		def migrationProjectBranch = 'master'
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)

		def migrationDir
	    def testProjectWorkDir
		/* The properties from the file build.properties */
		def buildProperties

		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"
		
		try {
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
				migrationDir = pwd() + '/Migration'
				testProjectWorkDir = pwd() + '/VSAMProject'
				stage('initialisation') {
					gitUtils.checkoutGitProject(migrationDir, "${gitUtils.getGitUrlQef()}/${migrationProjectLocation}", migrationProjectBranch)
					gitUtils.checkoutGitProject(testProjectWorkDir, "${gitUtils.getGitUrlQef()}/${testProjectLocation}", testProjectBranch)
					spUtils.downloadIwEclipseLinux(mxBuildVersion, "${migrationDir}/eclipse/eclipse")
					dir(migrationDir) {
					    buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': migrationDir, 'mxBuildVersion': mxBuildVersion])
					    echo buildProperties.toString()
					    withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                            sh "$MVN_CMD -f pom.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
				        }
					}
				}
				
				stage('copy files') {																																											   
					sh "cp -rT ${testProjectWorkDir}/natural-vsam-csharp/src-natural ${migrationDir}/eclipseWorkspace/Test-Project/src-natural"
					sh "cp ${testProjectWorkDir}/natural-vsam-csharp/nat2csharp.properties ${migrationDir}/eclipseWorkspace/Test-Project"
					sh "cp -rT ${testProjectWorkDir}/natural-vsam-csharp/src-natural-csharp ${migrationDir}/eclipseWorkspace/Test-Project/expected/src-natural-csharp"				
				}
				
                stage('nat2csharp') {
					miscUtils.prepareAndRunMigration('nat2csharp', migrationDir, javaVersion, true)
				}

				stage('result-comparison') {
                    sh "mkdir -p ${migrationDir}/result-comparison/log"
					sh "mkdir -p ${migrationDir}/result-comparison/tmp"
					actualDir = "${migrationDir}/eclipseWorkspace/Test-Project/src-natural-csharp"
					expectedDir = "${migrationDir}/eclipseWorkspace/Test-Project/expected/src-natural-csharp"
					compareResult = resultComparisonUtils.resultCompare(actualDir, expectedDir, [], [], "${migrationDir}/result-comparison/log", "${migrationDir}/result-comparison/tmp")
					if (compareResult != 0) {
						unstable 'Deviations in file comparison of src-natural-csharp folder'
					}
                }
			}
		} catch (ex) {
			miscUtils.errorWithStackTrace(ex)
		} finally {
		    stage('finalize') {
				sh returnStatus: true, script: "mv ${migrationDir}/coverage/migration.exec ${migrationDir}/coverage/vsam-custom-migration-jacoco.exec"
                archiveArtifacts allowEmptyArchive: true, artifacts: '**/*.log, **/eclipseWorkspace/Test-Project/src-natural-csharp/**/*,**/*.exec', excludes: '**/.metadata/**'
            }
		}		
	}	
}
