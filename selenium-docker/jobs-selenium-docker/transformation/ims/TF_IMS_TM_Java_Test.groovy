@Library('TestUtils') _

/**
 * Checkout and run the IMS TM tests in Java.
 *
 * @param mxBuildVersion  The maxenso version to run the IMS TM tests.
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * 		  type: Boolean
 * @param javaVersion The java version the test will run with
 * @param useDifferentTestProjectBranch By default the ims-tm-java project is checked out with branch master. 
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

nodeTF('DELinux2') {

	timestamps {
		def mxVersionUtils = new MxVersionUtils()
		def dockerUtils = new DockerUtils()
		def gitUtils = new GitUtils()
		def miscUtils = new MiscUtils()
		
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		def dockerGroupId = miscUtils.getDockerGroupID()
		def testProjectDir = "${pwd()}/ims-tm-java"
		withCodeCoverage = Boolean.parseBoolean(withCodeCoverage)
		
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		
		deleteDir()
		docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion))
				.inside("--network=host -v /var/run/docker.sock:/var/run/docker.sock -v jenkins-m2-repo:/var/m2_repo:rw --group-add ${dockerGroupId}") {
			def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
			def mvnFlags = "-q -Dinnowake.version=${mxBuildVersion} -Dinnowake.version.full.build=${fullBuild}"
			buildDescription "withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} fullBuild=${fullBuild}"
			
			stage('initialisation') {
				gitUtils.checkoutGitProject(testProjectDir, "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/ims/ims-tm-java.git", testProjectBranch)
				gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), testProjectDir)
				copyArtifacts projectName: 'TF_IMS_Java_Migration',
							  filter: 'IMS_Migration_Java/eclipseWorkspace/Test-Project/src-*-java/tm/',
							  selector: lastSuccessful(),
							  parameters: "mxBuildVersion=${fullBuild}",
							  fingerprintArtifacts: true
				sh "rm -rf ${testProjectDir}/src-cobol-java ${testProjectDir}/src-ims-java"
				sh "cp -r IMS_Migration_Java/eclipseWorkspace/Test-Project/src-cobol-java/tm ${testProjectDir}/src-cobol-java"
				sh "cp -r IMS_Migration_Java/eclipseWorkspace/Test-Project/src-ims-java/tm ${testProjectDir}/src-ims-java"
			}
			
			stage('build') {
				dir(testProjectDir) {
					withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
						sh "$MVN_CMD -U dependency:resolve ${mvnFlags}"
						sh "$MVN_CMD clean compile test-compile ${mvnFlags}"
					}
				}
			}
			
			stage('test') {
				withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
					dir(testProjectDir) {
						retry(2){
	                		timeout(time: 30, unit: 'MINUTES') {
								catchError(message: 'IMS TM tests failed', buildResult: 'UNSTABLE', catchInterruptions: false, stageResult: 'UNSTABLE') {
									sh "$MVN_CMD test ${mvnFlags} -DskipCodeCoverage=${!withCodeCoverage} -Dtest=!ImsProfilingTest"
								}
								catchError(message: 'IMS TM profiling test failed', buildResult: 'UNSTABLE', catchInterruptions: false, stageResult: 'UNSTABLE') {
									sh "$MVN_CMD test ${mvnFlags} -DskipCodeCoverage=${!withCodeCoverage} -Dtest=ImsProfilingTest -Djvm.options=\"-Dfile.encoding=Cp1252 -Dinnowake.lib.core.profile.ProfilingEnabled=true\""
								}
							}
						}
					}
				}
			}
			
			stage('finalize') {
				junit allowEmptyResults: true, testResults: 'ims-tm-java/target/surefire-reports/*.xml'
				sh returnStatus: true, script: "mv ${testProjectDir}/target/jacoco.exec ${testProjectDir}/target/ims-tm-java-jacoco.exec"
				archiveArtifacts allowEmptyArchive: true, artifacts: '**/test/res/**/*.out, ims-tm-java/target/ims-tm-java-jacoco.exec'
			}
		}
	}
}
