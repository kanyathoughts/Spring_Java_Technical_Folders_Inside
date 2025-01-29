@Library('TestUtils') _

/**
 * Checkout and run the ims-db-java project.
 *
 * @param mxBuildVersion  The maxenso version to run the IMS tests.
 * @param javaVersion The java version the test will run with
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * 		  type: Boolean
 * @param useDifferentTestProjectBranch By default the ims-db-java project is checked out with branch master. 
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param environmentLabel The resource label that will be locked while the job is running.
 */

timestamps {	
	def mxVersionUtils = new MxVersionUtils()
	def dockerUtils = new DockerUtils()
	def gitUtils = new GitUtils()
	def miscUtils = new MiscUtils()
	def perfUtils = new PerformanceUtils()
	withCodeCoverage = Boolean.parseBoolean(withCodeCoverage)

	if ((environmentLabel == 'perf-environment') && withCodeCoverage) {
		echo 'Exiting job...'
		error 'The code coverage measurements affect the performance therefore we should not measure the code coverage and the performance at the same time. Please choose environmentLabel=trafo-environment when choosing withCodeCoverage to true'
	}

	advancedLock(environmentLabel) {
		nodeTF(env.linuxNode, environmentLabel.contains('perf')) {	
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
			def pathToLogFile = 'ims-db-java/test/res/outputs/IMS_DB_Java_performance_report.log'

			def dockerGroupId = miscUtils.getDockerGroupID()
			def testProjectDir = "${pwd()}/ims-db-java"
			
			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion))
					.inside("--network=host -v /var/run/docker.sock:/var/run/docker.sock -v jenkins-m2-repo:/var/m2_repo:rw --group-add ${dockerGroupId}") {
				def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
				def mvnFlags = "-q -Dinnowake.version=${mxBuildVersion} -Dinnowake.version.full.build=${fullBuild}"
				buildDescription "javaVersion=${javaVersion} withCodeCoverage=${withCodeCoverage} testProjectBranch=${testProjectBranch} environmentLabel=${environmentLabel} fullBuild=${fullBuild}"
			
				stage('initialisation') {
					gitUtils.checkoutGitProject(testProjectDir, "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/ims/ims-db-java.git", testProjectBranch)
					gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), testProjectDir)
					copyArtifacts projectName: 'TF_IMS_Java_Migration',
						filter: 'IMS_Migration_Java/eclipseWorkspace/Test-Project/src-*-java/db/',
						selector: lastSuccessful(),
						parameters: "mxBuildVersion=${fullBuild}",
						fingerprintArtifacts: true
					sh "rm -rf ${testProjectDir}/src-cobol-java/db ${testProjectDir}/src-ims-java/db"
					sh "cp -r IMS_Migration_Java/eclipseWorkspace/Test-Project/src-cobol-java/db ${testProjectDir}/src-cobol-java/db"
					sh "cp -r IMS_Migration_Java/eclipseWorkspace/Test-Project/src-ims-java/db ${testProjectDir}/src-ims-java/db"
				}
				
				stage('build') {
					dir(testProjectDir) {
						withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
							sh "$MVN_CMD compile ${mvnFlags}"
						}
					}
				}
				
				stage('test') {
					withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
						dir(testProjectDir) {
							try {
								retry(2) {
									timeout(time: 45, unit: 'MINUTES') {
										sh "$MVN_CMD test ${mvnFlags} -DskipCodeCoverage=${!withCodeCoverage} -Dtest='tests.performance.**' -Dbulktest=true"
									}
								}
							} catch (ex) {
								unstable "after mvn test: ${ex}"
							}
						}
					}
				}

				stage('finalize') {
					sh returnStatus: true, script: "mv ${testProjectDir}/target/jacoco.exec ${testProjectDir}/target/ims-db-java-perf-jacoco.exec"
					archiveArtifacts artifacts: "${pathToLogFile}, ims-db-java/target/ims-db-java-perf-jacoco.exec", allowEmptyArchive: true
				}

				stage('reporting') {
					when(currentBuild.currentResult == 'SUCCESS', 'Performance reporting is not executed if the job is unstable') {
						def buildResult = build job: 'TF_Performance_Reporting', propagate: false, parameters: [
							string(name: 'mxBuildVersion',    value: mxBuildVersion),
							string(name: 'pathToLogFile',     value: pathToLogFile),
							string(name: 'buildURL',          value: BUILD_URL),
							string(name: 'pageID',            value: '166891899'),
							string(name: 'numDaysDataToKeep', value: '20')
						]
						miscUtils.evaluateBuildResult(buildResult, "TF_Performance_Reporting")
					}
		     	}
			}
		}
	}
}