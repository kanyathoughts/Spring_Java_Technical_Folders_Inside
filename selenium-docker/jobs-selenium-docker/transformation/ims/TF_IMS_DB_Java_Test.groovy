@Library('TestUtils') _

/**
 * Checkout and run the ims-db-java project.
 *
 * @param mxBuildVersion  The maxenso version to run the IMS tests.
 * @param javaVersion The java version the test will run with
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * 		  type: Boolean
 */

nodeTF('USLinux9-Trafo') {	
	timestamps {
	    def mxVersionUtils = new MxVersionUtils()
	    def dockerUtils = new DockerUtils()
	    def gitUtils = new GitUtils()
	    def miscUtils = new MiscUtils()
	    def perfUtils = new PerformanceUtils()
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		withCodeCoverage = Boolean.parseBoolean(withCodeCoverage)

		def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
		def dockerGroupId = miscUtils.getDockerGroupID()
		def testProjectDir = "${pwd()}/ims-db-java"
			
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			
		deleteDir()
		docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion))
				.inside("--network=host -v /var/run/docker.sock:/var/run/docker.sock -v jenkins-m2-repo:/var/m2_repo:rw --group-add ${dockerGroupId}") {
			def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
			def mvnFlags = "-q -Dinnowake.version=${mxBuildVersion} -Dinnowake.version.full.build=${fullBuild}"
			def gradleFlags = "-PiWBuildVersion=${mxBuildVersion} -PiWFullBuildVersion=${fullBuild}"
			buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} withCodeCoverage=${withCodeCoverage} fullBuild=${fullBuild}"
			
		    stage('initialisation') {
				gitUtils.checkoutGitProject(testProjectDir, "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/ims/ims-db-java.git", testProjectBranch)
				gitUtils.getLicenseFile(mxVersion, testProjectDir)
				configFileProvider([configFile(fileId: 'gradle_properties', targetLocation: './ims-db-java', variable: 'pathToPropertyFile')]) {
				    withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-Nexus', passwordVariable: 'mavenPassword', usernameVariable: 'mavenUser')]) {
				    }
			    }
			}
				
			stage('maven-compile') {
				dir(testProjectDir) {
					withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
						sh "$MVN_CMD compile ${mvnFlags}"
					}
				}
			}
				
			stage('maven-test') {
				withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
					dir(testProjectDir) {
						try {
							retry(2) {
								timeout(time: 5, unit: 'MINUTES') {
									sh "$MVN_CMD test ${mvnFlags} -DskipCodeCoverage=${!withCodeCoverage} -Dtest='tests.hints.**, tests.async.**, tests.db.reload.wqatf1152.WQATF**, tests.schema.**, tests.nednn.**'"
								}
							}
						} catch (ex) {
							unstable "after mvn test: ${ex}"
						}
					}
				}
			}

			stage('gradle-compile') {
				dir(testProjectDir) {
					sh 'chmod +x gradlew'
					sh "./gradlew clean compileJava ${gradleFlags}"
				}
			}

			stage('gradle-test') {
				dir(testProjectDir) {
					retry(2) {
						timeout(time: 10, unit: 'MINUTES') {
							sh "./gradlew test ${gradleFlags}"
						}
					}
				}
			}

			stage('finalize') {
				sh returnStatus: true, script: "mv ${testProjectDir}/target/jacoco.exec ${testProjectDir}/target/ims-db-java-jacoco.exec"
				archiveArtifacts allowEmptyArchive: true, artifacts: 'ims-db-java/target/ims-db-java-jacoco.exec'
			}
		}
	}
}