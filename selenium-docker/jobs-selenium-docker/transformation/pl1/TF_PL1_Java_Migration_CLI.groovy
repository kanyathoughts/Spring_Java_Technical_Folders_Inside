@Library('TestUtils') _

/**
 * Transforms PL/I to Java, using the PL12Java CLI.
 *
 * @param mxBuildVersion  The maxenso build under test
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param javaVersion The java version the test will run with
 */
timestamps {
	def mxVersionUtils = new MxVersionUtils()
	def gitUtils = new GitUtils()
	def miscUtils = new MiscUtils()
	def resultComparisonUtils = new ResultComparisonUtils()
	def dockerUtils = new DockerUtils()

	nodeTF('Docker-host') {
		try {
		    def workDir = pwd()
		    def migrationProjectDir = "${workDir}/Test-Project"
		    def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
		    def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
			def remoteProjectLocation ='innowake-test-projects/transformation-test-projects/pl1-test.git'
		    
		    buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		    buildDescription "testProjectBranch=${testProjectBranch} javaVersion=${javaVersion}"

			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
				
				stage('initialisation') {
					dir(migrationProjectDir) {
						gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
						gitUtils.getLicenseFile(mxVersion, '.')
						sh 'mv pl1-java/* .'
						sh 'rm -r pl1-java/'
						sh 'rm expected/.gitkeep'
						sh 'mv src-pl1-java/* expected/.'
					}
					withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
						sh "$MVN_CMD dependency:copy -Dartifact=innowake.products.mee.source.migration.pl1:" +
								"mee-source-migration-pl1-standalone-dist:${mxBuildVersion} -DoutputDirectory=${migrationProjectDir}"
					}
				}

				stage('pl12java') {
					dir(migrationProjectDir) {
						sh "java -jar mee-source-migration-pl1-standalone-dist*.jar -p pli2java.properties " +
								"${migrationProjectDir}/src-pl1"
						if (manager.logContains(".*BUILD FAILED.*")) {
							error 'BUILD FAILED detected in logs, failing build'
						}
					}
				}

				stage('result-comparison') {
					sh "mkdir -p ${workDir}/result-comparison/log"
					sh "mkdir -p ${workDir}/result-comparison/tmp"
					def actualDir = "${migrationProjectDir}/src-pl1-java"
					def expectedDir = "${migrationProjectDir}/expected"
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
				archiveArtifacts allowEmptyArchive: true, artifacts: 'Test-Project/src-pl1-java/**/*, **/*?.log', excludes: 'Test-Project/test/res/jcl/**/*', fingerprint: true
			}
		}
	}
}