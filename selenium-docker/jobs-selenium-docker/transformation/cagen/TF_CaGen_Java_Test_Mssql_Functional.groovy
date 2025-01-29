@Library('TestUtils') _

/**
 * Run CAGen Functional UI tests for Gen banking application.
 * @param caGenBuildVersion  The caGen/hotfix version to migrate the Gen Application.
 * @param javaVersion The java version the test will run with
 * Requires port 8088 to be available to run UI tests
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

nodeTF('Docker-host && Region-US && !USLinux2') {
	timestamps {
		def dockerUtils = new DockerUtils()
		def gitUtils = new GitUtils()
		def miscUtils = new MiscUtils()
		def mxVersionUtils = new MxVersionUtils()
		def spUtils = new SharepointUtils()
		def user = miscUtils.getUserID()
		def group = miscUtils.getGroupID()
		
		def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(caGenBuildVersion)
		def testProjectDir = "${pwd()}/functional-tests-mssql"
		def workDir = "${pwd()}/backend"
		
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, caGenBuildVersion)
        
		buildName "#${env.BUILD_ID} - ${caGenBuildVersion}"
		buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"
		
		stage('initialization') {
			deleteDir()
			docker.image('alpine:3.5').pull()
			docker.image('quay.io/testcontainers/ryuk:0.2.3').pull()
			docker.image('mcr.microsoft.com/mssql/server:2019-CU10-ubuntu-16.04').pull()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside {
				gitUtils.checkoutGitProject(testProjectDir, "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/cagen/functional-tests-mssql.git", testProjectBranch)
				spUtils.downloadJobArtifact('TF_CaGen_Java_Migration', caGenBuildVersion, 'backend.zip', pwd())
				spUtils.downloadJobArtifact('TF_CaGen_Java_Migration', caGenBuildVersion, 'frontend.zip', pwd())
				sh "unzip -q ./backend.zip -d ./backend"
				sh "unzip -q ./frontend.zip -d ./frontend"
				sh "cp -R ${testProjectDir}/src/test ${workDir}/src"
				sh "cp -R ${testProjectDir}/pom.xml ${workDir}"
			}
		}
        
		stage('perform tests') {
			docker.image(dockerUtils.pullJenkinsEnvironmentCaGenImage(javaVersion)).inside('--network=host -v /var/run/docker.sock:/var/run/docker.sock -v root-jenkins-m2-repo:/var/m2_repo:rw -u=0') {
				dir(workDir) {
					withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
						try {
							sh "$MVN_CMD -Dserver.port=8088 -Dcom.innowake.gen.database-type=mssql -Dcom.innowake.gen-targeting.database-type=mssql -Dspring.datasource.driver-class-name=com.microsoft.sqlserver.jdbc.SQLServerDriver -Dspring.datasource.url='jdbc:sqlserver://localhost:1433;database=INTDB' -Dcom.innowake.gen.framework.version=${caGenBuildVersion} -Dlogging.level.com.innowake=OFF -Dlogging.level.org.springframework=OFF test"
							sh returnStatus: true, script: "mv ${workDir}/target/jacoco.exec ${workDir}/target/cagen-mssql-functional-jacoco.exec"
							archiveArtifacts allowEmptyArchive: true, artifacts: 'target/cagen-mssql-functional-jacoco.exec'
						} catch (ex) {
							unstable "perform test exception: ${ex}"
						}
					}
				}
				sh "chown -hR ${user}:${group} *"
			}
		}
	}
}
