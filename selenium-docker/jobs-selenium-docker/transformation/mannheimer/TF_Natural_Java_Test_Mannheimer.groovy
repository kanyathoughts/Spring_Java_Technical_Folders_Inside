@Library('TestUtils') _

/**
 * Run the Mannheimer tests.
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param nat2javaBuild  The build version of a TF_Natural_Java_Migration_Mannheimer job to fetch the Mannheimer jars from.
 *        type: Extensible Choice -> File Choice Parameter
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * 		  type: boolean
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */


nodeTF('Docker-host && Region-EU') {

	timestamps {

		def mxVersionUtils = new MxVersionUtils()
		def gitUtils = new GitUtils()
		def dockerUtils = new DockerUtils()
		def miscUtils = new MiscUtils()
		def spUtils = new SharepointUtils()

		def workDir
		def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/mannheimer/mannheimer.git'
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		def dbPort = '50000'
		
		def antFlags = ''
		/* A string with properties to be passed to ant scripts. To be created later, see below. */
		def antProperties
		def antFile
		def buildProperties

		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "nat2javaBuild=${nat2javaBuild} withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"

		try {
			docker.image(dockerUtils.pullDockerDb2Image('mannheimerDB2')).withRun { container ->

				stage('Waiting for DB') {
					dockerUtils.waitForDb2(container.id, 2)
				}
				
				deleteDir()
				docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("--network=container:${container.id} -v jenkins-m2-repo:/var/m2_repo:rw") {
					workDir = pwd()

					stage('initialisation') {
						gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
						gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), workDir)						
						spUtils.downloadJobArtifact('TF_Natural_Java_Migration_Mannheimer', nat2javaBuild, 'mannheimer.jar', 'MANNHEIMER_LIBS')
						antFile = "${workDir}/run-tests.xml"
						buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
						antProperties = "-DworkDir=${workDir} -DjvmDir=java -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DcompilerLevel=${miscUtils.getCompilerLevel(javaVersion)} -DmxJarsDir=${buildProperties['iwJarDir']} -DmannheimerDbPort=${dbPort} -DwithCodeCoverage=${withCodeCoverage}"
						/* WQATF-435 - fetch artifacts from Nexus using maven. */
						withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
							sh "$MVN_CMD -f ${workDir}/pom-mannheimer-dependencies.xml dependency:copy-dependencies -DoutputDirectory=${buildProperties['mannheimerJarDir']}"
							sh "$MVN_CMD -f ${workDir}/pom-innowake-runtime-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
						}
					}

					stage('compile tests') {
						sh "ant ${antFlags} -buildfile ${antFile} ${antProperties} clean compile"
					}

					stage('run tests') {
						sh "ant ${antFlags} -buildfile ${antFile} ${antProperties} run"
					}
				}
			}
		} catch (ex) {
			miscUtils.errorWithStackTrace(ex)
		} finally {
			stage('finalize') {
				archiveArtifacts allowEmptyArchive: true, artifacts: 'log/**/*, coverage/Mannheimer_tests.exec'
				junit allowEmptyResults: true, healthScaleFactor: 0.0, keepLongStdio: true, testResults: 'log/report/xml/*.xml'
			}
		}
	}
}
