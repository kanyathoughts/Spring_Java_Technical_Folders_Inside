@Library('TestUtils') _

/**
 * Run vsam tests in java
 *
 * @param mxBuildVersion  The maxenso build to use.
 *        type: Extended Choice
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		  If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

timestamps {
	def gitUtils = new GitUtils()
	def mxVersionUtils = new MxVersionUtils()
	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	
	nodeTF('Docker-host') {
		catchError {
			def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)

			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
				def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
				buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} fullBuild=${fullBuild}"
				
				stage('init') {
					def workDir = pwd()
					def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/natural-vsam.git'
					gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
					gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), workDir)
					copyArtifacts([
					               projectName: 'TF_Natural_Java_Migration_Vsam',
					               filter: '**/*.java', 
					               selector: lastSuccessful(),
					               parameters: "mxBuildVersion=${fullBuild}",
					               target: 'artifacts',
					               fingerprintArtifacts: true,
					               flatten: true
					               ])
    				// Remove default java files and copy over actual migrated files from the TF_Natural_Java_Migration_VSAM job.                
                    sh "rm -rf ${workDir}/src-natural-java/vsam/test/transformed/*"	  
                    sh "cp -r artifacts/* ${workDir}/src-natural-java/vsam/test/transformed"
				}
				
				stage('test') {
					withMaven(maven: 'Default', options: [junitPublisher()], publisherStrategy: 'EXPLICIT') {
						sh "$MVN_CMD -DmvnCompilerVersion=${miscUtils.getCompilerLevel(javaVersion)} -Dinnowake.version=${mxBuildVersion} -Dinnowake.version.full.build=${fullBuild} test"						
						archiveArtifacts allowEmptyArchive: true, artifacts: 'target/jacoco.exec'
					}
				}
			}
		}
	}
}
