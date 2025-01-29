@Library('TestUtils') _

/**
 * Run the nat2java on the Mannheimer project against a certain maxenso build.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *		type: Extensible Choice -> File Choice Parameter
 * @param javaVersion The java version the test will run with
 *		type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

timestamps {
	
	def mxVersionUtils = new MxVersionUtils()
	def gitUtils = new GitUtils()
	def miscUtils = new MiscUtils()
	def resultComparisonUtils = new ResultComparisonUtils()
	def dockerUtils = new DockerUtils()
	def spUtils = new SharepointUtils()
	
	def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/mannheimer/mannheimer.git'
	def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
	
	nodeTF('OS-Windows') {
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"
		
		stage('setup-test-workspace') {
			deleteDir()
			gitUtils.checkoutGitProject('sourceProject', "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
			println spUtils.downloadJobArtifact('TF_Natural_Java_Script_Signing_Mannheimer', mxBuildVersion, "Continentale-${mxBuildVersion}.zip", '.')
			unzip dir: '.', quiet: true, zipFile: "Continentale-${mxBuildVersion}.zip"
			bat "xcopy /q /s sourceProject\\eclipseWorkspace\\Mannheimer-PREP\\src-natural conti\\Mannheimer-Convert\\src\\natural"
			bat "xcopy /q /s sourceProject\\eclipseWorkspace\\Mannheimer-Java\\src\\java conti\\Mannheimer-Java\\src\\java"
            withAnt(installation: 'Default') {
				def workDir = pwd()
				def eclipseDir = "${workDir}/eclipse-${mxBuildVersion}"
				def eclipseWorkspaceDir = "${workDir}/conti"
				def buildPropsFile = "${workDir}/sourceProject/build.properties"
				def buildFile = "${workDir}/sourceProject/build-nat2java.xml"
				def antFlags = ''
	            def antPropertiesWindows = "-DworkDir=${workDir} -DeclipseDir=${eclipseDir} -DeclipseWorkspaceDir=${eclipseWorkspaceDir} -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion} -propertyfile ${buildPropsFile}"
            	bat "ant -buildfile ${buildFile} ${antFlags} ${antPropertiesWindows} init-workspace"
            }
		}
		
		stage('migration') {
			dir('conti') {
				withJava(javaVersion) {
					bat 'Mannheimer_antstarter.bat'
				}
				stash name: 'migration', includes: 'Mannheimer-Java/src/java/**'
			}
		}
		
		stage('create-and-upload-jars') {
			zip dir: 'conti/Mannheimer-Java/src/java/', zipFile: 'MANNHEIMER_LIBS/mannheimer-src.jar'
			zip dir: 'conti/Mannheimer-Java/target/classes/', zipFile: 'MANNHEIMER_LIBS/mannheimer.jar'
			spUtils.uploadJobArtifact(mxBuildVersion, 'MANNHEIMER_LIBS/mannheimer.jar')
			spUtils.uploadJobArtifact(mxBuildVersion, 'MANNHEIMER_LIBS/mannheimer-src.jar')
		}
	}
	
	nodeTF('Docker-host && Region-EU') {
		deleteDir()
		docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
			stage('result-comparison') {
				gitUtils.checkoutGitProject('sourceProject', "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
				unstash 'migration'
				
				def workDir = pwd()
				def expectedDir = "${workDir}/sourceProject/expected"
				def actualDir = "${workDir}/Mannheimer-Java/src/java"
				// Turn windows line endings into linux line endings to avoid adjusting line endings in all expected files in gitlab
				sh "find ${expectedDir} -type f -print0 | xargs -0 sed -i 's/\\r//g'"
				sh "find ${actualDir} -type f -print0 | xargs -0 sed -i 's/\\r//g'"
				sh "mkdir -p ${workDir}/result-comparison/log"
				sh "mkdir -p ${workDir}/result-comparison/tmp"
				def compareResult = resultComparisonUtils.resultCompare(actualDir, expectedDir, [], [], "${workDir}/result-comparison/log", "${workDir}/result-comparison/tmp")
				if (compareResult != 0) {
					unstable 'Deviations in file comparison'
				}
				zip dir: "${workDir}/result-comparison", zipFile: "result-comparison.zip"
			}
			
			stage('finalize') {
				spUtils.uploadJobArtifact(mxBuildVersion, 'result-comparison.zip')
			}
		}
	}
}
