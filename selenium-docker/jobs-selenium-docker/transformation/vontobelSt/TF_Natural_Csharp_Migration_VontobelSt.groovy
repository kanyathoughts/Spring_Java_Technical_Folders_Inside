@Library('TestUtils') _

/**
 * Run the nat2csharp on the Natural-Test project against a certain maxenso build.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *		type: Extensible Choice -> File Choice Parameter 
 * @param javaVersion The java version the test will run with
 *		type: Choice Parameter
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
		def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/natural-test/natural-csharp-vontobelSt.git'
		def migrationProjectBranch = 'master'
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		def migrationDir
		def testProjectWorkDir
		def vs_solutionDir
		def actualDir
		def expectedDir
		def compareResult
		def listExtension = mxVersionUtils.getVersionListByArtifactUrlSuffix('releases/innowake/products/mee/visualstudio/extensions/appmod-vs-extensions')
		def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
		def fullBuild
		def precompilationExecuted = false

		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"
		
		try {
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
			    fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
				migrationDir = pwd() + '/migration'
				testProjectWorkDir = pwd() + '/test-project'
				vs_solutionDir = testProjectWorkDir + '/VS_Solution'
				
				stage('initialisation') {
					gitUtils.checkoutGitProject(migrationDir, "${gitUtils.getGitUrlQef()}/${migrationProjectLocation}", migrationProjectBranch)
					gitUtils.checkoutGitProject(testProjectWorkDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
					spUtils.downloadIwEclipseLinux(mxBuildVersion, "${migrationDir}/eclipse/eclipse")
					dir(migrationDir) {
						def buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': migrationDir, 'mxBuildVersion': mxBuildVersion])
						if (listExtension.contains(fullBuild)) {
    						withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
    							sh "$MVN_CMD dependency:copy -Dartifact=innowake.products.mee.visualstudio.extensions:appmod-vs-extensions:${mxBuildVersion}:zip -DoutputDirectory=tmp/"
								sh "$MVN_CMD -f pom.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
    						}
							unzip zipFile: "tmp/appmod-vs-extensions-${mxBuildVersion}.zip", dir: "${vs_solutionDir}/Scripts", quiet: true
						} else {				
							withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
								sh "$MVN_CMD -f pom.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
							}
						}
					}
					sh "cp -rT ${testProjectWorkDir}/migration/. ${migrationDir}/eclipseWorkspace/Test-Project"
				}
				
				stage('natural2csharp') {
					miscUtils.prepareAndRunMigration('nat2csharp', migrationDir, javaVersion, true)
				}		
				
				stage('natural2csharp-attribute') {
					when(listExtension.contains(fullBuild), "Artifact 'appmod-vs-extensions' is not available in version ${fullBuild}") {
						sh "sed -i \"s|target-folder-csharp = .*|target-folder-csharp = src-natural-csharp-attribute|g\" ${migrationDir}/eclipseWorkspace/Test-Project/nat2csharp.properties"
						sh "sed -i \"s|transform-natural-to-pure-csharp = [a-z]+|transform-natural-to-pure-csharp = false|g\" ${migrationDir}/eclipseWorkspace/Test-Project/nat2csharp.properties"
						miscUtils.prepareAndRunMigration('nat2csharp', migrationDir, javaVersion, true, false)
						/*
						 * Archive the generated C# sources before further processing.
						 * This is the result right after cobol2csharp stage.
						 */
						zip dir: "${migrationDir}/eclipseWorkspace/Test-Project/src-natural-csharp-attribute", zipFile: "src-natural-csharp-attribute-after-nat2csharp.zip"
						/*
						 * C# source files have been generated now by nat2csharp. 
						 * Copy the *.cs and *.ccopy source files from Test-Project/ to VS_Solution/.
						 */
						sh "cp -rf ${migrationDir}/eclipseWorkspace/Test-Project/src-natural-csharp-attribute/* ${vs_solutionDir}/natural-csharp-vontobelSt/src-natural-csharp/"
						stash includes: 'test-project/VS_Solution/', name: 'VS_Solution_before_precompilation'
					}
				}
			}
				
			stage('precompile-csharp-sources') {
			    when(listExtension.contains(fullBuild), "Artifact 'appmod-vs-extensions' is not available in version ${fullBuild}") {
				/*
				 * Switch to a Windows node with MS build tools, unstash VS_Solution, run precompilation, stash VS_Solution again with precompiled C# sources.
				 * Note: the precompilation tool (aka visualstudio.extensions) have already been fetched by maven in the "prepare" stage.
				 */
					nodeTF('Tool-msbuild') {
						deleteDir()
						unstash name: 'VS_Solution_before_precompilation'
						dir('test-project/VS_Solution/') {
						    powershell script: 'Scripts\\precompileSources.ps1'
						}
						stash includes: 'test-project/VS_Solution/', name: 'VS_Solution_after_precompilation'
					}
					precompilationExecuted = true
			    }
			}
				
			/* Back to Linux node and docker container. */
		    docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside {
				stage('result-comparison-nat2csharp') {
					sh "mkdir -p ${migrationDir}/result-comparison/log"
					sh "mkdir -p ${migrationDir}/result-comparison/tmp"
					actualDir = "${migrationDir}/eclipseWorkspace/Test-Project/src-natural-csharp"
					expectedDir = "${migrationDir}/eclipseWorkspace/Test-Project/expected/src-natural-csharp"
					compareResult = resultComparisonUtils.resultCompare(actualDir, expectedDir, [], [], "${migrationDir}/result-comparison/log", "${migrationDir}/result-comparison/tmp")
					if (compareResult != 0) {
						unstable 'Deviations in file comparison of src-natural-csharp folder'
					}
				}
			
				stage('result-comparison-nat2csharp-attribute') {
				    when(precompilationExecuted, "precompilation did not run") {
    				    unstash name: 'VS_Solution_after_precompilation'
    					/*
    					 * The precompilation creates additional C# sources in two additional folders
    					 * We need to copy these files to the Test-Project
    					 */
    					sh returnStatus: true, script: "cp -rf test-project/VS_Solution/natural-csharp-vontobelSt/src-natural-csharp/* ${migrationDir}/eclipseWorkspace/Test-Project/src-natural-csharp-attribute"
    					
    					sh "mkdir -p ${migrationDir}/result-comparison/logAttribute"
    					sh "mkdir -p ${migrationDir}/result-comparison/tmpAttribute"
    					actualDir = "${migrationDir}/eclipseWorkspace/Test-Project/src-natural-csharp-attribute"
    					expectedDir = "${migrationDir}/eclipseWorkspace/Test-Project/expected/src-natural-csharp-attribute"
    					compareResult = resultComparisonUtils.resultCompare(actualDir, expectedDir, [], [], "${migrationDir}/result-comparison/logAttribute", "${migrationDir}/result-comparison/tmpAttribute")
    					if (compareResult != 0) {
    						unstable 'Deviations in file comparison of src-natural-csharp-attribute folder'
    					}
				    }
				}
			}
		} catch (ex) {
			miscUtils.errorWithStackTrace(ex)
		} finally {
			stage('finalize') {
				docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
					sh returnStatus: true, script: "mv ${migrationDir}/coverage/migration.exec ${migrationDir}/coverage/natural-csharp-migration-jacoco.exec"
					archiveArtifacts allowEmptyArchive: true, artifacts: 'log/**/*,**/*.exec'
					zip zipFile: 'natural_csharp_src.zip', dir: "${migrationDir}/eclipseWorkspace/Test-Project/src-natural-csharp"
					spUtils.uploadJobArtifact(mxBuildVersion, 'natural_csharp_src.zip')				
					if (precompilationExecuted) {
						zip zipFile: 'natural_csharp_src_attribute.zip', dir: "${migrationDir}/eclipseWorkspace/Test-Project/src-natural-csharp-attribute"
						spUtils.uploadJobArtifact(mxBuildVersion, 'natural_csharp_src_attribute.zip')
					}
					zip zipFile: 'result-comparison.zip', dir: "${migrationDir}/result-comparison"
					spUtils.uploadJobArtifact(mxBuildVersion, 'result-comparison.zip')
				}
			}
		}
	}
}