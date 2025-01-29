@Library('TestUtils') _

/**
 * Transforms the Cobol sources of the NMSLO codebase to C#, using a headless Eclipse.
 *
 * @param mxBuildVersion  The maxenso build under test
 * @param useDifferentTestProjectBranch By default the branch of the LeanFT project is checked out based on the selected mxVersion. 
 * 	      If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param javaVersion The java version the test will run with
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 */

nodeTF('Docker-host && Region-US') {
	
	timestamps {
		
		def gitUtils = new GitUtils()
		def mxVersionUtils = new MxVersionUtils()
		def miscUtils = new MiscUtils()
		def resultComparisonUtils = new ResultComparisonUtils()
		def dockerUtils = new DockerUtils()
		
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		def buildProperties
		def workDir
		def antFlags = ''
		def antFile_cobol2csharp
		def antProperties
		def spUtils = new SharepointUtils()

		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "testProjectBranch=${testProjectBranch} javaVersion=${javaVersion} withCodeCoverage=${withCodeCoverage}"
		
		try {
			deleteDir()
			
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
				workDir = pwd()
				antProperties = "-DworkDir=${workDir} -DjvmDir=java -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion} -DwithCodeCoverage=${withCodeCoverage}"
				antFile_cobol2csharp = "${workDir}/build.xml"
				stage('prepare') {
					gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/nmslo.git", testProjectBranch)
					spUtils.downloadIwEclipseLinux(mxBuildVersion, "${workDir}/eclipse/eclipse")
					
					buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
					
					withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
						sh "$MVN_CMD dependency:copy -Dartifact=innowake.products.mee.visualstudio.extensions:appmod-vs-extensions:${mxBuildVersion}:zip -DoutputDirectory=tmp/"
						sh "$MVN_CMD -f ${workDir}/pom.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
					}
					unzip zipFile: "tmp/appmod-vs-extensions-${mxBuildVersion}.zip", dir: 'VS_Solution/Scripts', quiet: true
				}
				
				stage('patch-cobol-sources') {
					withAnt(installation: 'Default') {
						def fakeDataMxJarsDir = "${workDir}/data/mxJars"
						sh "mkdir -p -m a=rwx ${fakeDataMxJarsDir}"
						sh "ant ${antFlags} -DworkDir=${workDir} -DmxJarsDir=${fakeDataMxJarsDir} -DmxBuildVersion=${mxBuildVersion} init"
						sh "ant ${antFlags} -Dbasedir=${buildProperties.testProjectDir} -DworkDir=${workDir} -DmxBuildVersion=${mxBuildVersion} patch-cobol-sources"
					}
					/* 
					 * If patching fails it leaves behind some files with suffixes *.orig and *.rej.
					 * They may be relevant for analysis and debugging, but not for further processing of this job. Therefore they are archived and then removed.
					 */
					cleanupPatchResidues(buildProperties.cobolSourcesDir, 'failed-cobol-patches')
				}
				
				stage('cobol2csharp') {
					withAnt(installation: 'Default') {
						sh "ant ${antFlags} -buildfile ${antFile_cobol2csharp} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} cobol2csharp"
						/*
						 * The generated C# sources are structured in folders:
						 * - EclipseWorkspace/Test-Project/src-cobol-csharp/copy
						 * - EclipseWorkspace/Test-Project/src-cobol-csharp/map
						 * - EclipseWorkspace/Test-Project/src-cobol-csharp/pgm
						 * VS_Solution expects a different structure. An ant call rearranges the C# sources from the actual to the expected folder structure. 
						 */
						sh "ant ${antFlags} -DworkDir=${workDir} -DmxBuildVersion=${mxBuildVersion} rearrange-csharp-sources"
					}
					/* WQATF-455: archive the generated C# sources before further processing.
					 * This is the result right after cobol2csharp incl. pre-/postprocessing, but before application of patches of C# files.
					 */
					sh "mkdir src-cobol-csharp-after-cobol2csharp; cp -rf ${buildProperties.csharpSourcesSubdir}/* src-cobol-csharp-after-cobol2csharp"
					zip dir: "src-cobol-csharp-after-cobol2csharp", zipFile: "src-cobol-csharp-after-cobol2csharp.zip"
					/*
					 * C# source files have been generated now by cobol2csharp. 
					 * Copy the *.cs and *.ccopy source files from Test-Project/ to VS_Solution/.
					 */
					sh "cp -rf ${buildProperties.csharpSourcesSubdir}/* VS_Solution"
					stash includes: 'VS_Solution/', name: 'VS_Solution_before_precompilation'
				}
			}
			
			stage('precompile-csharp-sources') {
				/*
				 * Switch to a Windows node with MS build tools, unstash VS_Solution, run precompilation, stash VS_Solution again with precompiled C# sources.
				 * Note: the precompilation tool (aka visualstudio.extensions) have already been fetched by maven in the "prepare" stage.
				 */
				nodeTF('Tool-msbuild') {
					deleteDir()
					unstash name: 'VS_Solution_before_precompilation'
					dir('VS_Solution') {
						powershell script: 'Scripts\\precompileSources.ps1'
					}
					stash includes: 'VS_Solution/', name: 'VS_Solution_after_precompilation'
				}
			}
			
			/* Back to Linux node and docker container. */
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside {
				stage('patch-csharp-sources') {
					unstash name: 'VS_Solution_after_precompilation'
					/*
					 * The precompilation creates additional C# sources in two additional folders
					 * - VS_Solution/Batch/Defs
					 * - VS_Solution/Online/Defs
					 * That's why the arguments to the cp commands for both folders are different to those of the other cp commands.
					 */
					sh returnStatus: true, script: "mkdir ${buildProperties.csharpSourcesSubdir}/Batch/Defs; cp -rf VS_Solution/Batch/Defs/* ${buildProperties.csharpSourcesSubdir}/Batch/Defs"
					sh returnStatus: true, script: "cp -rf VS_Solution/Batch/Pgms/* ${buildProperties.csharpSourcesSubdir}/Batch/Pgms"
					sh returnStatus: true, script: "mkdir ${buildProperties.csharpSourcesSubdir}/Online/Defs; cp -rf VS_Solution/Online/Defs/* ${buildProperties.csharpSourcesSubdir}/Online/Defs"
					sh returnStatus: true, script: "cp -rf VS_Solution/Online/Pgms/* ${buildProperties.csharpSourcesSubdir}/Online/Pgms"
					sh returnStatus: true, script: "cp -rf VS_Solution/Online/Maps/* ${buildProperties.csharpSourcesSubdir}/Online/Maps"
					sh returnStatus: true, script: "cp -rf VS_Solution/Common/Copy/* ${buildProperties.csharpSourcesSubdir}/Common/Copy"
					
					withAnt(installation: 'Default') {
						sh "ant ${antFlags} -Dbasedir=${buildProperties.testProjectDir} -DworkDir=${workDir} -DmxBuildVersion=${mxBuildVersion} patch-csharp-sources"
					}
					/* 
					 * If patching fails it leaves behind *.orig and *.rej files.
					 * They may be relevant for analysis and debugging, but not for further processing of this job. Therefore they are archived and then removed.
					 */
					cleanupPatchResidues(buildProperties.csharpSourcesDir, 'failed-csharp-patches')
				}
				
				stage('result-comparison') {
					sh "mkdir -p ${workDir}/result-comparison/log"
					sh "mkdir -p ${workDir}/result-comparison/tmp"
					def actualDir = "${workDir}/eclipseWorkspace/Test-Project/src-cobol-csharp"
					def expectedDir = "${workDir}/eclipseWorkspace/Test-Project/expected/src-cobol-csharp"
					def compareResult = resultComparisonUtils.resultCompare(actualDir, expectedDir, [], [], "${workDir}/result-comparison/log", "${workDir}/result-comparison/tmp")
					if (compareResult != 0) {
						unstable 'Deviations in file comparison'
					}
					zip dir: "${workDir}/result-comparison", zipFile: "result-comparison.zip"
				}
			}
		} catch (ex) {
			miscUtils.errorWithStackTrace(ex)
		} finally {
			stage('archive') {
				docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
					zip zipFile: 'csharpSources.zip', archive: false, dir: "${buildProperties.csharpSourcesSubdir}"
					sh returnStatus: true, script: 'cp eclipse-environment.log log/'
					archiveArtifacts allowEmptyArchive: true, artifacts: 'log/*,**/*.exec'
					spUtils.uploadJobArtifact(mxBuildVersion, 'csharpSources.zip')
					spUtils.uploadJobArtifact(mxBuildVersion, 'src-cobol-csharp-after-cobol2csharp.zip')
					spUtils.uploadJobArtifact(mxBuildVersion, 'result-comparison.zip')
				}
			}
		}
	}
}

/**
 * Moves .orig and .rej files that are created in case of patch failures to another folder,
 * marks the build as unstable and archives the .orig and .rej files.
 * 
 * @param folder containing patched files
 * @param targetFolder target folder for .orig and .rej files that are created in case of patch failures
 */
def cleanupPatchResidues(folder, targetFolder) {
	sh "mkdir ${targetFolder}"
	def status1 = sh returnStatus: true, script: "find ${folder} \\( -name *.orig -or -name *.rej \\) -exec mv --force \\{\\} ${targetFolder} \\;"
	def status = sh returnStatus: true, script: "find ${folder} \\( -name *.orig -or -name *.rej \\) -exec rm --verbose --force \\{\\} \\;"
	def numberOfFiles = sh(returnStdout: true, script: "ls ${targetFolder}| wc -l").trim().toInteger()
	if (numberOfFiles !=  0) {
		unstable "Patching failed, see ${targetFolder}"
		archiveArtifacts artifacts: "${targetFolder}", allowEmptyArchive: true
	}
}
