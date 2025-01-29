@Library('TestUtils') _

/**
 * Run the nat2java on the KIDICAP project against a certain maxenso build.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * 		  type: Boolean
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

nodeTF('OS-Linux && Region-EU', true) {

	timestamps {
		
		def mxVersionUtils = new MxVersionUtils()
		def gitUtils = new GitUtils()
		def miscUtils = new MiscUtils()
		def resultComparisonUtils = new ResultComparisonUtils()
		def dockerUtils = new DockerUtils()
		def spUtils = new SharepointUtils()
		
		def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/kidicap/kidicap-184.git'
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
   		def workDir
   		def antFlags = ''
		def antFile_nat2Java
		def antFile_bflex
		/* A string with properties to be passed to ant scripts. To be created later, see below. */
		def antProperties
		/* The properties from the file build.properties */
		def buildProperties
		def eclipseWorkspaceDir
				
	    withCodeCoverage = Boolean.parseBoolean(withCodeCoverage)
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"
		
		try {
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
				workDir = pwd()
				
				stage('setup-test-workspace') {
					gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                	spUtils.downloadIwEclipseLinux(mxBuildVersion, "${workDir}/eclipse")
					buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
					antProperties = "-DworkDir=${workDir} -DjvmDir=java -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion} -DwithCodeCoverage=${withCodeCoverage}"
					gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), workDir)
					eclipseWorkspaceDir = "${workDir}/eclipseWorkspace"
				 	antFile_nat2Java = "${workDir}/build-nat2java.xml"
					antFile_bflex = "${workDir}/build-bflex.xml"
					
					sh "mkdir -p ${workDir}/log"
					sh "mkdir -p ${workDir}/result-comparison/log"
					sh "mkdir -p ${workDir}/result-comparison/tmp"
                	/* WQATF-435 - fetch artifacts from Nexus using maven. */
                	withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                		sh "$MVN_CMD -f ${workDir}/pom-gip-dependencies.xml dependency:copy-dependencies -DoutputDirectory=${buildProperties['gipJarDir']}"
                		sh "$MVN_CMD -f ${workDir}/pom-innowake-transformation-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
                		sh "$MVN_CMD -f ${workDir}/pom-innowake-runtime-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
                	}
					
					if (javaVersion == 'java17') {
						def content = readFile 'eclipseWorkspace/KIDICAP-Java/.settings/org.eclipse.jdt.core.prefs'
						content += 'org.eclipse.jdt.core.compiler.codegen.inlineJsrBytecode=enabled\n' +
							'org.eclipse.jdt.core.compiler.codegen.targetPlatform=11\n' +
							'org.eclipse.jdt.core.compiler.compliance=11\n' +
							'org.eclipse.jdt.core.compiler.problem.assertIdentifier=error\n' +
							'org.eclipse.jdt.core.compiler.problem.enablePreviewFeatures=disabled\n' +
							'org.eclipse.jdt.core.compiler.problem.enumIdentifier=error\n' +
							'org.eclipse.jdt.core.compiler.problem.reportPreviewFeatures=warning\n' +
							'org.eclipse.jdt.core.compiler.release=enabled\n' +
							'org.eclipse.jdt.core.compiler.source=11'
						writeFile encoding: 'UTF-8', file: 'eclipseWorkspace/KIDICAP-Java/.settings/org.eclipse.jdt.core.prefs', text: content
					}
				}
				
				stage('init-nat2java') {
                	withAnt(installation: 'Default') {
                    	/* WQATF-435 - fake a ${workDir}/data/mxJars folder so that init-nat2java can be executed without failure. */
                    	def fakeDataMxJarsDir = "${workDir}/data/mxJars"
                    	sh "mkdir -p -m a=rwx ${fakeDataMxJarsDir}"
                		sh "ant ${antFlags} -buildfile ${antFile_nat2Java} ${antProperties} -DtransformCopycodes=true -DmxJarsDir=${fakeDataMxJarsDir} init-nat2java"
                	}
				}
				
				stage('prepare-eclipse-workspace') {
					withAnt(installation: 'Default') {
						sh "ant ${antFlags} -buildfile ${antFile_nat2Java} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} prepare-eclipse-workspace"
					}
				}
				
				stage('run-nat2java') {
                    withAnt(installation: 'Default') {
                        timeout(time: 3, unit: 'HOURS') {
                            sh "ant ${antFlags} -buildfile ${antFile_nat2Java} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} nat2java"
                        }
                    }
				}
				
				stage('pack-kidicap-jar') {
                	withAnt(installation: 'Default') {
                		sh "ant ${antFlags} -buildfile ${antFile_nat2Java} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} packJars"
                	}
				}
				
				stage('result-comparison') {
					def actualDir = "${eclipseWorkspaceDir}/KIDICAP-Java/src/java-mee"
					def wcc = 'withCopycodes'
					def expectedDir = "${workDir}/expected/${wcc}"
					def expectedZip = "${expectedDir}/${wcc}.zip"
					sh "unzip -q ${expectedZip} -d ${expectedDir}"
					sh "rm ${expectedZip}"
					def compareResult = resultComparisonUtils.resultCompare(actualDir, expectedDir, [], [], "${workDir}/result-comparison/log", "${workDir}/result-comparison/tmp")
					if (compareResult != 0) {
						unstable 'Deviations in file comparison'
					}
					zip dir: "${workDir}/result-comparison", zipFile: "result-comparison.zip"
					
					def javaErrorsLogFile = "${workDir}/log/java-errors.log"
					fileSize = miscUtils.getFileSize(javaErrorsLogFile)
					if (fileSize > 0) {
						error "The generated Java code has compile errors, see ${javaErrorsLogFile}"
					}
				}
				
				stage('compile-bflex-AGTV') {
	                withAnt(installation: 'Default') {
	                	sh "ant ${antFlags} -buildfile ${antFile_bflex} ${antProperties} -DcompilerLevel=${miscUtils.getCompilerLevel(javaVersion)} -DmxJarsDir=${buildProperties['iwJarDir']} -DbflexLibCompileDir=${buildProperties['iwJarDir']} init-bflex-variant compile"
	                }
				}
			}
		} catch (ex) {
			miscUtils.errorWithStackTrace(ex)
		} finally {
			stage('finalize') {
				docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
					archiveArtifacts allowEmptyArchive: true, artifacts: 'log/**/*,**/*.exec'
					spUtils.uploadJobArtifact(mxBuildVersion, 'result-comparison.zip')
					spUtils.uploadJobArtifact(mxBuildVersion, 'KIDICAP_LIBS/kidicap.jar')
					spUtils.uploadJobArtifact(mxBuildVersion, 'KIDICAP_LIBS/kidicap-src.jar')					
					spUtils.uploadJobArtifact(mxBuildVersion, 'KIDICAP_LIBS/bflex-agtv.jar')
				}
			}
		}
	}
}
