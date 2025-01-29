@Library('TestUtils') _

/**
 * Transforms the Cobol and IMS sources of the ims-test-project to CSharp, using a headless Eclipse, and compares the
 * newly migrated code with a stable version of migrated code to make sure there are no differences.
 *
 * @param mxBuildVersion  The maxenso build under test
 * @param useDifferentTestProjectBranches By default all projects are checked out with branch master. 
 *         If useDifferentTestProjectBranches is set to true the branch is overridden by the parameters imsMigrationBranch, imsTmCsharpBranch and imsDbCsharpBranch.
 * @param imsMigrationBranch The branch of the ims-migration project that will be used if useDifferentTestProjectBranches is set.
 * @param imsTmCsharpBranch The branch of the ims-tm-csharp project that will be used if useDifferentTestProjectBranches is set.
 * @param imsDbCsharpBranch The branch of the ims-db-csharp project that will be used if useDifferentTestProjectBranches is set.
 * @param javaVersion The java version the test will run with.
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 */
timestamps {
	def gitUtils = new GitUtils()
	def miscUtils = new MiscUtils()
	def resultComparisonUtils = new ResultComparisonUtils()
	def dockerUtils = new DockerUtils()
	def mxVersionUtils = new MxVersionUtils()
	def spUtils = new SharepointUtils()

	nodeTF('Docker-host') {
		def gitlabRootDir = 'innowake-test-projects/transformation-test-projects'
		def customMigrationProjectLocation =  gitlabRootDir  + '/custom-migration.git'  
		def imsMigrationProjectLocation = gitlabRootDir + '/ims/ims-migration.git'
		def tmCsharpProjectLocation = gitlabRootDir + '/ims/ims-tm-csharp.git'
		def dbCsharpProjectLocation = gitlabRootDir + '/ims/ims-db-csharp.git'
		def customMigrationProjectBranch = 'master'
		
		def imsMigrationBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranches, imsMigrationBranch, mxBuildVersion)
		def imsTmCsharpBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranches, imsTmCsharpBranch, mxBuildVersion)
		def imsDbCsharpBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranches, imsDbCsharpBranch, mxBuildVersion)

		def workDir
		def buildProperties

		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "imsMigrationBranch=${imsMigrationBranch} imsTmCsharpBranch=${imsTmCsharpBranch} imsDbCsharpBranch=${imsDbCsharpBranch} javaVersion=${javaVersion} withCodeCoverage=${withCodeCoverage}"

		try {
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
				workDir = pwd() + '/IMS_Migration_Csharp'

				stage('initialisation') {
					gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${customMigrationProjectLocation}", customMigrationProjectBranch)
					spUtils.downloadIwEclipseLinux(mxBuildVersion, "${workDir}/eclipse/eclipse")
					dir(workDir){
						buildProperties = miscUtils.readPropertyFile('build.properties', ['mxBuildVersion': mxBuildVersion, 'workDir': workDir])
						withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
							sh "$MVN_CMD -f pom.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
						}
					}
					// WQATF-1417: Only keep src folders and .properties files
					def tempFolderName = 'tempFolder'
					dir("${workDir}/eclipseWorkspace/Test-Project/${tempFolderName}") {
						gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/${imsMigrationProjectLocation}", imsMigrationBranch)
						sh 'mv eclipseWorkspace/Test-Project/src-cobol ../.'
						sh 'mv eclipseWorkspace/Test-Project/src-ims ../.'
						sh 'mv eclipseWorkspace/Test-Project/*.properties ../.'
						deleteDir()
					}
					dir("${workDir}/expected") {
						dir(tempFolderName) {
							gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/${tmCsharpProjectLocation}", imsTmCsharpBranch)
							sh 'cp -r ims-tm-csharp/src-cobol-csharp ../.'
							sh 'cp -r ims-tm-csharp/src-ims-csharp ../.'
							deleteDir()

							gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/${dbCsharpProjectLocation}", imsDbCsharpBranch)
							sh 'cp -r ims-db-csharp/src-cobol-csharp/db ../src-cobol-csharp'
							sh 'cp -r ims-db-csharp/src-ims-csharp/db ../src-ims-csharp'
							deleteDir()
						}
					}						
				}

				stage('cobol2csharp') {
					miscUtils.prepareAndRunMigration('cobol2csharp', workDir, javaVersion, withCodeCoverage)
				}
				
				stage('cobol2csharp-result-comparison') { 
					sh "mkdir -p ${workDir}/result-comparison/c2cs/log"
					sh "mkdir -p ${workDir}/result-comparison/c2cs/tmp"
					compareCode(workDir, "${workDir}/eclipseWorkspace/Test-Project/src-cobol-csharp", "${workDir}/expected/src-cobol-csharp", "${workDir}/result-comparison/c2cs/log", "${workDir}/result-comparison/c2cs/tmp")
					def csharpErrorsLogFile = "${workDir}/csharp-errors.log"
					isFile = miscUtils.fileExists(csharpErrorsLogFile)
					if (isFile && miscUtils.getFileSize(csharpErrorsLogFile) > 0) {
						error "There occurred an error during the Cobol to CSharp migration, see ${csharpErrorsLogFile}"
					}
				}

				stage('ims2csharp') {
					miscUtils.prepareAndRunMigration('ims2csharp', workDir, javaVersion, withCodeCoverage, false)
				}

				stage('ims2csharp-result-comparison') {
					sh "mkdir -p ${workDir}/result-comparison/i2cs/log"
					sh "mkdir -p ${workDir}/result-comparison/i2cs/tmp"
					compareCode(workDir, 'eclipseWorkspace/Test-Project/src-ims-csharp', 'expected/src-ims-csharp', 'result-comparison/i2cs/log', 'result-comparison/i2cs/tmp')
					def csharpErrorsLogFile = "${workDir}/csharp-errors.log"
					isFile = miscUtils.fileExists(csharpErrorsLogFile)
					if (isFile && miscUtils.getFileSize(csharpErrorsLogFile) > 0) {
						error "There occurred an error during the IMS to CSharp migration, see ${csharpErrorsLogFile}"
					}
				}
			}
			
		} catch (ex) {
			miscUtils.errorWithStackTrace(ex)
		} finally {
			stage('finalize') {
				archiveArtifacts allowEmptyArchive: true, artifacts: 'IMS_Migration_Csharp/log/**/*,IMS_Migration_Csharp/result-comparison/**/*,IMS_Migration_Csharp/eclipseWorkspace/Test-Project/src-ims-csharp/**/*,IMS_Migration_Csharp/eclipseWorkspace/Test-Project/src-cobol-csharp/**/*,**/*.exec'
			}
		}
	}
}

def compareCode(workDir, actualDir, expectedDir, logDir, tmpDir) {
	dir(workDir) {
		def resultComparisonUtils = new ResultComparisonUtils()
		def compareResult = resultComparisonUtils.resultCompare(actualDir, expectedDir, [], ['nednn/', 'workbench/'], logDir, tmpDir)
		if (compareResult != 0) {
			unstable 'Deviations in file comparison'
		}
	}
}
