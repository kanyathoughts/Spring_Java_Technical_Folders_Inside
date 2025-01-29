@Library('TestUtils') _

/**
 * Run test cases for the migration object filter
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
	nodeTF('Docker-host') {
		def mxVersionUtils = new MxVersionUtils()
		def gitUtils = new GitUtils()
		def miscUtils = new MiscUtils()
		def dockerUtils = new DockerUtils()
		
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		def migrationList = [
			[migrationType: 'cobol2csharp', resultFolderName: 'src-cobol-csharp'],
			[migrationType: 'cobol2java', resultFolderName: 'src-cobol-java'],
			[migrationType: 'ims2csharp', resultFolderName: 'src-ims-csharp'],
			[migrationType: 'ims2java', resultFolderName: 'src-ims-java'],
			[migrationType: 'nat2csharp', resultFolderName: 'src-natural-csharp'],
			[migrationType: 'nat2java', resultFolderName: 'src-natural-java'],
			[migrationType: 'pli2java', resultFolderName: 'src-pl1-java']
		]
		
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"
		
		catchError(buildResult: 'UNSTABLE', stageResult: 'UNSTABLE') {
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
				if (mxBuildVersion == mxVersionUtils.getFullBuild(mxBuildVersion)) {
					stage('init') {
						gitUtils.checkoutGitProject('sources', "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/migration-object-filter.git", testProjectBranch)
					}
					
					migrationList.each { entry ->
						try {
							stage(entry.migrationType) {
								// WQATF-154: This when block is a workaround for WMEE-14479
								when(entry.migrationType != 'ims2csharp' && entry.migrationType != 'nat2csharp' && entry.migrationType != 'pli2java') {
									def migrationDir = pwd() + "/migration/${entry.migrationType}"
									initMigration(entry.migrationType, migrationDir)
									miscUtils.prepareAndRunMigration(entry.migrationType, migrationDir, javaVersion, true)
									sh returnStatus: true, script: "mv ${migrationDir}/coverage/migration.exec ${migrationDir}/coverage/${entry.migrationType}-jacoco.exec"
									archiveArtifacts allowEmptyArchive: true, artifacts: '**/*.exec'
									runResultComparision(migrationDir, entry.resultFolderName)
								}
							}
						} catch (ex) {
							miscUtils.unstableWithStackTrace(ex)
						}
					}
				} else {
					echo "${mxBuildVersion} is a runtime only version, therefore skipping the migration for this version"
				}
			}
		}
		miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
	}
}

/**
 * Setup all necessary folders and run the result comparision
 * 
 * @param workDir The directory where the result comparision is executed
 * @param resultFolderName Name of the folder that contains the migrated files, e.g. src-natural-java
 */
def runResultComparision(workDir, resultFolderName) {
	def resultComparisonUtils = new ResultComparisonUtils()
	
	sh "mkdir -p ${workDir}/result-comparison/log"
	sh "mkdir -p ${workDir}/result-comparison/tmp"
	def actualDir = "${workDir}/eclipseWorkspace/Test-Project/${resultFolderName}"
	def expectedDir = "${workDir}/eclipseWorkspace/Test-Project/expected/${resultFolderName}"
	def compareResult = resultComparisonUtils.resultCompare(actualDir, expectedDir, [], [], "${workDir}/result-comparison/log", "${workDir}/result-comparison/tmp")
	if (compareResult != 0) {
		unstable 'Deviations in file comparison'
	}
}

/**
 * Checks out the custom-migration project, downloads the eclipse bundle, downloads the migration jar
 * files and copies the files for the migration (source files, expected files, filter and properties)
 * to the migration project
 * 
 * @param migrationType Migration type like nat2java, this name must be the same as the launch file in 
 * the custom-migration project
 * @param workDir The directory where the result comparision is executed
 */
def initMigration(migrationType, workDir) {
	def spUtils = new SharepointUtils()
	def gitUtils = new GitUtils()
	def miscUtils = new MiscUtils()
	
	gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/custom-migration.git", 'master')
	spUtils.downloadIwEclipseLinux(mxBuildVersion, "${workDir}/eclipse/eclipse")
	dir(workDir) {
		withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
			def buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
			sh "$MVN_CMD -f pom.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
		}
	}
	sh "cp -r ${pwd()}/sources/${migrationType}/* ${workDir}/eclipseWorkspace/Test-Project"
}
