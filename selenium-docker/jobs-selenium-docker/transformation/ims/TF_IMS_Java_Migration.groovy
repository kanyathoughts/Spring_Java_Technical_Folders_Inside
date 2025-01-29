@Library('TestUtils') _

/**
 * Transforms the Cobol and IMS sources of the ims-test-project to Java, using a headless Eclipse, and compares the 
 * newly migrated code with a stable version of migrated code to make sure there are no differences.
 * 
 * @param mxBuildVersion  The maxenso build under test
 * @param useDifferentTestProjectBranches By default all projects are checked out with branch master. 
 *         If useDifferentTestProjectBranches is set to true the branch is overridden by the parameters imsMigrationBranch, imsTmJavaBranch and imsDbJavaBranch.
 * @param imsMigrationBranch The branch of the ims-migration project that will be used if useDifferentTestProjectBranches is set.
 * @param imsTmJavaBranch The branch of the ims-tm-java project that will be used if useDifferentTestProjectBranches is set.
 * @param imsDbJavaBranch The branch of the ims-db-java project that will be used if useDifferentTestProjectBranches is set.
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
		def tmJavaProjectLocation = gitlabRootDir + '/ims/ims-tm-java.git'
		def dbJavaProjectLocation = gitlabRootDir + '/ims/ims-db-java.git'
		def customMigrationProjectBranch = 'master'

        def imsMigrationBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranches, imsMigrationBranch, mxBuildVersion)
        def imsTmJavaBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranches, imsTmJavaBranch, mxBuildVersion)
        def imsDbJavaBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranches, imsDbJavaBranch, mxBuildVersion)
        
        def workDir
        def buildProperties

        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "imsMigrationBranch=${imsMigrationBranch} imsTmJavaBranch=${imsTmJavaBranch} imsDbJavaBranch=${imsDbJavaBranch} javaVersion=${javaVersion} withCodeCoverage=${withCodeCoverage}"
        
        try {
        	deleteDir()
        	docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
        		workDir = pwd() + '/IMS_Migration_Java'

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
							gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/${tmJavaProjectLocation}", imsTmJavaBranch)
							sh 'cp -r src-cobol-java ../.'
							sh 'cp -r src-ims-java ../.'
							deleteDir()

							gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/${dbJavaProjectLocation}", imsDbJavaBranch)
							sh 'cp -r src-cobol-java/db ../src-cobol-java'
							sh 'cp -r src-ims-java/db ../src-ims-java'
							deleteDir()
						}
					}						
                }
                
                stage('cobol2java') {
                    miscUtils.prepareAndRunMigration('cobol2java', workDir, javaVersion, withCodeCoverage)
                    dir("${workDir}/eclipseWorkspace/Test-Project/src-cobol-java") {
                        sh "find . -type f -exec sed -i 's#/\\*\\spatch\\s:##g; s#\\send\\*\\*\\*\\*/##g' {} +"
                    }
                }
                
                stage('cobol2java-result-comparison') {
                    sh "mkdir -p ${workDir}/result-comparison/c2j/log"
                    sh "mkdir -p ${workDir}/result-comparison/c2j/tmp"
                    compareCode(workDir, 'eclipseWorkspace/Test-Project/src-cobol-java', 'expected/src-cobol-java', 'result-comparison/c2j/log', 'result-comparison/c2j/tmp')
                    def javaErrorsLogFile = "${workDir}java-errors.log"
                    isFile = miscUtils.fileExists(javaErrorsLogFile)
                    if (isFile && miscUtils.getFileSize(javaErrorsLogFile) > 0) {
                        error "The generated Java code has compile errors, see ${javaErrorsLogFile}"
                    }
                }
                
                stage('ims2java') {
                    miscUtils.prepareAndRunMigration('ims2java', workDir, javaVersion, withCodeCoverage, false)
                }
                
                stage('ims2java-result-comparison') {
                    sh "mkdir -p ${workDir}/result-comparison/i2j/log"
                    sh "mkdir -p ${workDir}/result-comparison/i2j/tmp"
                    compareCode(workDir, 'eclipseWorkspace/Test-Project/src-ims-java', 'expected/src-ims-java', 'result-comparison/i2j/log', 'result-comparison/i2j/tmp')
                    def javaErrorsLogFile = "${workDir}/java-errors.log"
                    isFile = miscUtils.fileExists(javaErrorsLogFile)
                    if (isFile && miscUtils.getFileSize(javaErrorsLogFile) > 0) {
                        error "The generated Java code has compile errors, see ${javaErrorsLogFile}"
                    }
                }
            }
        } catch (ex) {
            miscUtils.errorWithStackTrace(ex)
        } finally {
            stage('finalize') {
                archiveArtifacts allowEmptyArchive: true, artifacts: '**/*.log,**/*.java,**/*.exec', excludes: '**/expected/**,ims-tm-java/,ims-db-java/'
            }
        }
    }
}

def compareCode(workDir, actualDir, expectedDir, logDir, tmpDir) {
    dir(workDir) {
        def resultComparisonUtils = new ResultComparisonUtils()
        def compareResult = resultComparisonUtils.resultCompare(actualDir, expectedDir, [], ['workbench/', 'backout/'], logDir, tmpDir)
        if (compareResult != 0) {
            unstable 'Deviations in file comparison'
        }
    }
}
