@Library('TestUtils') _

/**
 * Build and run runtime licensing tests with following setup: 
 * License Missing: cobol runtime (ant + maven) + Compiler extensions (ant + maven)
 * License Present: cobol runtime (ant + maven) + Compiler extensions (ant + maven)
 * License in Jar: cobol runtime (ant + maven) + Compiler extensions (ant + maven)
 *
 * @param mxBuildVersion The maxenso/innowake version to test
 * @param javaVersion The java version the test will run with
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param migrationBuild The build of the TF_Cobol_Java_Migration_License_Validation job to fetch the migrated files from
 */

nodeTF('Docker-host') {

    def svnUtils = new SvnUtils()
    def gitUtils = new GitUtils()
    def dockerUtils = new DockerUtils()
    def mxVersionUtils = new MxVersionUtils()
    def miscUtils = new MiscUtils()
    def projectNameJava = 'license-validation-java'
    def goodLicenseDir = "${pwd()}/${projectNameJava}/licenseGood"
    def licenseDirName = 'license'
    def licenseDir = "${pwd()}/${projectNameJava}/${licenseDirName}"
    def licenseJarName = 'innowake_license.jar'
    def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
    def m2Repo = '/var/m2_repo'
    def workDir = pwd()
    def licenseNames = ['innowake.lic', 'maxenso.lic']
    def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
	
    buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	try{

		licenseNames.each { licenseName ->
    		/*
    		Some times not all locks from previous test are disabled, this can cause java.nio.file.AccessDeniedException when trying to delete dirs.
    		The following tries to avoid this.
    		*/
    		timeout(3) {
    			retry(5) {
    				sleep 10
    				deleteDir()
    			}
    		}
    		docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("-v jenkins-m2-repo:${m2Repo}:rw") {
    			def fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
    			def versionParameters = "-Dinnowake.version=${mxBuildVersion} -Dinnowake.version.full.build=${fullBuild}"
    			buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} migrationBuild=${migrationBuild} fullBuild=${fullBuild}"

    			stage("${licenseName} setup") {
    				gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/license-validation.git", testProjectBranch)
    				gitUtils.getLicenseFile(mxVersion, goodLicenseDir)
    				// Rename license
    				sh returnStatus: true, script: "mv ${goodLicenseDir}/maxenso.lic ${goodLicenseDir}/${licenseName}"
    				sh "mkdir ${licenseDir}"

    				if (migrationBuild.matches('.*StatusBuildSelector.*')) {
    					copyArtifacts([
    					               projectName: 'TF_Cobol_Java_Migration_License_Validation',
    					               filter: '**/*.java',
    					               selector: lastSuccessful(),
    					               parameters: "mxBuildVersion=${mxBuildVersion}",
    					               target: "${projectNameJava}/src-cobol-java/test",
    					               fingerprintArtifacts: true,
    					               flatten: true
    					               ])
    				} else {
    					copyArtifacts([
    					               projectName: 'TF_Cobol_Java_Migration_License_Validation',
    					               filter: '**/*.java',
    					               selector: buildParameter(migrationBuild),
    					               target: "${projectNameJava}/src-cobol-java/test",
    					               fingerprintArtifacts: true,
    					               flatten: true
    					               ])
    				}
    				dir(projectNameJava) {
    					withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
    						sh "$MVN_CMD ${versionParameters} dependency:resolve"
    					}
    				}
    			}

    			dir(projectNameJava) {             
    				stage("${licenseName} license missing") {
    					withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
    						// License-missing: maven
    						runCompile(1, "$MVN_CMD ${versionParameters} clean compile")
    						// After compilation without license we need to compile the source with license, otherwise the following test will not launch
    						runCompile(0, "$MVN_CMD -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} clean compile")
    						runTest(1, "$MVN_CMD ${versionParameters} exec:java")
    					}
    					withAnt(installation: 'Default') {
    						// License-missing: ant
    						runCompile(1, "ant -silent -quiet -Denv.M2_REPO=${m2Repo} ${versionParameters} compile")
    						// After compilation without license we need to compile the source with license, otherwise the following test will not launch
    						runCompile(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} compile")
    						runTest(1, "ant -Denv.M2_REPO=${m2Repo} ${versionParameters} runTest")
    					}
    				}

    				stage("${licenseName} license present") {
    					withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
    						// License-present: maven
    						runCompile(0, "$MVN_CMD -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} compile")
    						runTest(0, "$MVN_CMD -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} exec:java")
    						// Prepare license-present-in-jar
    						sh "jar cf ${workDir}/${licenseJarName} -C ${goodLicenseDir} ${licenseName}"
    						// Install license file to local repo
    						sh "$MVN_CMD install:install-file -Dfile=${workDir}/${licenseJarName} -DgroupId=innowake_license -DartifactId=innowake_license -Dversion=1.0 -Dpackaging=jar"
    						// License-present-in-jar: maven
    						runCompile(0, "$MVN_CMD -o -f pom_withLicenseJar.xml ${versionParameters} compile")
    						runTest(0, "$MVN_CMD -o -f pom_withLicenseJar.xml ${versionParameters} exec:java")
    						// Remove license file from local repo
    						sh "$MVN_CMD dependency:purge-local-repository -DmanualInclude=innowake_license"
    					}
    					withAnt(installation: 'Default') {
    						// License-present: ant
    						runCompile(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} compile")
    						runTest(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} runTest")
    						// License-present-in-jar: ant
    						runCompile(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.jar=${workDir}/${licenseJarName} ${versionParameters} compile")
    						runTest(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.jar=${workDir}/${licenseJarName} ${versionParameters} runTest")
    					}
    				}

    				stage("${licenseName} license invalid") {
    					prepareLicenseFile('regression-tests-enable-all-invalid.lic', 'innowake', licenseDir, licenseName, mxVersion)
    					withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
    						// License-invalid: maven
    						runCompile(1, "$MVN_CMD -Dinnowake.license.location=${licenseDir} ${versionParameters} compile")
    						// After compilation without license we need to compile the source with license, otherwise the following test will not launch
    						runCompile(0, "$MVN_CMD -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} clean compile")
    						runTest(1, "$MVN_CMD ${versionParameters} exec:java")
    					}
    					withAnt(installation: 'Default') {
    						// License-invalid: ant
    						runCompile(1, "ant -silent -quiet -Denv.M2_REPO=${m2Repo} ${versionParameters} compile")
    						// After compilation without license we need to compile the source with license, otherwise the following test will not launch
    						runCompile(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} compile")
    						runTest(1, "ant -Denv.M2_REPO=${m2Repo} ${versionParameters} runTest")
    					}
    				}

    				stage("${licenseName} license expired") {
    					prepareLicenseFile('regression-tests-enable-all-expired.lic', 'innowake', licenseDir, licenseName, mxVersion)
    					withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
    						// License-expired: maven
    						runCompile(1, "$MVN_CMD -Dinnowake.license.location=${licenseDir} ${versionParameters} compile")
    						// After compilation without license we need to compile the source with license, otherwise the following test will not launch
    						runCompile(0, "$MVN_CMD -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} clean compile")
    						runTest(1, "$MVN_CMD ${versionParameters} exec:java")
    					}
    					withAnt(installation: 'Default') {
    						// License-expired: ant
    						runCompile(1, "ant -silent -quiet -Denv.M2_REPO=${m2Repo} ${versionParameters} compile")
    						// After compilation without license we need to compile the source with license, otherwise the following test will not launch
    						runCompile(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} compile")
    						runTest(1, "ant -Denv.M2_REPO=${m2Repo} ${versionParameters} runTest")
    					}
    				}

    				stage("${licenseName} license incorrect runtime version") {
    					when(mxVersion >= '21.4', 'Tests not implemented for versions below 21.4') {  
    						def runTestFor = { nestedFile, nestedFolder, expdRC ->
    							echo '----------------------------------------'
    							echo "License under test is: ${nestedFile}"
    							prepareLicenseFile(nestedFile, nestedFolder, licenseDir, licenseName, mxVersion)
    							withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
    								if (expdRC == 0) {                          
    									// License-correct: maven
    									runCompile(0, "$MVN_CMD -Dinnowake.license.location=${licenseDir} ${versionParameters} clean compile")
    									runTest(0, "$MVN_CMD -Dinnowake.license.location=${licenseDir} ${versionParameters} exec:java")
    									// Prepare license-correct-in-jar
    									sh "jar cf ${workDir}/${licenseJarName} -C ${licenseDir} ${licenseName}"
    									// Install license file to local repo
    									sh "$MVN_CMD -e install:install-file -Dfile=${workDir}/${licenseJarName} -DgroupId=innowake_license -DartifactId=innowake_license -Dversion=1.0 -Dpackaging=jar"
    									// License-correct-in-jar: maven
    									runCompile(0, "$MVN_CMD -o -f pom_withLicenseJar.xml ${versionParameters} clean compile")
    									runTest(0, "$MVN_CMD -o -f pom_withLicenseJar.xml ${versionParameters} exec:java")
    									// Remove license file from local repo
    									sh "$MVN_CMD dependency:purge-local-repository -DmanualInclude=innowake_license"
    								} else {
    									// License-incorrect: maven
    									runCompile(1, "$MVN_CMD -Dinnowake.license.location=${licenseDir} ${versionParameters} clean compile")
    									// After compilation with invalid license we need to compile the source with a working license, otherwise the following test will not launch
    									runCompile(0, "$MVN_CMD -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} clean compile")
    									runTest(1, "$MVN_CMD -Dinnowake.license.location=${licenseDir} ${versionParameters} exec:java")
    									// Prepare license-incorrect-in-jar
    									sh "jar cf ${workDir}/${licenseJarName} -C ${licenseDir} ${licenseName}"
    									// Install license file to local repo
    									sh "$MVN_CMD -e install:install-file -Dfile=${workDir}/${licenseJarName} -DgroupId=innowake_license -DartifactId=innowake_license -Dversion=1.0 -Dpackaging=jar"
    									// License-incorrect-in-jar: maven
    									runCompile(1, "$MVN_CMD -o -f pom_withLicenseJar.xml ${versionParameters} clean compile")
    									// After compilation with invalid license we need to compile the source for in-jar with a working license, otherwise the following test will not launch
    									sh "$MVN_CMD dependency:purge-local-repository -DmanualInclude=innowake_license"
    									sh "jar cf ${workDir}/${licenseJarName} -C ${goodLicenseDir} ${licenseName}"
    									sh "$MVN_CMD -e install:install-file -Dfile=${workDir}/${licenseJarName} -DgroupId=innowake_license -DartifactId=innowake_license -Dversion=1.0 -Dpackaging=jar"
    									runCompile(0, "$MVN_CMD -o -f pom_withLicenseJar.xml ${versionParameters} clean compile")
    									// For test we need to reset the license
    									sh "jar cf ${workDir}/${licenseJarName} -C ${licenseDir} ${licenseName}"
    									sh "$MVN_CMD -e install:install-file -Dfile=${workDir}/${licenseJarName} -DgroupId=innowake_license -DartifactId=innowake_license -Dversion=1.0 -Dpackaging=jar"
    									runTest(1, "$MVN_CMD -o -f pom_withLicenseJar.xml ${versionParameters} exec:java")
    									// Remove license file from local repo
    									sh "$MVN_CMD dependency:purge-local-repository -DmanualInclude=innowake_license"
    								}
                                }
    							//for debugging:use this when with maven was not used
    							//sh 'mkdir target'
    							//sh "jar cf ${workDir}/${licenseJarName} -C ${licenseDir} ${licenseName}"                             
    							withAnt(installation: 'Default') {
    								if (expdRC == 0) {
    									// License-correct: ant
    									runCompile(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${licenseDir} ${versionParameters} compile")
    									runTest(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${licenseDir} ${versionParameters} runTest")
    									// License-correct-in-jar: ant
    									runCompile(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.jar=${workDir}/${licenseJarName} ${versionParameters} compile")
    									runTest(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.jar=${workDir}/${licenseJarName} ${versionParameters} runTest")
    								} else {
    									// License-incorrect: ant
    									runCompile(1, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${licenseDir} ${versionParameters} compile")
    									// After compilation with invalid license we need to compile the source with license, otherwise the following test will not launch
    									runCompile(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} compile")
    									runTest(1, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${licenseDir} ${versionParameters} runTest")
    									// License-incorrect-in-jar: ant
    									runCompile(1, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.jar=${workDir}/${licenseJarName} ${versionParameters} compile")
    									// After compilation with invalid license we need to compile the source with license, otherwise the following test will not launch
    									runCompile(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.jar=${goodLicenseDir} ${versionParameters} compile")
    									runTest(1, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.jar=${workDir}/${licenseJarName} ${versionParameters} runTest")
    								}
    							}
    						}

    						runTestFor('runtime-license-major-version.lic', 'trafo', 0)
    						runTestFor('runtime-license-major-version-in-mee.lic', 'trafo', 0)

    						runTestFor('runtime-license-minor-version.lic', 'trafo', 1)
    						runTestFor('runtime-license-minor-version-in-mee.lic', 'trafo', 1)

    						runTestFor('runtime-license-trunk-version.lic', 'trafo', 0)
    						runTestFor('runtime-license-trunk-version-in-mee.lic', 'trafo', 0)
    					}
    				}

    				stage("${licenseName} license with cobol runtime present") {
    					prepareLicenseFile('regression-tests-runtime-natclipse.lic', 'innowake', licenseDir, licenseName, mxVersion)
    					withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
    						// License-runtime-present: maven
    						runCompile(0, "$MVN_CMD -Dinnowake.license.location=${licenseDir} ${versionParameters} compile")
    						runTest(0, "$MVN_CMD -Dinnowake.license.location=${licenseDir} ${versionParameters} exec:java")
    						// Prepare license-present-in-jar
    						sh "jar cf ${workDir}/${licenseJarName} -C ${licenseDir} ${licenseName}"
    						// Install license file to local repo
    						sh "$MVN_CMD install:install-file -Dfile=${workDir}/${licenseJarName} -DgroupId=innowake_license -DartifactId=innowake_license -Dversion=1.0 -Dpackaging=jar"
    						// License-runtime-present-in-jar: maven
    						runCompile(0, "$MVN_CMD -o -f pom_withLicenseJar.xml ${versionParameters} compile")
    						runTest(0, "$MVN_CMD -o -f pom_withLicenseJar.xml ${versionParameters} exec:java")
    						// Remove license file from local repo
    						sh "$MVN_CMD dependency:purge-local-repository -DmanualInclude=innowake_license"
    					}
    					withAnt(installation: 'Default') {
    						// License-runtime-present: ant
    						runCompile(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${licenseDir} ${versionParameters} compile")
    						runTest(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${licenseDir} ${versionParameters} runTest")
    						// License-runtime-present-in-jar: maven
    						runCompile(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.jar=${workDir}/${licenseJarName} ${versionParameters} compile")
    						runTest(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.jar=${workDir}/${licenseJarName} ${versionParameters} runTest")
    					}
    				}

    				stage("${licenseName} license with cobol runtime missing") {
    					prepareLicenseFile('cobolclipse-mining.lic', 'mining', licenseDir, licenseName, mxVersion)
    					withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
    						// License-incorrect-cobol: maven
    						runCompile(1, "$MVN_CMD -Dinnowake.license.location=${licenseDir} ${versionParameters} compile")
    						// After compilation without license we need to compile the source with license, otherwise the following test will not launch
    						runCompile(0, "$MVN_CMD -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} clean compile")
    						runTest(1, "$MVN_CMD ${versionParameters} exec:java")
    					}
    					withAnt(installation: 'Default') {
    						// License-incorrect-cobol: ant
    						runCompile(1, "ant -silent -quiet -Denv.M2_REPO=${m2Repo} ${versionParameters} compile")
    						// After compilation without license we need to compile the source with license, otherwise the following test will not launch
    						runCompile(0, "ant -Denv.M2_REPO=${m2Repo} -Dinnowake.license.location=${goodLicenseDir} ${versionParameters} compile")
    						runTest(1, "ant -Denv.M2_REPO=${m2Repo} ${versionParameters} runTest")
    					}
    				}
    			}
    		}
    	}
	}catch(ex) {
		miscUtils.errorWithStackTrace(ex)
	} finally {
		stage('finalize') {
			sh returnStatus: true, script: "mv ${workDir}/license-validation-java/target/jacoco.exec ${workDir}/license-validation-java/target/license-validation-java-jacoco.exec"
			archiveArtifacts allowEmptyArchive: true, artifacts: 'license-validation-java/target/license-validation-java-jacoco.exec'
		}
	}
}

/**
 * Clean up all previously pulled license files in folder license and then pulls license file from git, renames the file according to licenseName and assures correct line ending \r\n
 * @param file Original license name in git
 * @param sourceFolder Source (sub) folder in git infrastructure/licenses
 * @param licenseDir Target folder for license file in job
 * @param licenseName License name under test
 * @param gitUtils Helper for git
 */
def prepareLicenseFile(file, sourceFolder, licenseDir, licenseName, mxVersion) {
    def gitUtils = new GitUtils()
    //revert potential changes from previous test cases
    sh returnStatus: true, script: "rm ${licenseDir}/*"
    gitUtils.getSingleFile('infrastructure/licenses', "${sourceFolder}/${file}", mxVersion, licenseDir)
    sh returnStatus: true, script: "mv ${licenseDir}/${file} ${licenseDir}/${licenseName}"
    dir(licenseDir) {
        //assure proper line ending, otherwise errors in runCompile(...)
        def fileContent = readFile(licenseName)
        fileContent = fileContent.split('\n').collect { it.trim() }
        def newFileContent = ''
        fileContent.each {
            newFileContent += it.toString() + '\r\n'
        }
        sh "rm ${licenseName}"
        writeFile encoding: 'utf-8', file: licenseName, text: newFileContent
    }
}

/**
 * Compiles a tests and sets an appropriate error message in case of a failure
 *
 * @param expectedReturnCode The expected return code is used to compare it with the actual one. If the values differ the test is set to unstable
 * @param script The script to run
 */
def runCompile(expectedReturnCode, script) {
    def errorMessage = 'Compilation was expected to fail, but succeeded.'
    if (expectedReturnCode == 0) {
        errorMessage = 'Compilation was expected to succeed, but failed.'
    }
    run(expectedReturnCode, script, errorMessage)
}

/**
 * Runs a tests and sets an appropriate error message in case of a failure
 *
 * @param expectedReturnCode The expected return code is used to compare it with the actual one. If the values differ the test is set to unstable
 * @param script The script to run
 */
def runTest(expectedReturnCode, script) {
    def errorMessage = 'License validation was expected to fail, but succeeded.'
    if (expectedReturnCode == 0) {
        errorMessage = 'License validation was expected to succeed, but failed.'
    }
    run(expectedReturnCode, script, errorMessage)
}

/**
 * Runs a script (compile or test) and sets an appropriate error message in case of a failure
 *
 * @param expectedReturnCode The expected return code is used to compare it with the actual one. If the values differ the test is set to unstable
 * @param script The script to run
 * @param errorMessage The messsage that is shown in case of a failure
 */
def run(expectedReturnCode, script, errorMessage) {
    def rc = sh returnStatus: true, script: script
    echo "----------------------"
    echo "Executed script = ${script.toString()}\nActual return code = ${rc.toString()}\nExpected return code = ${expectedReturnCode.toString()}"
    if (rc != expectedReturnCode) {
        unstable errorMessage
    }
    echo "----------------------"
}