@Library('TestUtils') _

/**
 * Build and run runtime licensing tests with following setup: 
 * License Missing: cobol runtime (msbuild)
 * License Present: cobol runtime (msbuild)
 *
 * @param mxBuildVersion The maxenso build under test
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param dotnetFrameworkVersion The .Net Framework version the build and the test will run with
 * @param migrationBuild The build of the TF_Cobol_Csharp_Migration_License_Validation job to fetch the migrated files from
 */

nodeTF('OS-Windows && Tool-msbuild') {

    timestamps {

        def mxVersionUtils = new MxVersionUtils()
        def svnUtils = new SvnUtils()
        def gitUtils = new GitUtils()
        def projectNameCsharp = 'license-validation-csharp'
        def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
        def licenseNames = ['innowake.lic', 'maxenso.lic']

        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)

        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "testProjectBranch=${testProjectBranch} dotnetFrameworkVersion=${dotnetFrameworkVersion} migrationBuild=${migrationBuild}"

        licenseNames.each { licenseName ->
            stage("${licenseName} setup") {
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

                gitUtils.checkoutGitProject(pwd(), "${gitUtils.getGitUrlQef()}/innowake-test-projects/transformation-test-projects/license-validation.git", testProjectBranch)
				
				if (migrationBuild.matches('.*StatusBuildSelector.*')) {
					copyArtifacts([
						projectName: 'TF_Cobol_Csharp_Migration_License_Validation',
						filter: '**/*.cs',
						selector: lastSuccessful(),
						parameters: "mxBuildVersion=${mxBuildVersion}",
						target: "${projectNameCsharp}/src-cobol-csharp/test",
						fingerprintArtifacts: true,
						flatten: true
					])
				} else {
					copyArtifacts([
						projectName: 'TF_Cobol_Csharp_Migration_License_Validation',
						filter: '**/*.cs',
						selector: buildParameter(migrationBuild),
						target: "${projectNameCsharp}/src-cobol-csharp/test",
						fingerprintArtifacts: true,
						flatten: true
					])
				}
                dir(projectNameCsharp) {
                    configFileProvider([configFile(fileId: 'NuGet.config', targetLocation: '.', variable: 'pathToNugetConfigFile')]) {
                        withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-Nexus', passwordVariable: 'nexusPw', usernameVariable: 'nexusUser')]) {
                            bat "nuget.exe sources update -name Nexus -username ${nexusUser} -password ${nexusPw}"
                        }
                        bat "nuget.exe restore license-validation-csharp.sln"
                        def output = bat returnStdout: true, script: "nuget.exe update license-validation-csharp.sln -Version ${mxBuildVersion} -Id InnoWake.NET.Dependencies-dist"
                        echo output
                        if (output.contains('is not found')) {
                            error "innowake artifacts with version ${mxBuildVersion} are not available"
                        }
                        bat "msbuild.exe license-validation-csharp.sln -t:Clean,Build -p:TargetFrameworkVersion=${dotnetFrameworkVersion}"
                    }
                }
            }

            stage("${licenseName} license missing") {
                dir(projectNameCsharp) {
                    /* No innowake.lic file has been provided, so this call is expected to fail. */
                    def rc = bat returnStatus: true, script: 'bin\\Debug\\license-validation-csharp.exe'
                    if (rc == 0) {
                        unstable 'License validation was expected to fail, but succeeded.'
                    }
                }
            }

            stage("${licenseName} license present") {
                dir(projectNameCsharp) {
                    //revert potential changes from previous test cases
                    bat returnStatus: true, script: "del /f ${licenseName}"
                    gitUtils.getLicenseFile(mxVersion, pwd())
                    // Rename license
                    bat "ren maxenso.lic ${licenseName}"
                    /* A innowake.lic file has been provided now, so this call is expected to succeed. */
                    def rc = bat returnStatus: true, script: 'bin\\Debug\\license-validation-csharp.exe'
                    if (rc != 0) {
                        unstable 'License validation was expected to succeed, but failed.'
                    }
                }
            }

            stage("${licenseName} license invalid") {
                def file = 'regression-tests-enable-all-invalid.lic'
                dir(projectNameCsharp) {
                    //revert potential changes from previous test cases
                    bat returnStatus: true, script: "del /f ${licenseName}"
                    gitUtils.getSingleFileOnWindows('infrastructure/licenses', "innowake/${file}", mxVersion, pwd())

                    bat "ren ${file} ${licenseName}"
                    /* An invalid license key has been provided now, so this call is expected to fail. */
                    def rc = bat returnStatus: true, script: 'bin\\Debug\\license-validation-csharp.exe'
                    if (rc == 0) {
                        unstable "License validation for ${licenseName} with invalid license was expected to fail, but succeeded."
                    }
                }
            }

            stage("${licenseName} license expired") {
                def file = 'regression-tests-enable-all-expired.lic'
                dir(projectNameCsharp) {
                    //revert potential changes from previous test cases
                    bat returnStatus: true, script: "del /f ${licenseName}"
                    gitUtils.getSingleFileOnWindows('infrastructure/licenses', "innowake/${file}", mxVersion, pwd())

                    bat "ren ${file} ${licenseName}"
                    /* An expired license key has been provided now, so this call is expected to fail. */
                    def rc = bat returnStatus: true, script: 'bin\\Debug\\license-validation-csharp.exe'
                    if (rc == 0) {
                        unstable "License validation for ${licenseName} with expired license was expected to fail, but succeeded."
                    }
                }
            }

            stage("${licenseName} license incorrect runtime version") {
                //note that every alpha has mxVersion 99.9 and therfore will always run in this test
                when(mxVersion >= '21.4', 'Tests not implemented for versions below 21.4') {               
                    def prepareTestFor = { nestedFile, nestedFolder ->
                        dir(projectNameCsharp) {
                            //revert potential changes from previous test cases
                            bat returnStatus: true, script: "del /f ${licenseName}"
                            gitUtils.getSingleFileOnWindows('infrastructure/licenses', "${nestedFolder}/${nestedFile}", mxVersion, pwd())

                            bat "ren ${nestedFile} ${licenseName}"
                            /* A mining license key with wrong version has been provided now, so this call is expected to fail. */
                            def rc = bat returnStatus: true, script: 'bin\\Debug\\license-validation-csharp.exe'
                            return rc
                        }
                    }
                    def file = 'runtime-license-major-version.lic'
                    if (prepareTestFor(file, 'trafo') != 0) {
                        unstable "License validation for ${file} as ${licenseName} was expected to succeed, but failed"
                    }
                    file = 'runtime-license-major-version-in-mee.lic'
                    if (prepareTestFor(file, 'trafo') != 0) {
                        unstable "License validation for ${file} as ${licenseName} was expected to succeed, but failed"
                    }
                    file = 'runtime-license-minor-version.lic'
                    if (prepareTestFor(file, 'trafo') == 0) {
                        unstable "License validation for ${file} as ${licenseName} was expected to fail, but succeeded."
                    }
                    file = 'runtime-license-minor-version-in-mee.lic'
                    if (prepareTestFor(file, 'trafo') == 0) {
                        unstable "License validation for ${file} as ${licenseName} was expected to fail, but succeeded."
                    }
                    file = 'runtime-license-trunk-version.lic'
                    if (prepareTestFor(file, 'trafo') != 0) {
                        unstable "License validation for ${file} as ${licenseName} was expected to succeed, but failed"
                    }
                    file = 'runtime-license-trunk-version-in-mee.lic'
                    if (prepareTestFor(file, 'trafo') != 0) {
                        unstable "License validation for ${file} as ${licenseName} was expected to succeed, but failed"
                    }
                }
            }

            stage("${licenseName} license with cobol runtime present") {
                def file = 'regression-tests-runtime-natclipse.lic'
                dir(projectNameCsharp) {
                    //revert potential changes from previous test cases
                    bat returnStatus: true, script: "del /f ${licenseName}"
                    gitUtils.getSingleFileOnWindows('infrastructure/licenses', "innowake/${file}", mxVersion, pwd())

                    bat "ren ${file} ${licenseName}"
                    /* A license key with runtime-cobol = enabled has been provided now, so this call is expected to succeed. */
                    def rc = bat returnStatus: true, script: 'bin\\Debug\\license-validation-csharp.exe'
                    if (rc != 0) {
                        unstable "License validation for ${licenseName} with property runtime-cobol = enabled was expected to succeed, but failed and returned: ${rc}"
                    }
                }
            }

            stage("${licenseName} license with cobol runtime missing") {
                def file = 'cobolclipse-mining.lic'
                dir(projectNameCsharp) {
                    //revert potential changes from previous test cases
                    bat returnStatus: true, script: "del /f ${licenseName}"
                    gitUtils.getSingleFileOnWindows('infrastructure/licenses', "mining/${file}", mxVersion, pwd())

                    bat "ren ${file} ${licenseName}"
                    /* A license key without runtime-cobol = enabled has been provided now, so this call is expected to fail. */
                    def rc = bat returnStatus: true, script: 'bin\\Debug\\license-validation-csharp.exe'
                    if (rc == 0) {
                        unstable "License validation for ${licenseName} with missing property runtime-cobol = enabled was expected to fail, but succeeded."
                    }
                }
            }
        }
    }
}