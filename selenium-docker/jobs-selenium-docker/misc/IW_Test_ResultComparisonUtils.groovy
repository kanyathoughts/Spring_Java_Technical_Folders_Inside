def lib = library("TestUtils@${branchName}")

/**
 * Tests job for ResultComparisonUtils methods.
 * Checks out several sets of expected files, actual files and regular expressions, and applies the result comparison on each of them.
 *
 * @param testsSelector a regular expression to select test cases, that shall be executed.
 *                       Default: .*  (matches anything)
 * @param branch The branch from which the test files should be pulled
 * @param branchName Name of the branch where changes are stored, default: master
 */
 timestamps {
    def miscUtils = lib.MiscUtils.new()
    def gitUtils = lib.GitUtils.new()
    def dockerUtils = lib.DockerUtils.new()
    def resultComparisonUtils = lib.ResultComparisonUtils.new()
    node('Docker-host') {
        def workDir = pwd()
        
        buildName "#${env.BUILD_ID} - ${branchName}"

        stage('clean up') {
            deleteDir()
        }
        docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside {

            stage('initialize') {
                gitUtils.checkoutGitProject('.', 'https://gitlab.consulting.sltc.com/appmod/qef/infrastructure/transformation/test-result-comparision.git', branch)
                testFolders = sh(script: 'ls', returnStdout: true).split('\n').collect { it.trim() }.findAll { ! it.contains('.') }
                def pathTestFor = 'absolute-path'
                def logFile = 'content-compare-result.log'

                //prepare absolut paths
                testFolders.each { testFolder ->
                    dir(testFolder) {
                        logContent = readFile("expectedLog/${pathTestFor}/${logFile}")
                        if (logContent.isEmpty()) {
                            echo "log file for test ${testFolder} was empty!"
                        } else {
                            logContent = logContent.replaceAll('/var/lib/jenkins/workspace/IW_Test_ResultComparisonUtils', workDir)
                            sh "rm -f ${logFile}"
                            dir("expectedLog/${pathTestFor}") {
                                writeFile encoding: 'utf-8', file: logFile, text: logContent
                            }
                        }
                    }
                }
                sh 'rm -rf *@tmp'
            }
            /* Tests with relative paths */
            findTestDirs = sh returnStdout: true, script: 'find . -mindepth 1 -maxdepth 1 -type d | sort'
            testDirs = findTestDirs.split('\n')
            for (int i = 0; i < testDirs.length; i++) {
                processTestDir(testDirs[i], testsSelector, resultComparisonUtils, 'relative-path')
            }

            echo "pwd: ${pwd()} ${testsSelector}"
            /* Tests with absolute paths */
            def findTestDirs = sh returnStdout: true, script: "find ${pwd()} -mindepth 1 -maxdepth 1 -type d | sort"
            def testDirs = findTestDirs.split('\n')
            echo "testDir: ${testDirs}\n----------------------"
            for (int i = 0; i < testDirs.length; i++) {
                processTestDir(testDirs[i], testsSelector, resultComparisonUtils, 'absolute-path')
            }
        }
    }
}   

def processTestDir(testDir, testsSelector, resultComparisonUtils, logSubDir) {
    sh "rm -fr --interactive=never ${testDir}/log/${logSubDir}* ${testDir}/tmp/${logSubDir}*"
    def configFileExists = fileExists "${testDir}/testConfig.json"

    if (! (testDir ==~ testsSelector)) {
        echo "${testDir} not executed, does not match ${testsSelector}"
    } else if (! configFileExists) {
        echo "${testDir} not executed, no test configuration found"
    } else {
        stage("${testDir}") {
            echo "Testing ${testDir}"
            def testConfig = readJSON file: "${testDir}/testConfig.json"
            echo "testConfig = ${testConfig}"
            def sortFiles = testConfig.sortFiles as Boolean
            def expectedRC = testConfig.expectedReturnCode as Integer
            def regexList = testConfig.regexList as List
            def excludedFiles = testConfig.excludedFiles as List
            def testSuccess = true
            def resultComparisonLogDir = "${testDir}/log/${logSubDir}"
            def resultComparisonTmpDir = "${testDir}/tmp/${logSubDir}"
            def actualFilesDir = "${testDir}/actualFiles"
            def expectedFilesDir = "${testDir}/expectedFiles"

            sh "mkdir -p ${resultComparisonLogDir}"
            sh "mkdir -p ${resultComparisonTmpDir}"

            def actualReturnCode = resultComparisonUtils.resultCompare(actualFilesDir, expectedFilesDir, regexList, excludedFiles, resultComparisonLogDir, resultComparisonTmpDir, sortFiles)
            if ((actualReturnCode == 0) != (expectedRC == 0)) {
                unstable "Test ${testDir} (${logSubDir}) FAILED!   expectedReturnCode=${expectedRC}, actualReturnCode=${actualReturnCode}"
                testSuccess = false
            }
            def actualLog = readFile "${testDir}/log/${logSubDir}/content-compare-result.log"
            def expectedLog = readFile "${testDir}/expectedLog/${logSubDir}/content-compare-result.log"
            if (actualLog != expectedLog) {
                unstable "Test ${testDir} (${logSubDir}) FAILED!   actual/expected logfiles deviate"
                echo "actualLog: ${actualLog}\nexpectedLog: ${expectedLog}"
                testSuccess = false
            }
            if (testSuccess) {
                echo "Test ${testDir} (${logSubDir}) SUCCESS"
            }
        }
    }
}