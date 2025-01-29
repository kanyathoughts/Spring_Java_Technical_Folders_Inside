def lib = library("TestUtils@${branchName}")

/**
* Test job for MiscUtils methods.
* NOTICE: This jobs needs several script approvals in jenkins.
*
* @param branchName Name of the branch where changes are stored, default: master
*/
timestamps {
    def dockerUtils = lib.DockerUtils.new()
    def gitUtils = lib.GitUtils.new()
    def miscUtils = lib.MiscUtils.new()
    
    node('OS-Linux') {
        def util2test = 'MiscUtils.groovy'
        def workDir = pwd()
        def utilMethods = []

        buildName "#${env.BUILD_ID} - ${branchName}"

        deleteDir()

        stage('Test getHostname') {
            // ??
        }

        stage('Test readPropertyFile') {
            // ??
            writeFile encoding: 'utf-8', file: 'bulid.properties', text: getBuildPropertiesContent()
        
            def expectedMxBuildVersion = '21.0.0'
            def expectedWorkDir = pwd()
            //readPropertyFile does property interpolation, how to test ? need qualifed build.properties file
            def buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': expectedWorkDir, 'mxBuildVersion': expectedMxBuildVersion])
            
            check(['workDir': expectedWorkDir, 'mxBuildVersion': expectedMxBuildVersion], buildProperties)
        }

        stage('Test getFileSize') {
            def testFilePrefix = '_getFileSize'
            sh "echo -n > emptyFile${testFilePrefix}"
            sh "echo 'Hello World' > simpleFile${testFilePrefix}"
            sh "echo 'Hello\n World ßäöü\\/§\rEOF' > complexFile${testFilePrefix}"
            
            check(0, miscUtils.getFileSize(pwd() + "/emptyFile${testFilePrefix}"))
            check(12, miscUtils.getFileSize(pwd() + "/simpleFile${testFilePrefix}"))
            check(25, miscUtils.getFileSize(pwd() + "/complexFile${testFilePrefix}"))
            
            checkExceptionThrown('java.nio.file.NoSuchFileException', miscUtils.&getFileSize, pwd() + "/notExistentFile${testFilePrefix}")

            //revert changes from test
            sh "rm -rf *${testFilePrefix}"
        }

        stage('Test isTextInFiles') {
            def testFilePrefix = '_isTextInFiles'
            //single file containing Hello World
            sh "echo 'Hello World' > file1${testFilePrefix}"
            check(true, miscUtils.isTextInFiles('Hello World', "file1${testFilePrefix}", '.'))
            //several files containing Hello World
            sh "echo 'Some very long text. Where to find Hello World ? Maybe in the middle!' > file2${testFilePrefix}"
            check(true, miscUtils.isTextInFiles('Hello World', "file*", '.'))
            sh "echo 'What we want is at the end Hello World' > file3${testFilePrefix}"
            check(true, miscUtils.isTextInFiles('Hello World', 'file*', '.'))
            //several files containing Hello World and one file does not
            sh "echo 'Goodbye World' > file4${testFilePrefix}"
            check(true, miscUtils.isTextInFiles('Hello World', 'file*', '.'))
            //single file does not contain Hello World
            check(false, miscUtils.isTextInFiles('Hello World', "file4${testFilePrefix}", '.'))

            check(false, miscUtils.isTextInFiles('NOT Existing Text in files', 'file', '.'))
            check(false, miscUtils.isTextInFiles('Hello World', 'NotExistentFilePattern', '.'))

            //revert changes from test
            sh "rm -rf *${testFilePrefix}"
        }

        stage('Test directoryExists') {
            def testFilePrefix = '_directoryExists'
            sh "mkdir folder1${testFilePrefix}"

            check(true, miscUtils.directoryExists("folder1${testFilePrefix}"))
            check(true, miscUtils.directoryExists("./folder1${testFilePrefix}"))
            check(true, miscUtils.directoryExists("${pwd()}/folder1${testFilePrefix}"))

            check(false, miscUtils.directoryExists("noExistentFolder${testFilePrefix}"))

            //revert changes from test
            sh "rm -rf *${testFilePrefix}"
        }

        stage('Test fileExists') {
            def testFilePrefix = '_fileExists'
            sh "echo -n > file1${testFilePrefix}"

            check(true, miscUtils.fileExists("file1${testFilePrefix}"))
            check(true, miscUtils.fileExists("./file1${testFilePrefix}"))
            check(true, miscUtils.fileExists("${pwd()}/file1${testFilePrefix}"))

            check(false, miscUtils.fileExists("noExistentFile${testFilePrefix}"))

            //revert changes from test
            sh "rm -rf *${testFilePrefix}"
        }

        stage('Test getInstalledJavaVersion') {
            docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                check('1.8', miscUtils.getInstalledJavaVersion())
            }
            docker.image(dockerUtils.pullJenkinsEnvironmentImage('java11')).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                check('11.0', miscUtils.getInstalledJavaVersion())
            }
            docker.image(dockerUtils.pullJenkinsEnvironmentImage('java17')).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                check('17.0', miscUtils.getInstalledJavaVersion())
            }
        }

        stage('Test getDockerGroupID') {
            //187 on DELinux1
            check('187', miscUtils.getDockerGroupID().trim())
        }

        stage('Test getGroupID') {
            //399838 on DELinux1
            check('399838', miscUtils.getGroupID().trim())
        }

        stage('Test getUserID') {
            //3137719 on DELinux1
            check('3137719', miscUtils.getUserID().trim())
        }

        stage('Test evaluateBuildResult') {
            //??
        }

        stage('Test evaluateBuildResultWithExit') {
            //??
        }

        stage('Test getIpByNodeName') {
            def expectedIps = ['DELinux1': '10.195.131.106', 'DELinux2': '10.195.131.101', 'DELinux3': '10.195.131.106', 'DELinux4': '10.195.131.100', 'USLeanft1': '10.241.173.121', 
                'USLeanft2': '10.241.205.43', 'USLeanft3': '10.241.205.41', 'USLeanft4': '10.241.205.42', 'USLeanft5': '10.241.173.121', 'USLeanft6': '10.241.205.43', 
                'USLeanft6-DM': '10.241.173.101', 'USLeanft6-DM-dev': '10.241.173.101', 'USLeanft7': '10.241.205.41', 'USLeanft7-DM': '10.241.173.119', 
                'USLeanft7-DM-dev': '10.241.173.119', 'USLeanft8': '10.241.205.42', 'USLeanft8-Trafo': '10.241.173.118', 'USLeanft8-Trafo-dev': '10.241.173.118', 
                'USLinux1': '10.241.173.126', 'USLinux2': '10.241.173.106', 'USLinux3': '10.25.35.211', 'USLinux4-perf': '10.241.173.122', 'USLinux5-DM': '10.241.173.116', 
                'USLinux6-DM': '10.241.173.120', 'USLinux7-DM': '10.241.173.104', 'USLinux8-DM': '10.241.173.102', 'USLinux9-Trafo': '10.241.173.103', 'USWindows1': '10.241.173.121', 
                'USWindows2': '10.241.205.43', 'USWindows3': '10.241.205.41', 'USWindows4': '10.241.205.42', 'USWindows5-perf': '10.241.173.107', 'USWindows8-Trafo': '10.241.173.118',
                'USLinux10-Mod': '10.241.173.124', 'USLinux2-AWS': '10.241.173.106']
            
            expectedIps.each { nodeName, ip -> check(ip, miscUtils.getIpByNodeName(nodeName)) }
            
            checkExceptionThrown('This node name could not be found', miscUtils.&getIpByNodeName, 'notANodeName')
        }

        stage('Test getHostNameByNodeName') {
            def expectedHostNames = ['DELinux1': 'qef-linux1-de.deloitte.com', 'DELinux2': 'qef-linux2-de.deloitte.com', 'DELinux3': 'qef-linux1-de.deloitte.com', 'DELinux4': 'qef-linux2-de.deloitte.com', 'USLeanft1': 'qef-windows1-us.deloitte.com', 
                        'USLeanft2': 'qef-windows2-us.deloitte.com', 'USLeanft3': 'qef-windows3-us.deloitte.com', 'USLeanft4': 'qef-windows4-us.deloitte.com', 'USLeanft5': 'qef-windows1-us.deloitte.com', 
                        'USLeanft6': 'qef-windows2-us.deloitte.com', 'USLeanft6-DM': 'qef-windows6-us-dm.deloitte.com', 'USLeanft6-DM-dev': 'qef-windows6-us-dm.deloitte.com', 'USLeanft7': 'qef-windows3-us.deloitte.com', 
                        'USLeanft7-DM': 'qef-windows7-us-dm.deloitte.com', 'USLeanft7-DM-dev': 'qef-windows7-us-dm.deloitte.com', 'USLeanft8': 'qef-windows4-us.deloitte.com', 'USLeanft8-Trafo': 'qef-windows8-us-trafo.deloitte.com', 
                        'USLeanft8-Trafo-dev': 'qef-windows8-us-trafo.deloitte.com', 'USLinux1': 'qef-linux1-us.deloitte.com', 'USLinux2': 'qef-linux2-us.deloitte.com', 'USLinux3': 'qef-linux3.deloitte.com', 
                        'USLinux4-perf': 'qef-linux4-us-perf.deloitte.com', 'USLinux5-DM': 'qef-linux5-us-dm.deloitte.com', 'USLinux6-DM': 'qef-linux6-us-dm.deloitte.com', 'USLinux7-DM': 'qef-linux7-us-dm.deloitte.com', 
                        'USLinux8-DM': 'qef-linux8-us-dm.deloitte.com', 'USLinux9-Trafo': 'qef-linux9-us-trafo.deloitte.com', 'USWindows1': 'qef-windows1-us.deloitte.com', 'USWindows2': 'qef-windows2-us.deloitte.com', 
                        'USWindows3': 'qef-windows3-us.deloitte.com', 'USWindows4': 'qef-windows4-us.deloitte.com', 'USWindows5-perf': 'qef-windows5-us-perf.deloitte.com', 'USWindows8-Trafo': 'qef-windows8-us-trafo.deloitte.com', 'USLinux2-AWS': 'qef-linux2-us.deloitte.com']
    
            expectedHostNames.each { nodeName, ip -> check(ip, miscUtils.getHostNameByNodeName(nodeName)) }
            
            checkExceptionThrown('This node name could not be found', miscUtils.&getHostNameByNodeName, 'notANodeName')
        }

        stage('Test notifyServerWatchdogsAboutFailure') {
            // ??
        }

        stage('Test notifyFailure') {
            // ??
        }

        stage('Test getBaseConfluenceURL') {
            check('https://amiconfluence.deloitte.com', miscUtils.getBaseConfluenceURL())
        }

        stage('Test isPerfEnvironment') {
            def expectedValues = [
                'DELinux1': '', 'DELinux2': '', 'DELinux3': '', 'DELinux4': '',
                'USLeanft1': '', 'USLeanft2': '', 'USLeanft3': '', 'USLeanft4': '', 'USLeanft5': '', 'USLeanft6': '', 'USLeanft7': '', 'USLeanft8': '',
                'USLeanft6-DM': '', 'USLeanft6-DM-dev': '', 'USLeanft7-DM': '', 'USLeanft7-DM-dev': '',
                'USLeanft8-Trafo': '', 'USLeanft8-Trafo-dev': '', 
                'USLinux1': '', 'USLinux2': '', 'USLinux3': '', 
                'USLinux4-perf': '-perf', 'USLinux5-DM': '', 'USLinux6-DM': '', 'USLinux7-DM': '', 
                'USLinux8-DM': '', 'USLinux9-Trafo': '', 
                'USWindows1': '', 'USWindows2': '', 'USWindows3': '', 'USWindows4': '', 
                'USWindows5-perf': '-perf', 'USWindows8-Trafo': '', 'USLinux2-AWS': '']
            expectedValues.each { nodeName, expected -> check(expected, miscUtils.isPerfEnvironment(nodeName)) }
        }

        stage('Test startTestSuite') {
            //??
        }

        stage('Test getDefaultJavaVersion') {
            check('java11', miscUtils.getDefaultJavaVersion())
        }

        stage('Test waitForMessageInLogFile') {
            //??
        }

        stage('Test waitForMessageInDockerLogs') {
            //??
        }

        stage('Test runScriptWithRetry') {
            //??
        }

        stage('Test errorWithStackTrace') {
            checkExceptionThrown('I got thrown!', miscUtils.&errorWithStackTrace, new IOException('I got thrown!'))
        }

        stage('Test getCompilerLevel') {
            check(1.8, miscUtils.getCompilerLevel('java8'))
            check(11,Integer.parseInt(miscUtils.getCompilerLevel('java11')))
            check(11, miscUtils.getCompilerLevel('java17'))
        }

        stage('Test isJobAlreadyRunningWithVersion') {
            //??
        }
    }
}

/*
* Test if a method call throws a certain Exception.
* @param expected  the name of the exception we expect to get thrown (e.g. 'java.nio.file.NoSuchFileException') or the error message
*  Type: String
* @param method The method we want to execute.
*               NOTICE: if you want to pass a class method, e.g. miscUtils.getHostname(), it needs to be written like this: miscUtils.&getHostname 
*  Type:  Closure<?>  
* @param ...args comma separated parameters for method, e.g, 'hello',4.2, True
* @return Job turns unstable if an unexpected exception or no exception at all was thrown
*/
def checkExceptionThrown(expected, Closure<?> method, ...args) {
    try {
        method(args)
    } catch(error) {
        if (expected.toLowerCase() == 'java.lang.exception') {
            echo "INFO: Catch for Exception could be too generic and might cause false-positiv test cases. Please try using a more specific exception"
        }
        if ( !error.toString().toLowerCase().contains(expected.toLowerCase())) {
            unstable "Exception '${expected}' not part of Error '${error}'"
        }
        return
    }
    unstable "No Exception was thrown"
}

/* We want to use the groovy power assertion instead of the 'normal' assert. 
* This is desireable because its output when failing the assertion is more meaningful than the normal one.
* Therefore every use of an assertion needs to be encapuslated by a @NonCPS method like this.
* !!This check does not terminate the pipeline process, it just turns stages into unstable with proper message!!
*/

@NonCPS
def check(expected, actual, boolean makeItUnstable = true) {
    checkHelp({ assert expected == actual }, makeItUnstable)
}

@NonCPS
def checkHelp(Closure<?> assertion, boolean makeItUnstable) {
    try {
        assertion?.call()
    } catch (AssertionError aE) {
        if (makeItUnstable) {
            unstable "${aE}"
        } else {
            echo "${aE}"
        }
    }
}

// ------------might delete later
def getCompileCMDFor(name) {
    def out = 'groovyc '
    compilationOrderForFile(name).each {
        out += "/home/groovy/scripts/${it}.groovy "
    }
    return out.trim()
}

def compilationOrderForFile(name) {
    name = name.minus('.groovy')
    switch (name) {
        case 'ChangeLogUtils':
            return ['ChangeLogUtils']
        case 'DockerUtils':
            return compilationOrderForFile('MxVersionUtils') + ['DockerUtils']
        case 'GitUtils':
            return ['GitUtils']
        case 'MiscUtils':
            return compilationOrderForFile('MxVersionUtils') + ['MiscUtils']
        case 'MxVersionUtils':
            return ['MxVersionUtils']
        case 'PerformanceUtils':
            return compilationOrderForFile('MiscUtils') + ['PerformanceUtils']
        case 'ProductDeliveryUtils':
            return ['ProductDeliveryUtils']
        case 'ResultComparisonUtils':
            return ['ResultComparisonUtils']
        case 'SharepointUtils':
            return ['SharepointUtils']
        case 'SvnUtils':
            return ['SvnUtils']
        default:
            unstable "${name} compilation order is unknown! Assuming no order"
            return [name]
    }
}

def getBuildPropertiesContent() {
    def contentAsBase64 = 
    'IyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjCiMgUHJvcGVydGllcywgdGhhdCBoYXZlIHRvIGJlIHNldCBiZWZvcmUsIGkuZS4gcGFzc2VkIGluIGJ5IHRoZSBjYWxsZXI6CiMgLSB3b3JrRGlyCiMgICBUaGUgd29ya2luZyBkaXJlY3Rvcnkgd2hlcmUgYWxsIGNoZWNrb3V0LCBidWlsZCBhbmQgdGVzdCBpcyBkb25lCiMgLSBteEJ1aWxkVmVyc2lvbgojICAgVGhlIGV4YWN0IG1heGVuc28gYnVpbGQgdG8gd29yayB3aXRoLiBTb21ldGhpbmcgbGlrZSAxNi4wLjAuMDUuCgojIFRlbXAgRGlyIC0gd2lsbCBiZSBkZWxldGVkIGFmdGVyIHRoZSB0ZXN0I' +
    'HJ1bgp0bXBEaXI9JHt3b3JrRGlyfS90bXAKCiMgV2hlcmUgbWF4ZW5zbyBqYXJzIGFyZSBsb2NhdGVkCm14SmFyc0Rpcj0vZGF0YS9teEphcnMvJHtteEJ1aWxkVmVyc2lvbn0KCiMgVGhlIHB1cmUgRWNsaXBzZSBidW5kbGUuCiMgVGhpcyBpcyBtYXhlbnNvIHZlcnNpb24gc3BlY2lmaWMhIFNvbWUgbWF4ZW5zbyB2ZXJzaW9ucyByZXF1aXJlIGFuIG9sZGVyIG9yIG5ld2VyIEVjbGlwc2UuCnJlcXVpcmVkRWNsaXBzZT1lY2xpcHNlLVNESy00LjExLWxpbnV4LWd0ay14ODZfNjQKZWNsaXBzZVppcD0ke3dvcmtEaXJ9LyR7cmVxdWlyZWRFY2xpcHNlfS50YX' +
    'IuZ3oKCiMgRWNsaXBzZSB3aWxsIGJlIGV4dHJhY3RlZCBhbmQgaW5zdGFsbGVkIGludG8gdGhpcyBkaXJlY3RvcnkuCiMgVGhlIGFudCBidWlsZCwgdGhhdCBidWlsZHMgdGhlIG1heGVuc28gRWNsaXBzZSwgcmVxdWlyZXMgZWNsaXBzZVVucGFja0Rpci4KZWNsaXBzZVVucGFja0Rpcj0ke3dvcmtEaXJ9CmVjbGlwc2VEaXI9JHtlY2xpcHNlVW5wYWNrRGlyfS9lY2xpcHNlCgojIE1heGVuc28gbGljZW5zZSBhbmQgaW5zdGFsbCB0b29sIGNvbmZpZwpsaWNlbnNlRGlyPSR7d29ya0Rpcn0KbWF4ZW5zb0xpYz0ke2xpY2Vuc2VEaXJ9L21heGVuc28ubGljCgo' +
    'jIEluc3RhbGwgdG9vbCBzdHVmZgpteFJlcXVpcmVkUGx1Z2luc1ppcD0ke214SmFyc0Rpcn0vaW5ub3dha2UtcmVxdWlyZWQtcGx1Z2lucy56aXAKbXhBbGxQbHVnaW5zWmlwPSR7bXhKYXJzRGlyfS9pbm5vd2FrZS1hbGwtcGx1Z2lucy56aXAKbXhBbGxQbHVnaW5zRGlyPSR7dG1wRGlyfS9idW5kbGVzCmluc3RhbGxUb29sU291cmNlUGF0aD0ke214SmFyc0Rpcn0vaW5zdGFsbC10b29sCmluc3RhbGxUb29sUGF0aD0ke3RtcERpcn0vaW5zdGFsbC10b29sCmluc3RhbGxDb25maWdTb3VyY2U9JHt3b3JrRGlyfS9pbnN0YWxsLWNvbmZpZ3VyYXRpb24uaW5p' + 
    'Cmluc3RhbGxDb25maWdUYXJnZXQ9JHtpbnN0YWxsVG9vbFBhdGh9L2luc3RhbGwtY29uZmlndXJhdGlvbi5pbmkKaW5zdGFsbFRvb2xKYXI9JHtpbnN0YWxsVG9vbFBhdGh9L2luc3RhbGwtbGliL2xpYi5qYXIKCiMgVGhlIEVjbGlwc2UgYW50IHRhc2tzIGZyb20gaW5ub3dha2UuCml3RWNsaXBzZUFudHRhc2tKYXI9JHt3b3JrRGlyfS9pbm5vd2FrZS1lY2xpcHNlLWFudHRhc2suamFyCgojIFRoZSBFY2xpcHNlIGluaS1maWxlCmVjbGlwc2VJbmlGaWxlPSR7ZWNsaXBzZURpcn0vZWNsaXBzZS5pbmkKCiMgVGhlIGVjbGlwc2Ugd29ya3NwYWNlIHdpdGggd' +
    'GhlIHByb2plY3QgY29udGVudCBpcyBsb2NhdGVkIGhlcmUuCmVjbGlwc2VXb3Jrc3BhY2VEaXI9JHt3b3JrRGlyfS9lY2xpcHNlV29ya3NwYWNlCgojIFRoZSBmcmVlbWFya2VyIHRlbXBsYXRlcyBmb3IgdGhlIHNjaGVtYSBnZW5lcmF0aW9uIHNob3VsZCBiZSBsb2NhdGVkIGhlcmUuCmZyZWVtYXJrZXJUZW1wbGF0ZXNEaXI9JHtlY2xpcHNlV29ya3NwYWNlRGlyfS9tZWUtc291cmNlLW1pZ3JhdGlvbi1uYXR1cmFsLWRpc3QvcmVzL2ZyZWVtYXJrZXIvdGVtcGxhdGVzCgojIExvZyBmaWxlcyB3aWxsIGJlIHdyaXR0ZW4gdG8gdGhpcyBkaXJlY3RvcnkuCm' + 
    'xvZ0Rpcj0ke3dvcmtEaXJ9L2xvZwplY2xpcHNlSW5zdGFsbExvZ0ZpbGU9JHtsb2dEaXJ9L2VjbGlwc2VJbnN0YWxsLmxvZwpuYXQyamF2YUxvZ0ZpbGU9JHtsb2dEaXJ9L25hdDJqYXZhLmxvZwpqYXZhRXJyb3JzTG9nRmlsZT0ke2xvZ0Rpcn0vamF2YS1lcnJvcnMubG9nCnNjaGVtYUdlbkxvZ0ZpbGU9JHtsb2dEaXJ9L3NjaGVtYUdlbmVyYXRpb24ubG9nCmVjbGlwc2VMb2dGaWxlPSR7bG9nRGlyfS9lY2xpcHNlLmxvZwplY2xpcHNlTWV0YWRhdGFMb2dGaWxlPSR7ZWNsaXBzZVdvcmtzcGFjZURpcn0vLm1ldGFkYXRhLy5sb2cKIyBUZXN0IGpvYnMgd3J' + 
    'pdGUgb3V0cHV0IGZpbGVzIHRvIHRoaXMgZGlyZWN0b3J5LgpvdXRwdXREaXI9JHt3b3JrRGlyfS9vdXRwdXQKCiMgS0lESUNBUCBwcm9qZWN0IGZpbGVzIGFuZCBmb2xkZXJzCml3SmFyRGlyPSR7d29ya0Rpcn0vSVdfTElCUwpraWRpY2FwSmFyRGlyPSR7d29ya0Rpcn0vS0lESUNBUF9MSUJTCmdpcEphckRpcj0ke3dvcmtEaXJ9L0dJUF9MSUJTCmtpZGljYXBKYXJGaWxlPSR7a2lkaWNhcEphckRpcn0va2lkaWNhcC5qYXIKa2lkaWNhcFNvdXJjZXNKYXJGaWxlPSR7a2lkaWNhcEphckRpcn0va2lkaWNhcC1zcmMuamFyCmtpZGljYXBKYXZhU291cmNlc0Rp' + 
    'cj0ke2VjbGlwc2VXb3Jrc3BhY2VEaXJ9L0tJRElDQVAtSmF2YS9zcmMvamF2YS1tZWUKa2lkaWNhcEphdmFDbGFzc2VzRGlyPSR7ZWNsaXBzZVdvcmtzcGFjZURpcn0vS0lESUNBUC1KYXZhL3RhcmdldC9jbGFzc2VzCm5hdDJqYXZhUHJvcGVydGllc0ZpbGU9JHtlY2xpcHNlV29ya3NwYWNlRGlyfS9LSURJQ0FQLVBSRVAvbmF0MmphdmEucHJvcGVydGllcwpidWlsZEluRWNsaXBzZUFudEZpbGU9JHtlY2xpcHNlV29ya3NwYWNlRGlyfS9LSURJQ0FQLUNvbnZlcnQvYnVpbGQueG1sCgojIEJGTEVYIHJlbGF0ZWQgcHJvcGVydGllcwpiZmxleERpcj0ke3dvc' + 
    'mtEaXJ9L2JmbGV4CmJmbGV4TGliQ29tcGlsZVN1YmRpcj1saWItY29tcGlsZQpiZmxleExpYkNvbXBpbGVEaXI9JHtiZmxleERpcn0vJHtiZmxleExpYkNvbXBpbGVTdWJkaXJ9CmJmbGV4TGliUnVudGltZURpcj0ke2JmbGV4RGlyfS9saWItcnVudGltZQpiZmxleEphdmFTb3VyY2VaaXA9JHtiZmxleERpcn0vYmZsZXgtc3JjLSR7YmZsZXhWYXJpYW50fS56aXAKYmZsZXhKYXZhU291cmNlRGlyPSR7YmZsZXhEaXJ9L2JmbGV4LXNyYy0ke2JmbGV4VmFyaWFudH0KYmZsZXhKYXZhQ2xhc3Nlc0Rpcj0ke2JmbGV4RGlyfS9iZmxleC1jbGFzc2VzLSR7YmZsZX' + 
    'hWYXJpYW50fQpiZmxleEphdmFDbGFzc2VzSmFyPSR7a2lkaWNhcEphckRpcn0vYmZsZXgtJHtiZmxleFZhcmlhbnR9LmphcgojIFRoZSBCRkxFWCBidWlsZCB1c2VzIHRoZSBjb21waWxlciBleHRlbnNpb25zCm1lZUNvbXBpbGVyRXh0ZW5zaW9uc0J1bmRsZVppcD0ke214SmFyc0Rpcn0vbWVlLWNvbXBpbGVyLWV4dGVuc2lvbnMtYnVuZGxlLnppcAoKIwojIFByb2R1Y3Rpb24gcmVsYXRlZCBwcm9wZXJ0aWVzCiMKCiMgbWVlIHByb2ZpbGluZwppbm5vd2FrZS5saWIuY29yZS5wcm9maWxlLlByb2ZpbGluZ0VuYWJsZWQ9ZmFsc2UKCiMgREIgcHJvcGVydGl' + 
    'lcwpvcmFEYlNlcnZpY2VOYW1lPU9SQ0xQREIxCmRiSXBBZGRyZXNzPWxvY2FsaG9zdApvcmFTeXN0ZW1Vc2VyPXN5c3RlbQpvcmFTeXN0ZW1Vc2VyUGFzc3dvcmQ9V29yeDIwMDAKZGJPcmFjbGVDb25uZWN0aW9uU3RyaW5nPWpkYmM6b3JhY2xlOnRoaW46QC8vJHtkYklwQWRkcmVzc306JHtkYlBvcnR9LyR7b3JhRGJTZXJ2aWNlTmFtZX0KZGJNc1NxbENvbm5lY3Rpb25TdHJpbmc9amRiYzpzcWxzZXJ2ZXI6Ly8ke2RiSXBBZGRyZXNzfToke2RiUG9ydH07ZGF0YWJhc2VOYW1lPSR7bXNzcWxEYk5hbWV9CmRiQ29ubmVjdGlvblN0cmluZz0ke2RiT3JhY2xlQ' + 
    '29ubmVjdGlvblN0cmluZ30KZGJEcml2ZXI9b3JhY2xlLmpkYmMuT3JhY2xlRHJpdmVyCmRiRHJpdmVyQ2xhc3NwYXRoPSR7Z2lwSmFyRGlyfS9vamRiYzgtMTkuMy4wLjAuamFyCgphZ3R2RG9ja2VySW1hZ2U9Z2lwX1IxNjUtYWd0di1teDE2CmFndHZEYlNjaGVtYT1LSURJQ0FQX1IxNjVfQUdUVgphZ3R2RGJQYXNzd29yZD1tb3JwaGV1cwphZ3R2MkRvY2tlckltYWdlPWdpcC0xNjUtYWd0djItbXgxNwphZ3R2MkRiU2NoZW1hPUtJRElDQVBfUjE2NV9BR1RWMgphZ3R2MkRiUGFzc3dvcmQ9bW9ycGhldXMKb2ZkYW5Eb2NrZXJJbWFnZT1naXBfUjE2NS1vZmR' + 
    'hbi1teDE2Cm9mZGFuRGJTY2hlbWE9S0lESUNBUF9SMTY1X09GREFOCm9mZGFuRGJQYXNzd29yZD1tb3JwaGV1cwoKcmVkdWNlRGJFeGVjdXRpb25EYXlBZ3R2PTIwMTYwMzE3CnJlZHVjZURiS2V5QWd0dj0iJ0JSRTIwMTYwMzAwMDIzMDAwMDAwMDAwMDAwJywnQlJFMjAxNjAzMDAwNDQwMDAwMDAwMDAwMDAnLCdCUkUyMDE2MDMzMDA0MDQwMDAwMDAwMDAwMCciCnJlZHVjZURiRXhlY3V0aW9uRGF5T2ZkYW49MjAxMzEwMTAKcmVkdWNlRGJLZXlPZmRhbj0iJ0JSRTIwMTMxMDAwMDEyMjAwMDAwMDAwMDAwJyIKCmNvbXBpbGVyTGV2ZWw9MTEK'
    return new String(contentAsBase64.decodeBase64())
}