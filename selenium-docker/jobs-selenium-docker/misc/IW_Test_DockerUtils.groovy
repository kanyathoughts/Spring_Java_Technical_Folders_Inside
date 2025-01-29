def lib = library("TestUtils@${branchName}")

/**
* Test job for DockerUtils methods.
*
* @param branchName Name of the branch where changes are stored, default: master
*/
timestamps {
    def dockerUtils = lib.DockerUtils.new()
    def gitUtils = lib.GitUtils.new()

    node('OS-Linux') {
        def workDir = pwd()
        def utilMethods = []

        buildName "#${env.BUILD_ID} - ${branchName}"

        deleteDir()

        stage('Test stopDocker') {
            // start a container to get container id
            // call stopDocker with container id
            // check if container is stopped

            //call stopDocker with invalid container id
        }

        stage('Test getActualHostPort1521') {
            // start a container to get container id
            // call getActualHostPort1521 with container id
            // check if ??

            //call getActualHostPort1521 with invalid container id
            // check if ??
        }

        stage('Test startDockerOracleMultiple') {
            // ??
        }

        stage('Test getNextFreeContainerNameSuffix') {
            // ??
        }

        stage('Test waitForOracle') {
            // returns true on success else throws error
        }

        stage('Test getHostPort1521') {
            // only for old GIP_... jobs
            check('1533', dockerUtils.getHostPort1521('gip-154-agtv-mx16'))
            check('1542', dockerUtils.getHostPort1521('gip-165-agtv-trunk'))
            check('1551', dockerUtils.getHostPort1521('employees'))
            check('1552', dockerUtils.getHostPort1521('oracle-MIGRATED'))

            check(null, dockerUtils.getHostPort1521('invalidKey'))
        }

        stage('Test startDockerDb2Multiple') {
            //??
        }

        stage('Test getActualHostPort50000') {
            //??
        }

        stage('Test waitForDb2') {
            //??
        }

        stage('Test startDockerPostgresMultiple') {
            //??
        }

        stage('Test getActualHostPort5432') {
            //??
        }

        stage('Test getJenkinsControllerImageName') {
            //??
        }

        stage('Test pullJenkinsEnvironmentImage') {
            //??
        }

        stage('Test pullJenkinsEnvironmentCaGenImage') {
            //??
        }

        stage('Test pullJenkinsEnvironmentCaGenUiImage') {
            //??
        }

        stage('Test getJenkinsEnvironmentCaGenUiImageName') {
            //??
        }

        stage('Test getJenkinsEnvironmentCaGenImageName') {
            //??
        }

        stage('Test getJenkinsEnvironmentImageName') {
            //??
        }

        stage('Test getEuDockerRegistryUrl') {
            check('https://qef-linux1-de.deloitte.com:5000', dockerUtils.getEuDockerRegistryUrl())
        }

        stage('Test getEuDockerRegistryHostAndPort') {
            check('qef-linux1-de.deloitte.com:5000', dockerUtils.getEuDockerRegistryHostAndPort())
        }

        stage('Test pullImage 1 param') {
            //??
        }

        stage('Test pullImageFromCustomRegistry') {
            //??
        }

        stage('Test pullDockerOracleImage') {
            //??
        }

        stage('Test getHostPortRange1521') {
            def expectedPortMap = [
                    'gip-154-agtv-mx15'        : '1560-1569',
                    'gip-154-ofdan-mx15'       : '1570-1579',
                    'gip-154-ofdve-mx15'       : '1580-1589',
                    'gip-154-agtv-mx16'        : '1590-1599',
                    'gip-154-ofdan-mx16'       : '1600-1609',
                    'gip-154-ofdve-mx16'       : '1610-1619',
                    'gip-165-agtv-mx15'        : '1620-1629',
                    'gip-165-agtv2-mx15'       : '1630-1639',
                    'gip-165-ofdan-mx15'       : '1640-1649',
                    'gip-165-agtv-mx16'        : '1650-1659',
                    'gip-165-agtv2-mx17'       : '1660-1669',
                    'gip-165-ofdan-mx16'       : '1670-1679',
                    'gip-165-agtv-trunk'       : '1680-1689',
                    'employees'                : '1690-1699',
                    'oracle-MIGRATED'          : '1700-1709',
                    'gip-154-agtv-mx16-wqm1429': '1710-1719',
                    'gip-165-agtv-mx16-wvm618' : '1720-1729',
                    'gip-184-agtv-mx17'        : '1730-1739'
            ]
            expectedPortMap.each {
                check(it.value, dockerUtils.getHostPortRange1521(it.key))
            }

            check(null, dockerUtils.getHostPortRange1521('invalidkey'))
        }

        stage('Test getHostPortRange5500') {
            def expectedPortMap = [
                    'gip-154-agtv-mx15'        : '5640-5649',
                    'gip-154-ofdan-mx15'       : '5650-5659',
                    'gip-154-ofdve-mx15'       : '5660-5669',
                    'gip-154-agtv-mx16'        : '5670-5679',
                    'gip-154-ofdan-mx16'       : '5680-5689',
                    'gip-154-ofdve-mx16'       : '5690-5699',
                    'gip-165-agtv-mx15'        : '5700-5709',
                    'gip-165-agtv2-mx15'       : '5710-5719',
                    'gip-165-ofdan-mx15'       : '5720-5729',
                    'gip-165-agtv-mx16'        : '5730-5739',
                    'gip-165-agtv2-mx17'       : '5740-5749',
                    'gip-165-ofdan-mx16'       : '5750-5759',
                    'gip-165-agtv-trunk'       : '5760-5769',
                    'employees'                : '5770-5779',
                    'oracle-MIGRATED'          : '5780-5789',
                    'gip-154-agtv-mx16-wqm1429': '5790-5799',
                    'gip-165-agtv-mx16-wvm618' : '5800-5809',
                    'gip-184-agtv-mx17'        : '5810-5819'
            ]
            expectedPortMap.each {
                check(it.value, dockerUtils.getHostPortRange5500(it.key))
            }

            check(null, dockerUtils.getHostPortRange5500('invalidkey'))
        }

        stage('Test getCredentials') {
            def expectedCredentialsMap = [
                    'gip-154-agtv-mx15' : ['kidicap_r154_agtv', 'morpheus'],
                    'gip-154-ofdan-mx15': ['kidicap_r154_ofdan', 'morpheus'],
                    'gip-154-ofdve-mx15': ['kidicap_r154_ofdve', 'morpheus'],
                    'gip-154-agtv-mx16' : ['kidicap_r154_agtv', 'morpheus'],
                    'gip-154-ofdan-mx16': ['kidicap_r154_ofdan', 'morpheus'],
                    'gip-154-ofdve-mx16': ['kidicap_r154_ofdve', 'morpheus'],
                    'gip-165-agtv-mx15' : ['kidicap_r165_agtv', 'morpheus'],
                    'gip-165-agtv2-mx15': ['kidicap_r165_agtv2', 'morpheus'],
                    'gip-165-ofdan-mx15': ['kidicap_r165_ofdan', 'morpheus'],
                    'gip-165-agtv-mx16' : ['kidicap_r165_agtv', 'morpheus'],
                    'gip-165-agtv2-mx17': ['kidicap_r165_agtv2', 'morpheus'],
                    'gip-165-ofdan-mx16': ['kidicap_r165_ofdan', 'morpheus'],
                    'gip-165-agtv-trunk': ['kidicap_r165_agtv', 'morpheus'],
                    'employees'         : ['test-3nf-trigger', 'worx2000'],
                    'oracle-MIGRATED'   : ['MIGRATED', 'morpheus'],
                    'gip-184-agtv-mx17' : ['kidicap_r184_agtv_devel', 'morpheus']
            ]
            expectedCredentialsMap.each {
                check(it.value, dockerUtils.getCredentials(it.key))
            }

            check(null, dockerUtils.getCredentials('invalidkey'))
        }

        stage('Test pullImage 2 params') {
            //??
        }

        stage('Test isOracleRunning') {
            //??
        }

        stage('Test getOracleVersion') {
            //??
        }

        stage('Test pullDockerDb2Image') {
            //??
        }

        stage('Test getHostPortRange50000') {
            def expectedPortMap = [
                    'mannheimerDB2'                         : '50000-50009',
                    'mannheimerDB2-dbImageAfter259Testcases': '50010-50019'
            ]

            expectedPortMap.each {
                check(it.value, dockerUtils.getHostPortRange50000(it.key))
            }

            check(null, dockerUtils.getHostPortRange50000('invalidkey'))
        }

        stage('Test getHostPortRangePostgres') {
            def expectedPortMap = [
                    'postgres': '5433-5443'
            ]

            expectedPortMap.each {
                check(it.value, dockerUtils.getHostPortRangePostgres(it.key))
            }

            check(null, dockerUtils.getHostPortRangePostgres('invalidkey'))
        }

        stage('Test getAvailablePort') {
            //??
        }
    }
}

/*
* Test if a method call throws a certain Exception.
* @param expected  the name of the exception we expect to get thrown (e.g. 'java.nio.file.NoSuchFileException')
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
        if ( !error.getMessage().toLowerCase().contains(expected.toLowerCase())) {
            unstable "Exception '${expected}' not part of Error Message '${error.getMessage()}'"
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