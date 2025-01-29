def lib = library("TestUtils@${branchName}")

/**
* Test job for SvnUtils methods.
*
* @param branchName Name of the branch where changes are stored, default: master
*/
timestamps {
    def dockerUtils =  lib.DockerUtils.new()
    def gitUtils = lib.GitUtils.new()
    def svnUtils = lib.SvnUtils.new()
    node('OS-Linux') {
        def workDir = pwd()

        buildName "#${env.BUILD_ID} - ${branchName}"

        deleteDir()

        stage('Test getSvnUrlQm') {
            check('http://poseidon.innowake.hq/svn/innowake-qm', svnUtils.getSvnUrlQm())
        }

        stage('Test getSvnUrlQa') {
            check('http://poseidon.innowake.hq/svn/innowake-qm/qa', svnUtils.getSvnUrlQa())
        }

        stage('Test getSvnUrlProject') {
            def expectedMxVersion = '19.1'
            def expectedProject = 'someName'
            check("http://poseidon.innowake.hq/svn/innowake-qm/qa/projects/maxenso/branches/${expectedMxVersion}/${expectedProject}", svnUtils.getSvnUrlProject(expectedMxVersion, expectedProject))
        }

        stage('Test getSvnUrlKm') {
            check('http://poseidon.innowake.hq/svn/innowake-qm/km', svnUtils.getSvnUrlKm())
        }

        stage('Test getSvnUrlKmTools') {
            check('http://poseidon.innowake.hq/svn/innowake-qm/km/tools', svnUtils.getSvnUrlKmTools())
        }

        stage('Test getSvnUrlQaBaseData') {
            check('http://poseidon.innowake.hq/svn/innowake-qm/qa/base/data', svnUtils.getSvnUrlQaBaseData())
        }

        stage('Test getSvnUrlQaBaseLib') {
            check('http://poseidon.innowake.hq/svn/innowake-qm/qa/base/lib', svnUtils.getSvnUrlQaBaseLib())
        }

        stage('Test getSvnUrlQaBaseScripts') {
            check('http://poseidon.innowake.hq/svn/innowake-qm/qa/base/scripts', svnUtils.getSvnUrlQaBaseScripts())
        }

        stage('Test getSvnUrlRm') {
            check('http://poseidon.innowake.hq/svn/innowake-qm/rm', svnUtils.getSvnUrlRm())
        }

        stage('Test getSvnUrlQaTools') {
            check('http://poseidon.innowake.hq/svn/innowake-qm/qa/tools', svnUtils.getSvnUrlQaTools())
        }

        stage('Test svnExport') {
            // ??
        }

        stage('Test svnExportFiles') {
            //??
        }

        stage('Test svnExportRecursive') {
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