@Library('TestUtils') _

/**
 * Testing new method for gitUtils.
 * @param startUserTests If false the intention of the starter was to execute the REMOTE tests only otherwise only execute the user tests
    This does not assure that the job really was starter by e.g Remote host. It just was the intention.
    The sanity checks assure that! If the intention was to start the Remote tests, 
    then jobStarter needs to be null otherwise it is senseless to execute the tests.
 * @param receiver A single email address as String which should receive emails when the job fails.
 */
node('USLinux2') {
    startUserTests = Boolean.parseBoolean(startUserTests)
    def gitUtils = new GitUtils()
    def dockerUtils = new DockerUtils()
    def miscUtils = new MiscUtils()
    def workingDir = pwd()
    def jobStarter = ''
    wrap([$class: 'BuildUser']) { jobStarter = env.BUILD_USER_ID } //otherwise env.BUILD_USER_ID is always jenkins  

    buildName "#${env.BUILD_ID} - ${(startUserTests ? 'user' : 'remote')} tests"
    buildDescription "startUserTests=${startUserTests} jobStarter=${jobStarter} receiver=${receiver}"
    if (startUserTests) {

        stage('Test: Notify only job starter') {
            //sanity check
            checkNotNull(jobStarter)
            if (currentBuild.currentResult == 'UNSTABLE') {
                error 'sanity check failed'
            }
            def actRC = 'UNSTABLE' 
            def notifyOnRC = ['FAILURE', 'UNSTABLE']
            def mail = [
                    subj: "Build ${currentBuild.number} of ${currentBuild.projectName} was not successful",
                    body: "You got this email because you are the job starter"
            ]

            receivers = ['willNeverReceiverA@mail.de']
            def emailSendTo = miscUtils.notifyFailure(mail, receivers, actRC, notifyOnRC)
            check(["${jobStarter}@deloitte.com"], emailSendTo)

            actRC = 'FAILURE'
            emailSendTo = miscUtils.notifyFailure(mail, receivers, actRC)
            check(["${jobStarter}@deloitte.com"], emailSendTo)
        }
    } else {

        stage('Test: REMOTE notify only receivers') {
            //sanity check
            checkNull(jobStarter)
            if (currentBuild.currentResult == 'UNSTABLE') {
                error 'sanity check failed'
            }
            //on remote there is no job starter, so only messsage the receivers
            def receivers = [receiver]
            def actRC = 'UNSTABLE' 
            def notifyOnRC = ['FAILURE', 'UNSTABLE']
            def mail = [
                    subj: "REMOTE Build ${currentBuild.number} of ${currentBuild.projectName} was not successful",
                    body: "You got this email because you are in the list of receivers"
            ]

            def emailSendTo = miscUtils.notifyFailure(mail, receivers, actRC, notifyOnRC)
            check(receivers, emailSendTo)

            actRC = 'FAILURE'            
            emailSendTo = miscUtils.notifyFailure(mail, receivers, actRC)
            check(receivers, emailSendTo)
        }
    }
}

/*
* Force usage of groovy power assertion instead of the 'normal' assert.
* Normal assert outputs on failure are not as expressive as power assertion
*/
@NonCPS
def checkNotNull(actual, boolean makeItUnstable = true) {
    checkHelp({ assert null != actual }, makeItUnstable)
}

@NonCPS
def checkNull(actual, boolean makeItUnstable = true) {
    checkHelp({assert null == actual}, makeItUnstable)
}

@NonCPS
def check(expected, actual, boolean makeItUnstable = true) {
    checkHelp({assert expected == actual}, makeItUnstable)
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
