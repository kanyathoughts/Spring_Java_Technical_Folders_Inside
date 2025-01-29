@Library('TestUtils') _

/**
 * Testing new method for gitUtils.
 * This start only works on UsLinux2 due to firewall.
 * @param receiver A single email address as String which should receive emails when the job fails.
 */
node('USLinux2') {
    def gitUtils = new GitUtils()
    def dockerUtils = new DockerUtils()
    def miscUtils = new MiscUtils()
    def workingDir = pwd()
    def jenkinsUser = 'elangenbucher'
    def jenkinsToken = '111a02a73c72b68d813a2ddb3fc45a5c5d'
    def jobToken = '1337mySup3rSecialT0ken42'
    def jenkinsRemoteJobName = 'Test_Email_Notification'
    def jenkinsHost = "qef-linux2.deloitte.com:8085"
    def jenkinsUrl = "http://${jenkinsUser}:${jenkinsToken}@${jenkinsHost}/job/${jenkinsRemoteJobName}"

    buildName "#${env.BUILD_ID} - start tests"
    buildDescription "receiver=${receiver}"

    stage('Start Test as User') {
        build wait: false, job: jenkinsRemoteJobName , parameters: [booleanParam(name: 'startUserTests', value: true), textParam(name: 'receiver', value: receiver)]
    }

    stage('Start Test as Remote') {
        def curlOut = sh returnStdout: true, script: "curl -X POST ${jenkinsUrl}/buildWithParameters?token=${jobToken}\"&\"startUserTests=false\"&\"receiver=${receiver}"
        echo "curlOut=${curlOut}"
        if (curlOut.contains('ERROR')) {
            unstable('unable to start test job as user')
        }
    }
}