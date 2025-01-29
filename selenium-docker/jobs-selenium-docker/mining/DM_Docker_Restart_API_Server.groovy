@Library('TestUtils') _
/**
 * This job restarts the mining api server in a specific environment.
 *
 * @param executeOn		The node where to restart the api server.
 */

node(executeOn) {
    stage('restart api-server') {
        sh 'docker restart api-server'
        sleep 60
    }
}