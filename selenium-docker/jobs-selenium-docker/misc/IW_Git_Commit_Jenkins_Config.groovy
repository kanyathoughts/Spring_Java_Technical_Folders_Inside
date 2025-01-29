@Library('TestUtils') _

/**
 * Commit the Jenkins configuration to git
 * @param requestId  The i.ris ID of the request, that motivates this commit. May be empty, if there is no such request.
 * @param commitComment  Explanatory free text
 * @param backupLogicalName  The logical name of this backup in git.
 *                           On every Jenkins instance this variable has to get a unique default value. When starting the job, that value shall not be changed.
 *                           It is regarded to be a constant, that is specific for the current Jenkins instance.
 */
node('built-in') {
    def commitMsg = ''
    def relativeGitProjectPath = 'infrastructure/environment/config.git'
    def jobWorkspace = pwd()
    def gitUtils = new GitUtils()
    def miscUtils = new MiscUtils()
    
    try {
        stage('init') {
            deleteDir()
            /* If the job has been started manually by a user, then input parameters are required and the commit message depends on the parameters.
                  Otherwise the job has been started by a timer. Then a fixed commit message is set automatically.
             */
            def userId = wrap([$class: 'BuildUser']) {
                return env.BUILD_USER_ID
            }
            if (userId != null && ! userId.isEmpty() && userId != 'timer') {
                buildDescription "triggered by user ${userId}"
                if (requestId.isEmpty() && commitComment.isEmpty()) {
                    error 'At least one parameter must be set!'
                }
                commitMsg = "${requestId}: ${userId}: ${commitComment}"
            } else {
                buildDescription 'triggered by timer'
                commitMsg = 'Automatic commit of Jenkins configuration to git'
            }

            dir('remote') {
                withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
                    sh "git clone --branch ${backupLogicalName} ${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/${relativeGitProjectPath}"
                }
            }
        }
        
        stage('collect-files') {
            dir('local') {
                // Copy the jenkins home directory into the job workspace
                sh "cp -rfv ${env.JENKINS_HOME}/ JENKINS_HOME"
                // Delete some folders
                sh 'rm -rfv JENKINS_HOME/config-history JENKINS_HOME/plugins JENKINS_HOME/fingerprints JENKINS_HOME/logs JENKINS_HOME/cache JENKINS_HOME/caches JENKINS_HOME/.groovy'
                // Find and delete all files that do not end with .xml
                sh 'find JENKINS_HOME/ -type f ! -name \'*.xml\' | xargs -L 1 rm -fv'
                // Find and delete all links
                sh 'find JENKINS_HOME/ -type l | xargs -L 1 rm -fv'
                // Find and delete all empty directories
                sh 'find JENKINS_HOME/ -type d -empty | xargs -L 1 rm -rfv'
                // Find and delete specific files which are not needed
                sh 'rm -fv JENKINS_HOME/queue.xml JENKINS_HOME/org.jenkinsci.plugins.workflow.flow.FlowExecutionList.xml'
                sh returnStdout: true, script: "rsync -r -v -u --delete JENKINS_HOME ${jobWorkspace}/remote/config/"
            }
        }
        
        stage('commit') {
            withCredentials([usernamePassword(credentialsId: 'USAppModQMUserSVC-User-PW', passwordVariable: 'gitPassword', usernameVariable: 'gitUser')]) {
                dir('remote/config') {
                    sh 'git add --all'
                    def commitStatus = sh returnStatus: true, script: "git -c user.name='${gitUser}' -c user.email='${gitUser}@deloitte.com' commit -m \"${commitMsg}\""
                    if (commitStatus == 0) {
                        sh 'git push -u origin'
                        sh 'git diff --name-only HEAD~1 HEAD'
                    }
                }
            }
        }
        
    } catch (ex) {
		miscUtils.notifyServerWatchdogsAboutFailure(currentBuild)
        error "exception ${ex}"
    }
}
