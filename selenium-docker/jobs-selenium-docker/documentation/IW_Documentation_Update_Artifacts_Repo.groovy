@Library('TestUtils') _

/**
* Clones the repository wp-artifacts that contains all artifacts (all generated PDFs except changelogs) of appmod-documentation.
* It copies all new generated PDFs to the repository and pushed it to gitlab.
*/
node('Documentation') {
    def dockerUtils = new DockerUtils()
    def gitUtils = new GitUtils()
    def jobWorkspace = pwd()
    def artifactsFolder = "${jobWorkspace}/wp-artifacts"
    def gitlabProject

    docker.image(dockerUtils.pullJenkinsEnvironmentImage('java11')).inside {
        stage('checkout') {
            withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
                gitlabProject = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/innowake-documentation/wp-artifacts.git"
            }

            if (!fileExists(artifactsFolder)) {
                sh "git clone --depth 1 --branch master ${gitlabProject} ${artifactsFolder}"
            } else {
                 dir(artifactsFolder) {
                    sh "git pull --depth 1"
                }
            }
        }
    }
    
    stage('copy files') {
        waitUntilScriptFinished('rsync -auv /data/appmod/wordpress/artifacts/* wp-artifacts')
    }

    stage('push to repository') {
        dir(artifactsFolder) {
            def status = sh returnStdout: true, script: "git status --porcelain"
            if(status.length() > 0) {
                withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
                        gitlabProject = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/innowake-documentation/wp-artifacts.git"
                    sh 'git add .'
                    sh "git commit -m \"Auto-Commit at ${getCurrentTime()}\""
                    sh "git push ${gitlabProject} --all"
                }
            } else {
                echo 'Nothing to commit'
            }
        }
    }
    
}

/**
* Waits until the script finished  
*/
def waitUntilScriptFinished(scriptCommand) {
    timeout(60) {
        waitUntil {
            script {
                def res = sh script: scriptCommand, returnStatus: true
                return (res == 0)
            }
        }
    }
}

/**
 * Returns the current time ('HH:mm')
 */
def getCurrentTime() {
	Date date = new Date()
	return date.format("HH:mm:ss")
}