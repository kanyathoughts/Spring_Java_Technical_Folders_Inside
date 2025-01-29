@Library('TestUtils') _

/**
 * A job to move projects from svn to git. The job uses the svn project name and searches the svn repository for all available branches.
 * @param relativeProjectPath   The relative path for the git repository in the qef git environment. I.e. playground/my-project.git if the full path is https://gitlab.consulting.sltc.com/appmod/qef/playground/my-project.git
 * @param projectNameSVN        The project name in the svn repository.
 */

node('DELinux2') {
    def dockerUtils = new DockerUtils()
    def gitUtils = new GitUtils()
    def workDir = pwd()

    buildName "#${env.BUILD_ID} - ${projectNameSVN}"
    buildDescription "projectNameSVN=${projectNameSVN} relativeProjectPath=${relativeProjectPath}"

    stage('init') {
        def final authors = '''avi.jhingran = Avi Jhingran <avjhingran@deloitte.com>
anja.weidle = Anja Weidle <aweidle@deloitte.com>
claudia.banua = Claudia Banua <cbanua@deloitte.com>
christian.kempter = Christian Kempter <ckempter@deloitte.com>
dominik.blum = Dominik Blum <dblum@deloitte.com>
dipesh.mittal = Dipesh Mittal <dipeshmittal@deloitte.com>
hilmar.acker = Hilmar Acker <hiacker@deloitte.com>
janka.jordan = Janka Lobner <jlobner@deloitte.com>
jochen.schmidt = Jochen Schmidt <joschmidt@deloitte.com>
julian.koenig = Julian K. Koenig <jukoenig@deloitte.com>
kanyakumari.darisi = Kanyakumari Darisi <kdarisi@deloitte.com>
klaus-martin.schmude = Klaus-Martin Schmude <kschmude@deloitte.com>
lichee.jain = Lichee R Jain <licjain@deloitte.com>
markus.svejkovsky = Markus Svejkovsky <msvejkovsky@deloitte.com>
nina.breinlich = Nina Breinlich <nbreinlich@deloitte.com>
nicolai.titze = Nicolai Titze <ntitze@deloitte.com>
oswald.yao = Oswald Yao <oswyao@deloitte.com>
pranavi.chittamuru = Pranavi Chittamuru <pchittamuru@deloitte.com>
preeti.koppula = Preeti Koppula <pkoppula@deloitte.com>
rohit.akhairamka.ext = Rohit Akhairamka <rakhairamka@deloitte.com>
sankhadeep.dey = Sankhadeep Dey <sadey@deloitte.com>
shayanta.sur = Shayanta Sur <shaysur@deloitte.com>
simon.horn = Simon Horn <simhorn@deloitte.com>
sonny.kodali = Sonny Kodali <sokodali@deloitte.com>
thomas.svejkovsky = Thomas Svejkovsky <tsvejkovsky@deloitte.com>
yagna.munukutla = Yagna Narayana Sastry Munukutla <ymunukutla@deloitte.com>
felix.schoppe = Felix Schoppe <fschoppe@deloitte.com>
jonas.hermann = Jonas Hermann <johermann@deloitte.com>
QMSRV1 = USAppModQMGitLabSVC <USAppModQMGitLabSVC@deloitte.com>
olivier.bagnack = Olivier Bagnack <obagnack@deloitte.com>
abhishe.onkar.ext = Abishek Onkar <aonkar@deloitte.com>
rohit.akhairamka.ext = Rohit Akhairamka <rakhairamka@deloitte.com>
ajay.athreya = Ajay Athreya <ajathreya@deloitte.com>
sumona.panda = Sumona Panda <sumpanda@deloitte.com>
potnuru.pravalika = Potnuru Pravalika <popravalika@deloitte.com>
eric.langenbucher = Eric Langenbucher <elangenbucher@deloitte.com>
felix.dieterle = Felix Dieterle <fdieterle@deloitte.com>
prashansa.shanthappa = Prashansa Viresh Shanthappa <pshanthappa@deloitte.com>
smruti.mohanty = Smruti Rekha Mohanty <smmohanty@deloitte.com>'''
        deleteDir()
        writeFile file: 'authors.txt', text: authors
    }

    docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside('-t --name svn2git -u=0') {
        try {
            stage('install svn2git') {
                sh 'apt-get -y update'
                sh 'apt-get -y install git-core git-svn ruby'
                sh 'gem install svn2git'
            }
            
            stage('init svn') {
                // This is necessary to store the SVN password in the container as svn2git is not able to pass the password over command line
                withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'svnPw', usernameVariable: 'svnUser')]) {
                    sh "svn co --username ${svnUser} --password ${svnPw} http://poseidon.innowake.hq/svn/innowake-qm/archive/qa/base/scripts /home/jenkins/svn"
                }
                sh 'sed -i "s|# store-passwords = no|store-passwords = yes|g" /root/.subversion/servers'
                sh 'sed -i "s|# store-plaintext-passwords = no|store-plaintext-passwords = yes|g" /root/.subversion/servers'
                //sh 'rm /root/.subversion/auth/svn.simple/bea13a9425e1d37b66ce534a2c60da35'
                /*adjusted path because of error message that the file or directory doesn't exist*/
                sh 'rm /root/.subversion/auth/svn.simple/*'
                withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'svnPw', usernameVariable: 'svnUser')]) {
                    sh "svn co --username ${svnUser} --password ${svnPw} http://poseidon.innowake.hq/svn/innowake-qm/archive/qa/base/scripts /home/jenkins/svn2"
                }
            }
            
            stage('configure git') {
                withCredentials([usernamePassword(credentialsId: 'USAppModQMUserSVC-User-PW', passwordVariable: 'gitPassword', usernameVariable: 'gitUser')]) {
                    sh "git config --global user.name '${gitUser}'"
                    sh "git config --global user.email '${gitUser}@deloitte.com'"
                }
            }

            stage('execute svn2git') {
                def branches = sh returnStdout: true, script: 'svn ls http://poseidon.innowake.hq/svn/innowake-qm/qa/projects/maxenso/branches/'
                branches = branches.split('\n')
                dir('repo') {
                    sh 'git init'
                    sh 'git config gc.auto 0' // Necessary to avoid a problem with gc during svn2git
                }
                for (branch in branches) {
                    def svnStatus = sh returnStatus: true, script: "svn ls http://poseidon.innowake.hq/svn/innowake-qm/qa/projects/maxenso/branches/${branch}${projectNameSVN} --depth empty"
                    if (svnStatus == 0) {
                        dir('tmp') {
                            sh 'git init'
                            sh 'git config gc.auto 0' // Necessary to avoid a problem with gc during svn2git
                            sh "svn2git http://poseidon.innowake.hq/svn/innowake-qm/qa/projects/maxenso/branches/${branch}${projectNameSVN} --verbose --authors ../authors.txt --notags --nobranches --notrunk"
                            //keep empty folders
                            sh 'find . -type d -empty -print | grep -v .git | xargs -I % touch %/.gitkeep'
                            sh 'git add --all'
                            try{
                                sh 'git commit -m "keep empty folders"'
                            }catch(e){
                                echo e.getMessage()
                            }
                            
                        }
                        dir('repo') {
                            sh 'git remote add test ../tmp'
                            sh 'git remote update'
                            def branchName = branch.replaceAll('/', '')
                            try{
                                sh "git checkout -b ${branchName} test/master"
                            }catch(e){
                                sh 'git commit -am "Committing changes to keep for a correct migration"'
                                sh "git checkout -b ${branchName} test/master"
                            }
                            sh 'git remote remove test'
                        }
                        sh 'rm -r tmp'
                    }
                }
                dir('repo') {
                    sh 'git checkout 99.9'
                    sh 'git checkout -b master'
                    sh 'git branch -d 99.9'
                }
            }
            
            stage('push project') {
                dir('repo') {
                    sh "git remote add origin ${gitUtils.getGitUrlQef()}/${relativeProjectPath}"
                    withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
                        sh "git push --set-upstream ${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/${relativeProjectPath} --all"
                    }
                }
            }
        } catch(e) {
            echo e.getMessage()
        } finally {
            stage('clean up') {
                def tmpExist = fileExists 'tmp'
                if (tmpExist) {
                    sh 'rm -r tmp'
                }
                sh returnStatus: false, script: 'rm -r repo'
            }
        }
    }
}