def lib = library("TestUtils@${branchName}")

/**
* Test job for GitUtils methods.
*
* @param branchName Name of the branch where changes are stored, default: master
*/
timestamps {
    def gitUtils = lib.GitUtils.new()
    def dockerUtils = lib.DockerUtils.new()
    def miscUtils = lib.MiscUtils.new()
    
    node('USLeanft1') {
        def workingDir = pwd()
        buildName "#${env.BUILD_ID} - ${branchName}"
        
        stage('show windows move problem') {
            dir("test") {
                dir('notEmpty') {
                        bat "echo hi >file"
                }
                bat "echo he > .filedings"
                bat "echo hoho > xma.s"
            }
            def content 
            dir('dest') {
                bat "move \"${workingDir}\\test\\*.*\" ."
                content = getFolderContent(pwd())
            }
            check(['notEmpty', '.filedings', 'xma.s'], content, false)
            //As you can see in the console log, content (aka actual) just contains .filedings and xma.s
            //So windows move does not move subfolders
            deleteDir()
        }


        stage('cleanup') {
            deleteDir()
        }

        stage("LeanFT: Linux-outside-docker-notempty-WQM-4795") {
            def dirOne = "${pwd()}\\linux-outside-notempty"
            def targetFolder = "${dirOne}\\TESTCASE_HYPERLINKING"
            def dirTwo = "${targetFolder}\\notEmptyFolder"
            def file = "${targetFolder}\\notEmptyFile.txt"
            def relGitProjectLink = "/innowake-test-projects/legacy-devops-test-projects/ndt/natcreator/testcase-hyperlinking.git"
            def absGitProjectLink = "${gitUtils.getGitUrlQef()}${relGitProjectLink}"
            def expectedAdditionalFiles = getGitProjectContent(absGitProjectLink)
            
            dir(targetFolder) {
                dir(dirTwo) {
                    bat "echo hi >${file}"
                }
                bat "Rmdir /Q /S \"${dirTwo}@tmp\""
                def contentBeforeCheckout = getFolderContent(targetFolder)

                gitUtils.checkoutGitProject(targetFolder, absGitProjectLink)
                def actualFiles = getFolderContent(targetFolder)
                //compare additional files
                def actualAdditionalFiles = actualFiles - contentBeforeCheckout
                def commonAdditionalFiles = expectedAdditionalFiles.intersect(actualAdditionalFiles)
                //the common items of expected additional and actual additional should be the same as the expected items
                check(expectedAdditionalFiles, commonAdditionalFiles)

                def actualAdditionalFilesDiff = expectedAdditionalFiles.plus(actualAdditionalFiles)
                actualAdditionalFilesDiff.removeAll(commonAdditionalFiles)

                //there should be no diff between expected file list from repro and the actual file list after the checkout
                check([], actualAdditionalFilesDiff)

                //compare working dir structure
                def expectedContentAfterCheckout = (contentBeforeCheckout + expectedAdditionalFiles).sort()
                def actualCommonFiles = expectedContentAfterCheckout.intersect(actualFiles)
                check(expectedContentAfterCheckout, actualCommonFiles)

                def actualFilesDiff = expectedContentAfterCheckout.plus(actualFiles)
                actualFilesDiff.removeAll(actualCommonFiles)
                check([], actualFilesDiff)

                if (currentBuild.currentResult == 'UNSTABLE') {
                    echo(
                        "expectedAdditionalFiles=${expectedAdditionalFiles}\nactual=${actualAdditionalFiles}\ncommon=${commonAdditionalFiles}\ndiff=${actualAdditionalFilesDiff}\n" + 
                        "expectedContentAfterCheckout=${expectedContentAfterCheckout}\nactual=${actualFiles}\ncommon=${actualCommonFiles}\ndiff=${actualFilesDiff}\n"
                    )
                }
            }

            if (! miscUtils.directoryExists(targetFolder)) {
                unstable "${targetFolder} could not be found!"
            } else if (! miscUtils.directoryExists(dirTwo)) {
                unstable "${targetFolder} could not be found!"
            } else if (! miscUtils.fileExists(file)) {
                unstable "${file} could not be found!"
            } else if (miscUtils.directoryExists("${targetFolder}/.git")) {
                unstable ".git folder was not deleted!"
            }
        }
    }

    node('Docker-host') {
        def workingDir = pwd()
        def paths = [pwd(), '.', './subfolder']

        paths.each { path ->

            stage('cleanup') {
                deleteDir()
            }

            stage("Linux-outside-docker-${path}") {
                def dirOne = "${pwd()}/linux-outside"
                dir(dirOne) {
                    gitUtils.checkoutGitProject(path, "${gitUtils.getGitUrlQef()}/release-management/automated-sharepoint-upload.git")
                }

                if (! miscUtils.directoryExists(dirOne)) {
                    unstable "${dirOne} could not be found!"
                } else if (miscUtils.directoryExists("${dirOne}/.git")) {
                    unstable ".git folder was not deleted!"
                }
            }

            stage("Linux-outside-docker-notempty-${path}") {
                def dirOne = "${pwd()}/linux-outside-notempty"
                def dirTwo = "${dirOne}/notEmptyFolder"
                def file = "${dirTwo}/notEmptyFile.txt"
                dir(dirOne) {
                    dir(dirTwo) {
                        sh "echo hi >${file}"
                    }
                    gitUtils.checkoutGitProject(path, "${gitUtils.getGitUrlQef()}/release-management/automated-sharepoint-upload.git")
                }

                if (! miscUtils.directoryExists(dirOne)) {
                    unstable "${dirOne} could not be found!"
                } else if (! miscUtils.directoryExists(dirTwo)) {
                    unstable "${dirTwo} could not be found!"
                } else if (! miscUtils.fileExists(file)) {
                    unstable "${file} could not be found!"
                } else if (miscUtils.directoryExists("${dirOne}/.git")) {
                    unstable ".git folder was not deleted!"
                }
            }

            stage("Linux-inside-docker-${path}") {
                docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside {
                    def dirOne = "${pwd()}/linux-inside"
                    dir(dirOne) {
                        gitUtils.checkoutGitProject(path, "${gitUtils.getGitUrlQef()}/release-management/automated-sharepoint-upload.git")
                    }

                    if (! miscUtils.directoryExists(dirOne)) {
                        unstable "${dirOne} could not be found!"
                    } else if (miscUtils.directoryExists("${dirOne}/.git")) {
                        unstable ".git folder was not deleted!"
                    }
                }
            }

            stage("Linux-inside-docker-notempty-${path}") {
                docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside {
                    def dirOne = "${pwd()}/linux-inside-notempty"
                    def dirTwo = "${dirOne}/notEmptyFolder"
                    def file = "${dirTwo}/notEmptyFile.txt"
                    dir(dirOne) {
                        dir(dirTwo) {
                            sh "echo hi >${file}"
                        }
                        gitUtils.checkoutGitProject(path, "${gitUtils.getGitUrlQef()}/release-management/automated-sharepoint-upload.git")
                    }

                    if (! miscUtils.directoryExists(dirOne)) {
                        unstable "${dirOne} could not be found!"
                    } else if (! miscUtils.directoryExists(dirTwo)) {
                        unstable "${dirTwo} could not be found!"
                    } else if (! miscUtils.fileExists(file)) {
                        unstable "${file} could not be found!"
                    }
                }
            }
        }
    }

    node('OS-Windows') {
        def workingDir = pwd()
        def paths = [pwd(), '.', '.\\subfolder']

        paths.each { path ->

            stage('cleanup') {
                deleteDir()
            }

            stage("Windows-outside-docker-${path}") {
                def dirOne = "${workingDir}\\windows-outside"
                dir(dirOne) {
                    gitUtils.checkoutGitProject(path, "${gitUtils.getGitUrlQef()}/release-management/automated-sharepoint-upload.git")
                }

                if (! miscUtils.directoryExists(dirOne)) {
                    unstable "${dirOne} could not be found!"
                } else if (miscUtils.directoryExists("${dirOne}\\.git")) {
                    unstable ".git folder was not deleted!"
                }
            }

            stage("Windows-oustide-docker-notempty-${path}") {
                def dirOne = "${workingDir}\\windows-outside-notempty"
                def dirTwo = "${dirOne}\\notEmptyFolder"
                def file = "${dirTwo}\\notEmptyFile.txt"
                dir(dirOne) {
                    dir(dirTwo) {
                        bat "echo hi >${file}"
                    }
                    gitUtils.checkoutGitProject(path, "${gitUtils.getGitUrlQef()}/release-management/automated-sharepoint-upload.git")
                }

                if (! miscUtils.directoryExists(dirOne)) {
                    unstable "${dirOne} could not be found!"
                } else if (! miscUtils.directoryExists(dirTwo)) {
                    unstable "${dirTwo} could not be found!"
                } else if (! miscUtils.fileExists(file)) {
                    unstable "${file} could not be found!"
                } else if (miscUtils.directoryExists("${dirOne}\\.git")) {
                    unstable ".git folder was not deleted!"
                }
            }
        }
    }
}

def getFolderContent(folder) {
    def content
    dir(folder) {
        content =  bat(script:'dir /B', returnStdout: true).split('\n').collect{ it.trim() }
        //first line is empy and second line is workingDir + command which are irrelevant
        content -= content[0]
        content -= content[0]
    }
    return content.sort()
}

def getGitProjectContent(url, branch = 'master') {
    def projectContent
    dir('prepare-getGitProjectContent') {
        git branch: branch, credentialsId: 'USAppModQMUserSVC-User-PW', url: url
        projectContent = bat(script:'dir /B',returnStdout: true).split('\n').collect{ it.trim() }
        //first line is empy and second line is workingDir + command which are irrelevant
        projectContent -= projectContent[0]
        projectContent -= projectContent[0]
        deleteDir()
    }
    bat "Rmdir /Q /S \"prepare-getGitProjectContent@tmp\""
    return projectContent.sort()
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
