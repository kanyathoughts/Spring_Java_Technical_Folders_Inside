@Library('TestUtils') _

/**
 * Tests certain features of the environment that needs to be configured on a windows system.
 * 
 * @param javaVersion The Java version that will be used to run the java tests
 * @param executeOn The Jenkins node the job will run on
 */

steps.node(executeOn) {
    timestamps {
        
        buildName "#${BUILD_NUMBER} - ${javaVersion} - ${executeOn}"
        
        deleteDir()
        
        stage('nuget') {
            stageNuget()
        }

        stage('dotnet') {
            stageDotNet()
        }
        
        stage('msbuild') {
            stageMsBuild()
        }

        stage('Web development build tools') {
            stageWebDev()
        }

        stage('bcp') {
            stageBcp()
        }

        stage('.NET Framework') {
            stageNetFramework()
        }

        stage('system info') {
            stageSystemInfo()
        }

        stage('svn checkout') {
            stageSvnCheckout()
        }

        stage('git checkout') {
            stageGitCheckout()
        }

        stage('shared libraries') {
            stageSharedLibaries()
        }

        stage('windows tools') {
            stageWindowsTools()
        }

        stage('local file system ops') {
            stageLocalFileSystemOps()
        }

        stage('java') {
            stageJava()
        }

        stage('ant') {
            stageAnt()
        }
        
        stage('maven') {
            stageMaven()
        }

        stage('docker') {
            stageDocker()
        }

        stage('curl') {
            stageCurl()
        }

        stage('sharepoint') {
            stageSharepoint()
        }
    }
}

/**
* Checks if the minimum installed version of .Net Framework is 4.6.1 or greater.
*/
def stageNetFramework() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE -.Net Framework failed', stageResult: 'UNSTABLE') {
        //details: https://docs.microsoft.com/de-de/dotnet/framework/migration-guide/how-to-determine-which-versions-are-installed
        int installedVersion = powershell returnStdout: true, script: '(Get-ItemProperty "HKLM:\\SOFTWARE\\Microsoft\\NET Framework Setup\\NDP\\v4\\Full").Release'
        def expectedVersion = 394254  
        assert installedVersion >= expectedVersion
    }
}

/**
* Checks if the installation of Web Development build tools was successfull.
*/
def stageWebDev() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Web development build tools failed', stageResult: 'UNSTABLE') {
        /**
        * installation is something like vs_buildtools.exe --add Microsoft.VisualStudio.Workload.WebBuildTools
        * If installation was successfull the path C:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\MSBuild\Microsoft\VisualStudio\v16.0\WebApplications should exist
        * AND contains the needed files:
        * - Microsoft.WebApplication.Build.Tasks.dll
        * - Microsoft.WebApplication.targets
        * see: https://stackoverflow.com/a/45341457
        */
        bat 'dir "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\MSBuild\\Microsoft\\VisualStudio\\v16.0\\WebApplications\\"'
        bat 'dir "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\MSBuild\\Microsoft\\VisualStudio\\v16.0\\WebApplications\\Microsoft.WebApplication.Build.Tasks.dll"'
        bat 'dir "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\MSBuild\\Microsoft\\VisualStudio\\v16.0\\WebApplications\\Microsoft.WebApplication.targets"'
    }
}

/**
*  Run bcp command test
*/
def stageBcp() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - bcp failed', stageResult: 'UNSTABLE') {
        bat 'bcp -v'
    }
}

def stageDotNet() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - dotnet failed', stageResult: 'UNSTABLE') {
        bat 'dotnet --version'
    }
}

/**
*  Run msbuild command test
*/
def stageMsBuild() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - msbuild failed', stageResult: 'UNSTABLE') {
        bat 'msbuild -version'
    }
}

/**
* Run nuget command test
*/
def stageNuget() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - nuget failed', stageResult: 'UNSTABLE') {
        bat 'nuget'
    }
}

def stageSystemInfo() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - get sytem info failed', stageResult: 'UNSTABLE') {
        bat 'hostname'
        bat 'whoami'
    }
}

/**
 * Run shared library tests
 */
def stageSharedLibaries() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - shared libaries failed', stageResult: 'UNSTABLE') {
        def svnUtils = new SvnUtils()
        echo svnUtils.getSvnUrlQm()
        def gitUtils = new GitUtils()
        echo gitUtils.getGitUrlQef()
        def mxVersionUtils = new MxVersionUtils()
        echo mxVersionUtils.getEclipseZip('21.0.0')
        def spUtils = new SharepointUtils()
        echo spUtils.getSiteUrl()
    }
}

/**
 * Run svn checkout tests
 */
def stageSvnCheckout() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - svn checkout failed', stageResult: 'UNSTABLE') {
        def svnUtils = new SvnUtils()
        def svnUrlProject = "${svnUtils.getSvnUrlQa()}/projects/Check_Jenkins_Installation"
        
        dir('stageSvnCheckout') {
            dir('commandLineSvn') {
                withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'svnPw', usernameVariable: 'svnUser')]) {
                    bat "svn --no-auth-cache --username ${svnUser} --password ${svnPw} --depth infinity --force export ${svnUrlProject} ."
                }
            }
            dir('checkoutStep') {
                checkout([
                    $class: 'SubversionSCM',
                    locations: [[
                        credentialsId: 'User-QMSRV1-for-SVN',
                        depthOption: 'infinity',
                        ignoreExternalsOption: true,
                        local: '.',
                        remote: svnUrlProject
                    ]],
                    workspaceUpdater: [$class: 'CheckoutUpdater']
                ])
            }
        }
    }
}

/**
 * Run git checkout tests
 */
def stageGitCheckout() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - git checkout failed', stageResult: 'UNSTABLE') {
        def gitUtils = new GitUtils()
        def relativeGitlabProjectPath = '/playground/check-jenkins-installation.git'
        def gitlabProject = "${gitUtils.getGitUrlQef()}${relativeGitlabProjectPath}"
        
        dir('stageGitCheckout') {
            dir('commandLineGit') {
                withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
                    def projectUrl = "."
                    bat "git clone --branch master ${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}${relativeGitlabProjectPath} ."
                    bat 'git status'
                }
            }
            dir('gitStep') {
                git branch: 'master', credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
                bat 'git status'
            }
            dir('checkoutStep') {
                checkout([
                    $class: 'GitSCM',
                    branches: [[name: '*/master']],
                    userRemoteConfigs: [[
                        credentialsId: 'USAppModQMUserSVC-User-PW',
                        url: gitlabProject
                    ]]
                ])
                bat 'git status'
            }
        }
    }
}

/**
 * Run different windows tools
 */
def stageWindowsTools() {
    ['xcopy /?'].each {
        def status = bat returnStatus: true, script: it
        if (status != 0) {
            unstable "FAILURE - windows command failed: ${it}"
        }
    }
}

/**
 * Run local file system operations
 */
def stageLocalFileSystemOps() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - something went wrong when working on the local file system', stageResult: 'UNSTABLE') {
        dir('stageLocalFileSystemOps') {
            def workdir = pwd()
            echo "pipeline step pwd() = ${pwd()}"
            def cdOutput = bat returnStdout: true, script: 'cd'
            echo "Windows command cd = ${cdOutput}"
    
            bat 'mkdir .\\dir1\\a\\b'
            bat 'echo hello1 > .\\dir1\\a\\b\\hello1.txt'
            bat 'type .\\dir1\\a\\b\\hello1.txt'
            def fileContent = readFile '.\\dir1\\a\\b\\hello1.txt'
            echo "pipeline step readFile = ${fileContent}"
    
            bat "mkdir ${workdir}\\dir2\\a\\b"
            bat "echo hello2 > ${workdir}\\dir2\\a\\b\\hello2.txt"
            bat "type ${workdir}\\dir2\\a\\b\\hello2.txt"
            fileContent = readFile "${workdir}\\dir2\\a\\b\\hello2.txt"
            echo "pipeline step readFile = ${fileContent}"
    
            def filesFound = findFiles glob: 'dir*/**/hello*.txt'
            echo "pipeline step findFiles = ${filesFound}"
    
            dir('./dir1/a/b') {
                bat 'dir'
                bat 'type hello1.txt'
                fileContent = readFile 'hello1.txt'
                echo "pipeline step readFile = ${fileContent}"
            }
            dir("${workdir}/dir2/a/b") {
                bat 'dir'
                bat 'type hello2.txt'
                fileContent = readFile 'hello2.txt'
                echo "pipeline step readFile = ${fileContent}"
            }
    
            writeFile file: './hello3.txt', text: 'hello3'
            bat 'type .\\hello3.txt'
            bat 'dir'
            fileContent = readFile './hello3.txt'
            echo "pipeline step readFile = ${fileContent}"
    
            writeFile file: "${pwd()}/hello4.txt", text: 'hello4'
            bat "type ${pwd()}\\hello4.txt"
            bat 'dir'
            fileContent = readFile './hello4.txt'
            echo "pipeline step readFile = ${fileContent}"
        }
    }
}

/**
 * Run java tests
 */
def stageJava() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - something went wrong with the java installation', stageResult: 'UNSTABLE') {
        dir('stageJava') {
           withJava(javaVersion) {
                bat 'java -version'
                bat 'echo public class HelloWorld  { public static void main(String[] args) { System.out.print(\"Hello World!\");}} > HelloWorld.java'
                bat 'javac HelloWorld.java'
                def output = bat returnStdout: true, script: 'java HelloWorld'
                echo output
                if (! output.endsWith('Hello World!')) {
                    unstable "FAILURE - java command failed"
                }
                bat 'echo Main-Class:  HelloWorld > MANIFEST.MF'
                bat 'jar cvmf MANIFEST.MF HelloWorld.jar HelloWorld.class'
                output = bat returnStdout: true, script: 'java -jar HelloWorld.jar'
                echo output
                if (! output.endsWith('Hello World!')) {
                    unstable "FAILURE - java/jar command failed"
                }
            }
        }
    }
}

/**
 * Run ant tests
 */
def stageAnt() {
    dir('ant') {
        def gitUtils = new GitUtils()
        withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
            bat "git clone --branch master ${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/playground/check-jenkins-installation.git ."
        }
        withAnt(installation: 'Default') {
            catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with ant!', stageResult: 'UNSTABLE') {
                def output = bat returnStdout: true, script: 'ant -version'
                echo output
                if (! output.contains('1.10.12')) {
                    unstable 'FAILURE - unexpected ant version'
                }
                bat 'ant -diagnostics'
            }
            catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with ant-contrib!', stageResult: 'UNSTABLE') {
                bat 'ant check-ant-contrib'
            }
            catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with junit for ant!', stageResult: 'UNSTABLE') {
                bat 'ant -debug check-junit'
                junit allowEmptyResults: true, testResults: 'report/junit/**/*.xml'
            }
            catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with junit or jacoco for ant!', stageResult: 'UNSTABLE') {
                bat 'ant -debug check-junit-with-jacoco'
                junit allowEmptyResults: true, testResults: 'report-with-jacoco/junit/**/*.xml'
            }
            catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with jar for ant!', stageResult: 'UNSTABLE') {
                bat 'ant -debug check-jar'
            }
            catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with if and unless for ant', stageResult: 'UNSTABLE') {
                def javaNumber = javaVersion == 'java8' ? '1.8' : '1.11'
                bat "ant -debug -DjavaVersion=${javaNumber} check-if-unless"
            }
        }
    }
}

/**
 * Run maven tests
 */
def stageMaven() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with the maven installation!', stageResult: 'UNSTABLE') {
        dir('maven') {
            def gitUtils = new GitUtils()
            withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
                bat "git clone --branch master ${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/playground/check-jenkins-installation.git ."
            }
            withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
                def status = bat returnStatus: true, script: 'mvn --version'
                if (status != 0) {
                    unstable 'FAILURE - mvn --version failed'
                }
                status = bat returnStatus: true, script: 'mvn --version | findstr -i "Apache Maven 3.5.0"'
                if (status != 0) {
                    unstable 'FAILURE - unexpected Maven version'
                }
                status = bat returnStatus: true, script: 'mvn help:effective-settings'
                if (status != 0) {
                    unstable 'FAILURE - mvn help:effective-settings failed'
                }
                status = bat returnStatus: true, script: 'mvn help:effective-settings | findstr -i "<localRepository>E:\\m2_repo\\repository</localRepository>"'
                if (status != 0) {
                    unstable "FAILURE - local repository in effective Maven settings not set correctly, should be E:\\m2_repo\\repository. Find actual value in output above."
                }
                status = bat returnStatus: true, script: 'mvn -U dependency:resolve'
                if (status != 0) {
                    unstable 'FAILURE - mvn -U dependency:resolve failed'
                }
                status = bat returnStatus: true, script: 'mvn clean test jacoco:report'
                publishHTML([reportDir: 'target/site/jacoco', reportFiles: 'index.html', reportName: 'JaCoCo Report'])
            }
        }
    }
}

/**
 *  Run docker command test
 */
def stageDocker() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - docker failed', stageResult: 'UNSTABLE') {
        bat 'docker ps'
    }
}

/**
 *  Run curl command test
 */
def stageCurl() {
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - curl failed', stageResult: 'UNSTABLE') {
        bat 'curl --version'
    }
}

/**
 * Run Sharepoint access tests
 */
def stageSharepoint() {
	def sharepointUtils = new SharepointUtils()
    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with the Sharepoint access!', stageResult: 'UNSTABLE') {
    	def spFolder = '/Shared Documents/share/QM - Quality Management/'
    	def spResult = sharepointUtils.folderExists(spFolder)
    	if ( ! spResult) {
    		unstable "FAILURE - fileExists(\"spFolder\") = ${spResult}, should have succeeded"
    	}
    	def spFilesAndFolders = sharepointUtils.listFilesAndFolders(spFolder)
    	if (spFilesAndFolders.size() == 0 ) {
    		unstable "FAILURE - listFilesAndFolder(\"spFolder\") = ${spFilesAndFolders}, but should list some files and folders"
    	}
	}
}
