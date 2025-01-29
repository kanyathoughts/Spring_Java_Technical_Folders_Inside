@Library('TestUtils') _

/**
 * Tests certain features of the Docker image for the Jenkins environment, that the environment has to provide.
 * 
 * @branch The jenkins-environment branch
 * @param javaVersion The Java version of the environment image to test
 * @param executeOn The Jenkins node the job will run on
 */

steps.node(executeOn) {
    timestamps {
        def dockerUtils = new DockerUtils()
		def imageName = dockerUtils.pullJenkinsEnvironmentImage(javaVersion)
		if (branch != 'master') imageName += "-${branch}"
		
		buildName "#${BUILD_NUMBER} - ${branch} - ${javaVersion} - ${executeOn}"

		stage('Init') {
   	        deleteDir()
   		}
		docker.image(imageName).inside('-v /media:/media -v /data/mxJars:/data/mxJars:rw -v jenkins-m2-repo:/var/m2_repo:rw') {
   		    stage('System-info') {
   		        stageSystemInfo()
   		    }
   		    stage('SVN Checkout') {
   		        stageSvnCheckout()
   		    }
   		    stage('Git Checkout') {
   		        stageGitCheckout()
   		    }
   		    stage('SVNutils') {
   		        stageSvnUtils()
   		    }
   		    stage('Linux-tools') {
   		        stageLinuxTools()
   		    }
   		    stage('Local-file-system-operations') {
   		        stageLocalFileSystemOps()
   		    }
   		    /*
   		     * Operations on a remote file system, at least write operations, are likely to fail. We don't expect them to work and therefore don't test them.
   		    stage('Remote-file-system-operations') {
   		        stageRemoteFileSystemOps()
   		    }
   		    */
   		    stage('Java') {
   		        stageJava()
   		    }
   		    stage('Ant') {
   		        stageAnt()
   		    }
   		    stage('Maven') {
   		        stageMaven()
   		    }
   		    stage('Sharepoint-access') {
   		    	stageSharepoint()
   		    }
   		    stage('Finalize') {
   		        archiveArtifacts allowEmptyArchive: true, defaultExcludes: false, artifacts: '**/*.html,**/*.xml,**/*.exec'
   		    }
	    }
    }
    
}

def stageSystemInfo() {
	catchError(buildResult: 'UNSTABLE', message: 'FAILURE - get sytem info failed', stageResult: 'UNSTABLE') {
		sh 'hostname -f'
		sh 'uname --all'
		sh 'id -a'
	}
}

def stageSvnCheckout() {
	catchError(buildResult: 'UNSTABLE', message: 'FAILURE - svn checkout failed', stageResult: 'UNSTABLE') {
	    def svnUtils = new SvnUtils()
		dir('./checkout') {
			checkout([
			    $class: 'SubversionSCM',
			    additionalCredentials: [],
			    excludedCommitMessages: '',
			    excludedRegions: '',
			    excludedRevprop: '',
			    excludedUsers: '',
			    filterChangelog: false,
			    ignoreDirPropChanges: false,
			    includedRegions: '',
			    locations: [[
			        credentialsId: 'User-QMSRV1-for-SVN',
			        depthOption: 'infinity',
			        ignoreExternalsOption: true,
			        local: '.',
			        remote: "${svnUtils.getSvnUrlQa()}/projects/Check_Jenkins_Installation"
			    ]],
			    workspaceUpdater: [$class: 'CheckoutUpdater']
			])
		}
		sh 'rm -r ./checkout'
	}
}

def stageGitCheckout() {
	catchError(buildResult: 'UNSTABLE', message: 'FAILURE - git checkout failed', stageResult: 'UNSTABLE') {
		def gitUtils = new GitUtils()
		dir('./checkout') {
			def projectUrl
			withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
				projectUrl = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/playground/check-jenkins-installation.git"
			}
			sh "git clone --branch master ${projectUrl}"
			dir('check-jenkins-installation') {
				sh 'git status'
			}
		}
		sh 'rm -r ./checkout'
	}
}

def stageSvnUtils() {
	catchError(buildResult: 'UNSTABLE', message: 'FAILURE - SvnUtils failed', stageResult: 'UNSTABLE') {
		def svnUtils = new SvnUtils()
		withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'svnPw', usernameVariable: 'svnUser')]) {
			svnUtils.svnExportRecursive("${svnUtils.getSvnUrlQa()}/projects/Check_Jenkins_Installation", '.', svnUser, svnPw)
		}
	}
}

def stageLinuxTools() {
	['cat --version', 'chmod --version', 'chown --version', 'cp --version', 'curl --version',
		'diff --version', 'echo test_echo', 'find --version', 'grep --version', 'gzip --version', 
		'gunzip --version', 'ls --version', 'mkdir --version', 'mkdir test && cd test && cd .. rm -d test', 
		'mv --version', 'rm --version', 'rsync --version', 'sed --version',
		'tail --version', 'tar --version', 'unzip -version', 'zip --version'].each {
    	def status = sh returnStatus: true, script: it
    	if (status != 0) {
    		unstable "FAILURE - Linux command failed: ${it}"
    	}
	}
}

def stageLocalFileSystemOps() {
	catchError(buildResult: 'UNSTABLE', message: 'FAILURE - something went wrong when working on the local file system', stageResult: 'UNSTABLE') {
		def workdir = pwd()
		echo "pipeline step pwd() = ${pwd()}"
		sh 'echo Linux command pwd = $(pwd)'

		sh 'mkdir -p ./dir1/a/b'
		sh 'echo hello1 > ./dir1/a/b/hello1.txt'
		sh 'cat ./dir1/a/b/hello1.txt'
		def fileContent = readFile './dir1/a/b/hello1.txt'
		echo "pipeline step readFile = ${fileContent}"

		sh "mkdir -p ${workdir}/dir2/a/b"
		sh "echo hello2 > ${workdir}/dir2/a/b/hello2.txt"
		sh "cat ${workdir}/dir2/a/b/hello2.txt"
		fileContent = readFile "${workdir}/dir2/a/b/hello2.txt"
		echo "pipeline step readFile = ${fileContent}"

		sh 'find . -name \'hello*.txt\''
		sh 'find dir* -name \'hello*.txt\''
		def filesFound = findFiles glob: 'dir*/**/hello*.txt'
		echo "pipeline step findFiles = ${filesFound}"

		dir('./dir1/a/b') {
			sh 'ls -la'
			sh 'cat hello1.txt'
			fileContent = readFile 'hello1.txt'
			echo "pipeline step readFile = ${fileContent}"
		}
		dir("${workdir}/dir2/a/b") {
			sh 'ls -la'
			sh 'cat hello2.txt'
			fileContent = readFile 'hello2.txt'
			echo "pipeline step readFile = ${fileContent}"
		}

		writeFile file: './hello3.txt', text: 'hello3'
		sh 'cat ./hello3.txt'
		sh 'ls -la'
		fileContent = readFile './hello3.txt'
		echo "pipeline step readFile = ${fileContent}"

		writeFile file: "${pwd()}/hello4.txt", text: 'hello4'
		sh "cat ${pwd()}/hello4.txt"
		sh 'ls -la'
		fileContent = readFile './hello4.txt'
		echo "pipeline step readFile = ${fileContent}"
	}
}

def stageJava() {
	catchError(buildResult: 'UNSTABLE', message: 'FAILURE - something went wrong with the java installation', stageResult: 'UNSTABLE') {
		dir('./java-jar') {
			sh 'java -version'
			sh 'echo "public class HelloWorld  { public static void main(String[] args) { System.out.print(\\"Hello World!\\");}}" > HelloWorld.java'
			sh 'javac HelloWorld.java'
			def output = sh returnStdout: true, script: 'java HelloWorld'
			echo output
			if (output != 'Hello World!') {
				unstable 'FAILURE - java command failed'
			}
			sh 'echo "Main-Class:  HelloWorld" > MANIFEST.MF'
			sh 'jar cvmf MANIFEST.MF HelloWorld.jar HelloWorld.class'
			output = sh returnStdout: true, script: 'java -jar HelloWorld.jar'
			echo output
			if (output != 'Hello World!') {
				unstable 'FAILURE - java/jar command failed'
			}
		}
		sh 'rm -r ./java-jar'
	}
}

def stageAnt() {
	withAnt(installation: 'Default') {
		catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with ant!', stageResult: 'UNSTABLE') {
			sh 'ant -version'
			def status = sh returnStatus: true, script: 'ant -version | grep \'version 1.10.12\''
			if (status != 0) {
	            unstable 'FAILURE - unexpected ant version'
			}
			sh 'ant -diagnostics'
		}
		catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with ant-contrib!', stageResult: 'UNSTABLE') {
			sh 'ant check-ant-contrib'
	    }
        catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with junit for ant!', stageResult: 'UNSTABLE') {
            sh 'ant -debug check-junit'
            junit allowEmptyResults: true, testResults: 'report/junit/**/*.xml'
        }
	    catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with junit or jacoco for ant!', stageResult: 'UNSTABLE') {
	    	sh 'ant -debug check-junit-with-jacoco'
	    	junit allowEmptyResults: true, testResults: 'report-with-jacoco/junit/**/*.xml'
	    }
        catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with jar for ant!', stageResult: 'UNSTABLE') {
            sh 'ant -debug check-jar'
        }
        catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with if and unless for ant', stageResult: 'UNSTABLE') {
            def javaNumber = javaVersion == 'java8' ? '1.8' : '1.11'
            sh "ant -debug -DjavaVersion=${javaNumber} check-if-unless"
        }
	}
}

def stageMaven() {
	catchError(buildResult: 'UNSTABLE', message: 'FAILURE - Something is wrong with the maven installation!', stageResult: 'UNSTABLE') {
		withMaven(maven: 'Default') {
			def status = sh returnStatus: true, script: '$MVN_CMD --version'
		    if (status != 0) {
		        unstable 'FAILURE - mvn --version failed'
		    }
			status = sh returnStatus: true, script: '$MVN_CMD --version | grep \'Apache Maven 3.5.0\''
            if (status != 0) {
                unstable 'FAILURE - unexpected Maven version'
            }
			status = sh returnStatus: true, script: '$MVN_CMD help:effective-settings'
			if (status != 0) {
                unstable 'FAILURE - mvn help:effective-settings failed'
			}
            status = sh returnStatus: true, script: '$MVN_CMD help:effective-settings | grep \'<localRepository>/var/m2_repo</localRepository>\''
            if (status != 0) {
                unstable 'FAILURE - local repository in effective Maven settings not set correctly, should be /var/m2_repo. Find actual value in output above.'
            }
            status = sh returnStatus: true, script: '$MVN_CMD -U dependency:resolve'
            if (status != 0) {
                unstable 'FAILURE - mvn -U dependency:resolve failed'
            }
            status = sh returnStatus: true, script: '$MVN_CMD clean test jacoco:report'
			publishHTML([reportDir: 'target/site/jacoco', reportFiles: 'index.html', reportName: 'JaCoCo Report'])
		}
	}
}

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
