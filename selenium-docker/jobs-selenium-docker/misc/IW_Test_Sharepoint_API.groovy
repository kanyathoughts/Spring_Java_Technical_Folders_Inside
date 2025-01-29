def lib = library("TestUtils@${branchName}")

/**
 * Test suite for the sharepoint API.
 * https://iriseu.deloitte.com/browse/WQM-4768
 * Executed twice:
 * - on Linux inside a container
 * - on Windows without container
 *
 * @param branchName Name of the branch where changes are stored, default: master
 */

timestamps {
	def dockerUtils = lib.DockerUtils.new()
	def miscUtils = lib.MiscUtils.new()
	
	node('OS-Linux && Docker-host') {
		buildName "#${env.BUILD_ID} - ${branchName}"
		
		deleteDir()
		docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
			runTestSuite('Linux')
		}
	}
	
	node('OS-Windows') {
		deleteDir()
		runTestSuite('Windows')
	}
}


def runTestSuite(os) {
	def spUtils = lib.SharepointUtils.new()
	def spTestBaseFolder = '/Shared Documents/share/QM - Quality Management/sharepoint-api-java/tests/'
	
	/*
	 * Tests for (non-)existence of files and folders. 
	 */
    stage("${os} existing-files-folders") {
		def report = []
		
		catchError(buildResult: 'UNSTABLE', catchInterruptions: false, message: 'Tests of existing files and folders failed', stageResult: 'UNSTABLE') {
			/* Base folder should exist. */
			report += runTest(true, "folderExists(\"${spTestBaseFolder}\")") {
				spUtils.folderExists(spTestBaseFolder)
			}
			/* Base folder should contain no files. */
			report += runTest([] as Set, "listFiles(\"${spTestBaseFolder}\")") {
				spUtils.listFiles(spTestBaseFolder) as Set
			}
			
			/* A subfolder that should not exist. */
			report += runTest(false, "folderExists(\"${spTestBaseFolder}thisfolderdoesnotexist/\")") {
				spUtils.folderExists("${spTestBaseFolder}/thisfolderdoesnotexist/")
			}
			/* A file that should not exist. */
			report += runTest(false, "fileExists(\"${spTestBaseFolder}thisfiledoesnotexist.txt\")") {
				spUtils.fileExists("${spTestBaseFolder}/thisfiledoesnotexist.txt")
			}
			
			/* Certain files and subfolders should exists. */
			def spFolder = "${spTestBaseFolder}listTests/"
			report += runTest(['this is a file.txt', 'this is a second file.txt'] as Set, "listFiles(\"${spFolder}\")") {
				spUtils.listFiles(spFolder) as Set
			}
			report += runTest(['this is a folder', 'this is a second folder'] as Set, "listFolders(\"${spFolder}\")") {
				spUtils.listFolders(spFolder) as Set
			}
			report += runTest(['this is a file.txt', 'this is a second file.txt', 'this is a folder', 'this is a second folder'] as Set, "listFilesAndFolders(\"${spFolder}\")") {
				spUtils.listFilesAndFolders(spFolder) as Set
			}
			report += runTest(true, "fileExists(\"${spFolder}this is a file.txt\")") {
				spUtils.fileExists("${spFolder}this is a file.txt")
			}
			report += runTest(true, "folderExists(\"${spFolder}this is a folder/\")") {
				spUtils.folderExists("${spFolder}this is a folder/")
			}
		}
		
		echo "TEST RESULTS - ${os} - existing-files-folders\n${report.join('\n')}"
    }
	
	/*
	 * Creation of files and folders
	 */
    stage("${os} create-files-folders") {
		def subFolder = 'createTests/'
		def spFolder = "${spTestBaseFolder}${subFolder}"
		def report = []
		
		/* Preparation: delete the test folder in Sharepoint. It might already exist, e.g. from tests before, and shall not disturb the following tests. */
		spUtils.deleteFolder(spFolder)
		
		catchError(buildResult: 'UNSTABLE', catchInterruptions: false, message: 'Tests of creation of files and folders failed', stageResult: 'UNSTABLE') {
			/* The test folder should not exist. */
			report += runTest(false, "folderExists(\"${spFolder}\")") {
				spUtils.folderExists(spFolder)
			}
			/* Create folder. */
			report += runTest(true, "createFolder(\"${spTestBaseFolder}\", \"${subFolder}\")") {
				spUtils.createFolder(spTestBaseFolder, subFolder)
			}
			/* Folder should exist now. */
			report += runTest(true, "folderExists(\"${spFolder}\")") {
				spUtils.folderExists(spFolder)
			}
			/* Folder should be empty. */
			report += runTest([] as Set, "listFilesAndFolders(\"${spFolder}\")") {
				spUtils.listFilesAndFolders(spFolder) as Set
			}
			/* Create some subfolders. */
			report += runTest(true, "createFolder(\"${spFolder}\", \"a/\")") {
				spUtils.createFolder(spFolder, 'a/')
			}
			report += runTest(true, "createFolder(\"${spFolder}\", \"bbb/\")") {
				spUtils.createFolder(spFolder, 'bbb/')
			}
			report += runTest(true, "createFolder(\"${spFolder}\", \"a sub folder/\")") {
				spUtils.createFolder(spFolder, 'a sub folder/')
			}
			
			/* Folder should not be empty anymore. */
			report += runTest(['a', 'bbb', 'a sub folder'] as Set, "listFilesAndFolders(\"${spFolder}\")") {
				spUtils.listFilesAndFolders(spFolder) as Set
			}
		}
		
		echo "TEST RESULTS - ${os} - create-files-folders\n${report.join('\n')}"
    }
    
	/*
	 * Upload and download of files
	 */
    stage("${os} upload-download-files") {
		def spFolder = "${spTestBaseFolder}uploadTests/"
		def report = []
		
		/* Setup */
		cleanSharepointFolder(spFolder)
		
		catchError(buildResult: 'UNSTABLE', catchInterruptions: false, message: 'Tests of file upload/download failed', stageResult: 'UNSTABLE') {
			/* Create a small file, upload it, download it again and compare the content. */
			if (isUnix()) {
				sh 'mkdir upload; mkdir download'
			} else {
				bat 'mkdir upload; mkdir download'
			}
			def fileContentBefore = 'This is\nthe content\nof this\nsmall test file.'
			writeFile file: 'upload/this is a small file.txt', text: fileContentBefore
			
			report += runTest(true, "uploadFile(\"${spFolder}\", \"upload/this is a small file.txt\")") {
				spUtils.uploadFile(spFolder, 'upload/this is a small file.txt')
			}
			report += runTest(true, "downloadFile(\"${spFolder}this is a small file.txt\", \"download/\")") {
				spUtils.downloadFile("${spFolder}this is a small file.txt", 'download/')
			}
			
			def fileContentAfter = readFile 'download/this is a small file.txt'
			if (fileContentAfter != fileContentBefore) {
				report += "NOK file content before upload and after download differ"
				unstable "file content before upload and after download differ"
			} else {
				report += "OK  file content before upload and after download match"
			}
			
			/* Check existence of file */
			report += runTest(true, "fileExists(\"${spFolder}this is a small file.txt\")") {
				spUtils.fileExists("${spFolder}this is a small file.txt")
			}
			
			/* Check folder content */
			report += runTest([] as Set, "listFolders(\"${spFolder}\")") {
				spUtils.listFolders(spFolder) as Set
			}
			report += runTest(['this is a small file.txt'] as Set, "listFiles(\"${spFolder}\")") {
				spUtils.listFiles(spFolder) as Set
			}
			report += runTest(['this is a small file.txt'] as Set, "listFilesAndFolders(\"${spFolder}\")") {
				spUtils.listFilesAndFolders(spFolder) as Set
			}
			
			/* Download into a non-existing local target folder, should fail */
			report += runTest(false, "downloadFile(\"${spFolder}this is a small file.txt\", \"nonexistingtargetfolder/\")") {
				spUtils.downloadFile("${spFolder}this is a small file.txt", 'nonexistingtargetfolder/')
			}
			
			/* Cleanup Sharepoint test folder */
			report += runTest(true, "deleteFile(\"${spFolder}this is a small file.txt\")") {
				spUtils.deleteFile("${spFolder}this is a small file.txt")
			}
			
			/* Test large volume download */
			def downloadFolder = "${spTestBaseFolder}downloadTests/"
			report += runTest(true, "downloadFile(\"${downloadFolder}this is a large file.zip\", \"./\")") {
				spUtils.downloadFile("${downloadFolder}this is a large file.zip", './')
			}
			
			/* Download non-existing file, should fail */
			report += runTest(false, "downloadFile(\"${downloadFolder}thisfiledoesnotexist.txt\", \"./\")") {
				spUtils.downloadFile("${downloadFolder}thisfiledoesnotexist.txt", './')
			}
			report += runTest(false, "downloadFile(\"${downloadFolder}nonexistingsubfolder/thisfiledoesnotexist.txt\", \"./\")") {
				spUtils.downloadFile("${downloadFolder}nonexistingsubfolder/thisfiledoesnotexist.txt", './')
			}
		}
		
		echo "TEST RESULTS - ${os} - upload-download-files\n${report.join('\n')}"
    }
    
	/*
	 * Deletion of files and folders
	 */
    stage("${os} delete-files-folders") {
		def spFolder = "${spTestBaseFolder}deleteTests/"
		def report = []
		
		/* Create some files and folders */
		spUtils.createFolder(spFolder, 'a/')
		spUtils.createFolder(spFolder, 'this is a folder/')
		writeFile file: 'this is a file.txt', text: 'This is\nthe content\nof this\nsmall test file.'
		writeFile file: 'this is another file.txt', text: 'This is\nthe content\nof this\nsmall test file.'
		spUtils.uploadFile(spFolder, './this is a file.txt')
		spUtils.uploadFile(spFolder, './this is another file.txt')
		spUtils.uploadFile("${spFolder}this is a folder/", './this is a file.txt')
		
		catchError(buildResult: 'UNSTABLE', catchInterruptions: false, message: 'Tests of deletion of files and folders failed', stageResult: 'UNSTABLE') {
			/* Verify files/folders before any deletions */
			report += runTest(['a', 'this is a folder', 'this is a file.txt', 'this is another file.txt'] as Set, "listFilesAndFolders(\"${spFolder}\")") {
				spUtils.listFilesAndFolders(spFolder) as Set
			}
			report += runTest(['this is a file.txt'] as Set, "listFilesAndFolders(\"${spFolder}this is a folder/\")") {
				spUtils.listFilesAndFolders("${spFolder}this is a folder/") as Set
			}
			
			/* Delete a subfolder. The subfolder is not empty, the deletion nevertheless succeeds. */
			report += runTest(true, "deleteFolder(\"${spFolder}this is a folder/\")") {
				spUtils.deleteFolder("${spFolder}this is a folder/")
			}
			/* Check content of test folder. */
			report += runTest(['a', 'this is a file.txt', 'this is another file.txt'] as Set, "listFilesAndFolders(\"${spFolder}\")") {
				spUtils.listFilesAndFolders(spFolder) as Set
			}
			/* Delete a file. */
			report += runTest(true, "deleteFile(\"${spFolder}this is a file.txt\")") {
				spUtils.deleteFile("${spFolder}this is a file.txt")
			}
			/* Check content of test folder. */
			report += runTest(['a', 'this is another file.txt'] as Set, "listFilesAndFolders(\"${spFolder}\")") {
				spUtils.listFilesAndFolders(spFolder) as Set
			}
			/* Delete subfolder, delete file. */
			report += runTest(true, "deleteFolder(\"${spFolder}a/\")") {
				spUtils.deleteFolder("${spFolder}a/")
			}
			report += runTest(true, "deleteFile(\"${spFolder}this is another file.txt\")") {
				spUtils.deleteFile("${spFolder}this is another file.txt")
			}
			/* Test folder should be empty now. */
			report += runTest([], "listFilesAndFolders(\"${spFolder}\")") {
				spUtils.listFilesAndFolders(spFolder)
			}
		}
		
		echo "TEST RESULTS - ${os} - delete-files-folders\n${report.join('\n')}"
    }
    
	/*
	 * Move of folders
	 */
    stage("${os} move-folders") {
		def spFolder = "${spTestBaseFolder}moveTests/"
		def fromFolder = "${spFolder}from/"
		def toFolder = "${spFolder}to/"
		def report = []
		
		spUtils.deleteFolder(spFolder)
		spUtils.createFolder(spTestBaseFolder, 'moveTests/')
		spUtils.createFolder(spFolder, 'from/')
		spUtils.createFolder(fromFolder, 'subfolder1/')
		spUtils.createFolder(fromFolder, 'subfolder2/')
		writeFile file: 'a-text-file.txt', text: 'Some\narbitrary\ntext content'
		spUtils.uploadFile("${spFolder}from/", 'a-text-file.txt')
		
		catchError(buildResult: 'UNSTABLE', catchInterruptions: false, message: 'Tests of folder move failed', stageResult: 'UNSTABLE') {
			report += runTest(true, "moveFolder(\"${fromFolder}\", \"${toFolder}\")") {
				spUtils.moveFolder(fromFolder, toFolder)
			}
			report += runTest(false, "folderExists(\"${fromFolder}\"") {
				spUtils.folderExists(fromFolder)
			}
			report += runTest(true, "folderExists(\"${toFolder}\"") {
				spUtils.folderExists(toFolder)
			}
			report += runTest(['a-text-file.txt', 'subfolder1', 'subfolder2'] as Set, "listFilesAndFolders(\"${toFolder}\")") {
				spUtils.listFilesAndFolders(toFolder) as Set
			}
		}
		
		echo "TEST RESULTS - ${os} - move-folders\n${report.join('\n')}"
    }
    
    /*
     * Download Eclipse bundle
     */
    stage("${os} download-eclipse-bundle") {
		def report = []
		
		catchError(buildResult: 'UNSTABLE', catchInterruptions: false, message: 'Tests of Eclipse bundle download failed', stageResult: 'UNSTABLE') {
			/* Find an arbitrary available Eclipse version. */
			def versions = spUtils.listFolders("${spUtils.getJenkinsArtifactsDir()}/IW_Eclipse_Bundle_Build/")
			if (versions.size() == 0) {
				error "There are no versions of Eclipse bundles available in Sharepoint"
			}
			def version = versions[0]
			dir('download-eclipse-bundle-linux') {
				report += runTest(true, "downloadIwEclipseLinux(\"${version}\", './')") {
					spUtils.downloadIwEclipseLinux(version, './')
				}
				def files = findFiles glob: 'eclipse.ini,.eclipseproduct'
				if (files.size() < 2) {
					report += "NOK downloadIwEclipseLinux(\"${version}\", './') - should have automatically created eclipse.ini and .eclipseproduct but didn't"
					unstable 'download of Linux Eclipse bundled failed, or unzip failed'
				} else {
					report += "OK  downloadIwEclipseLinux(\"${version}\", './') - eclipse.ini and .eclipseproduct have been created"
				}
			}
			dir('download-eclipse-bundle-windows') {
				report += runTest(true, "downloadIwEclipseWindows(\"${version}\", './')") {
					spUtils.downloadIwEclipseWindows(version, './')
				}
				def files = findFiles glob: 'eclipse.ini,.eclipseproduct'
				if (files.size() < 2) {
					report += "NOK downloadIwEclipseWindows(\"${version}\", './') - should have automatically created eclipse.ini and .eclipseproduct but didn't"
					unstable 'download of Windows Eclipse bundled failed, or unzip failed'
				} else {
					report += "OK  downloadIwEclipseWindows(\"${version}\", './') - eclipse.ini and .eclipseproduct have been created"
				}
			}
			/* Download into a non-existing local target folder. Should create the target folder automatically. */
			def targetFolder = 'download-eclipse-bundle-linux-2/'
			report += runTest(true, "downloadIwEclipseLinux(\"${version}\", \"${targetFolder}\")") {
				spUtils.downloadIwEclipseLinux(version, targetFolder)
			}
			def eclipseFiles = findFiles glob: "${targetFolder}eclipse.ini,${targetFolder}.eclipseproduct"
			if (eclipseFiles.size() < 2) {
				report += "NOK downloadIwEclipseLinux(\"${version}\", \"${targetFolder}\") - should have automatically created ${targetFolder}eclipse.ini and ${targetFolder}.eclipseproduct but didn't"
				unstable 'download of Windows Eclipse bundled failed, or unzip failed'
			} else {
				report += "OK  downloadIwEclipseLinux(\"${version}\", \"${targetFolder}\") - ${targetFolder}eclipse.ini and ${targetFolder}.eclipseproduct have been created"
			}
			/* Download of a non-existing version -> should fail. The method should throw an error. */
			try {
				version = '999.999.999'
				report += runTest(false, "downloadIwEclipseLinux(\"${version}\", './')") {
					spUtils.downloadIwEclipseLinux(version, './')
				}
				unstable "download of a non-existing Eclipse version should have failed, but didn't"
			} catch (exc) {
				echo "error is expected here. exc = ${exc}"
				report += "OK  downloadIwEclipseLinux(\"${version}\", './') failed as expected"
			}
		}
		
		echo "TEST RESULTS - ${os} - download-eclipse-bundle\n${report.join('\n')}"
    }
    
    /*
     * Upload job artifacts
     */
    stage("${os} upload-job-artifacts") {
		def report = []
		def jobFolder = "${spUtils.getJenkinsArtifactsDir()}/${env.JOB_NAME}"
		def version = '22.2.0'
		def versionFolder = "${jobFolder}/${version}"
		
		/* Setup */
		/* Delete the job artifacts folder. */
		spUtils.deleteFolder("${jobFolder}/")
		/* Create two job artifacts and upload them. */
		if (isUnix()) {
			sh 'mkdir --parents tmp/uploadjobartifacts'
		} else {
			bat 'mkdir tmp\\uploadjobartifacts'
		}
		writeFile file: 'tmp/uploadjobartifacts/artifact1.txt', text: 'The content of the first artifact'
		writeFile file: 'tmp/uploadjobartifacts/artifact2.txt', text: 'The content of the second artifact'
		
		catchError(buildResult: 'UNSTABLE', catchInterruptions: false, message: 'Tests of job artifact upload failed', stageResult: 'UNSTABLE') {
			/* Make sure the job artifacts folder doesn't exist. It shouldn't by setup. */
			report += runTest(false, "folderExists(\"${jobFolder}/\")") {
				spUtils.folderExists("${jobFolder}/")
			}
			
			/* Upload the first job artifact. Respective folders and files have to exist afterwards. */
			report += runTest(true, "uploadJobArtifact(\"${version}\", \"tmp/uploadjobartifacts/artifact1.txt\")") {
				spUtils.uploadJobArtifact(version, 'tmp/uploadjobartifacts/artifact1.txt')
			}
			report += runTest(true, "folderExists(\"${jobFolder}/\")") {
				spUtils.folderExists("${jobFolder}/")
			}
			report += runTest([version] as Set, "listFilesAndFolders(\"${jobFolder}/\")") {
				spUtils.listFilesAndFolders("${jobFolder}/") as Set
			}
			report += runTest(true, "folderExists(\"${versionFolder}/\")") {
				spUtils.folderExists("${versionFolder}/")
			}
    		report += runTest(['artifact1.txt', 'README.txt'] as Set, "listFilesAndFolders(\"${versionFolder}/\")") {
    			spUtils.listFilesAndFolders("${versionFolder}/") as Set
			}
    		
			/* Upload the second job artifact. Two artifacts have to exist afterwards and one version folder. */
			report += runTest(true, "uploadJobArtifact(\"${version}\", \"tmp/uploadjobartifacts/artifact2.txt\")") {
				spUtils.uploadJobArtifact(version, 'tmp/uploadjobartifacts/artifact2.txt')
			}
			report += runTest([version] as Set, "listFilesAndFolders(\"${jobFolder}/\")") {
				spUtils.listFilesAndFolders("${jobFolder}/") as Set
			}
    		report += runTest(['artifact1.txt', 'artifact2.txt', 'README.txt'] as Set, "listFilesAndFolders(\"${versionFolder}/\")") {
    			spUtils.listFilesAndFolders("${versionFolder}/") as Set
			}
    
			/* Upload the first job artifact again. The existing folder should be renamed. */
			report += runTest(true, "uploadJobArtifact(\"${version}\", \"tmp/uploadjobartifacts/artifact1.txt\")") {
				spUtils.uploadJobArtifact(version, 'tmp/uploadjobartifacts/artifact1.txt')
			}
			report += runTest([version, "${version}___1"] as Set, "listFilesAndFolders(\"${jobFolder}/\")") {
				spUtils.listFilesAndFolders("${jobFolder}/") as Set
			}
    		report += runTest(['artifact1.txt', 'artifact2.txt', 'README.txt'] as Set, "listFilesAndFolders(\"${versionFolder}___1/\")") {
    			spUtils.listFilesAndFolders("${versionFolder}___1/") as Set
			}
    		report += runTest(['artifact1.txt', 'README.txt'] as Set, "listFilesAndFolders(\"${versionFolder}/\")") {
    			spUtils.listFilesAndFolders("${versionFolder}/") as Set
			}
    		
			/* Upload the second job artifact again. */
			report += runTest(true, "uploadJobArtifact(\"${version}\", \"tmp/uploadjobartifacts/artifact2.txt\")") {
				spUtils.uploadJobArtifact(version, 'tmp/uploadjobartifacts/artifact2.txt')
			}
			report += runTest([version, "${version}___1"] as Set, "listFilesAndFolders(\"${jobFolder}/\")") {
				spUtils.listFilesAndFolders("${jobFolder}/") as Set
			}
    		report += runTest(['artifact1.txt', 'artifact2.txt', 'README.txt'] as Set, "listFilesAndFolders(\"${versionFolder}/\")") {
    			spUtils.listFilesAndFolders("${versionFolder}/") as Set
			}
    		
			/* Upload the first job artifact a third time. This should trigger renaming of the existing version folder. */
			report += runTest(true, "uploadJobArtifact(\"${version}\", \"tmp/uploadjobartifacts/artifact1.txt\")") {
				spUtils.uploadJobArtifact(version, 'tmp/uploadjobartifacts/artifact1.txt')
			}
			report += runTest([version, "${version}___1", "${version}___2"] as Set, "listFilesAndFolders(\"${jobFolder}/\")") {
				spUtils.listFilesAndFolders("${jobFolder}/") as Set
			}
    		report += runTest(['artifact1.txt', 'artifact2.txt', 'README.txt'] as Set, "listFilesAndFolders(\"${versionFolder}___1/\")") {
    			spUtils.listFilesAndFolders("${versionFolder}___1/") as Set
			}
    		report += runTest(['artifact1.txt', 'artifact2.txt', 'README.txt'] as Set, "listFilesAndFolders(\"${versionFolder}___2/\")") {
    			spUtils.listFilesAndFolders("${versionFolder}___2/") as Set
			}
    		report += runTest(['artifact1.txt', 'README.txt'] as Set, "listFilesAndFolders(\"${versionFolder}/\")") {
    			spUtils.listFilesAndFolders("${versionFolder}/") as Set
			}
		}
		
		echo "TEST RESULTS - ${os} - upload-job-artifact\n${report.join('\n')}"
    }
    
}


/**
 * Executes a test. The Sharepoint API call to be executed is wrapped in a closure. A text line is returned that outlines the test result.
 * @param expected  Expected result
 * @param callString  A String that outlines the call (name and parameters).
 * @param testCall Closure that wraps the Sharepoint API call.
 */
def runTest(expected, callString, testCall) {
	def actual = testCall.call()
	echo "${callString} = ${actual}"
	def result
	if (actual != expected) {
		result = "NOK ${callString} = ${actual}, expected is ${expected}"
		unstable result
	} else {
		result = "OK  ${callString} = ${actual}"
	}
	return result
}

def cleanSharepointFolder(spFolder) {
	def spUtils = lib.SharepointUtils.new()
	def fileList = spUtils.listFiles(spFolder)
	for (file in fileList) {
		spUtils.deleteFile("${spFolder}${file}")
	}
	def folderList = spUtils.listFolders(spFolder)
	for (folder in folderList) {
		spUtils.deleteFolder("${spFolder}${folder}/")
	}
}
