@Library('TestUtils') _

/**
 * Builds the documentation.
 * - Walks through all documentation folders,
 * - builds Eclipse help plugins from the HTML files,
 * - pushes the Eclipse help plugins to Nexus, where the product build of PD can fetch them,
 * - pushes the PDF documents to Nexus, where the product build of PD can fetch them.
 * Note that the SOA and Angular UI documentation exists as PDF only, and so only PDF documents are pushed to Nexus, but no Eclipse help. 
 * 
 * @param iwVersion  The innoWake version to build the documentation from. Contains strings like "innowake-21", "innowake-19-ir2", etc.
 */

node('Docker-host') {
    
    timestamps() {
        
        def gitUtils = new GitUtils()
        def miscUtils = new MiscUtils()
        def dockerUtils = new DockerUtils()
        
        def nexusUrlSnapshotsQm = 'http://triton.innowake.hq/nexus/content/repositories/snapshots-qm'
		
        buildName "#${env.BUILD_ID} - ${iwVersion}"

		try {
            deleteDir()
            docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                def documentationDir = "${pwd()}/wp-artifacts/${iwVersion}"
           		def eclipseHelpDir = 'eclipse-help/'
                
                stage('init') {
                    def docuUrl
                    withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
                        docuUrl = gitUtils.getGitUrlDocumentationArtifacts('USAppModQMUserSVC-Access-Token', gitlabToken)
                    }
                    sh "git clone --branch master ${docuUrl}"
                    withMaven(maven: 'Default') {
                        dir(documentationDir) {
                            sh "$MVN_CMD -U clean deploy"
                        }
                    }
                }
                
                def manualsFind = sh returnStdout: true, script: "find ${documentationDir} -maxdepth 4 -regextype posix-extended -regex '.*-manual/pom\\.xml' | xargs dirname"
                def manuals = manualsFind.split('\n') as ArrayList
				
                echo 'Manuals with pom.xml found:'
				echo manualsFind
                    
                manuals.each {
                    manualDir ->
                    def mdSplit = manualDir.split('/') as ArrayList
                    def stageName = "${mdSplit[mdSplit.size() - 2]}/${mdSplit[mdSplit.size() - 1]}"
                    stage(stageName) {
                        echo "Processing manual directory: ${manualDir}"
                        dir(manualDir) {
                            def pomFile = "${pwd()}/pom.xml"
                            def pomData =  readMavenPom file: pomFile
                            echo "Data from ${pomFile}: artifactId=${pomData.artifactId} groupdId=${pomData.groupId} version=${pomData.version}"
                            echo "Deploying manual ${manualDir} with maven"
                            withMaven(maven: 'Default') {
								/* Special handling for soa-connector documentation. Its PDF has to be deployed, but no Eclipse help. */
								if (! stageName.contains('soa-connector') && ! stageName.contains('angular-ui')) {
									echo 'Deploying Eclipse Help'
									sh "$MVN_CMD -U --file ${pomFile} -Declipse-help.directory=${eclipseHelpDir} clean deploy"
								}
									
                                def pdfFile = sh returnStdout: true, script: 'ls -1 *.pdf'
								// There is either exactly one PDF file or none at all, but never more than one.
								pdfFile = pdfFile.trim()
                                if (pdfFile) {
									// PDF files are named like 'user-manual_Batch_innoWake21.pdf' or ''Anwenderhandbuch_gui-importer_innoWake21.pdf'
									// artifact ID expected not to contain version
									echo 'Deploying PDF'
									def artifactId = pdfFile.substring(0, pdfFile.lastIndexOf('_'))
                                    sh "$MVN_CMD deploy:deploy-file -Durl=${nexusUrlSnapshotsQm} -DrepositoryId=snapshots-qm -Dfile=${pdfFile} -DgroupId=innowake.documentation -DartifactId=${artifactId} -Dversion=${pomData.version} -DgeneratePom=true"
                                }
                            }
                        }
                    }
                }
            }
        } catch (ex) {
            unstable "exception ${ex}"
            miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is failed", body: currentBuild.absoluteUrl], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
        }
    }
}
