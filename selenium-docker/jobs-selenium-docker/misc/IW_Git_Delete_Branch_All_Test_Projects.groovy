@Library('TestUtils') _

/**
 * Deletes a branch to all in the job listed git projects. 
 * 
 * @param branchName     Branch with this name will be deleted in all listed projects.
 */

node('Docker-host && Region-US') {
	def unstableBranchDeletion = [:]
	def successfulBranchDeletion = [:]
    timestamps {
        stage('init') {
            def gitUtils = new GitUtils()
            def dockerUtils = new DockerUtils()
            def relativeProjectPaths = [
                //'infrastructure/licenses.git',
                'innowake-test-projects/ui-test-projects/leanft.git',
                'innowake-test-projects/ui-test-projects/leanft-ld.git',
                'release-management/customer-txtfiles.git',
				
                'innowake-test-projects/transformation-test-projects/batch-unit-test.git',
                'innowake-test-projects/transformation-test-projects/cics-test-suite.git',
                'innowake-test-projects/transformation-test-projects/exec-sql.git',
                'innowake-test-projects/transformation-test-projects/kidicap/kidicap.git',
                'innowake-test-projects/transformation-test-projects/kidicap/kidicap-184.git',
                'innowake-test-projects/transformation-test-projects/kidicap/kidicap-mini.git',
                'innowake-test-projects/transformation-test-projects/license-validation.git',
                'innowake-test-projects/transformation-test-projects/mannheimer/mannheimer.git',
                'innowake-test-projects/transformation-test-projects/mannheimer/mannheimer-script-signing.git',
                'innowake-test-projects/transformation-test-projects/testcase-cobol-async-api.git',
                'innowake-test-projects/transformation-test-projects/testcase-cobol-async-api-csharp.git',
                'innowake-test-projects/transformation-test-projects/customer-zero/customer-zero-java.git',
                'innowake-test-projects/transformation-test-projects/customer-zero/customer-zero-csharp.git',
                'innowake-test-projects/transformation-test-projects/pl1-test.git',
                'innowake-test-projects/transformation-test-projects/nmslo.git',
                'innowake-test-projects/transformation-test-projects/ims/ims-db-csharp.git',
                'innowake-test-projects/transformation-test-projects/ims/ims-db-java.git',
                'innowake-test-projects/transformation-test-projects/ims/ims-migration.git',
                'innowake-test-projects/transformation-test-projects/ims/ims-tm-csharp.git',
                'innowake-test-projects/transformation-test-projects/ims/ims-tm-java.git',
				'innowake-test-projects/transformation-test-projects/cobol-java-test',
		        
                'innowake-test-projects/legacy-devops-test-projects/base/expert.git',
                'innowake-test-projects/legacy-devops-test-projects/base/expert-view-demo.git',
                'innowake-test-projects/legacy-devops-test-projects/base/fieldtracing.git',
                'innowake-test-projects/legacy-devops-test-projects/mdd/gui-importer.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/jcl-test.git',
                'innowake-test-projects/legacy-devops-test-projects/mdd/maxenso-example/maxenso-example-documentation.git',
                'innowake-test-projects/legacy-devops-test-projects/mdd/maxenso-example/maxenso-example-swing.git',
                'innowake-test-projects/legacy-devops-test-projects/mdd/maxenso-example/maxenso-example-vaadin.git',
                'innowake-test-projects/legacy-devops-test-projects/base/mig-training.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/read-only-modules.git',
                'innowake-test-projects/legacy-devops-test-projects/mdd/ria/ria-vaadin.git',
                'innowake-test-projects/legacy-devops-test-projects/mdd/ria/ria-swing.git',
                'innowake-test-projects/legacy-devops-test-projects/mdd/ria/ria-core.git',
                'innowake-test-projects/legacy-devops-test-projects/sc/sc-encryption-tests.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-cobol-bms-map.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-cobol-opensystems.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-code-completion.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-cobol-syntax-highlighting.git',				
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-code-completion-natural.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-crosslibrary-dependency.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/testcase-db-maintenance.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-editor-syntaxhighlighting.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natcreator/testcase-hyperlinking.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-illegal-characters.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-iris-1466.git',
                'innowake-test-projects/legacy-devops-test-projects/base/testcase-iris-6867.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/testcase-jmap-editor.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-line-numbers.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-line-references.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-local-steplibs.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-cobol.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/testcase-mee.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/testcase-mee-jcpy.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/testcase-mee-steplibs.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-natanalyzer.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/cobolclipse/testcase-qs.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/tests-qs.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-sql-statements.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-substitutionsreferenzen.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-svn-lock.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-target-propertypage-readonly.git',
                'innowake-test-projects/legacy-devops-test-projects/mdd/tooltips/testcase-tooltips-vaadin.git',
                'innowake-test-projects/legacy-devops-test-projects/mdd/tooltips/testcase-tooltips-swing.git',
                'innowake-test-projects/legacy-devops-test-projects/mdd/tooltips/testcase-tooltips-core.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natclipse/testcase-upload-compile-dependencies.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/testcase-vaadin.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/testcase-vaadin-print.git',
                'innowake-test-projects/legacy-devops-test-projects/ndt/natcreator/tests-nc-qs.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/wmee-4757.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/wmee-8009.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/wmee-8814.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/wmee-7231-target.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/wmee-7231-src.git',
                'innowake-test-projects/legacy-devops-test-projects/mee/testcase-cics.git',
                'innowake-test-projects/legacy-devops-test-projects/sc/soa-connector-open-system.git',
				
                'innowake-test-projects/transformation-test-projects/cagen/integration-tests-mssql.git',
                'innowake-test-projects/transformation-test-projects/cagen/functional-tests-mssql.git',
                'innowake-test-projects/transformation-test-projects/cagen/integration-tests-oracle.git',
                'innowake-test-projects/transformation-test-projects/cagen/functional-tests-oracle.git'
				'innowake-test-projects/transformation-test-projects/cagen/cagen-ui.git',
                
                // artifacts tested by them are only available on trunk:
                //'innowake-test-projects/transformation-test-projects/natural-test/natural-csharp-vontobelSt.git',
                //'innowake-test-projects/transformation-test-projects/natural-test/natural-migration.git',
                //'innowake-test-projects/transformation-test-projects/natural-test/natural-db-backup.git',
                'innowake-test-projects/transformation-test-projects/small-csharp-tests/torpedo-test.git'
            ]
            buildName "#${env.BUILD_ID} - ${branchName}"
            buildDescription "branchName=${branchName}"
			
            stage('delete branch and push') {
                timeout (time: 60, unit: 'SECONDS') {
                    input "The branch ${branchName} will be deleted in all projects. Only proceed here if you are sure!"
                }
                for (relativeProjectPath in relativeProjectPaths) {
                    deleteDir()
                    def project
					try{
						docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside('-v /data/mxJars:/data/mxJars:ro') {
							try{
								withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
									project = "${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/${relativeProjectPath}"
									def cloneReturnStatus = sh returnStatus: true, script: "git clone ${project} ."
								}
								sh "git push origin --delete ${branchName}"
								echo 'Finished deleting remote branch.'
								successfulBranchDeletion.put(relativeProjectPath, branchName)
							} catch(e) {
								unstable 'Branch does not exist.'
								unstableBranchDeletion.put(relativeProjectPath, branchName)
							}
						}
					} catch(e) {
						unstable e.getMessage()
				 	}
				}
            }
        }
    }

	stage('process results') {
		echo 'Successful branch deletion'
		echo successfulBranchDeletion.toString()
		echo 'Unstable branch deletion'
		echo unstableBranchDeletion.toString()
	}
}
