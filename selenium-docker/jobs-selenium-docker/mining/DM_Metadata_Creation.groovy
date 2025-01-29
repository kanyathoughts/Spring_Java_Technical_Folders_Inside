/**
 * @param miningServerURL the mining environment into which the metadata will be inserted
 */
@Library('TestUtils') _

node('DM_MetaData') {

	timestamps {

		def gitUtils = new GitUtils()
		def jobWorkspace = pwd()
		def testProject = 'EclipseTestProject'
		def eclipseWorkspace = "${jobWorkspace}\\workspace"
		def runResultsDir = 'RunResults'
		def testProjectBranch = 'master'

		buildName "#${env.BUILD_ID} - mining.dbdumps.CreateMetaDataTest"

		stage('Checkout test project') {
			deleteDir()
			def gitlabProject = "${gitUtils.getGitUrlQef()}/innowake-test-projects/ui-test-projects/leanft.git"
			dir(testProject) {
				git branch: testProjectBranch, credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
			}
		}

		stage('Run Test') {
			try {
				timeout(time: 4, unit: 'HOURS') {
					def workspace = "${eclipseWorkspace}\\mining.dbdumps.CreateMetaDataTest"
					dir(workspace) { deleteDir() }
					dir(runResultsDir) { deleteDir() }
					echo "run test mining.dbdumps.CreateMetaDataTest"
					withMaven(maven: 'Default', mavenSettingsConfig: 'windows_maven_settings') {
						dir(testProject) { bat "mvn clean -DworkspaceDir=${workspace} -DminingServerURL=${miningServerURL} -Dtest=mining.dbdumps.CreateMetaDataTest test" }
					}
				}
			} catch (e) {
				unstable e.toString()
			}
		}

		stage('Archive run results') {
			dir(testProject) {
				archiveArtifacts allowEmptyArchive: true, artifacts: "${runResultsDir}/**/*"
				publishHTML allowMissing: true, alwaysLinkToLastBuild: true, keepAll: true, reportDir: runResultsDir, reportFiles: 'runresults.html', reportName: "mining.dbdumps.CreateMetaDataTest", reportTitles: "#${env.BUILD_ID} - mining.dbdumps.CreateMetaDataTest"
			}
		}
	}
}
