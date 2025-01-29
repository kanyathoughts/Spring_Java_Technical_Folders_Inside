import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
@Library('TestUtils') _
/**
 * This job runs all specified db cutter tests in a pipeline.
 * 
 * @param buildTag			The db cutter version/tag to use.
 * @param deploy			Whether to deploy to the staging server or not.
 * @param executeTestsOn		The node where to execute the UI tests.
 * @param executeEnvironmentOn		The node where the environment under test should be started.	
 */
node('MOD') {
	buildName "#${env.BUILD_ID} - ${buildTag}"
	buildDescription "buildTag=${buildTag} deploy=${deploy} executeTestsOn=${executeTestsOn} executeEnvironmentOn=${executeEnvironmentOn}"
	def miscUtils = new MiscUtils()
	def workDir = pwd()
	def environmentBuildResult
	def unstableStagesString = ''
	def failedStagesString = ''
	def totalTestAmount = 0
	def testCases = [
		'testcases.dbcutter.LicensingInvalidTest',
		'testcases.dbcutter.SmokeTest',
		'testcases.dbcutter.SmokeTestCrawler',
		'testcases.dbcutter.InsightsQueryPathsTest',
		'testcases.dbcutter.ProjectManagementTest',
		'testcases.dbcutter.BoundedContextsTest',
		'testcases.dbcutter.InsightsLegacyDatabaseTest',
		'testcases.dbcutter.CutByModuleTest',
		'testcases.dbcutter.CutByTableTest',
		'testcases.dbcutter.InsightsConflictsTest',
		'testcases.dbcutter.ModuleRelationsImporterTest',
		'testcases.dbcutter.EntitiesTest',
		'testcases.dbcutter.ToolsQueryEditorTest',
		'testcases.dbcutter.AccessPatternAnalysisTest',
		'testcases.dbcutter.ReportsTest',
		'testcases.dbcutter.SQLValidatorTest',
		'testcases.dbcutter.ClientManagementTest',
		'testcases.dbcutter.LegacyDBEditorTest',
		'testcases.dbcutter.ToolsUserSettingsTest',
        'testcases.dbcutter.ToolsMembersAdminRoleTest',
        'testcases.dbcutter.ToolsMembersEditorRoleTest',
		'testcases.dbcutter.ToolsMembersViewerRoleTest',
		'testcases.dbcutter.ToolsMembersManagerRoleTest',
        'testcases.dbcutter.MiningExtensionsExportTest',
        'testcases.dbcutter.MiningExtensionsImportTest',
		'testcases.dbcutter.MiningModuleDeepLinkingTest',
        'testcases.dbcutter.SmokeTestWithoutRedis',
		'testcases.dbcutter.AccessPatternQueryAnalysisTest'
	]

	stage('check if already running') {
		if (currentBuild.previousBuild.rawBuild.isBuilding() || currentBuild.previousBuild.previousBuild.rawBuild.isBuilding()) {
			error("A previous build is still running.")
		}
	}
		
	stage('clean up workspace') {
		deleteDir()
	}

	stage('dbcutter.DBCrawlerTest') {
		def buildResult = build job: 'Mod_Test_DB_Crawler', propagate: false, parameters: [
			string(name: 'buildTag', value: buildTag),
			[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeEnvironmentOn], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
		miscUtils.evaluateBuildResult(buildResult)
		if (buildResult.result.equals('UNSTABLE')) {				
			unstableStagesString += "dbcutter.DBCrawlerTest:${buildResult.number},"
		} else if (buildResult.result.equals('FAILURE')) {
			failedStagesString += "dbcutter.DBCrawlerTest:${buildResult.number},"
		}
		totalTestAmount++
				
		echo "Build result of dbcutter.DBCrawlerTest: ${buildResult.result}"
	}

    stage('dbcutter.DBCrawlerDB2Test') {
		def buildResult = build job: 'Mod_Test_DB_Crawler_DB2', propagate: false, parameters: [
			string(name: 'buildTag', value: buildTag),
			[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeEnvironmentOn], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
		miscUtils.evaluateBuildResult(buildResult)
		if (buildResult.result.equals('UNSTABLE')) {				
			unstableStagesString += "dbcutter.DBCrawlerDB2Test:${buildResult.number},"
		} else if (buildResult.result.equals('FAILURE')) {
			failedStagesString += "dbcutter.DBCrawlerDB2Test:${buildResult.number},"
		}
		totalTestAmount++
				
		echo "Build result of dbcutter.DBCrawlerDB2Test: ${buildResult.result}"
	}


	stage('start env with invalid license') {
		environmentBuildResult = build job: 'Mod_Docker_Start_DB_Cutter_Env', propagate: false, parameters: [
			string(name: 'buildTag', value: buildTag),
			booleanParam(name: 'invalidLicense', value: true),
            booleanParam(name: 'useRedisAsCache', value: true),
			[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeEnvironmentOn], nodeEligibility: [$class: 'AllNodeEligibility']]
		]
		miscUtils.evaluateBuildResult(environmentBuildResult)
		echo "Build result of Mod_Docker_Start_DB_Cutter_Env: ${environmentBuildResult.result}"
		sleep 45
	}
		
	if (environmentBuildResult.result == 'SUCCESS') {
		testCases.each {
			testCase ->
			def stageName = testCase.replace('testcases.','')

			if (testCase == 'testcases.dbcutter.SmokeTest') {
				stage('start env with valid license') {
					environmentBuildResult = build job: 'Mod_Docker_Start_DB_Cutter_Env', propagate: false, parameters: [
						string(name: 'buildTag', value: buildTag),
						booleanParam(name: 'invalidLicense', value: false),
                        booleanParam(name: 'useRedisAsCache', value: true),
						[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeEnvironmentOn], nodeEligibility: [$class: 'AllNodeEligibility']]
					]
					miscUtils.evaluateBuildResult(environmentBuildResult)
					echo "Build result of Mod_Docker_Start_DB_Cutter_Env: ${environmentBuildResult.result}"
					sleep 30
				}

				stage('add users to keycloak') {
					environmentBuildResult = build job: 'Mod_Docker_Add_Users_to_Keycloak', propagate: false, parameters: [
						[$class: 'NodeParameterValue', name: 'executeAgainst', labels: ['USLinux10-Mod'], nodeEligibility: [$class: 'AllNodeEligibility']]
					]
					miscUtils.evaluateBuildResult(environmentBuildResult)
					echo "Build result of Mod_Docker_Add_Users_to_Keycloak: ${environmentBuildResult.result}"
					sleep 10
				}
			}

            if (testCase == 'testcases.dbcutter.MiningExtensionsExportTest') {
                stage('start mining env with extension') {
		            environmentBuildResult = build job: 'Mod_Docker_Start_Mining_with_Extension', propagate: false, parameters: [
			            string(name: 'buildTag', value: buildTag)]
		            miscUtils.evaluateBuildResult(environmentBuildResult)
		            echo "Build result of Mod_Docker_Start_Mining_with_Extension: ${environmentBuildResult.result}"
		            sleep 30
	            }
            }

            if(testCase == 'testcases.dbcutter.SmokeTestWithoutRedis') {
                stage('start env without redis as cache') {
		            environmentBuildResult = build job: 'Mod_Docker_Start_DB_Cutter_Env', propagate: false, parameters: [
			            string(name: 'buildTag', value: buildTag),
			        booleanParam(name: 'invalidLicense', value: false),
                    booleanParam(name: 'useRedisAsCache', value: false),
			        [$class: 'NodeParameterValue', name: 'executeOn', labels: [executeEnvironmentOn], nodeEligibility: [$class: 'AllNodeEligibility']]
		            ]
		            miscUtils.evaluateBuildResult(environmentBuildResult)
		            echo "Build result of Mod_Docker_Start_DB_Cutter_Env: ${environmentBuildResult.result}"
		            sleep 30
	            }
            }

			stage (stageName) {
				def buildResult = build job: 'Mod_Selenium_Testjob', propagate: false, parameters: [
					string(name: 'buildTag', value: buildTag), 
					string(name: 'testcase', value: testCase), 
					[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeTestsOn], nodeEligibility: [$class: 'AllNodeEligibility']],
					[$class: 'NodeParameterValue', name: 'executeTestAgainst', labels: [executeEnvironmentOn], nodeEligibility: [$class: 'AllNodeEligibility']]
				]
				miscUtils.evaluateBuildResult(buildResult)
						
				if (buildResult.result.equals('UNSTABLE')) {				
					unstableStagesString += "${stageName}:${buildResult.number},"
				} else if (buildResult.result.equals('FAILURE')) {
					failedStagesString += "${stageName}:${buildResult.number},"
				}

				totalTestAmount++

				echo "Build result test case ${testCase}: ${buildResult.result}"

				copyArtifacts filter: 'RunResults/*/**', optional: true, projectName: 'Mod_Selenium_Testjob', selector: specific(buildResult.number.toString()), target: "${workDir}/${testCase}"
				sleep 20

				if (testCase == 'testcases.dbcutter.SmokeTest') {
					def smokeTestResult = buildResult.result
					if (deploy == 'true') {
					stage('deploy to staging') {
							def deployResult = build job: 'Mod_Deploy_DB_Cutter_Env', wait: true, propagate: false, parameters: [string(name: 'buildTag', value: buildTag)]
							if (deployResult.result == 'SUCCESS') {
								stage ('Sending success mail notifications') {
									def subject = "DB-Cutter Staging Deployment: ${buildTag}"
									body = "DB-Cutter version ${buildTag} was deployed to DB-Cutter Staging."
									body += '<br/><br/>DB-Cutter Staging: http://10.241.173.106:5001/ (User: db-cutter, Password: password)'
									body += "<br/><br/>Smoketest Result: ${smokeTestResult}"
									mail body: body, mimeType: 'text/html', subject: subject, to: miscUtils.getMailReceivers(currentBuild.projectName)
								}
								stage ('Sending failure mail notifications') {
									Utils.markStageSkippedForConditional('Sending failure mail notifications')
								}
							} else {
								miscUtils.evaluateBuildResult(deployResult)
								stage ('Sending success mail notifications') {
									Utils.markStageSkippedForConditional('Sending success mail notifications')
								}
								stage ('Sending failure mail notifications') {
									Utils.markStageSkippedForConditional('Sending failure mail notifications')
								}
							} 
						}
					} else {
						echo 'Deploy parameter was not set. Not deploying.'
						stage('deploy to staging') {
							Utils.markStageSkippedForConditional('deploy to staging')
						}
						stage ('Sending success mail notifications') {
							Utils.markStageSkippedForConditional('Sending success mail notifications')
						}
						stage ('Sending failure mail notifications') {
							Utils.markStageSkippedForConditional('Sending failure mail notifications')
						}
					}
				}
			}
		}
	}

	stage('archive run results') {
			def unstableStages = []
			def failedStages = []
			def logOutput = ''
			if (unstableStagesString) {
				unstableStages = unstableStagesString.split(',')
				unstableStages.each {
					unstableTest ->
					def stageInfo = unstableTest.split(':')
					logOutput += "${stageInfo[0]}   \n"
				}
			}
			if (failedStagesString) {
				failedStages = failedStagesString.split(',')
				failedStages.each {
					failedTest ->
					def stageInfo = failedTest.split(':')
					logOutput += "${stageInfo[0]}   \n"
				}
			}
			if (logOutput) {
				echo "${logOutput}"
			}
			
			def unstablePercentage = ((unstableStages.size()+failedStages.size()) / totalTestAmount) * 100 
			def stablePercentage = 100 - unstablePercentage
			
			echo "\nTotal DB Cutter tests: ${totalTestAmount}"
			echo "\nPercentage of unstable tests: ${unstablePercentage}%"
			echo "\nPercentage of stable tests: ${stablePercentage}%"
			
			buildDescription "${currentBuild.getDescription()} totalTestCount=${totalTestAmount} ${unstableStagesString ? 'unstableStages='+unstableStagesString : ''} ${failedStagesString ? 'failedStages='+failedStagesString : ''}"			
			
/*			office365ConnectorSend webhookUrl: 'https://deloitte.webhook.office.com/webhookb2/995b7703-9911-45a7-8c2b-ce51a8d5bedb@36da45f1-dd2c-4d1f-af13-5abe46b99921/JenkinsCI/ca173051917e48fd82ed82ddb95a2cd1/ed71d0bb-a103-4bbd-a076-3d9e4e977b78',
				factDefinitions:[
					[ name: "Version", template: "${buildTag}"],
					[ name: "Total Tests", template: "${totalTestAmount}"],
					[ name: "Pass Rate", template: "${stablePercentage}%"],
					[ name: "Unstable Tests", template: "${logOutput}"]
				]*/
	}
}
