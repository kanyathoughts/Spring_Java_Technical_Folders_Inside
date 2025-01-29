/**
 * This job triggers another job called 'IW_Docker_Delete_Outdated_Alpha_And_Beta_Builds'. 
 * A list of jobs will be passed with the the number of days to keep builds on the server. 
 * All outdated alpha and beta builds will be deleted (older than numberOfDays).
 * 
 * IW_Docker_Cleanup_Linux is triggered periodically every sunday.
 */
node('built-in') {
	stage('init') {
		def jobNames = [
			[jobName: 'IW_Eclipse_Bundle_Build', numberOfDays: 14],
			[jobName: 'LD_LeanFT_CompleteTest', numberOfDays: 30],
			[jobName: 'LD_LeanFT_Testjob', numberOfDays: 20],
			[jobName: 'LD_LeanFT_SmokeTests', numberOfDays: 30],
			[jobName: 'LD_LeanFT_IMS_Workbench_Testjob', numberOfDays: 20],
			[jobName: 'TF_Test_Suite_KIDICAP_184', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Migration_KIDICAP_184', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Migration_KIDICAP_184_Result_Comparison', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_184_AGTV', numberOfDays: 30],
			[jobName: 'TF_Test_Suite_KIDICAP_165', numberOfDays: 30],
			[jobName: 'TF_Test_Suite_KIDICAP_Mini', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_Mini_AGTV', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_Mini_OFDAN', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_Mini_OFDVE', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Migration_KIDICAP_Mini', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Migration_KIDICAP_165', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Migration_KIDICAP_165_Result_Comparison', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV_Result_Comparison', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part0103', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part0103_Result_Comparison', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part0409', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part0409_Result_Comparison', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part1020', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part1020_Result_Comparison', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_165_OFDAN', numberOfDays: 30],
			[jobName: 'TF_Natural_Java_Test_KIDICAP_165_OFDAN_Result_Comparison', numberOfDays: 30],
			[jobName: 'DM_LeanFT_Mining_Testjob', numberOfDays: 30],
			[jobName: 'DM_Tests_Discovery', numberOfDays: 30],
			[jobName: 'DM_Tests_Plugin', numberOfDays: 30],
			[jobName: 'DM_Test_Suite', numberOfDays: 30],
			[jobName: 'DM_Test_Version_Handshake', numberOfDays: 30],
			[jobName: 'DM_Tests_WebUI', numberOfDays: 30],
			[jobName: 'DM_Selenium_Mining_Testjob', numberOfDays: 30]
		]
		jobNames.each { entry ->
			stage("trigger IW_Docker_Delete_Outdated_Alpha_And_Beta_Builds - ${entry.jobName}"){
				catchError(buildResult: 'UNSTABLE', message: "The job ${entry.jobName} doesn't exist.", stageResult: 'UNSTABLE') {
					build job: 'IW_Docker_Delete_Outdated_Alpha_And_Beta_Builds', parameters: [
						string(name: 'jobName', value: entry.jobName),
						booleanParam(name: 'deleteAlphaBuilds', value: true),
						booleanParam(name: 'deleteBetaBuilds', value: true),
						string(name: 'numberOfDays', value: "${entry.numberOfDays}")
					], wait: true
				}
			}
		}
	}
}