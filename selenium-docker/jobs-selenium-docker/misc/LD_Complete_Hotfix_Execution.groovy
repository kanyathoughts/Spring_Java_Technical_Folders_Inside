@Library('TestUtils') _

/**
 * Runs IW_Eclipse_Bundle_Build
 * Runs Archive_control-center_and_lcm-api-client
 * Runs LD_LeanFT_SmokeTests
 * Runs TF_Cobol_Java_Test_ExecSQL
 * Runs a complete KIDICAP_165 test:
 * - nat2java (once with and once without copycodes)
 * - production AGTV
 * - production OFDAN
 * - production AGTV2_part0103
 * - production AGTV2_part0409
 * - production AGTV2_part1020
 * Runs a complete Mannheimer test:
 * - nat2java
 * - start DB2 DB in docker with Mannheimer data
 * - Mannheimer test suite
 * - stop DB2 DB in docker
 * Runs TF_Cobol_Java_Test_Batch
 * Runs a complete Customer Zero test (>= 22.0):
 * - TF_Cobol_Java_Migration_Customer_Zero
 * - TF_Cobol_Java_Migration_CLI_Customer_Zero
 * - TF_Cobol_Csharp_Migration_Customer_Zero
 * - TF_Cobol_Java_Test_Customer_Zero
 * Runs TF_Test_Suite_Async_API (>= 18.0.0)
 * Runs TF_Test_Suite_License_Validation (>= 19.0)
 * Runs TF_Cobol_Java_Test_CICS (>=18.0.0)
 * Runs a complete KIDICAP_184 test (>=17.0.0):
 * - nat2java
 * - production AGTV
 * Runs a complete KIDICAP_mini test (>= 21.0.0):
 * - expert-script-signing
 * - nat2java
 * - production AGTV
 * - production OFDAN
 * - production OFDVE
 * - production REGRESSION
 * Runs TF_Test_Suite_PL1 (== 21.7.0 && >= 23.0)
 * Runs TF_Test_Suite_Cobol_Java (== 22.3 || 22.4 || > 23.1)
 * @param mxBuildVersion  The maxenso build to use.
 */
 
node {
	timestamps() {
	
		echo "mxBuildVersion = ${mxBuildVersion}"
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "mxBuildVersion=${mxBuildVersion} javaVersion=${javaVersion}"

		def miscUtils = new MiscUtils()

		//splits mxBuildVersion at dots and gets the first number as Integer
		def branchVar = mxBuildVersion.tokenize('\\.')[0] as Integer
    	//splits mxBuildVersion at dots and gets the second number as Integer
		def branchVar2nd = mxBuildVersion.tokenize('\\.')[1] as Integer
		
		//map to store every build job, which should be ran in parallel
		def jobsToRun = [:]
		
		jobsToRun["TF_Test_Suite_KIDICAP_165"] = {
			stage ('TF_Test_Suite_KIDICAP_165') {    
				miscUtils.startTestSuite('TF_Test_Suite_KIDICAP_165', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
			}
		}
		jobsToRun["TF_Test_Suite_Mannheimer"] = {
			stage ('TF_Test_Suite_Mannheimer') {    
				miscUtils.startTestSuite('TF_Test_Suite_Mannheimer', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
			}
		}
		if (branchVar >= 23 || (branchVar == 22 && branchVar2nd >=2)) {
			//LD_LeanFT_SmokeTests and LD_LeanFT_CompleteTest shouldn't be run in parallel. Therefore, LD_Test_Suite that executes SmokeTest first and then CompleteTest is used.
			jobsToRun["LD_Test_Suite"] = {
				stage('LD_Test_Suite') {
					miscUtils.startTestSuite('LD_Test_Suite', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name:'javaVersion', value: javaVersion), booleanParam(name:'isHotfix', value: true)])
				}
			}
		} else {
			jobsToRun["LD_LeanFT_SmokeTests"] = {
				stage ('LD_LeanFT_SmokeTests') {    
					miscUtils.startTestSuite('LD_LeanFT_SmokeTests', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), [$class: 'NodeParameterValue', name: 'executeOn', labels: ['USLeanft1'], nodeEligibility: [$class: 'AllNodeEligibility']], string(name: 'javaVersion', value: javaVersion)])
				}
			}
		}
		if (branchVar >= 19) { 
		    jobsToRun["TF_Cobol_Java_Test_ExecSQL"] = {
			    stage ('TF_Cobol_Java_Test_ExecSQL') {    
					miscUtils.startTestSuite('TF_Cobol_Java_Test_ExecSQL', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
			    }
		    }
		}
		if (branchVar >= 17) { 
		    jobsToRun["TF_Cobol_Java_Test_Batch"] = {
			    stage ('TF_Cobol_Java_Test_Batch') {    
					miscUtils.startTestSuite('TF_Cobol_Java_Test_Batch', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
			    }
		    }
		}
		if (branchVar >= 17) {
			jobsToRun["TF_Test_Suite_KIDICAP_184"] = {
				stage('TF_Test_Suite_KIDICAP_184') {
					miscUtils.startTestSuite('TF_Test_Suite_KIDICAP_184', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
				}
			}
		}
		if (branchVar >= 18) {
			jobsToRun["TF_Test_Suite_Async_API"] = {
				stage('TF_Test_Suite_Async_API') {
					miscUtils.startTestSuite('TF_Test_Suite_Async_API', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
				}
			}
			jobsToRun["TF_Cobol_Java_Test_CICS"] = {
				stage('TF_Cobol_Java_Test_CICS') {
					miscUtils.startTestSuite('TF_Cobol_Java_Test_CICS', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
				}
			}
		}
		if (branchVar >= 19) {
			jobsToRun["TF_Test_Suite_License_Validation"] = {
				stage('TF_Test_Suite_License_Validation') {
					miscUtils.startTestSuite('TF_Test_Suite_License_Validation', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
				}
			}
		}
		if (branchVar >= 21) {
			jobsToRun["TF_Test_Suite_KIDICAP_Mini"] = {
				stage('TF_Test_Suite_KIDICAP_Mini') {
					miscUtils.startTestSuite('TF_Test_Suite_KIDICAP_Mini', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
				}
			}
		}
    	if((branchVar == 21 && branchVar2nd == 7) || (branchVar >= 23)) {
      		jobsToRun["TF_Test_Suite_PL1"] = {
        		stage('TF_Test_Suite_PL1') {
					miscUtils.startTestSuite('TF_Test_Suite_PL1', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
        		}
      		}
    	}
		if (branchVar >= 22) {
			jobsToRun["TF_Test_Suite_Customer_Zero"] = {
				stage('TF_Test_Suite_Customer_Zero') {
					miscUtils.startTestSuite('TF_Test_Suite_Customer_Zero', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
				}
			}
		}
		if ((branchVar == 19 && branchVar2nd == 1) || (branchVar == 21 && branchVar2nd == 6) || (branchVar == 22 && branchVar2nd >= 2) || branchVar >= 23) {
			jobsToRun["TF_Test_Suite_NMSLO"] = {
				stage('TF_Test_Suite_NMSLO') {
					miscUtils.startTestSuite('TF_Test_Suite_NMSLO', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
				}
			}
		}
		if (branchVar == 22 && branchVar2nd == 2 || branchVar >= 23) {
			jobsToRun["TF_Test_Suite_IMS"] = {
				stage('TF_Test_Suite_IMS') {
					miscUtils.startTestSuite('TF_Test_Suite_IMS', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
				}
			}
		}
		if (branchVar > 22 || (branchVar == 22 && branchVar2nd >= 3)) {
			jobsToRun["TF_Test_Suite_VSAM"] = {
				stage('TF_Test_Suite_VSAM') {
					build job: 'TF_Test_Suite_VSAM', parameters: [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
					string(name: 'javaVersion', value: javaVersion)] 
				}
			}
		}
		if ((branchVar == 22 && branchVar2nd == 3) || (branchVar == 22 && branchVar2nd == 4) || (branchVar == 23 && branchVar2nd > 1) || (branchVar > 23)) {
			jobsToRun["TF_Test_Suite_Cobol_Java"] = {
				stage('TF_Test_Suite_Cobol_Java') {
					miscUtils.startTestSuite('TF_Test_Suite_Cobol_Java', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
				}
			}
		}
		//all jobs will be running in parallel
		parallel jobsToRun
	}
}
