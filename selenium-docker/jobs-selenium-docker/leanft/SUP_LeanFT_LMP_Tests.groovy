@Library("TestUtils") _
node('USLinux1') {
	stage('clean up workspace') {
        deleteDir()
    }
	stage('Trigger Start_LMP_Dev') {	
		build job: 'SUP_LMP_Dev_Env_Start', parameters: [booleanParam(name: 'isLdapActive', value: false)]
	}
}

node(executeOn) {
	def svnUtils = new SvnUtils()
	
	buildName "#${env.BUILD_ID}"
			
	stage('Setup workspace')  {
		cleanWs()
		checkout([ $class: 'SubversionSCM', locations: [[ credentialsId: 'User-QMSRV1-for-SVN', depthOption: 'infinity', ignoreExternalsOption: true, local: 'LMP_14.53', remote: "http://poseidon.innowake.hq/svn/innowake-qm/rm/LicenseManagementPortal-v2/LMPLeanFT_14.53" ]], workspaceUpdater: [$class: 'CheckoutUpdater'] ])
	}
 
	stage('run test')  {
		echo 'Starting LMP Test Suite'
	    def testcases = ['testcases.lmpsmoketests.TestCase1', 'testcases.lmpsmoketests.TestCase2', 
						 'testcases.lmpsmoketests.TestCase3', 'testcases.lmpsmoketests.TestCase4', 
						 'testcases.lmpsmoketests.TestCase5', 'testcases.demo.LicenseFromTemplateTest']
	    testcases.each {
	        testcase ->
	            try {
		            dir('LMP_14.53') {
						withMaven(maven : 'Default', mavenSettingsConfig: 'windows_maven_settings') {
							bat "mvn -DloginUserName=${loginUserName} -Dtest=${testcase} test"
						}
					}
	        	}
	        	catch (e) {
	        	    unstable "Failures during ${testcase}"
	            }
				finally {
	            	bat "ren LMP_14.53\\RunResults RunResults_${testcase}"
	            }
	    }
    }
    
    stage('Archive run results')  {
		try {	
			archiveArtifacts 'LMP_14.53\\RunResults_*/**/*'
	    }
    	catch (e) {
	    	unstable 'folder does not exist'
		}
    }
}