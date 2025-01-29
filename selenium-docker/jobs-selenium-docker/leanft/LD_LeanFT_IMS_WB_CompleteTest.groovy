@Library('TestUtils') _

/**
* Starts LD_LeanFT_IMS_Workbench_Testjob with all automated IMS Workbench test cases.
*
* @param mxBuildVersion IMS Workbench build
* @param executeOn      The Jenkins node the test will run on
* @param javaVersion    The java version the test will run with
*/
node('OS-Linux') {
    timestamps {
        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	    buildDescription "javaVersion=${javaVersion} executeOn=${executeOn}"

        def spUtils = new SharepointUtils()
        def miscUtils = new MiscUtils()
		def dockerUtils = new DockerUtils()
		def reuseWorkspace = false
        def successfulTestCases = [:]
        def unstableTestCases = [:]
	    def testCases = ['LaunchWorkbenchAndLoadIMSDllFileTest', 
            'DBDLevelSegmentFieldPropertiesTest', 
            'DBDPreviewFunctionalityTest',
			'DBDGenerateFunctionalityTest',
			'GenerateSaveAllFunctionalityTest',
			'PreviewAllSQLFunctionalityTest',
			'LoadIMSConfigurationPropertiesTest',
			'DBDDiscardFunctionalityTest',
			'DBDDiscardAllChangesFunctionalityTest',
			'SelectedElementsHighlightTest',
			'IndexRebuildfunctionalityTest',
			'FullSchemaGenerationTest',
			'DBDDeleteSegmentTest',
			'DBDDeleteFieldTest',
			'GenerateDVViewFunctionalityTest'
            ]
			//GenerateDVViewFunctionalityTest has to be the last test

        stage('clean up workspace') {
		    deleteDir()
	    }

		docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
        	if (spUtils.fileExists("/Shared Documents/delivery/untested/innowake/ims-workbench/ims-workbench-dist-${mxBuildVersion}.zip") || spUtils.fileExists("/Shared Documents/delivery/untested/innowake/ims-workbench/IW22-IR2/ims-workbench-dist-${mxBuildVersion}.zip")) {
            	testCases.each {
		    	    testCase ->
		    	    stage(testCase) {
						if (testCase == 'GenerateDVViewFunctionalityTest') {
							reuseWorkspace = false
						}
                    	def job = [job: 'LD_LeanFT_IMS_Workbench_Testjob', propagate: false, parameters: [
				    	    extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), 
				    	    string(name: 'testCase', value: testCase),
							booleanParam(name: 'reuse_workspace', value: reuseWorkspace),  
				    	    [$class: 'NodeParameterValue', name: 'executeOn', labels: [executeOn], nodeEligibility: [$class: 'AllNodeEligibility']],
				    	    string(name: 'javaVersion', value: javaVersion)
			        	]]
                    	def buildResult = build job
			        	if (buildResult.result == 'SUCCESS') {
				    	    successfulTestCases.put(testCase, buildResult.absoluteUrl)
			        	} else {
				    	    echo "Restarting test case ${testCase}"
				    	    buildResult = build job
				    	    miscUtils.evaluateBuildResult(buildResult)
				    	   if (buildResult.result == 'SUCCESS') {
						        successfulTestCases.put(testCase, buildResult.absoluteUrl)
				    	    } else {
						        unstableTestCases.put(testCase, buildResult.absoluteUrl)
				    	    }
			        	}
						reuseWorkspace = true
                    	echo "Build result test case ${testCase}: ${buildResult.result}"
                    	publishHTML([allowMissing: true, alwaysLinkToLastBuild: true, keepAll: true, reportDir: "${testCase}/RunResults", reportFiles: 'runresults.html', reportName: testCase, reportTitles: "#${env.BUILD_ID} - ${mxBuildVersion}: ${testCase}"])
                	}
            	}

            	echo 'Successful test cases:'
				echo successfulTestCases.toString()
				echo 'Unstable test cases:'
				echo unstableTestCases.toString()

            	def totalTestAmount = testCases.size();
            	def unstablePercentage = (unstableTestCases.size() / totalTestAmount) * 100 
		    	def stablePercentage = 100 - unstablePercentage
            	echo "\nTotal tests executed: ${totalTestAmount}"
		    	echo "\nPercentage of unstable tests: ${String.format('%.2f',unstablePercentage)}%"
		    	echo "\nPercentage of stable tests: ${String.format('%.2f',stablePercentage)}%"
        	} else {
            	echo "Artifact with version ${mxBuildVersion} is not available in Sharepoint"
        	}
		}
    }
}
