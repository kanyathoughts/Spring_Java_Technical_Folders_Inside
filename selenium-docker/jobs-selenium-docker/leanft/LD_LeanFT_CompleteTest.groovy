@Library('TestUtils') _

/**
 * Compile and run all available test cases for maxenso/innowake on US Server
 * 
 * @param mxBuildVersion The maxenso/innowake build to test.
 * @param executeOn The Jenkins node the test will run on
 * @param javaVersion The java version the test will run with
 */

node('OS-Linux') {
	
	buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	buildDescription "javaVersion=${javaVersion}"
	
	def miscUtils = new MiscUtils()
	def workDir = pwd()
	def testCases = [
		'testcases.base.FieldTracerTest',
		'testcases.base.EclipseInstallToolTest',
		'testcases.base.ErrorLogViewTest',
		'testcases.base.EclipseViewForExpertScriptsTest',
		'testcases.base.GeneralLicensingTest',
		'testcases.base.GeneralLicensingSmokeTest',

		'testcases.mdd.AuthFileTest',
		'testcases.mdd.AutoImportTest',
		'testcases.mdd.BusinessRulesEditorTest',
		'testcases.mdd.FlowlayoutTest',
		'testcases.mdd.IntegrityViolationTest',
		'testcases.mdd.MDDSmokeTestsTest',
		'testcases.mdd.PreferencesAndPropertiesTest',
		'testcases.mdd.ReferenceViewTest',
		'testcases.mdd.ShortcutsTest', 
		'testcases.mdd.UiEditorTest', // Only parts are automated
		
		'testcases.mee.Cobol2JavaTest',
		'testcases.mee.GdaAccessFromJarTest',
		'testcases.mee.MeeclipseArraysOnGroupsTest',
		'testcases.mee.MeeclipseAutomaticAssemblingOfJCopiesAndDataAreasOnChangeTest',
		'testcases.mee.MeeclipseBatchclipseTest',
		'testcases.mee.MeeclipseBMSMapPreviewTest',
		'testcases.mee.MeeclipseCICSBMSOperandsTest',
		'testcases.mee.MeeclipseCobolIncludeJCopyCodeTest',
		'testcases.mee.MeeclipseCobolMeeSteplibsTest',
		'testcases.mee.MeeclipseCobolRedefineTest',
		'testcases.mee.MeeclipseCobolTypesTest',
		'testcases.mee.MeeclipseDataTypesTest',
		'testcases.mee.MeeclipseJMapEditorTest',
		'testcases.mee.MeeclipseLaunchableTest',
		'testcases.mee.MeeclipseMeeEditorJDTTest',
		'testcases.mee.MeeclipseMeeEditorSpecificTest',
		'testcases.mee.MeeclipseMonitoringAppTest',
		'testcases.mee.MeeclipseNaturalIncludeJCopyCodeTest',
		'testcases.mee.MeeclipseNaturalRedefineTest',
		'testcases.mee.MeeclipseProjectsTest',
		'testcases.mee.MeeclipseVaadinPrintFunctionTest',
		'testcases.mee.MeeclipseVaadinTestsTest',
		'testcases.mee.MeeclipseViewDefinitionsTest',
		'testcases.mee.MeeclipseWorkspacePreferencesTest',
		'testcases.mee.MeeCobol2JavaFeaturesTest',
		'testcases.mee.MeeDBHealthCheckTest',
		'testcases.mee.MeeDBMaintenanceToolTest',
		'testcases.mee.MeeDBMaintenanceToolSchemaNameTest',
		'testcases.mee.MeeScreenReaderCompatibilityPluginTest',
		'testcases.mee.Natural2JavaTest',
		'testcases.mee.PL12JavaTest',
		
		'testcases.ndt.AssemblerclipseTest',
		'testcases.ndt.CobolclipseBMSMapViewerEnhancementTest',
		'testcases.ndt.CobolclipseCopybookAssemblingTest',
		'testcases.ndt.CobolclipseExtendedTest',
		'testcases.ndt.CobolclipseOptionalSettingTest',
		'testcases.ndt.CobolclipsePeekDefinitionFunctionalityTest',
		'testcases.ndt.CobolclipseSyntaxHighlightingTest',
		'testcases.ndt.DDMEditorTest',
		'testcases.ndt.ErrorMessageEditorTest',
		'testcases.ndt.LCMAccessibilityTest',
		'testcases.ndt.MapEditorTest',
		'testcases.ndt.NatCreatorCreateFromTemplateTest',
		'testcases.ndt.NatcreatorHyperlinkingTest',
		'testcases.ndt.NaturalCallSequenceViewTest',
		'testcases.ndt.NaturalDependenciesTest',
		'testcases.ndt.NaturalEditorCodeCollapseAndCodeProtectionTest',
		'testcases.ndt.NaturalEditorCodeFormattingAndCodeStructuringTest',
		'testcases.ndt.NaturalEditorHyperlinkingAndProjectlinkingTest',
		'testcases.ndt.NaturalEditorLineNumberReferencesTest',
		'testcases.ndt.NaturalEditorToUpperAndToLowerCaseCodeTranslationTest',
		'testcases.ndt.NaturalLCMConfigurationTest',
		'testcases.ndt.NaturalLogicalViewTest',
		'testcases.ndt.NaturalObjectInfoViewTest',
		'testcases.ndt.NaturalObjectsTest',
		'testcases.ndt.NaturalRemotePreviewTest',
		'testcases.ndt.NaturalRulesViewTest',
		'testcases.ndt.NaturalTargetPropertyPageTest',
		'testcases.ndt.NaturalTargetViewTest',
		'testcases.ndt.NormalizationTest',
		'testcases.ndt.ObjectCaseTest',
		'testcases.ndt.ProjectReferencesTest',
		'testcases.ndt.RemoteOperationsTest',
		'testcases.ndt.SteplibsTest',
		'testcases.ndt.TargetCollisionErrorsTest',
		'testcases.ndt.TargetSharingTest',
		'testcases.ndt.TargetViewTest',
		'testcases.ndt.TeamFileTest',
		
		'testcases.sc.SoaConnectorDataStructureTest',
	]
	def scRequiredProjects = ['testcases.ndt.LCMAccessibilityTest']
	def reuseWorkspace = false
	def unstableTestCases = [:]
	def successfulTestCases = [:]
	def scEnvironmentHost = 'usawsconl00235.us.deloitte.com'
	def scEnvironmentNode
	
	stage('clean up workspace') {
		deleteDir()
	}
	
	testCases.each {
		testCase ->
		stage(testCase) {
			if (testCase in scRequiredProjects) {
				def scBuildResult = build job: 'LD_Start_SC_Env', propagate: false, parameters: [
					extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
					string(name: 'javaVersion', value: javaVersion),
					[$class: 'NodeParameterValue', name: 'executeOn', labels: ['USLinux1'], nodeEligibility: [$class: 'AllNodeEligibility']]
				]
				miscUtils.evaluateBuildResult(scBuildResult)
				scEnvironmentHost = scBuildResult.buildVariables['scEnvironmentHost']
				scEnvironmentNode = scBuildResult.buildVariables['scEnvironmentNode']
			}
			
			def job = [job: 'LD_LeanFT_Testjob', propagate: false, parameters: [
				extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), 
				string(name: 'testcase', value: testCase), 
				booleanParam(name: 'reuse_workspace', value: reuseWorkspace), 
				[$class: 'NodeParameterValue', name: 'executeOn', labels: [executeOn], nodeEligibility: [$class: 'AllNodeEligibility']],
				string(name: 'scEnvironmentHost', value: scEnvironmentHost),
				string(name: 'javaVersion', value: javaVersion)
			]]
			def buildResult = build job
			// If the build is unstable repeat the testcase and check again if the build is successful. If the build is unstable a second time the test is marked as unstable
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
			// both testcases change the license file inside the eclipse folder therefore it is required to checkout a new eclipse folder
			if (testCase.contains('GeneralLicensingSmokeTest') || testCase.contains('GeneralLicensingTest')) {
				reuseWorkspace = false
			} else {
				reuseWorkspace = true
			}
			echo "Build result test case ${testCase}: ${buildResult.result}"
			// copyArtifacts filter: 'RunResults/*/**', optional: true, projectName: 'LD_LeanFT_Testjob', selector: specific(buildResult.number.toString()), target: "${workDir}/${testCase}"
			publishHTML([allowMissing: true, alwaysLinkToLastBuild: true, keepAll: true, reportDir: "${testCase}/RunResults", reportFiles: 'runresults.html', reportName: testCase, reportTitles: "#${env.BUILD_ID} - ${mxBuildVersion}: ${testCase}"])
			//publishHTML([allowMissing: true, alwaysLinkToLastBuild: true, keepAll: true, reportName: testCase, reportTitles: "#${env.BUILD_ID} - ${mxBuildVersion}: ${testCase}"])

			if (testCase in scRequiredProjects) {
				def scBuildResult = build job: 'LD_Stop_SC_Env', propagate: false, parameters: [
					[$class: 'NodeParameterValue', name: 'executeOn', labels: [scEnvironmentNode], nodeEligibility: [$class: 'AllNodeEligibility']]
				]
				miscUtils.evaluateBuildResult(scBuildResult)
			}
		}
	}
	
	stage('process results'){
	echo successfulTestCases.toString()
		def totalTestAmount = testCases.size();
		echo 'Unstable test cases'
		unstableTestCases.each { key, value ->
			echo "${key}: ${value}"
		}
		def unstablePercentage = (unstableTestCases.size() / totalTestAmount) * 100 
		def stablePercentage = 100 - unstablePercentage
		
		echo "\nTotal tests executed: ${totalTestAmount}"
		echo "Number of successful tests: ${successfulTestCases.size()}"
		echo "Number of unstalbe tests: ${unstableTestCases.size()}"
		echo "\nPercentage of unstable tests: ${String.format('%.2f',unstablePercentage)}%"
		echo "\nPercentage of stable tests: ${String.format('%.2f',stablePercentage)}%"
		
		// archiveArtifacts '**/*'
	}
}
