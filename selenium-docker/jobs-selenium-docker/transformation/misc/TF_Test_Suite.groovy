@Library('TestUtils') _

/**
 * Job script to compile and run all available test cases for Transformation
 *
 * @param mxBuildVersion 	The maxenso/innowake build to test.
 * @param javaVersion 		The java version the test will run with
 */
nodeTF {

    buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
    buildDescription "javaVersion=${javaVersion}"

	def miscUtils = new MiscUtils()
	def withCodeCoverage = false

	parallel License_Validation: {
		miscUtils.startTestSuite('TF_Test_Suite_License_Validation', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)], 0)
	}, ExecSQL_Test: {
		miscUtils.startTestSuite('TF_Cobol_Java_Test_ExecSQL', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)], 10)
	}, Batch_Tests: {
		miscUtils.startTestSuite('TF_Cobol_Java_Test_Batch', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)], 20)
	}, KIDICAP_mini_complete_test: {
		miscUtils.startTestSuite('TF_Test_Suite_KIDICAP_Mini', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), string(name: 'javaVersion', value: javaVersion)], 30)
	}, CICS_Tests: {
		miscUtils.startTestSuite('TF_Cobol_Java_Test_CICS', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)], 40)
	}, IMS_and_NMSLO: {
		miscUtils.startTestSuite('TF_Test_Suite_IMS', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), string(name: 'javaVersion', value: javaVersion)], 50)
		miscUtils.startTestSuite('TF_Test_Suite_NMSLO', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)])
	}, Customer_Zero_Complete_Test: {
		miscUtils.startTestSuite('TF_Test_Suite_Customer_Zero', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), string(name: 'environmentLabel', value: 'perf-environment'), string(name: 'javaVersion', value: javaVersion)], 60)
	}, Cobol_Async_API_Tests: {
		miscUtils.startTestSuite('TF_Test_Suite_Async_API', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)], 70)
	}, KIDICAP_184_complete_test: {
		miscUtils.startTestSuite('TF_Test_Suite_KIDICAP_184', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), string(name: 'javaVersion', value: javaVersion)], 80)
	}, Mannheimer_complete_test: {
		miscUtils.startTestSuite('TF_Test_Suite_Mannheimer', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), string(name: 'javaVersion', value: javaVersion)], 90)
	}, PL1_Complete_Test: {
		miscUtils.startTestSuite('TF_Test_Suite_PL1', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)], 100)
	}, Natural_Test: {
		miscUtils.startTestSuite('TF_Test_Suite_Natural_Csharp_VontobelSt', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)], 110)
	}, Natural_VSAM: {
		miscUtils.startTestSuite('TF_Test_Suite_VSAM', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)], 120)
	}, TF_Migration_Object_Filter: {
		miscUtils.startTestSuite('TF_Migration_Object_Filter', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)], 130)
	}, TF_Test_Suite_Cobol_Java: {
		miscUtils.startTestSuite('TF_Test_Suite_Cobol_Java', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), string(name: 'javaVersion', value: javaVersion)], 130)
	}

	if (Calendar.getInstance().get(Calendar.DAY_OF_WEEK) == Calendar.SATURDAY) {
		withCodeCoverage = true
		parallel KIDICAP_mini_complete_test: {
			miscUtils.startTestSuite('TF_Test_Suite_KIDICAP_Mini', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), string(name: 'javaVersion', value: javaVersion)], 0)
		}, IMS_and_NMSLO: {
			miscUtils.startTestSuite('TF_Test_Suite_IMS', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), string(name: 'javaVersion', value: javaVersion)], 10)
		}, Customer_Zero_Complete_Test: {
			miscUtils.startTestSuite('TF_Test_Suite_Customer_Zero', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), string(name: 'environmentLabel', value: 'trafo-environment'), string(name: 'javaVersion', value: javaVersion)], 20)
		}, KIDICAP_184_complete_test: {
			miscUtils.startTestSuite('TF_Test_Suite_KIDICAP_184', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), string(name: 'javaVersion', value: javaVersion)], 30)
		}, Mannheimer_complete_test: {
			miscUtils.startTestSuite('TF_Test_Suite_Mannheimer', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), string(name: 'javaVersion', value: javaVersion)], 40)
		}
		KIDICAP_complete_test: {
			miscUtils.startTestSuite('TF_Test_Suite_KIDICAP_165', [extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion), booleanParam(name: 'withCodeCoverage', value: withCodeCoverage), string(name: 'javaVersion', value: javaVersion)])
		}
	}
}
