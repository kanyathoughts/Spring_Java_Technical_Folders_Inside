@Library('TestUtils') _

/**
 * Do a result comparison of the actual output of a KIDICAP AGTV2 production part1020 against the expected output.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param productionRun  The AGTV2 part1020 production to fetch the output files from.
 *        type: Build selector for artifact copy
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

nodeTF('Docker-host && Region-EU') {

	timestamps {
		
		def mxVersionUtils = new MxVersionUtils()
		def gitUtils = new GitUtils()
		def miscUtils = new MiscUtils()
		def compareUtils = new ResultComparisonUtils()
		def dockerUtils = new DockerUtils()
		
		def productionSteps = ['10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20']
		def subDirForSteps = [
		    '10': '10_bic_aus',
		    '11': '11_bflex_aus',
		    '12': '12_klassik_aus',
		    '13': '13_bic_ein',
		    '14': '14_bflex_ein',
		    '15': '15_klassik_ein',
		    '16': '16_pdsst_aus',
		    '17': '17_pdsst_ein',
		    '18': '18_prot',
		    '19': '19_pd_aus',
		    '20': '20_pd_ein'
		]
		
		def resultComparisonRegexList = [
		    /* Dates, timestamps */
		    /* 24.05.18   09:56:41                                  Seite      1 */
		    '[0-9][0-9]\\.[0-9][0-9]\\.[12][0-9]   \\?[0-9][0-9]:[0-9][0-9]:[0-9][0-9] ',
		    ' [0-9][0-9]\\.[0-9][0-9]\\.\\(20\\)\\?[12][0-9]',
		    ' [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\(\\.[0-9]\\)\\?\\(VM option.*\\)\\?',
		    /* 16.5      24/05/1809:47:59KIDICAP KIDICAP                         EUR */
			' [0-9][0-9]\\/[0-9][0-9]\\/[12][0-9][0-9][0-9]:[0-9][0-9]:[0-9][0-9]KIDICAP',
			/* JO038KIDICAP                       20180807      EUR                             */
			' 20[12][0-9][0-9][0-9][0-9][0-9]      EUR',
			/*  ge�ndert               5  224ZVB0000000000000002703798392           07.2016                20160831            20180524 */
			'            20[12][0-9][0-9][0-9][0-9][0-9]',
			/* -XX:InitialHeapSize=523711168 -XX:MaxHeapSize=6442450944 -XX:+PrintCommandLineFlags -XX:+PrintVMOptions -XX:+UseCompressedClassPointers -XX:+UseCompressedOops -XX:+UseParallelGC */
			'.*-XX:InitialHeapSize=.\\+',
			/* VM option '+PrintCommandLineFlags' */
			'VM option .*',
			/* WQATF-603 */
			'.*WARNING: sun\\.reflect\\.Reflection\\.getCallerClass.*'
		]
		
		def resultComparisonExcludedFilesForSteps = [
		    '10': [],
		    '11': [],
		    '12': [],
		    '13': [],
		    '14': [],
		    '15': [],
		    '16': [],
		    '17': [],
		    '18': ['CMWKF01.bin' /* binary file */, 'CMPRINT.log' /* see WQATF-278 */],
		    '19': [],
		    '20': []
		]
		
		def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/kidicap/kidicap.git'
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		def workDir
		
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "productionRun=${productionRun} testProjectBranch=${testProjectBranch}"

		try {
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
				workDir = pwd()
						
				stage('initialisation') {
				    gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
				    if (productionRun.matches('.*StatusBuildSelector.*')) {
						copyArtifacts([
							projectName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part1020',
							filter: 'log/**',
							selector: lastSuccessful(),
							parameters: "mxBuildVersion=${mxBuildVersion}",
							target: '.',
							fingerprintArtifacts: true,
							flatten: false
						])
					} else {
						copyArtifacts([
							projectName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part1020',
							filter: 'log/**',
							selector: buildParameter(productionRun),
							target: '.',
							fingerprintArtifacts: true,
							flatten: false
						])
					}
					sh returnStatus: true, script: "mkdir -p ${workDir}/result-comparison/log"
					sh "echo -n '' > ${workDir}/result-comparison/log/result-comparison.log"
				}
				
				productionSteps.each {
					step ->
					stage("productionPart${step}") {
						def subDirForStep = subDirForSteps[step]
						def actualFilesDir = "${workDir}/log/${subDirForStep}"
						def expectedFilesDir = "${workDir}/expected/logfiles/agtv2_1020/${subDirForStep}"
						def resultComparisonLogDir = "${workDir}/result-comparison/log/${subDirForStep}"
						def resultComparisonTmpDir = "${workDir}/result-comparison/tmp/${subDirForStep}"
						sh returnStatus: true, script: "mkdir -p ${resultComparisonLogDir}"
						sh returnStatus: true, script: "mkdir -p ${resultComparisonTmpDir}"
						def compareResult = compareUtils.resultCompare(actualFilesDir, expectedFilesDir, resultComparisonRegexList, resultComparisonExcludedFilesForSteps[step], resultComparisonLogDir, resultComparisonTmpDir)
						if (compareResult != 0) {
							unstable "Deviation found in step \"${step}\""
							sh "echo Deviation found in step \"${step}\" >> ${workDir}/result-comparison/log/result-comparison.log"
						}
					}
				}
			}
		} catch (ex) {
			miscUtils.errorWithStackTrace(ex)
		} finally {
			stage('finalize') {
				archiveArtifacts allowEmptyArchive: true, artifacts: 'log/**/*,result-comparison/**/*'
			}
		}
		
	}
	
}
