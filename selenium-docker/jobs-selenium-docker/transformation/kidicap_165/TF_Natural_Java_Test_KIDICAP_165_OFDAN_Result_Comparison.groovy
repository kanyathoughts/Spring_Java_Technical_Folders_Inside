@Library('TestUtils') _

/**
 * Do a result comparison of the actual output of a KIDICAP OFDAN production against the expected output.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param productionRun  The OFDAN production to fetch the output files from.
 *        type: Build selector for artifact copy
 * @param parallelMode  Whether to run production steps of KIDICAP parallelized (using the AsyncApi).
 *        type: boolean
 * @param reduceDbVolume  Indicates that the job shall reduce the amount of data in the KIDICAP DB.
 *        type: boolean
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
		
		def productionSteps = ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '18', '23']
		def subDirForSteps = [
		    '01': 'OFDAN_01_RZ_Plausi',
		    '02': 'OFDAN_02_Plausi_Protokoll',
		    '03': 'OFDAN_03_Vorinstanz',
		    '04': 'OFDAN_04_Berechnung',
		    '05': 'OFDAN_05_BG_Abrechnung',
		    '06': 'OFDAN_06_DEUEV',
		    '07': 'OFDAN_07_ZVK',
		    '08': 'OFDAN_08_ErgPaket',
		    '09': 'OFDAN_09_Abstimmung',
		    '10': 'OFDAN_10_Ueberleitung',
		    '11': 'OFDAN_11_UL_Listen',
		    '18': 'OFDAN_18_Bezugsartenliste',
		    '23': 'OFDAN_23_Nachinstanz'
		]
		
		def resultComparisonRegexList = [
		    /* Combined date/times
		       GB                                            10.09.16 05.04.201820:20:19
		    */
		    ' [0-9][0-9]\\.[0-9][0-9]\\.20[12][0-9][0-9][0-9]:[0-9][0-9]:[0-9][0-9]',
		    /* Times
		       20:24:31.4
		       20:24:31
		       20:24
		       Leading or trailing blanks or other characters have to match too.
		       Otherwise this could match anything else, e.g. a MAC address.
		    */
		    ' [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9]',
		    ' [0-9][0-9]:[0-9][0-9]:[0-9][0-9]',
		    ' [0-9][0-9]:[0-9][0-9]',
		    /* Dates
		       23.03.2017
		       23.03.17
		       03.2016
		       18-04-05
		       2016/03
		       Leading or trailing blanks or other characters have to match too.
		       Otherwise this could match anything else - insurance customer numbers or alike.
		    */
		    ' [0-9][0-9]\\.[0-9][0-9]\\.20[12][0-9]',
		    ' [0-9][0-9]\\.[0-9][0-9]\\.[12][0-9]',
		    '[0-9][0-9]\\.[0-9][0-9]\\.[12][0-9] ',
		    ' [0-9][0-9]\\.20[12][0-9]',
		    '[12][0-9]\\-[0-9][0-9]\\-[0-9][0-9] ',
		    ' 20[12][0-9]\\/[0-9][0-9]',
		    /* <CreDtTm>2018-04-05T20:20:19Z</CreDtTm> */
		    '<CreDtTm>20[0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z</CreDtTm>',
		    
		    /* Special cases */
		    /* Timestamp: D429E797521C0000 */
		    'Timestamp: .\\+',
		    /* Seite      7                                                 11.04.18  10:11:47 */
		    'Seite \\+[0-9]\\+ ',
		    /* PWNBG AKIFIKOS 201603171327557 */
		    'PWNBG AKIFIKOS 20160317[0-9][0-9][0-9][0-9][0-9][0-9][0-9]',
		    /* 0101ZVE 115705042018GIP-AGTV-TEST */
		    /* 0101ZVE 002605042018GIP-AGTV-TEST */
		    ' [0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]20[12][0-9]',
		    /* -XX:InitialHeapSize=523711168 -XX:MaxHeapSize=6442450944 -XX:+PrintCommandLineFlags -XX:+PrintVMOptions -XX:+UseCompressedClassPointers -XX:+UseCompressedOops -XX:+UseParallelGC */
			'.*-XX:InitialHeapSize=.\\+',
			/* VM option '+PrintCommandLineFlags' */
			'VM option .*',
			/* WQATF-603 */
			'.*WARNING: sun\\.reflect\\.Reflection\\.getCallerClass.*'
		]
		
		def resultComparisonExcludedFilesForSteps = [
		    '01': [],
		    '02': [],
		    '03': [],
		    '04': [],
		    '05': [],
		    '06': [],
		    '07': [],
		    '08': ['CMWKF01.DAT' /* binary file */, 'CMWKF06.DAT' /* binary file */, 'CMWKF08.DAT' /* binary file */],
		    '09': [],
		    '10': [],
		    '11': ['CMWKF01.DAT' /* binary file */],
		    '18': ['CMWKF01.DAT' /* binary file */],
		    '23': []
		]
		
		def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/kidicap/kidicap.git'
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		def workDir
		
		parallelMode = Boolean.parseBoolean(parallelMode)
		reduceDbVolume = Boolean.parseBoolean(reduceDbVolume)
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "productionRun=${productionRun} parallelMode=${parallelMode} reduceDbVolume=${reduceDbVolume} testProjectBranch=${testProjectBranch}"

		try {
			deleteDir()
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
				workDir = pwd()
				
				stage('initialisation') {
				    gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
				    if (productionRun.matches('.*StatusBuildSelector.*')) {
						copyArtifacts([
							projectName: 'TF_Natural_Java_Test_KIDICAP_165_OFDAN',
							filter: 'log/**',
							selector: lastSuccessful(),
							parameters: "mxBuildVersion=${mxBuildVersion},reduceDbVolume=${reduceDbVolume},parallelMode=${parallelMode}",
							target: '.',
							fingerprintArtifacts: true,
							flatten: false
						])
					} else {
						copyArtifacts([
							projectName: 'TF_Natural_Java_Test_KIDICAP_165_OFDAN',
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
						def expectedFilesDir = "${workDir}/expected/logfiles/reduceDbVolume_${reduceDbVolume}/parallelMode_${parallelMode}/ofdan/${subDirForStep}"
						def resultComparisonLogDir = "${workDir}/result-comparison/log/${subDirForStep}"
						def resultComparisonTmpDir = "${workDir}/result-comparison/tmp/${subDirForStep}"
						sh returnStatus: true, script: "mkdir -p ${resultComparisonLogDir}"
						sh returnStatus: true, script: "mkdir -p ${resultComparisonTmpDir}"
						def compareResult = compareUtils.resultCompare(actualFilesDir, expectedFilesDir, resultComparisonRegexList, resultComparisonExcludedFilesForSteps[step], resultComparisonLogDir, resultComparisonTmpDir, parallelMode)
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
