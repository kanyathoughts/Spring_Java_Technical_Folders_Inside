@Library('TestUtils') _

/**
 * Do a result comparison of the actual output of a KIDICAP AGTV2 production part0409 against the expected output.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param productionRun  The AGTV2 part0409 production to fetch the output files from.
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
		
		def productionSteps = ['04', '05', '06', '07', '08', '09']
		def subDirForSteps = [
		    '04': '04_kdkopie_aus',
		    '05': '05_mit_ebenen_aend_aus',
		    '06': '06_ohne_ebenen_aend_aus',
		    '07': '07_kdkopie_ein',
		    '08': '08_mit_ebenen_aend_ein',
		    '09': '09_ohne_ebenen_aend_ein'
		]
		
		def resultComparisonRegexList = [
		    /* Dates, timestamps */
		    ' [0-9][0-9]\\.[0-9][0-9]\\.\\(20\\)\\?[12][0-9]',
		    ' [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\(\\.[0-9]\\)\\?',
		    ' [0-9][0-9]:[0-9][0-9] Uhr',

		    /* Special cases */
		    /* Gel�schte S�tze aus tempor�ren Hilfsdateien; Zeitstempel D4576BAA65940000 : */
		    'Zeitstempel [A-Za-z0-9]\\{16\\}',
		    /* Zeitstempel f�r Hilfsdateien     : D4576BAA65940000 */
		    /* - Parameter Job-ID           : D4576A6ACCED8000 */
		    /* ID des Jobs : D4576A6ACCED8000 */
		    ' : [A-Za-z0-9]\\{16\\}',
		    /* - Laufzeit                   : 4,6 Sekunden */
		    ' : [0-9]\\+\\(,[0-9]\\)\\? Sekunden',
		    /* ISN Workfile-Referenz            : 41 */
		    'ISN Workfile-Referenz            : [0-9]\\+',
		    /* KO16501001800000KIDICAP 201805171456429 */
		    '1800000KIDICAP 20[12][0-9]\\{12\\}',
		    /* AP1025                                                                               */
		    /* AP10221                                                                              */
		    'AP102[0-9]\\( \\|[0-9]\\) ',
		    /* -XX:InitialHeapSize=523711168 -XX:MaxHeapSize=6442450944 -XX:+PrintCommandLineFlags -XX:+PrintVMOptions -XX:+UseCompressedClassPointers -XX:+UseCompressedOops -XX:+UseParallelGC */
			'.*-XX:InitialHeapSize=.\\+',
			/* VM option '+PrintCommandLineFlags' */
			'VM option .*',
			/* WQATF-603 */
			'.*WARNING: sun\\.reflect\\.Reflection\\.getCallerClass.*'
		]
		
		def resultComparisonExcludedFilesForSteps = [
		    '04': [],
		    '05': [],
		    '06': [],
		    '07': [],
		    '08': [],
		    '09': []
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
							projectName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part0409',
							filter: 'log/**',
							selector: lastSuccessful(),
							parameters: "mxBuildVersion=${mxBuildVersion}",
							target: '.',
							fingerprintArtifacts: true,
							flatten: false
						])
					} else {
						copyArtifacts([
							projectName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part0409',
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
						def expectedFilesDir = "${workDir}/expected/logfiles/agtv2_0409/${subDirForStep}"
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
