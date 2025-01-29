@Library('TestUtils') _

/**
 * Do a result comparison of the actual output of a KIDICAP AGTV2 production part0103 against the expected output.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param productionRun  The AGTV2 part0103 production to fetch the output files from.
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
		
		def productionSteps = ['01', '02', '03']
		def subDirForSteps = [
		    '01': '01_bflex_bd_awb_list',
		    '02': '02_bflex_bd_awb_such',
		    '03': '03_bflex_bd_sfb',
		]
		
		def resultComparisonRegexList = [
		    /* Dates, timestamps */
		    ' [0-9][0-9]\\.[0-9][0-9]\\.\\(20\\)\\?[12][0-9]',
		    ' [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\(\\.[0-9]\\)\\?',
		    
		    /* Special cases */
		    /* - Parameter Job-ID           : D45E82234BF90000 */
		    'Job-ID           : [A-Za-z0-9]\\{16\\}',
		    /*  Workfile geschlossen; ID: D45E8224430B0000 */
		    'ID: [A-Za-z0-9]\\{16\\}',
		    /* - Laufzeit                   : 1380,1 Sekunden */
		    ' : [0-9]\\+\\(,[0-9]\\)\\? Sekunden',
		    /* DATEINAME       =  PBNTXX-L.20180523.061957 */
		    /* DATEINAME       =  PBNTTM-S.20180523.064313 */
			' =  PBNT\\(XX-L\\|TM-S\\)\\.20[12][0-9]\\{5\\}\\.[0-9]\\{6\\}',
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
		    '03': []
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
							projectName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part0103',
							filter: 'log/**',
							selector: lastSuccessful(),
							parameters: "mxBuildVersion=${mxBuildVersion}",
							target: '.',
							fingerprintArtifacts: true,
							flatten: false
						])
					} else {
						copyArtifacts([
							projectName: 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part0103',
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
						def expectedFilesDir = "${workDir}/expected/logfiles/agtv2_0103/${subDirForStep}"
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
