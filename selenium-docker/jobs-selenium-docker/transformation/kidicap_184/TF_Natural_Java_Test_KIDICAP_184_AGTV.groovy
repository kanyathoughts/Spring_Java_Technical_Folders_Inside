@Library('TestUtils') _

/**
 * Run the production of the KIDICAP 18.4 project.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param kidicapBuild  The build version of a KIDICAP_nat2java job to fetch the KIDICAP jars from.
 *        type: Extensible Choice -> File Choice Parameter
 * @param parallelMode  Whether to run production steps of KIDICAP parallelized (using the AsyncApi).
 *                      If true, then run them parallelized, otherwise sequentially.
 *                      Note: only steps, that have been enabled for parallelization by GIP can use this mode.
 *        type: boolean
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * 		  type: Boolean
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param performanceProfiling  Flag to switch on/off performance profiling
 *        type: boolean
 */

nodeTF('Docker-host && Region-EU', Boolean.parseBoolean(performanceProfiling)) {
	if (Boolean.parseBoolean(performanceProfiling) && Boolean.parseBoolean(withCodeCoverage)) {
		echo 'Exiting job...'
		error 'The code coverage measurements affect the performance therefore we should not measure the code coverage and the performance at the same time. ' +
				'Please choose either of performanceProfiling or withCodeCoverage to true'
	}

	timestamps {
		
		def mxVersionUtils = new MxVersionUtils()
		def gitUtils = new GitUtils()
		def miscUtils = new MiscUtils()
		def compareUtils = new ResultComparisonUtils()
		def dockerUtils = new DockerUtils()
		def performanceUtils = new PerformanceUtils()
		def spUtils = new SharepointUtils()
		def prodUtils = new ProductDeliveryUtils()

		def dbPort = '1521'
		def dbSchema = 'KIDICAP_R184_AGTV_DEVEL'
		def productionSteps = ['01', '02', '03', '04', '05', '07', '08', '09', '10', '11', '12', '13', '16', '17', '18', '19', '21', '23']
		def subDirForSteps = [
			'01': 'AGTV_01_RZ_Plausi',
			'02': 'AGTV_02_Plausi_Protokoll',
			'03': 'AGTV_03_Vorinstanz',
			'04': 'AGTV_04_Berechnung',
			'05': 'AGTV_05_BG_Abrechnung',
			'07': 'AGTV_07_ZVK',
			'08': 'AGTV_08_ErgPaket',
			'09': 'AGTV_09_Abstimmung',
			'10': 'AGTV_10_Ueberleitung',
			'11': 'AGTV_11_UL_Listen',
			'12': 'AGTV_12_UL_kameral',
			'13': 'AGTV_13_UL_kaufm',
			'16': 'AGTV_16_DTR_ZVK_Liefer',
			'17': 'AGTV_17_DTR_ZVK_erstellen',
			'18': 'AGTV_18_Bezugsartenliste',
			'19': 'AGTV_19_Zahlbarmachung',
			'21': 'AGTV_21_VB_Statistik',
			'23': 'AGTV_23_Nachinstanz'
		]
		
		def schemaResultComparisonRegexList = [
			/* Maxenso/innowake version number 
			    RETURN '18.0.0.04';
			    RETURN '19.2.00-alpha-202005020352';
			    RETURN '21.1.0-alpha-202105020352';
			*/
			'[0-9][0-9]\\.[0-9]\\.\\([0-9]\\.\\)\\?[0-9].\\+;'
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
		       18-04-05
		       Leading or trailing blanks or other characters have to match too.
		       Otherwise this could match anything else - insurance customer numbers or alike.
		    */
		    ' [0-9][0-9]\\.[0-9][0-9]\\.20[12][0-9]',
		    ' [0-9][0-9]\\.[0-9][0-9]\\.[12][0-9]',
		    '[12][0-9]\\-[0-9][0-9]\\-[0-9][0-9] ',
		    /* <CreDtTm>2018-04-05T20:20:19Z</CreDtTm> */
		    '<CreDtTm>20[12][0-9]\\-[0-9][0-9]\\-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z</CreDtTm>',
		    
		    /* Special cases */
		    /* Timestamp: D429E797521C0000 */
		    'Timestamp: .\\+',
		    /* Seite      7                                                 11.04.18  10:11:47 */
		    'Seite \\+[0-9]\\+ ',
		    /* -XX:InitialHeapSize=523711168 -XX:MaxHeapSize=6442450944 -XX:+PrintCommandLineFlags -XX:+PrintVMOptions -XX:+UseCompressedClassPointers -XX:+UseCompressedOops -XX:+UseParallelGC */
		    '.*-XX:InitialHeapSize=.\\+',
		    /* VM option '+PrintCommandLineFlags' */
		    'VM option .*',
		    /* WQATF-602 */
		    '.*WARNING: sun\\.reflect\\.Reflection\\.getCallerClass.*'
		]
		def resultComparisonExcludedFilesForSteps = [
			'01': [],
			'02': [],
			'03': [],
			'04': [],
			'05': [],
			'07': [],
			'08': ['CMWKF01.DAT' /* binary file */, 'CMWKF06.DAT' /* binary file */, 'CMWKF08.DAT' /* binary file */],
			'09': [],
			'10': [],
			'11': ['CMWKF01.DAT' /* binary file */],
			'12': ['CMWKF04.DAT' /* Content of this file changes twice a year */],
			'13': ['CMWKF01.DAT' /* binary file */, 'CMWKF08.DAT' /* binary file */],
			'16': [],
			'17': ['CMWKF04.DAT', 'CMWKF05.DAT' /* Content of these two files changes twice a year */],
			'18': ['CMWKF01.DAT' /* binary file */],
			'19': ['CMWKF03.DAT' /* binary file */],
			'21': [],
			'23': []
		]
		
		def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
		def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/kidicap/kidicap-184.git'
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		def dockerImage = dockerUtils.pullImage('gip-184-agtv-mx17', mxVersion)
		def workDir
		def antFlags = ''
		def antProperties
		def productionAntFile
		def prepareDbAntFile
		def schemaDir
		def antFile_schemaGeneration
		def buildProperties
		
		parallelMode = Boolean.parseBoolean(parallelMode)
		
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "kidicapBuild=${kidicapBuild} parallelMode=${parallelMode} withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} performanceProfiling=${performanceProfiling}"
		
		try {
			deleteDir()
			docker.image(dockerImage).withRun {
				container ->
			    
				stage('waiting-for-DB') {
					dockerUtils.waitForOracle(container.id, 5)
				}
			    
				stage('retrieve-DB-schema') {
					workDir = pwd()
					docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {      
						gitUtils.getSingleFile(remoteProjectLocation,'build.properties', testProjectBranch, workDir)
						buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
						withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
							def fileName = 'innowake-mee-udf-oracle5'
							def groupID = 'innowake.products.mee.runtime.natural.datastore'
							sh "$MVN_CMD dependency:copy -Dartifact=${groupID}:${fileName}:${kidicapBuild}:jar -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
						}
						spUtils.downloadAndExtractJobArtifact('TF_Natural_Java_Schema_Generation_KIDICAP_184', kidicapBuild, "schema-${dbSchema}.zip", workDir)
						sh "rm -f schema-${dbSchema}.zip"
					}
				}
				 	
				stage('init-schema-reload') {
					sh "docker cp ${buildProperties['iwJarDir']}/innowake-mee-udf-oracle5.jar ${container.id}:/home/oracle"
					sh "docker cp schema_oracle_DS_Trigger ${container.id}:/home/oracle/schema_oracle_DS_Trigger"
					sh "docker exec -u root --workdir / ${container.id} chown -R oracle:dba /home/oracle/schema_oracle_DS_Trigger"
					sh "docker exec -u root --workdir / ${container.id} chown -R oracle:dba /home/oracle/innowake-mee-udf-oracle5.jar"
				}
				
				stage('reload-UDF-jar') {
					buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'bflexVariant': 'agtv', 'mxBuildVersion': mxBuildVersion, 'dbPort': dbPort])
					def hostname = 'localhost'
					def dbConnectString
					withCredentials([usernamePassword(credentialsId: 'gip184agtv', passwordVariable: 'pass', usernameVariable: 'user')]) {
						dbConnectString = "$user/$pass@//${hostname}:${dbPort}/${buildProperties.oraDbServiceName}"
					}
					def dropJavaLog = sh returnStdout: true, script: "docker exec ${container.id} sh dropjava -user ${dbConnectString} -jarsasdbobjects /home/oracle/innowake-mee-udf-oracle5.jar"
					echo dropJavaLog
					def loadJavaLog = sh returnStdout: true, script: "docker exec ${container.id} sh loadjava -user ${dbConnectString} -jarsasdbobjects /home/oracle/innowake-mee-udf-oracle5.jar"
					echo loadJavaLog
					if ( ! dropJavaLog.isEmpty() || ! loadJavaLog.isEmpty()) {
						unstable 'Something went wrong during the reload of the UDF-jar.'
					} else {
						echo 'UDF-jar successfully reloaded'
					}
				}
				
				stage('reload-schema') {
					sh returnStdout: true, script: "docker exec -i ${container.id} rm -f ${schemaDir}/README.sql"
					withCredentials([usernamePassword(credentialsId: 'gip184agtv', passwordVariable: 'pass', usernameVariable: 'user')]) {
						def sqlplusScript = "/bin/sh -c 'for entry in /home/oracle/schema_oracle_DS_Trigger/*;do sqlplus $user/$pass@${buildProperties.oraDbServiceName} @\$entry;done'"
						def output = sh returnStdout: true, script: "docker exec -i ${container.id} ${sqlplusScript}"
						echo output
					}
				}
                
				deleteDir()
				docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("--network=container:${container.id} -v jenkins-m2-repo:/var/m2_repo:rw") {
					workDir = pwd()
                	
					stage('Initialize workspace') {
						gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
						gitUtils.getLicenseFile(mxVersion, workDir)
						spUtils.downloadJobArtifact('TF_Natural_Java_Migration_KIDICAP_184', kidicapBuild, 'kidicap.jar', 'KIDICAP_LIBS')
						spUtils.downloadJobArtifact('TF_Natural_Java_Migration_KIDICAP_184', kidicapBuild, 'bflex-agtv.jar', 'KIDICAP_LIBS')
						antProperties = "-DworkDir=${workDir} -DjvmDir=java -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion} -DdbPort=${dbPort} -DparallelMode=${parallelMode} -DwithCodeCoverage=${withCodeCoverage} -Dinnowake.lib.core.profile.ProfilingEnabled=${performanceProfiling}"
						sh returnStatus: true, script: "mkdir -p ${workDir}/result-comparison/log"
						sh "echo -n '' > ${workDir}/result-comparison/log/result-comparison.log"
						productionAntFile = "${workDir}/run-production-agtv.xml"
						prepareDbAntFile  = "${workDir}/prepare-DB.xml"
						buildProperties = miscUtils.readPropertyFile("${workDir}/build.properties", ['workDir': workDir, 'bflexVariant': 'agtv', 'mxBuildVersion': mxBuildVersion, 'dbPort': dbPort])
						def antPropertiesDbCommon = "${antProperties} -DdbDriver=${buildProperties.dbDriver} -DdbDriverClasspath=${buildProperties.dbDriverClasspath} -DdbUrl=${buildProperties.dbConnectionString}"
						def dbPropsRetention = "${antPropertiesDbCommon} -DdbUser=${buildProperties.oraSystemUser} -DdbPassword=${buildProperties.oraSystemUserPassword}"
						/* WQATF-435 - fetch artifacts from Nexus using maven. */
						withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
							sh "$MVN_CMD -f ${workDir}/pom-gip-dependencies.xml dependency:copy-dependencies -DoutputDirectory=${buildProperties['gipJarDir']}"
							sh "$MVN_CMD -f ${workDir}/pom-innowake-runtime-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
						}
						sh "ant ${antFlags} -buildfile ${prepareDbAntFile} ${dbPropsRetention} adjustRetention"
					}
				
					productionSteps.each {
						step ->
						stage("productionPart${step}") {
							sh "ant ${antFlags} -buildfile ${productionAntFile} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} runProductionPart${step}"
							
							def subDirForStep = subDirForSteps[step]
							def actualFilesDir = "${workDir}/log/${subDirForStep}"
							def expectedFilesDir = "${workDir}/expected/logfiles/agtv/${subDirForStep}"
							def resultComparisonLogDir = "${workDir}/result-comparison/log/${subDirForStep}"
							def resultComparisonTmpDir = "${workDir}/result-comparison/tmp/${subDirForStep}"
							sh returnStatus: true, script: "mkdir -p ${resultComparisonLogDir}"
							sh returnStatus: true, script: "mkdir -p ${resultComparisonTmpDir}"
							def compareResult = compareUtils.resultCompare(actualFilesDir, expectedFilesDir, resultComparisonRegexList, resultComparisonExcludedFilesForSteps[step], resultComparisonLogDir, resultComparisonTmpDir, parallelMode)
							if (compareResult != 0) {
								unstable "Deviation found in step \"${step}\""
								sh "echo Deviation found in step \"${step}\" >> ${workDir}/result-comparison/log/result-comparison.log"
							}
							
							def errorLogFile = "${actualFilesDir}/CMPRINT-error.log"
							if (miscUtils.getFileSize(errorLogFile) > 0) {
								error "There have been errors, file ${errorLogFile} is not empty"
							}
							if (miscUtils.isTextInFiles('Exception', 'CMPRINT.log', actualFilesDir)) {
								error "There have been Exceptions in some CMPRINT.log in directory ${actualFilesDir}"
							}
						}
					}

					stage('reporting') {
						when(Boolean.parseBoolean(performanceProfiling) && currentBuild.currentResult == 'SUCCESS',
							'Performance reporting is not executed if the job is unstable or performanceProfiling=false') {
							def pathToLogFile = performanceUtils.mergeLogFiles('log', 'mergedPerformanceLogs.log', 'Production*.log')

							archiveArtifacts pathToLogFile
							def buildResult = build job: 'TF_Performance_Reporting', propagate: false, parameters: [
								string(name: 'mxBuildVersion',    value: mxBuildVersion),
								string(name: 'pathToLogFile',     value: pathToLogFile),
								string(name: 'buildURL',          value: BUILD_URL),
								string(name: 'pageID',            value: '166907822'),
								string(name: 'numDaysDataToKeep', value: '20')
							]
							miscUtils.evaluateBuildResult(buildResult, "TF_Performance_Reporting")
						}
					}
				}
			}
		} catch (ex) {
			miscUtils.errorWithStackTrace(ex)
		} finally {
			stage('finalize') {
				docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
					zip dir: "${workDir}/result-comparison", zipFile: "result-comparison.zip"
					archiveArtifacts allowEmptyArchive: true, artifacts: 'log/**/*,coverage/*.exec'
					spUtils.uploadJobArtifact(mxBuildVersion, 'result-comparison.zip')
				}
			}
		}	
	}
}
