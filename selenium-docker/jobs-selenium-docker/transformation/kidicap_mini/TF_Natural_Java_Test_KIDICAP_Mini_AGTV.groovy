@Library('TestUtils') _

/**
 * Run a production of the KIDICAP_mini project.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param kidicapBuild  The build version of a KIDICAP_nat2java job to fetch the KIDICAP jars from.
 *        type: Extensible Choice -> File Choice Parameter
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * 		  type: Boolean
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

nodeTF('Docker-host && Region-EU') {

	timestamps {
		
		def mxVersionUtils = new MxVersionUtils()
		def gitUtils = new GitUtils()
		def compareUtils = new ResultComparisonUtils()
		def dockerUtils = new DockerUtils()
		def miscUtils = new MiscUtils()
		def spUtils = new SharepointUtils()
		
		def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
		def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/kidicap/kidicap-mini.git'
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		withCodeCoverage = Boolean.parseBoolean(withCodeCoverage)
		
		def dbPort = '1521'
		def dbSchema = 'KIDICAP_R154_AGTV'
		def dockerImage = dockerUtils.pullImage('gip-154-agtv-mx16', mxVersion)
		def workDir
		def antFlags = ''
		def antProperties
		def buildProperties
        def productionAntFile
		def productionSteps = ['MetaData', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '21', '23']
		def jarsForDownload = ['bflex-agtv', 'kidicap']

		def subDirForSteps = [
		    'MetaData': 'AGTV_Metadata',
			'01': 'AGTV_01_RZ_Plausi',
			'02': 'AGTV_02_Plausi_Protokoll',
			'03': 'AGTV_03_Vorinstanz',
			'04': 'AGTV_04_Berechnung',
			'05': 'AGTV_05_BG_Abrechnung',
			'06': 'AGTV_06_DEUEV',
			'07': 'AGTV_07_ZVK',
			'08': 'AGTV_08_ErgPaket',
			'09': 'AGTV_09_Abstimmung',
			'10': 'AGTV_10_Ueberleitung',
			'11': 'AGTV_11_UL_Listen',
			'12': 'AGTV_12_UL_kameral',
			'13': 'AGTV_13_UL_kaufm',
			'14': 'AGTV_14_DTR_DEUEV_BDO',
			'15': 'AGTV_15_DTR_DEUEV_VDAK',
			'16': 'AGTV_16_DTR_ZVK_Liefer',
			'17': 'AGTV_17_DTR_ZVK_erstellen',
			'18': 'AGTV_18_Bezugsartenliste',
			'19': 'AGTV_19_Zahlbarmachung',
			'21': 'AGTV_21_VB_Statistik',
			'23': 'AGTV_23_Nachinstanz'
		]
		
		def resultComparisonRegexList = [
			/* Ignore all Java command line options. */
			'.*-XX:.*', '.*VM option .*', '.*WARNING: sun\\.reflect\\.Reflection\\.getCallerClass.*'
		]
		def resultComparisonExcludedFiles = ['ProductionPart*.log']
		
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "kidicapBuild=${kidicapBuild} withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"
		
		try {
			docker.image(dockerImage).withRun {
			    container ->
			    
			    stage('waiting-for-DB') {
					dockerUtils.waitForOracle(container.id, 5)
				}
				
                deleteDir()
                docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("--network=container:${container.id} -v jenkins-m2-repo:/var/m2_repo:rw") {
                    workDir = pwd()
            		if ( ! mxVersionUtils.isSince_mx16(mxBuildVersion)) {
            			productionSteps = productionSteps.drop(1)
            			subDirForSteps = subDirForSteps.drop(1)
            		}
                    
                    stage('initialisation') {
                    	gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
						gitUtils.getSingleFile('infrastructure/licenses', 'gip-customer.lic', mxVersion, workDir)
						if (mxVersion < '19') {
							sh 'mv gip-customer.lic maxenso.lic'					
						} else {
							sh 'mv gip-customer.lic innowake.lic'							
						}
                    	productionAntFile = "${workDir}/run-production-agtv.xml"
						jarsForDownload.each {
							jarForDownload ->
                    			spUtils.downloadJobArtifact('TF_Natural_Java_Migration_KIDICAP_Mini', kidicapBuild, "${jarForDownload}.jar", 'KIDICAP_LIBS')
                    	}
						buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
                        antProperties = "-DworkDir=${workDir} -DjvmDir=java -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion} -DdbPort=${dbPort} -DparallelMode=false -DwithCodeCoverage=${withCodeCoverage} -DmxJarsDir=${buildProperties['iwJarDir']}"
				    	sh returnStatus: true, script: "mkdir -p ${workDir}/result-comparison/log"
						sh "echo -n '' > ${workDir}/result-comparison/log/result-comparison.log"
	                	/* WQATF-435 - fetch artifacts from Nexus using maven. */
	                	withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
	                		sh "$MVN_CMD -f ${workDir}/pom-gip-dependencies.xml dependency:copy-dependencies -DoutputDirectory=${buildProperties['gipJarDir']}"
	                		sh "$MVN_CMD -f ${workDir}/pom-innowake-runtime-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
	                	}
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
							def compareResult = compareUtils.resultCompare(actualFilesDir, expectedFilesDir, resultComparisonRegexList, resultComparisonExcludedFiles, resultComparisonLogDir, resultComparisonTmpDir)

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
    			}
    		}
		} catch (ex) {
			miscUtils.errorWithStackTrace(ex)
		} finally {
			stage('finalize') {
				archiveArtifacts allowEmptyArchive: true, artifacts: 'log/**/*,result-comparison/**/*,coverage/*.exec'
			}
		}
		
	}
	
}
