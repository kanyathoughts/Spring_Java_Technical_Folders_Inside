@Library('TestUtils') _

/**
 * Run regression tests of the KIDICAP_mini project.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param kidicapBuild  The build version of a KIDICAP_nat2java job to fetch the KIDICAP jars from.
 *        type: Extensible Choice -> File Choice Parameter
 * @param mergeLess If SQL MERGE statements should be used or not. If true, then MERGE statements will be avoided (see WMEE-7333), otherwise they will be used.
 *        type: boolean
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
		
		def regressionSteps
		def subDirForSteps
		
		/* Step 03 is a regression test for WMEE-6819. This ticket has not been fixed for the branches 17IR, 17IR2 and 17IR3. */
		if (mxVersion in '17.0.1' .. '17.0.3') {
			regressionSteps = ['01', '02', '04', '05']
			subDirForSteps = [
			    '01': 'REGR_01',
				'02': 'REGR_02',
				'04': 'REGR_04',
				'05': 'REGR_05'
			]
		/* Steps 06 and 07 have been added to 18.0.0 and all newer versions */
		} else if (mxVersion >= '18.0.0') {
			regressionSteps = ['01', '02', '03', '04', '05', '06', '07']
			subDirForSteps = [
			    '01': 'REGR_01',
			    '02': 'REGR_02',
				'03': 'REGR_03',
				'04': 'REGR_04',
				'05': 'REGR_05',
				'06': 'REGR_06',
				'07': 'REGR_07'
			]
		} else {
			regressionSteps = ['01', '02', '03', '04', '05']
			subDirForSteps = [
			    '01': 'REGR_01',
				'02': 'REGR_02',
				'03': 'REGR_03',
				'04': 'REGR_04',
				'05': 'REGR_05'
			]
		}
		
		def resultComparisonRegexList = [
		    'Seite \\+[0-9]\\+ \\+[0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]  [0-9][0-9]:[0-9][0-9]:[0-9][0-9]',
			/* Ignore all Java command line options. */
		    '.*-XX:.*', '.*VM option .*', '.*WARNING: sun\\.reflect\\.Reflection\\.getCallerClass.*'
		]
		def resultComparisonExcludedFiles = ['ProductionPart*.log']
		
		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "kidicapBuild=${kidicapBuild} mergeLess=${mergeLess} withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"

		try {
			docker.image(dockerImage).withRun {
			    container ->
			    
			    stage('waiting-for-DB') {
					dockerUtils.waitForOracle(container.id, 5)
				}
                
				deleteDir()
                docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("--network=container:${container.id} -v jenkins-m2-repo:/var/m2_repo:rw") {
					workDir = pwd()
				
					stage('initialisation') {
	                    gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
						gitUtils.getSingleFile('infrastructure/licenses', 'gip-customer.lic', mxVersion, workDir)
						if (mxVersion < '19') {
							sh 'mv gip-customer.lic maxenso.lic'					
						} else {
							sh 'mv gip-customer.lic innowake.lic'							
						}
				    	productionAntFile = "${workDir}/run-production-regression.xml"
						spUtils.downloadJobArtifact('TF_Natural_Java_Migration_KIDICAP_Mini', kidicapBuild, 'kidicap.jar', 'KIDICAP_LIBS')
                    	
						buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
	                    antProperties = "-DworkDir=${workDir} -DjvmDir=java -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion} -DdbPort=${dbPort} -DparallelMode=false -DwithCodeCoverage=${withCodeCoverage} -DmxJarsDir=${buildProperties['iwJarDir']} -DmergeLess=${mergeLess} -DmxJarsDir=${buildProperties['iwJarDir']}"
				    	sh returnStatus: true, script: "mkdir -p ${workDir}/result-comparison/log"
						sh "echo -n '' > ${workDir}/result-comparison/log/result-comparison.log"
	                	/* WQATF-435 - fetch artifacts from Nexus using maven. */
	                	withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
	                		sh "$MVN_CMD -f ${workDir}/pom-gip-dependencies.xml dependency:copy-dependencies -DoutputDirectory=${buildProperties['gipJarDir']}"
	                		sh "$MVN_CMD -f ${workDir}/pom-innowake-runtime-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
	                	}
					}
			
					regressionSteps.each {
						step ->
						stage("regression${step}") {
							sh "ant ${antFlags} -buildfile ${productionAntFile} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} runRegression${step}"
							
							def subDirForStep = subDirForSteps[step]
							def actualFilesDir = "${workDir}/log/${subDirForStep}"
							def expectedFilesDir = "${workDir}/expected/logfiles/regression/${subDirForStep}"
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
