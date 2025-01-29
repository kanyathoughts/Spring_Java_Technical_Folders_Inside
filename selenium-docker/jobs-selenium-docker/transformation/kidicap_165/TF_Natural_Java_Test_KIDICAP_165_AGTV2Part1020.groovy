@Library('TestUtils') _

/**
 * Run a production of the KIDICAP project.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param kidicapBuild  The build version of a KIDICAP_nat2java job to fetch the KIDICAP jars from.
 *        type: Extensible Choice -> File Choice Parameter
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 *        type: Boolean
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
        def miscUtils = new MiscUtils()
        def dockerUtils = new DockerUtils()
        def compareUtils = new ResultComparisonUtils()
		def spUtils = new SharepointUtils()
		def prodUtils = new ProductDeliveryUtils()
        
        def dbPort = '1521'
        def dbSchema = 'KIDICAP_R165_AGTV2'
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
        def schemaResultComparisonRegexList = [
			/* Maxenso/innowake version number 
			    RETURN '18.0.0.04';
			    RETURN '19.2.00-alpha-202005020352';
			    RETURN '21.1.0-alpha-202105020352';
			*/
			'[0-9][0-9]\\.[0-9]\\.\\([0-9]\\.\\)\\?[0-9].\\+;'
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
        
        def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
        def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/kidicap/kidicap.git'
        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
        def dockerImage = dockerUtils.pullImage('gip-165-agtv2-mx17', mxVersion)
        def workDir
        def antFlags = ''
        /* A string with properties to be passed to ant scripts. To be created later, see below. */
        def antProperties
        def productionAntFile
        def prepareDbAntFile
        def schemaDir
        def antFile_schemaGeneration
        def buildProperties
        
        withCodeCoverage = Boolean.parseBoolean(withCodeCoverage)
        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "kidicapBuild=${kidicapBuild} withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"

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
						spUtils.downloadAndExtractJobArtifact('TF_Natural_Java_Schema_Generation_KIDICAP_165', kidicapBuild, "schema-${dbSchema}.zip", workDir)
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
					withCredentials([usernamePassword(credentialsId: 'gip165agtv2', passwordVariable: 'pass', usernameVariable: 'user')]) {
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
					withCredentials([usernamePassword(credentialsId: 'gip165agtv2', passwordVariable: 'pass', usernameVariable: 'user')]) {
						def sqlplusScript = "/bin/sh -c 'for entry in /home/oracle/schema_oracle_DS_Trigger/*;do sqlplus $user/$pass@${buildProperties.oraDbServiceName} @\$entry;done'"
						def output = sh returnStdout: true, script: "docker exec -i ${container.id} ${sqlplusScript}"
						echo output
					}
				}
                
				deleteDir()
                docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside("--network=container:${container.id} -v jenkins-m2-repo:/var/m2_repo:rw") {
                	workDir = pwd()
            
	            	stage('initialisation') {
                		gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
						gitUtils.getLicenseFile(mxVersion, workDir)
		                spUtils.downloadJobArtifact('TF_Natural_Java_Migration_KIDICAP_165', kidicapBuild, 'kidicap_transformCopycodes=true.jar', 'KIDICAP_LIBS')
						sh 'mv KIDICAP_LIBS/kidicap_transformCopycodes=true.jar KIDICAP_LIBS/kidicap.jar'
						spUtils.downloadJobArtifact('TF_Natural_Java_Migration_KIDICAP_165', kidicapBuild, 'bflex-agtv_transformCopycodes=true.jar', 'KIDICAP_LIBS')
						sh 'mv KIDICAP_LIBS/bflex-agtv_transformCopycodes=true.jar KIDICAP_LIBS/bflex-agtv.jar'
		                antProperties = "-DworkDir=${workDir} -DjvmDir=java -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion} -DdbPort=${dbPort} -DwithCodeCoverage=${withCodeCoverage}"
		                sh returnStatus: true, script: "mkdir -p ${workDir}/result-comparison/log"
		                sh "echo -n '' > ${workDir}/result-comparison/log/result-comparison.log"
		                productionAntFile = "${workDir}/run-production-agtv2.xml"
		        		prepareDbAntFile = "${workDir}/prepare-DB.xml"
		                buildProperties = miscUtils.readPropertyFile("${workDir}/build.properties", ['workDir': workDir, 'bflexVariant': 'agtv', 'mxBuildVersion': mxBuildVersion, 'dbPort': dbPort])
		                def antPropertiesDbCommon = "${antProperties} -DdbDriver=${buildProperties.dbDriver} -DdbDriverClasspath=${buildProperties.dbDriverClasspath} -DdbUrl=${buildProperties.dbConnectionString}"
		                def dbPropsRetention = "${antPropertiesDbCommon} -DdbUser=${buildProperties.oraSystemUser} -DdbPassword=${buildProperties.oraSystemUserPassword}"
			            /* WQATF-435 - fetch artifacts from Nexus using maven. */
			            withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
			               	sh "$MVN_CMD -f ${workDir}/pom-gip-dependencies.xml dependency:copy-dependencies -DoutputDirectory=${buildProperties['gipJarDir']}"
			               	sh "$MVN_CMD -f ${workDir}/pom-innowake-runtime-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
			            }
		                sh "ant ${antFlags} -buildfile ${prepareDbAntFile} ${dbPropsRetention} -DmxJarsDir=${buildProperties['iwJarDir']} adjustRetention"
	            	}
			        
			        productionSteps.each {
			            step ->
			            stage("batchPart${step}") {
			                sh "ant ${antFlags} -buildfile ${productionAntFile} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} runBatchPart${step}"
			                
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
				docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
					zip dir: "${workDir}/result-comparison", zipFile: "result-comparison.zip"
					archiveArtifacts allowEmptyArchive: true, artifacts: 'log/**/*, coverage/*.exec'
					spUtils.uploadJobArtifact(mxBuildVersion, 'result-comparison.zip')
				}
            }
        }
    }
}
