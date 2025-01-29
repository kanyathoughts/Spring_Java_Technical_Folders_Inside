@Library('TestUtils') _

/**
 * Create MySQL schemas for KIDICAP production jobs.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * 		  type: Boolean
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 * @param paging  Whether paging will be activated or not. If true, paging will be activated. 
 * 				  Note: this features is not available in all product branches.
 *        type: boolean
 * @param mergeLess If SQL MERGE statements should be used or not. If true, then MERGE statements will be avoided (see WMEE-7333), otherwise they will be used.
 * 				    Note: this feature is not available in all product branches.
 *        type: boolean
 * @param parallelMode  Whether to run production steps of KIDICAP parallelized (using the AsyncApi).
 *                      If true, then run them parallelized, otherwise sequentially.
 *                      Note: only steps, that have been enabled for parallelization by GIP can use this mode.
 *        type: boolean
 */

nodeTF('OS-Linux && Region-EU') {
	
	timestamps {
		
		def mxVersionUtils = new MxVersionUtils()
		def gitUtils = new GitUtils()
		def miscUtils = new MiscUtils()
		def compareUtils = new ResultComparisonUtils()
		def dockerUtils = new DockerUtils()
		def spUtils = new SharepointUtils()

		def dbPort = '1521'
        def dbSchemas = ['KIDICAP_R165_AGTV', 'KIDICAP_R165_AGTV2', 'KIDICAP_R165_OFDAN']
				
		def schemaResultComparisonRegexList = [
			/* Maxenso/innowake version number 
			    RETURN '18.0.0.04';
			    RETURN '19.2.00-alpha-202005020352';
			    RETURN '21.1.0-alpha-202105020352';
			*/
			'[0-9][0-9]\\.[0-9]\\.\\([0-9]\\.\\)\\?[0-9].\\+;'
		]

		def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
		def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/kidicap/kidicap.git'
		def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
		def dockerImage = dockerUtils.pullImage('gip-165-agtv-mx16', mxVersion)
		
		def buildProperties
		def workDir
		
        paging = Boolean.parseBoolean(paging)
        mergeLess = Boolean.parseBoolean(mergeLess)
        parallelMode = Boolean.parseBoolean(parallelMode)
        withCodeCoverage = Boolean.parseBoolean(withCodeCoverage)

		buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
		buildDescription "withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch} paging=${paging} mergeLess=${mergeLess} parallelMode=${parallelMode}"

        deleteDir()
        docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
            workDir = pwd()
            def schemaDir = "${workDir}/eclipseWorkspace/KIDICAP-Java/res/schema_oracle_DS_Trigger"
            def antFlags = ''
            def antFile = "${workDir}/build-schema.xml"
            /* WQATF-435 - fake a ${workDir}/data/mxJars folder so that init-schemaGeneration and init-nat2java can be executed without failure. */
            def fakeDataMxJarsDir = "${workDir}/data/mxJars"

            stage('setup-test-workspace') {
                gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                spUtils.downloadIwEclipseLinux(mxBuildVersion, "${workDir}/eclipse")
                buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
                gitUtils.getLicenseFile(mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion), workDir)
                eclipseWorkspaceDir = "${workDir}/eclipseWorkspace"

                sh "mkdir -p ${workDir}/log"
                sh "mkdir -p ${workDir}/result-comparison/log"
                sh "mkdir -p ${workDir}/result-comparison/tmp"
                /* WQATF-435 - fetch artifacts from Nexus using maven. */
                withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                    sh "$MVN_CMD -f ${workDir}/pom-gip-dependencies.xml dependency:copy-dependencies -DoutputDirectory=${buildProperties['gipJarDir']}"
                    sh "$MVN_CMD -f ${workDir}/pom-innowake-transformation-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
                    sh "$MVN_CMD -f ${workDir}/pom-innowake-runtime-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
                }

                if (javaVersion == 'java17') {
                    def content = readFile 'eclipseWorkspace/KIDICAP-Java/.settings/org.eclipse.jdt.core.prefs'
                    content += 'org.eclipse.jdt.core.compiler.codegen.inlineJsrBytecode=enabled\n' +
                        'org.eclipse.jdt.core.compiler.codegen.targetPlatform=11\n' +
                        'org.eclipse.jdt.core.compiler.compliance=11\n' +
                        'org.eclipse.jdt.core.compiler.problem.assertIdentifier=error\n' +
                        'org.eclipse.jdt.core.compiler.problem.enablePreviewFeatures=disabled\n' +
                        'org.eclipse.jdt.core.compiler.problem.enumIdentifier=error\n' +
                        'org.eclipse.jdt.core.compiler.problem.reportPreviewFeatures=warning\n' +
                        'org.eclipse.jdt.core.compiler.release=enabled\n' +
                        'org.eclipse.jdt.core.compiler.source=11'
                    writeFile encoding: 'UTF-8', file: 'eclipseWorkspace/KIDICAP-Java/.settings/org.eclipse.jdt.core.prefs', text: content
                }
            }

            dbSchemas.each {
                dbSchema ->
                stage(dbSchema) {
                    //init
                    def antProperties = "-DworkDir=${workDir} -DjvmDir=java -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion} -DdbPort=${dbPort} -DparallelMode=${parallelMode} -Dpaging=${paging} -DmergeLess=${mergeLess} -DwithCodeCoverage=${withCodeCoverage} -DdbSchema=${dbSchema}"
                    sh "mkdir -p -m a=rwx ${fakeDataMxJarsDir}"
                    sh "ant -buildfile ${antFile} ${antProperties} -DmxJarsDir=${fakeDataMxJarsDir} init-schemaGeneration"

                    //prepare
                    withAnt(installation: 'Default') {
                        sh "ant ${antFlags} -buildfile ${antFile} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} prepare-eclipse-workspace"
                    }

                    //generate
                    sh "ant -buildfile ${antFile} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} schemaGeneration"

                    //validity check
                    sh "rm -f ${schemaDir}/README.sql"
                    def expectedDir = "${workDir}/expected/logfiles/dbSchema/agtv"
                    if (dbSchema == 'KIDICAP_R165_AGTV2') {
                        expectedDir += '2'
                    }
                    if (dbSchema == 'KIDICAP_R165_OFDAN') {
                        expectedDir = expectedDir.replace('agtv', 'ofdan')
                    }
                    def resultComparisonLogDir = "${workDir}/result-comparison-schema/log"
                    def resultComparisonTmpDir = "${workDir}/result-comparison-schema/tmp"
                    sh returnStatus: true, script: "mkdir -p ${resultComparisonLogDir}"
                    sh returnStatus: true, script: "mkdir -p ${resultComparisonTmpDir}"
                    def compareResult = compareUtils.resultCompare(schemaDir, expectedDir, schemaResultComparisonRegexList, [], resultComparisonLogDir, resultComparisonTmpDir)
                    def resultCompareZipName = "result-comparision-${dbSchema}.zip"
                    zip dir: workDir, glob: 'result-comparison-schema/**/*', zipFile: resultCompareZipName, archive: false
                    spUtils.uploadJobArtifact(mxBuildVersion, resultCompareZipName)
                    if (compareResult != 0) {
                        unstable 'Deviations in file comparison'
                    }

                    //publish
                    def zipName = "schema-${dbSchema}.zip"
                    zip dir: "${workDir}/eclipseWorkspace/KIDICAP-Java/res/", glob: 'schema_oracle_DS_Trigger/**/*', zipFile: zipName, archive: false
                    spUtils.uploadJobArtifact(mxBuildVersion, zipName)
                }
            }
        }
	}
}
