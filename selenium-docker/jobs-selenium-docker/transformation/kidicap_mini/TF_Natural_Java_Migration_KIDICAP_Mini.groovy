@Library('TestUtils') _

/**
 * Run the nat2java on the KIDICAP_mini project against a certain maxenso build.
 * Use the signed expert scripts from the TF_Natural_Java_Script_Signing_KIDICAP_Mini job and a customer license.
 *
 * @param mxBuildVersion The maxenso build to use. This is the test object.
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
		def miscUtils = new MiscUtils()
		def resultComparisonUtils = new ResultComparisonUtils()
		def dockerUtils = new DockerUtils()
		def spUtils = new SharepointUtils()
		
		def kidicapProject = "KIDICAP_mini"
		def bflexVariants = ["agtv", "ofdan", "ofdve"]
        def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
        def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/kidicap/kidicap-mini.git'
        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
        def workDir
        def antFlags = ''
        def antFile_nat2Java
        def antFile_bflex
        /* A string with properties to be passed to ant scripts. To be created later, see below. */
        def antProperties
        /* The properties from the file build.properties */
        def buildProperties
                
        withCodeCoverage = Boolean.parseBoolean(withCodeCoverage)
        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"

        try {
			deleteDir()
            docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
            	workDir = pwd()
            	
				stage('setup-test-workspace') {
    				gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
					gitUtils.getSingleFile('infrastructure/licenses', 'gip-customer.lic', mxVersion, '.')
					if (mxVersion < '19') {
						sh 'mv gip-customer.lic maxenso.lic'	
					} else {
						sh 'mv gip-customer.lic innowake.lic'
					}
					spUtils.downloadIwEclipseLinux(mxBuildVersion, "${workDir}/eclipse")
					buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
					antProperties = "-DworkDir=${workDir} -DjvmDir=java -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion} -DwithCodeCoverage=${withCodeCoverage}"
					antFile_nat2Java = "${workDir}/build-nat2java.xml"
					antFile_bflex = "${workDir}/build-bflex.xml"
					sh "mkdir -p ${workDir}/log"
					sh "mkdir -p ${workDir}/result-comparison/log"
					sh "mkdir -p ${workDir}/result-comparison/tmp"

                    /* WQATF-127 - Add version number to mee-source-migration-deps-general.jar and mee-source-migration-dist.jar and update .classpaths */
                    sh "sed -i 's/mee-source-migration-dist.jar/mee-source-migration-dist-${mxBuildVersion}.jar/' ${antFile_nat2Java}"

                	/* WQATF-435 - fetch artifacts from Nexus using maven. */
                	withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                		sh "$MVN_CMD -f ${workDir}/pom-gip-dependencies.xml dependency:copy-dependencies -DoutputDirectory=${buildProperties['gipJarDir']}"
                		sh "$MVN_CMD -f ${workDir}/pom-innowake-transformation-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']}"
                		sh "$MVN_CMD -f ${workDir}/pom-innowake-runtime-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
                	}
                	
					/* WQATF-1000 - compiler level 11 when using java 17 */
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

            	stage('patch') {
            		/* Replace the checked out mee-source-migration-natural-dist project by that provided by the signing job and switch to the customer license. */
            		def migrDir = buildProperties['meeSourceMigrationProject']
					dir(migrDir) {
						deleteDir()
					}
                    spUtils.downloadJobArtifact('TF_Natural_Java_Script_Signing_KIDICAP_Mini', mxBuildVersion, "mee-source-migration-natural-dist-${mxBuildVersion}.zip", workDir)
					unzip dir: 'eclipseWorkspace', zipFile: "mee-source-migration-natural-dist-${mxBuildVersion}.zip", quiet: true
					def eclipseDir = buildProperties['eclipseDir']
                    sh "rm ${eclipseDir}/*.lic; cp innowake.lic ${eclipseDir}/innowake.lic"
                    /* Remove the sources of the customer specific expert scripts.
                     * They are provided by the mee-source-migration-natural-dist-<version>.zip both as source files and packaged in a jar file.
                     * To make it sure that the expert scripts from the jar file are used here the sources are removed now. 
                    */
                    sh "rm -rf ${migrDir}/src"
            	}
            	
                stage('init-nat2java') {
                	withAnt(installation: 'Default') {
                    	/* WQATF-435 - fake a ${workDir}/data/mxJars folder so that init-nat2java can be executed without failure. */
                    	def fakeDataMxJarsDir = "${workDir}/data/mxJars"
                        sh "mkdir -p -m a=rwx ${fakeDataMxJarsDir}"
                		sh "ant ${antFlags} -buildfile ${antFile_nat2Java} ${antProperties} -DtransformCopycodes=true -DmxJarsDir=${fakeDataMxJarsDir} init-nat2java"
                	}
                }

                stage('prepare-eclipse-workspace') {
                	withAnt(installation: 'Default') {
                		sh "ant ${antFlags} -buildfile ${antFile_nat2Java} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} prepare-eclipse-workspace"
                	}
                }

                stage('run-nat2java') {
                	withAnt(installation: 'Default') {
                		sh "ant ${antFlags} -buildfile ${antFile_nat2Java} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} nat2java"
                	}
                }

                stage('pack-kidicap-jar') {
                	withAnt(installation: 'Default') {
                    	sh "ant ${antFlags} -buildfile ${antFile_nat2Java} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} packJars"
                	}
                }

                stage('result-comparison') {
					def eclipseWorkspaceDir = buildProperties['eclipseWorkspaceDir']
                    def actualDir = "${eclipseWorkspaceDir}/KIDICAP-Java/src/java-mee"
                    def expectedDir = "${workDir}/expected/withCopycodes"
                    def compareResult = resultComparisonUtils.resultCompare(actualDir, expectedDir, [], [], "${workDir}/result-comparison/log", "${workDir}/result-comparison/tmp")
                    if (compareResult != 0) {
                    	unstable 'Deviations in file comparison'
                    }
                    def javaErrorsLogFile = "${workDir}/log/java-errors.log"
                    fileSize = miscUtils.getFileSize(javaErrorsLogFile)
                    if (fileSize > 0) {
                    	error "The generated Java code has compile errors, see ${javaErrorsLogFile}"
                    }
                }

                bflexVariants.each {
                    bflexVariant ->
                    stage("compile-bflex-${bflexVariant}") {
                        withAnt(installation: 'Default') {
                        	sh "ant ${antFlags} -buildfile ${antFile_bflex} ${antProperties} -DcompilerLevel=${miscUtils.getCompilerLevel(javaVersion)} -DmxJarsDir=${buildProperties['iwJarDir']} -DbflexLibCompileDir=${buildProperties['iwJarDir']} -DbflexVariant=${bflexVariant} init-bflex-variant compile"
                        }
                    }
                }
            }
        } catch (ex) {
			miscUtils.errorWithStackTrace(ex)
		} finally {
			stage('archive') {
                docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
					archiveArtifacts allowEmptyArchive: true, artifacts: 'log/**/*,result-comparison/,**/*.exec'
                    spUtils.uploadJobArtifact(mxBuildVersion, 'KIDICAP_LIBS/kidicap.jar')
					spUtils.uploadJobArtifact(mxBuildVersion, 'KIDICAP_LIBS/kidicap-src.jar')    
                    bflexVariants.each {
                        bflexVariant ->
                            spUtils.uploadJobArtifact(mxBuildVersion, "KIDICAP_LIBS/bflex-${bflexVariant}.jar")
                    } 
				}
            }
        }
        
    }
    
}
