@Library('TestUtils') _

/**
 * Run the ReferenceCollector expert script on the KIDICAP_mini project against a certain maxenso build.
 * Use the signed expert scripts from the TF_Natural_Java_Script_Signing_KIDICAP_Mini job and a customer license.
 *
 * @param mxBuildVersion The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
 * @param javaVersion The java version the test will run with
 *        type: Choice Parameter
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 *         If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */

nodeTF('Docker-host && Region-EU') {

    timestamps {

        def mxVersionUtils = new MxVersionUtils()
        def gitUtils = new GitUtils()
        def miscUtils = new MiscUtils()
        def dockerUtils = new DockerUtils()
        def spUtils = new SharepointUtils()

        def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
        def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/kidicap/kidicap-mini.git'
        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
        def workDir
        def antFileReferences
        /* A string with properties to be passed to ant scripts. To be created later, see below. */
        def antProperties
        /* The properties from the file build.properties */
        def buildProperties

        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"

        try {
            deleteDir()
            docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                workDir = pwd()

                stage('setup-test-workspace') {
                    gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                    gitUtils.getSingleFile('infrastructure/licenses', 'gip-customer.lic', mxVersion, '.')
                    sh 'mv gip-customer.lic innowake.lic'
                    spUtils.downloadIwEclipseLinux(mxBuildVersion, "${workDir}/eclipse")
                    buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
                    antProperties = "-DworkDir=${workDir} -DjvmDir=java -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion}"
                    antFileReferences = "${workDir}/build-references.xml"
                    sh "mkdir -p ${workDir}/log"

                    /* WQATF-127 - Add version number to mee-source-migration-deps-general.jar and mee-source-migration-dist.jar and update .classpaths */
                    sh "sed -i 's/mee-source-migration-dist.jar/mee-source-migration-dist-${mxBuildVersion}.jar/' ${workDir}/build-references.xml"

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

                stage('prepare-eclipse-workspace') {
                    withAnt(installation: 'Default') {
                        sh "ant -buildfile ${antFileReferences} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} prepare-eclipse-workspace"
                    }
                }

                stage('collect-references') {
                    withAnt(installation: 'Default') {
                        sh "ant -buildfile ${antFileReferences} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} references"
                    }
                }

                stage('result-comparison') {
                    def eclipseWorkspaceDir = buildProperties['eclipseWorkspaceDir']
                    def diffRC = sh returnStatus:true, script: "diff -q ${eclipseWorkspaceDir}/KIDICAP-PREP/output.txt ${workDir}/expected/referenceCollector/output.txt"
                    if (diffRC != 0) {
                        unstable 'Deviations in file comparison'
                    }
                }
            }
        } catch (ex) {
            miscUtils.errorWithStackTrace(ex)
        } finally {
            stage('archive') {
                archiveArtifacts allowEmptyArchive: true, artifacts: 'log/**/*, eclipseWorkspace/KIDICAP-PREP/output.txt'
            }
        }
    }
}
