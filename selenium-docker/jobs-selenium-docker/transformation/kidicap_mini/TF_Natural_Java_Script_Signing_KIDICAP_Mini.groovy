@Library('TestUtils') _

/**
 * Performs signing of expert scripts of the KIDICAP_mini project.
 * 
 * The job provides a complete Eclipse project "mee-source-migration-natural-dist" as job artifact. It contains the customers expert scripts packed in a jar file and an appropriate signature:
 * - lib/expert-scripts.jar
 * - signature/sig/.script-sign
 * The .classpath file of the project does not refer to expert script sources in the src/ folder, but to the jar file instead.
 * The mee-soure-migration-natural-dist artifact is ready to be sent to the customer or pulled by other jobs, in particular the TF_Natural_Java_Migration_KIDICAP_Mini job.
 * No further modifications are required by its user.
 * 
 * @param mxBuildVersion  The maxenso build to use. This is the test object.
 *        type: Extensible Choice -> File Choice Parameter
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
        def spUtils = new SharepointUtils()
        
        def kidicapProject = 'KIDICAP_mini'
        def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
        def remoteProjectLocation = 'innowake-test-projects/transformation-test-projects/kidicap/kidicap-mini.git'
        def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
        def antFlags = ''
        def antFile_nat2Java
        def antFile_scriptSigning
        def buildProperties
        def workDir
        def logDir
        
        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "javaVersion=${javaVersion} testProjectBranch=${testProjectBranch}"
        
        try {  
            stage('init') {
                deleteDir()
                docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                	workDir = pwd()
    				gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                	gitUtils.getSingleFile('infrastructure/licenses', 'gip-customer.lic', mxVersion, '.')
                	gitUtils.getSingleFile('infrastructure/licenses', 'script-signing.lic', mxVersion, '.')
                	gitUtils.getSingleFile('infrastructure/licenses', 'script-signing.key', mxVersion, '.')
                	spUtils.downloadIwEclipseLinux(mxBuildVersion, "${workDir}/eclipse")
                	antFile_nat2Java = "${workDir}/build-nat2java.xml"
                	antFile_scriptSigning = "${workDir}/build-script-signing.xml"
                	buildProperties = miscUtils.readPropertyFile('build.properties', ['workDir': workDir, 'mxBuildVersion': mxBuildVersion])
                	logDir = buildProperties['logDir']
                	/* a=rwx avoids issues with write access to the logDir inside a docker container. */
                	sh "mkdir -p -m a=rwx ${logDir}"
                    /* WQATF-127 - Add version number to mee-source-migration-deps-general.jar and mee-source-migration-dist.jar and update .classpaths */
                    def fileNames = ['mee-source-migration-deps-general', 'mee-source-migration-dist']
                    fileNames.each { oldName ->
                        def newName = "${oldName}-${mxBuildVersion}"
                        if (oldName == 'mee-source-migration-dist') {
                            sh "sed -i 's/${oldName}.jar/${newName}.jar/' ${antFile_nat2Java}"
                        }
                        dir('eclipseWorkspace/mee-source-migration-natural-dist') {
                            def classpaths = ['.classpath', '.classpath-nat2java', '.classpath-script-signing']
                            classpaths.each { cp ->
                                sh "sed -i 's/${oldName}.jar/${newName}.jar/' ${cp}"
                            }
                        }
                    }
                	/* WQATF-435 - fetch artifacts from Nexus using maven. */
                	withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                		sh "$MVN_CMD -f ${workDir}/pom-gip-dependencies.xml dependency:copy-dependencies -DoutputDirectory=${buildProperties['gipJarDir']}"
                		sh "$MVN_CMD -f ${workDir}/pom-innowake-transformation-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']}"
                		sh "$MVN_CMD -f ${workDir}/pom-innowake-runtime-dependencies.xml dependency:copy-dependencies -Dinnowake.version=${mxBuildVersion} -DoutputDirectory=${buildProperties['iwJarDir']} -Dmdep.stripVersion=true"
                	}
                }
            }
            
            stage('signing') {
                def antProperties
                docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                    antProperties = "-DworkDir=${workDir} -DjvmDir=java -DjavaVersion=${miscUtils.getInstalledJavaVersion()} -DmxBuildVersion=${mxBuildVersion}"
                    withAnt(installation: 'Default') {
                        /* Do some initialization for nat2java, which is appropriate for the script signing too.
                         * Then compile the expert scripts to Java class files using a headless Eclipse. No compile errors may occur.
                         * Then pack the class files in a jar file.
                         */
                    	/* WQATF-435 - fake a ${workDir}/data/mxJars folder so that init-nat2java can be executed without failure. */
                    	def fakeDataMxJarsDir = "${workDir}/data/mxJars"
                    	sh "mkdir -p -m a=rwx ${fakeDataMxJarsDir}"
                        sh "ant ${antFlags} -buildfile ${antFile_nat2Java} ${antProperties} -DmxJarsDir=${fakeDataMxJarsDir} init-nat2java"
                        sh "ant ${antFlags} -buildfile ${antFile_nat2Java} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} prepare-eclipse-workspace"
                        sh "ant ${antFlags} -buildfile ${antFile_scriptSigning} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} compile-expert-scripts"
                        def rc = sh returnStatus: true, script: "grep '.*\\.java, project:.*, path:.*[Ee]rror.*' ${logDir}/java-errors-mee-src-mig-nat.log"
                        if (rc == 0) {
                            error 'Compilation error(s) expert scripts!'
                        }
                        sh "ant ${antFlags} -buildfile ${antFile_scriptSigning} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} pack"
                        /* Initialize the script signing. */
                        sh "ant ${antFlags} -buildfile ${antFile_scriptSigning} ${antProperties} -DmxJarsDir=${buildProperties['iwJarDir']} init-script-signing"
                    }
                }
                /*
                 * Script signing requires:
                 * - a license, which is protected by a key file
                 * - the key file, which is protected by a password
                 * - the password for the key file
                 * The signing procedure itself is restricted to a specific user, hostname and MAC address, which are specified in the signing license.
                 * They are read from the license file, which is syntactically Java properties file.
                 * The signing has to be executed in an environment with the specific user + hostname + MAC address. This is achieved by executing the signing in a Docker container.
                 * This procedure does not require a headless Eclipse.
                 */
                withCredentials([string(credentialsId: 'Script-Signing-Key-File-Password', variable: 'signingPw')]) {
                    def licenseProperties = readProperties file: buildProperties['scriptSigningLicenseFile']
                    def user = licenseProperties['system-user']
                    def hostname = licenseProperties['host-name']
                    def macAddress = licenseProperties['mac-addresses'].split(',')[0]
                    echo "user=${user} hostname=${hostname} macAddress=${macAddress}"
                    def containerParms = "--user jenkins:jenkins --hostname ${hostname} --mac-address ${macAddress} -w ${workDir} -v ${workDir}:${workDir} -v jenkins-m2-repo:/var/m2_repo:rw"
                    /* WQATF-435: fake data/mxJars folder by setting mxJarsDir property to IW_LIB */
                    def containerCmd = "sh -c \"ant ${antFlags} -buildfile ${antFile_scriptSigning} ${antProperties} -DscriptSigningKeyFilePassword=${signingPw} -DmxJarsDir=${buildProperties['iwJarDir']} exec-script-signing\""
                    docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).withRun(containerParms, containerCmd) {
                        cntr ->
                        sh "docker container wait ${cntr.id}"
                        /* docker.image().withRun() does not write the docker log to the job output, as docker.image().inside() does. So
                         * - this is done explicitely here, and additionally
                         * - the docker log is captured in a log file to be published as job artifact.
                         */
                        def logFile = "${logDir}/exec-script-signing.log"
                        sh "docker logs ${cntr.id} > ${logFile}"
                        sh "cat ${logFile}"
                        /* If a particular success message did not occur, the signing must have failed. */
                        def rc = sh returnStatus: true, script: "grep 'Number of signed jars' ${logFile}"
                        if (rc != 0) {
                            error 'Expert script signing failed!'
                        }
                    }
                }
            }
                
            stage('finalize') {
            	/* Prepare the mee-source-migration-natural-dist project so that it can be used as a deliverable without further modification. */
                def migrDir = buildProperties['meeSourceMigrationProject']
                dir("${migrDir}/bin") {
                    deleteDir()
                }
                sh "cp ${migrDir}/.classpath-nat2java ${migrDir}/.classpath"
                sh "rm ${migrDir}/.classpath-*"
                sh "cp ${buildProperties['iwJarDir']}/mee-source-migration-*.jar ${migrDir}/lib"
                sh "sed -i 's/kind=\"var\" path=\"IW_LIBS/kind=\"lib\" path=\"lib/' ${migrDir}/.classpath"                
                docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
                    def eclipseWsDir = buildProperties['eclipseWorkspaceDir']
                    zip dir: eclipseWsDir, glob: 'mee-source-migration-natural-dist/**/*', zipFile: "mee-source-migration-natural-dist-${mxBuildVersion}.zip", archive: false
                    spUtils.uploadJobArtifact(mxBuildVersion, "mee-source-migration-natural-dist-${mxBuildVersion}.zip")
				}
            }
            
        } catch (ex) {
			miscUtils.errorWithStackTrace(ex)
		} finally {
			archiveArtifacts allowEmptyArchive: true, artifacts: 'log/,*.log'
        }
    }
}
