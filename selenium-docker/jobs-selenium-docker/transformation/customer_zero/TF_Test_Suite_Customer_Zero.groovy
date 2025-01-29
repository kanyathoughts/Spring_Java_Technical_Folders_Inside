@Library('TestUtils') _

/**
 * Runs a complete test of the customer zero project by triggering the following jobs (in the given order):
 *  - TF_Cobol_Java_Migration_Customer_Zero.groovy
 *  - TF_Cobol_Java_Migration_CLI_Customer_Zero.groovy
 *  - TF_Cobol_Csharp_Migration_Customer_Zero.groovy
 *  - TF_Cobol_Java_Test_Customer_Zero.groovy
 * @param mxBuildVersion The maxenso build to use.
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * 		  type: Boolean
 * @param javaVersion The java version the test will run with
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranchCsharp The branch that will be used for C# if useDifferentTestProjectBranch is set.
 * @param differentTestProjectBranch The branch that will be used for Java test and migration if useDifferentTestProjectBranch is set.
 */

def miscUtils = new MiscUtils()
def mxVersionUtils = new MxVersionUtils()
def dockerUtils = new DockerUtils()

def unstableStages = [:]
def successfulStages = [:]

nodeTF('Docker-host') {
    catchError {
        timestamps() {           
	        def dotnetDependencyVersions = mxVersionUtils.getVersionListByArtifactUrlSuffix('releases-dotnet/InnoWake.NET.Dependencies-dist', 'releases-dotnet/innowake.runtime-dotnet-dist', 'releases-dotnet/innowake.runtime-dotnet-testframework-dist', 'releases-dotnet/innowake.runtime-ims-torpedo-dotnet-dist', 'releases-dotnet/innowake.runtime-torpedo-dotnet-dist')
            def javaMigrationBuildSelector
            def csharpMigrationBuildSelector
            def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
            def testProjectBranch = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranch, mxBuildVersion)
            def testProjectBranchCsharp = mxVersionUtils.getTestProjectBranch(useDifferentTestProjectBranch, differentTestProjectBranchCsharp, mxBuildVersion)
						
            withCodeCoverage = Boolean.parseBoolean(withCodeCoverage)
            
            def fullBuild
            docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
	            fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
            }
            buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
            buildDescription "withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} fullBuild=${fullBuild} testProjectBranchJava=${testProjectBranch} testProjectBranchCsharp=${testProjectBranchCsharp}"

            stage('TF_Cobol_Java_Migration_Customer_Zero') {                
                def buildResult = build job: 'TF_Cobol_Java_Migration_Customer_Zero', propagate: false, parameters: [
                        extendedChoice(name: 'mxBuildVersion', value: fullBuild),
                        booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
                        string(name: 'javaVersion', value: javaVersion),
					    booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					    listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)
                ]
                javaMigrationBuildSelector = mxVersionUtils.getBuildSelector(mxBuildVersion, fullBuild, buildResult.number)
                miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)                
            }

            stage('TF_Cobol_Java_Migration_CLI_Customer_Zero') {
                def migrationCliVersions = mxVersionUtils.getVersionListByArtifactUrlSuffix('releases/innowake/products/mee/source/migration/cobol/mee-source-migration-cobol-standalone-dist')
                when(migrationCliVersions.contains(mxBuildVersion), "Artifact is not available in version ${mxBuildVersion}") {
                    def buildResult = build job: 'TF_Cobol_Java_Migration_CLI_Customer_Zero', propagate: false, parameters: [
                            extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
                            string(name: 'javaVersion', value: javaVersion),
					        booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					        listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)
                    ]
                    miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
                }
            }

            stage('TF_Cobol_Java_Test_Customer_Zero') {
                def environmentLabel = withCodeCoverage ?  'trafo-environment' : 'perf-environment'
                def buildResult = build job: 'TF_Cobol_Java_Test_Customer_Zero', propagate: false, parameters: [
                        extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
                        string(name: 'environmentLabel', value: environmentLabel),
                        string(name: 'javaVersion', value: javaVersion),
                        booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
                        string(name: 'migrationBuild', value: javaMigrationBuildSelector),
					    booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					    listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)
                ]
                miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)
            }

            stage('TF_Cobol_Csharp_Migration_Customer_Zero') {                
                when(mxVersion >= '22.3', 'Correct job execution is available only for branches >= 22.3.') {
                    def buildResult = build job: 'TF_Cobol_Csharp_Migration_Customer_Zero', propagate: false, parameters: [
                            extendedChoice(name: 'mxBuildVersion', value: fullBuild),
                            booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
                            string(name: 'javaVersion', value: javaVersion),
					        booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					        listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranch)
                    ]
                    csharpMigrationBuildSelector = mxVersionUtils.getBuildSelector(mxBuildVersion, fullBuild, buildResult.number)
                    miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
                }
            }
            
            stage('TF_Cobol_Csharp_Test_Customer_Zero') {
                when(dotnetDependencyVersions.contains(mxBuildVersion), "Artifact is not available in version ${mxBuildVersion}") {
                    when(mxVersion >= '22.3', 'Correct job execution is available only for branches >= 22.3.') {
                        def buildResult = build job: 'TF_Cobol_Csharp_Test_Customer_Zero', propagate: false, parameters: [
                                extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
                                string(name: 'environmentLabel', value: 'perf-environment'),
                                string(name: 'javaVersion', value: javaVersion),
                                string(name: 'migrationBuild', value: csharpMigrationBuildSelector),
					            booleanParam(name: 'useDifferentTestProjectBranch', value: useDifferentTestProjectBranch),
					            listGitBranches(name: 'differentTestProjectBranch', value: differentTestProjectBranchCsharp)
                        ]
                        miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)
                    }
                }
            }

            echo 'Successful stages:'
            echo successfulStages.toString()
            echo 'Unstable stages:'
            echo unstableStages.toString()
        }
    }
    miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}\nFollowing stages are unstable:\n${unstableStages.toString()}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
}