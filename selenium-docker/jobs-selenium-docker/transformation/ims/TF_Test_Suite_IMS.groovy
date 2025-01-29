@Library('TestUtils') _

/**
 * Runs a complete IMS test:
 * - TF_IMS_Java_Migration (migration of Cobol and Ims files to java)
 * - TF_IMS_TM_Java_Test
 * - TF_IMS_DB_Java_Test_Performance
 * Runs the java based test once with java8 and once with java11.
 * - TF_IMS_Csharp_Migration (migration of Cobol and Ims files to csharp)
 * - TF_IMS_TM_Csharp_Test
 * - TF_IMS_DB_Csharp_Test_Performance
 *
 * @param mxBuildVersion  The maxenso build to use.
 * @param withCodeCoverage Whether the code coverage will be measured or not.
 * @param javaVersion The java version the test will run with.
 */

def miscUtils = new MiscUtils()
def mxVersionUtils = new MxVersionUtils()
def dockerUtils = new DockerUtils()

timestamps() {
	def unstableStages = [:]
	def successfulStages = [:]
	def listDbCsharp = mxVersionUtils.getVersionListByArtifactUrlSuffix('releases-dotnet/InnoWake.NET.Dependencies-dist', 'releases-dotnet/innowake.runtime-dotnet-dist', 'releases-dotnet/innowake.runtime-dotnet-testframework-dist', 'releases-dotnet/innowake.runtime-torpedo-dotnet-dist')
	def listTmCsharp = mxVersionUtils.getVersionListByArtifactUrlSuffix('releases-dotnet/InnoWake.NET.Dependencies-dist', 'releases-dotnet/innowake.runtime-dotnet-dist', 'releases-dotnet/innowake.runtime-dotnet-testframework-dist', 'releases-dotnet/innowake.runtime-ims-torpedo-dotnet-dist', 'releases-dotnet/innowake.runtime-torpedo-dotnet-dist')
	
	nodeTF('Docker-host') {
		catchError {
			echo "mxBuildVersion = ${mxBuildVersion}"
			buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
			
			def fullBuild
			docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
				fullBuild = mxVersionUtils.getFullBuild(mxBuildVersion)
			}
			buildDescription "withCodeCoverage=${withCodeCoverage} javaVersion=${javaVersion} fullBuild=${fullBuild}"
			withCodeCoverage = Boolean.parseBoolean(withCodeCoverage)
			
			stage('TF_IMS_Java_Migration') {
				def buildResult = build job: 'TF_IMS_Java_Migration', quietPeriod: 0, propagate: false,
					parameters: [
						extendedChoice(name: 'mxBuildVersion', value: fullBuild),
						booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
						string(name: 'javaVersion', value: javaVersion)
					]
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
			}
	
			stage('TF_IMS_TM_Java_Test') {
				def buildResult = build job: 'TF_IMS_TM_Java_Test', quietPeriod: 0, propagate: false,
					parameters: [
						extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
						booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
						string(name: 'javaVersion', value: javaVersion)
					]
				miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages, "with javaVersion=${javaVersion}")
			}
			
			stage('TF_IMS_DB_Java_Test') {
				def buildResult = build job: 'TF_IMS_DB_Java_Test', quietPeriod: 0, propagate: false,
					parameters: [
						extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
						booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
						string(name: 'javaVersion', value: javaVersion)
					]
				miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages, "with javaVersion=${javaVersion}")
			}
			
			stage('TF_IMS_DB_Java_Test_Performance') {
				def environmentLabel = withCodeCoverage ?  'trafo-environment' : 'perf-environment'
				def buildResult = build job: 'TF_IMS_DB_Java_Test_Performance', quietPeriod: 0, propagate: false,
					parameters: [
						extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
						booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
						string(name: 'javaVersion', value: javaVersion),
						extendedChoice(name: 'environmentLabel', value: environmentLabel)
					]
				miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages, "with javaVersion=${javaVersion}")
			}
			
			stage('TF_IMS_Csharp_Migration') {
				def buildResult = build job: 'TF_IMS_Csharp_Migration', quietPeriod: 0, propagate: false,
					parameters: [
						extendedChoice(name: 'mxBuildVersion', value: fullBuild),
						booleanParam(name: 'withCodeCoverage', value: withCodeCoverage),
						string(name: 'javaVersion', value: javaVersion)
					]
				echo "Build result IMS_Migration_Csharp: ${buildResult.result}"
				miscUtils.evaluateBuildResultWithExit(buildResult, unstableStages, successfulStages)
			}
	
			stage('TF_IMS_TM_Csharp_Test') {
				when(listTmCsharp.contains(mxBuildVersion), "Artifact is not available in version ${mxBuildVersion}") {
					['v4.7.2', 'v4.8'].each {
						dotnetFrameworkVersion ->
						def buildResult = build job: 'TF_IMS_TM_Csharp_Test', quietPeriod: 0, propagate: false,
	    		    		parameters: [
	    		    			extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
	    						string(name: 'dotnetFrameworkVersion', value: dotnetFrameworkVersion)
	    		     		]
			    		miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages, "with dotnetFrameworkVersion=${dotnetFrameworkVersion}")
					}
				}
			}
			
			stage('TF_IMS_DB_Csharp_Test') {
				when(listDbCsharp.contains(mxBuildVersion), "Artifact is not available in version ${mxBuildVersion}") {
					def buildResult = build job: 'TF_IMS_DB_Csharp_Test', quietPeriod: 0, propagate: false, 
						parameters: [
							extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion)
						]
					miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)
					if (buildResult.result != 'FAILURE') {
						build job: 'LD_LeanFT_IMS_WB_CompleteTest', propagate: false, wait: false, 
						parameters: [
							extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
							[$class: 'NodeParameterValue', name: 'executeOn', labels: ['USLeanft1'], nodeEligibility: [$class: 'AllNodeEligibility']],
							string(name: 'javaVersion', value: 'java11')
							]
					}
				}
			}

			stage('TF_IMS_WB_Csharp_Test') {
				when(listDbCsharp.contains(mxBuildVersion), "Artifact is not available in version ${mxBuildVersion}") {
					def buildResult = build job: 'TF_IMS_WB_Csharp_Test', quietPeriod: 0, propagate: false, 
						parameters: [
							extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion)
						]
					miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)
				}
			}
			
			stage('TF_IMS_DB_Csharp_Test_Performance') {
				when(listDbCsharp.contains(mxBuildVersion), "Artifact is not available in version ${mxBuildVersion}") {
					def buildResult = build job: 'TF_IMS_DB_Csharp_Test_Performance', quietPeriod: 0, propagate: false, 
						parameters: [
							extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
							extendedChoice(name: 'environmentLabel', value: 'perf-environment')
						]
					miscUtils.evaluateBuildResult(buildResult, unstableStages, successfulStages)
				}
			}
			
			echo 'Successful stages:'
			echo successfulStages.toString()
			echo 'Unstable stages:'
			echo unstableStages.toString()
		}
		miscUtils.notifyFailure([subj: "build ${currentBuild.number} of ${currentBuild.projectName} is unstable", body: "${currentBuild.absoluteUrl} mxBuildVersion = ${mxBuildVersion}\nFollowing stages are unstable:\n${unstableStages.toString()}"], miscUtils.getMailReceivers(currentBuild.projectName), currentBuild.currentResult)
	}
}