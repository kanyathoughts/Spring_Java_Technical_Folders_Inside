@Library('TestUtils') _

/**
 * Extracts key information from a performance logger file, creates a set of
 * csv files that contain the key information, and then archives the resulting
 * set of csv files.
 * Updates the historical table and line charts in a Confluence page using performance data from a CSV file.
 *
 * @param pathToLogFile Path to the performance logger file
 * @param pageID ID of the Confluence page
 * @param mxBuildVersion Maxenso build under test
 * @param buildURL URL of the Jenkins build that ran the performance tests
 * @param numDaysDataToKeep Number of days worth of performance data to report to the Confluence page
 * @param reportToConfluence Only if set to true the data is published on confluence
 * @param useDifferentTestProjectBranch By default the project is checked out with branch master.
 * 		If useDifferentTestProjectBranch is set to true the branch is overridden by the parameter differentTestProjectBranch.
 * @param differentTestProjectBranch The branch that will be used if useDifferentTestProjectBranch is set.
 */
timestamps {
    nodeTF('Docker-host') {

        def dockerUtils = new DockerUtils()
        def perfUtils = new PerformanceUtils()
        def mxVersionUtils = new MxVersionUtils()
        def gitUtils = new GitUtils()
        def miscUtils = new MiscUtils()
        def workDir = pwd()
        def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion(mxBuildVersion)
        def remoteProjectLocation = 'infrastructure/transformation/performance-comparison.git'
        def jobName = buildURL.split('/')[buildURL.split('/').length-2]
        def pathToCSVFile = jobName + '_logs_summarized_profiling.csv'
        def testProjectBranch = useDifferentTestProjectBranch ? differentTestProjectBranch : master

        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "pathToLogFile=${pathToLogFile} pageID=${pageID} jobName=${jobName} numDaysDataToKeep=${numDaysDataToKeep} reportToConfluence=${reportToConfluence} testProjectBranch=${testProjectBranch}"

        deleteDir()
        docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside {
            stage('initialisation') {
                copyArtifacts projectName: jobName,
                        filter: pathToLogFile,
                        selector: specific(buildURL.tokenize('/')[-1]),
                        parameters: "mxBuildVersion=${mxBuildVersion}"
                perfUtils.createAndArchiveCSVs(pathToLogFile, jobName)
            }

            stage('performance-comparison') {
                when (mxVersion == '99.9', 'We will skip the performance-comparison stage, because we only run the stage against a' +
                        ' mxBuildVersion that corresponds with the master branch of the product.') {
                    gitUtils.checkoutGitProject(workDir, "${gitUtils.getGitUrlQef()}/${remoteProjectLocation}", testProjectBranch)
                    def pathToExpectedCSVFile = "${workDir}/${jobName}/expectedPerfLog.csv"
                    def pathToAllowedDeviationsCSVFile = "${workDir}/${jobName}/allowedDeviations.csv"
                    catchError(buildResult: 'UNSTABLE', stageResult: 'UNSTABLE') {
                        def errorMessage = perfUtils.comparePerformanceLogs(pathToCSVFile, pathToExpectedCSVFile,
                                pathToAllowedDeviationsCSVFile, ['Invocations', 'Own Avg/µs'])
                        if (!errorMessage.isEmpty()) {
                            unstable 'There were significant deviations between the actual and expected performance data' +
                                    " for ${jobName}:\n\n${errorMessage}If you would like to see or update the expected" +
                                    " performance data, please visit ${gitUtils.getGitUrlQef()}/${remoteProjectLocation}."
                        } else {
                            echo 'The were no significant deviations between the actual and expected performance data' +
                                    " for ${jobName}\n"
                        }
                    }
                }
            }

            stage('reporting') {
            	when (mxVersion == '99.9', 'We will skip the reporting stage, because we only run the stage against a' +
            			' mxBuildVersion that corresponds with the master branch of the product.'){
					if (Boolean.parseBoolean(reportToConfluence)) {
						perfUtils.reportToConfluence(pageID, pathToCSVFile, mxBuildVersion, buildURL, numDaysDataToKeep.toInteger(), true)
				    } else {
					    echo 'Confluence reporting skipped'
				    }
				}
					
            }
        }
    }
}