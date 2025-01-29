@Library('TestUtils') _

/**
 * Calculates the expected performance values based on the average of multiple runs, creates a 
 *      expectedPerfLog.csv file that contains the key information with averaged data
 *      README.txt file that containts URLs to each run of TF_Performance_Reporting job that was used
 * and then archives these files
 * @param jobName Job for which is to be calculated average of performance report data
 * @param buildNumbers TF_Performance_Reporting jobs build numbers to calculate the average. Delimiter is comma. E.g. 22,23,24
 */

nodeTF('Docker-host') {
    timestamps {            
                
        def dockerUtils = new DockerUtils()
        def miscUtils = new MiscUtils()
        def perfUtils = new PerformanceUtils()
        
        def buildNumbers = buildNumbers.split(',').toList()
        def buildUrl  = ''       
        def header = ['Id', 'Invocations', 'Own/µs', 'Own Min/µs', 'Own Avg/µs', 'Own Max/µs']
        def profilingLogRecords = []

        buildName "#${env.BUILD_ID} - ${jobName}"
        buildDescription "buildNumbers=${buildNumbers}"
        def csvFile = "${jobName}_logs_summarized_profiling.csv"

        deleteDir()
        docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside {
            stage('setup') {
                buildNumbers.each { buildNumber ->
                    copyArtifacts([
    					projectName: 'TF_Performance_Reporting',
    					filter: csvFile,
    					selector: specific(buildNumber),
    					target: '.',
    					flatten: true
    				])
    				sh "mv ${jobName}_logs_summarized_profiling.csv ${buildNumber}_${jobName}_logs_summarized_profiling.csv"
    				buildUrl += "${JENKINS_URL}job/TF_Performance_Reporting/${buildNumber}/" + "\n"
                }
            }
        
            stage('read logs'){                
                buildNumbers.each { buildNumber ->
                   def logFile = "${buildNumber}_${jobName}_logs_summarized_profiling.csv"                     
                   profilingLogRecords.add(perfUtils.get2DList(logFile))
                }
            }
            
            stage('calculate') {
                def average = perfUtils.calculateAverage(profilingLogRecords)
                writeCSV file: 'expectedPerfLog.csv', records: [header] + average
            }
                        
            stage('readme'){
                def readmeContent = "The ${jobName}_logs_summarized_profiling.csv file from Performance_Reporting for the following builds was obtained by running the ${env.JOB_NAME}:\n${buildUrl}Job run: ${env.BUILD_URL}"
    			writeFile file: 'README.txt', text: readmeContent
            }
            
            stage('archive'){
                archiveArtifacts 'expectedPerfLog.csv, README.txt'
            }
        }
    }
}
