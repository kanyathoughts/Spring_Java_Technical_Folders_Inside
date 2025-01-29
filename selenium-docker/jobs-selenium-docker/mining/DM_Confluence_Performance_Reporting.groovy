import groovy.json.JsonSlurper
import org.jenkinsci.plugins.workflow.support.steps.build.RunWrapper
import hudson.tasks.test.*
@Library('TestUtils') _

/**
 * Publish results from a specific run of DM_LeanFT_Mining_Performance_Testjob to Confluence.
 * 
 * @param build_number	        	the build number of DM_LeanFT_Mining_Performance_Testjob
 */

node('DM') {
	deleteDir()

	// get mxBuildVersion and test name from DM_LeanFT_Mining_Performance_Testjob
	def miscUtils = new MiscUtils()
	def workDir = pwd()
	def params
	def mxBuildVersion
	def testcase
	def duration

	buildName "Confluence Reporting"

	stage ('Copy Run Results') {
		params = Jenkins.getInstance().getItemByFullName('DM_LeanFT_Mining_Performance_Testjob')
		.getBuildByNumber(build_number.toInteger())
		.getAction(hudson.model.ParametersAction)
		mxBuildVersion = params.getParameter('mxBuildVersion').value
		testcase = params.getParameter('testcase').value
		testcase = testcase.replace('testcases.mining.performance.','')
		// reset params since ParametersAction is not serializable
		params = null

		if (testcase.equals('ModuleSearchTest')) {
			duration = getExecutionTime(Jenkins.getInstance().getItemByFullName('DM_LeanFT_Mining_Performance_Testjob')
			.getBuildByNumber(build_number.toInteger()))
		}

		buildDescription "mxBuildVersion=${mxBuildVersion} testcase=${testcase} build_number=${build_number}"
		copyArtifacts filter: 'RunResults/*/**', optional: true, projectName: 'DM_LeanFT_Mining_Performance_Testjob', selector: specific(build_number), target: "${workDir}/${testcase}"
	}

	stage ('Report to Confluence') {
		try {
			withCredentials([
				usernamePassword(credentialsId: 'USAppModQMUserSVC-at-deloitte-com-User-PW', passwordVariable: 'qmUserPw', usernameVariable: 'qmUserName')
			]) {
				// Pull the Confluence page content from
				// https://amiconfluence.deloitte.com/pages/viewpage.action?pageId=182125709
				def pageId = 179143274


				def responseJson = sh(script: "curl -u ${qmUserName}:\"${qmUserPw}\" -X GET \"${miscUtils.getBaseConfluenceURL()}/rest/api/content/${pageId}?expand=body.storage,version,space\" -k", returnStdout: true).trim()
				def pageInfo = readJSON text: responseJson
				def pageContent = pageInfo.body.storage.value

				//echo "PageInfo: ${pageInfo}"
				//echo "Content: ${pageContent}"

				// Parse the html retrieved above and extend it
				def versionMatch = getRegexMatch(mxBuildVersion, ~/(.*-20\d{2})(\d{2})(\d{2})(.*)/)
				def dateString = versionMatch.isEmpty() ? "n/a" : "${versionMatch[2]}/${versionMatch[3]}"
				// add rebuild number to date if needed
				if (versionMatch[4].contains('-')) {
					dateString += versionMatch[4].substring(versionMatch[4].lastIndexOf('-'))
				}

				switch(testcase) {
					case 'QATestprojectsDiscoveryTest':
					def countTotalFiles
					def countUndiscovered
					def countErrors
					def countCobol
					def countJava
					def countJcl
					def countNatural
					def countSql
					def countXml
					def timeDiscCode
					def timeDiscMetrics
					def fileSizeMetricsExcel
					def fileSizeCodeLog
					def fileSizeMetricsLog
					def match

					def perfResultsFilePath = "${workDir}/${testcase}/RunResults/performance-metrics-discovery.txt"
					if (fileExists(perfResultsFilePath)) {
						def perfData = readFile(file: perfResultsFilePath)

						echo "${perfData}"
						perfData.split('\n').each { line ->
							if (line.contains('Total files')) {
								match = getRegexMatch(line, ~/:\s(\d+[,?\d*]+)/)
								countTotalFiles = match.isEmpty() ? "n/a" : match.get(1).replaceAll(',','')
							} else if (line.contains('complete Discover Code')) {
								match = getRegexMatch(line, ~/(\d{2}:\d{2}:\d{2})/)
								timeDiscCode = match.isEmpty() ? "n/a" : timeToMinutes(match.get(1))
							} else if (line.contains('complete Discover Metrics - CSV') && !line.contains('From jobView')) {
								match = getRegexMatch(line, ~/(\d{2}:\d{2}:\d{2})/)
								timeDiscMetrics = match.isEmpty() ? "n/a" : timeToMinutes(match.get(1))
							} else if (line.contains('Metrics CSV file')) {
								match = getRegexMatch(line, ~/\s(\d+[,?\d]*\sKB)/)
								fileSizeMetricsExcel = match.isEmpty() ? "n/a" : match.get(1)
							} else if (line.contains('code log')) {
								match = getRegexMatch(line, ~/\s(\d+[,?\d]*\sKB)/)
								fileSizeCodeLog = match.isEmpty() ? "n/a" : match.get(1)
							} else if (line.contains('metrics log')) {
								match = getRegexMatch(line, ~/\s(\d+[,?\d]*\sKB)/)
								fileSizeMetricsLog = match.isEmpty() ? "n/a" : match.get(1)
							} else if (line.contains('Undiscovered entries')) {
								match = getRegexMatch(line, ~/:\s(\d{1,3}(,\d{3})*(\.\d+)?)/)
								countUndiscovered = match.isEmpty() ? "n/a" : match.get(1)
							} else if (line.contains('Error entries')) {
								match = getRegexMatch(line, ~/:\s(\d{1,3}(,\d{3})*(\.\d+)?)/)
								countErrors = match.isEmpty() ? "n/a" : match.get(1)
							} else if (line.contains('Total') && line.contains('COBOL')) {
								match = getRegexMatch(line, ~/\s(\d{1,3}(,\d{3})*(\.\d+)?)/)
								countCobol = match.isEmpty() ? "n/a" : match.get(1)
							} else if (line.contains('Total') && line.contains('JAVA')) {
								match = getRegexMatch(line, ~/\s(\d{1,3}(,\d{3})*(\.\d+)?)/)
								countJava = match.isEmpty() ? "n/a" : match.get(1)
							} else if (line.contains('Total') && line.contains('NATURAL')) {
								match = getRegexMatch(line, ~/\s(\d{1,3}(,\d{3})*(\.\d+)?)/)
								countNatural = match.isEmpty() ? "n/a" : match.get(1)
							} else if (line.contains('Total') && line.contains('JCL')) {
								match = getRegexMatch(line, ~/\s(\d{1,3}(,\d{3})*(\.\d+)?)/)
								countJcl = match.isEmpty() ? "n/a" : match.get(1)
							} else if (line.contains('Total') && line.contains('SQL')) {
								match = getRegexMatch(line, ~/\s(\d{1,3}(,\d{3})*(\.\d+)?)/)
								countSql = match.isEmpty() ? "n/a" : match.get(1)
							} else if (line.contains('Total') && line.contains('XML')) {
								match = getRegexMatch(line, ~/\s(\d{1,3}(,\d{3})*(\.\d+)?)/)
								countXml = match.isEmpty() ? "n/a" : match.get(1)
							}
						}

						// set version number in pie chart
						match = getRegexMatch(pageContent, ~/(.*subTitle.+?>).+?(<.*)/)
						if (!match.isEmpty()) {
							pageContent = 	match[1] +
							"${mxBuildVersion}" +
							match[2]
						} else {
							echo "Version number could not be set in pie chart."
						}

						// set compilation counts in pie chart
						match = getRegexMatch(pageContent, ~/(.*Modules<\/td>).+?(<\/tr>.*)/)
						if (!match.isEmpty()) {
							pageContent = 	match[1] +
							"<td colspan=\"1\">${countCobol}</td>" +
							"<td colspan=\"1\">${countJava}</td>" +
							"<td colspan=\"1\">${countJcl}</td>" +
							"<td colspan=\"1\">${countNatural}</td>" +
							"<td colspan=\"1\">${countSql}</td>" +
							"<td colspan=\"1\">${countXml}</td>" +
							"<td colspan=\"1\">${countUndiscovered}</td>" +
							match[2]
						} else {
							echo "Compilation counts could not be set in pie chart."
						}

						// Split page at end of each relevant table
						def splitPage = pageContent.split('</tbody>',4)
						if (splitPage.size() == 4) {
							// add rows to Discovery Performance and File/Error Count tables
							pageContent = 	splitPage[0] +
							'</tbody>' +
							splitPage[1] +
							'<tr>' +
							"<td colspan=\"1\">${dateString}</td>" +
							"<td colspan=\"1\">${mxBuildVersion}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${timeDiscCode+timeDiscMetrics}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${timeDiscCode}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${timeDiscMetrics}</td>" +
							'<td style=\"text-align: right;\" colspan=\"1\"><br /></td>' +
							'<td colspan=\"1\"><br /></td>' +
							'<td colspan=\"1\"><br /></td>' +
							'</tr>' +
							'</tbody>' +
							splitPage[2] +
							'<tr>' +
							"<td colspan=\"1\">${dateString}</td>" +
							"<td colspan=\"1\">${mxBuildVersion}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${countTotalFiles}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${countErrors}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${countUndiscovered}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${fileSizeMetricsExcel}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${fileSizeCodeLog}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${fileSizeMetricsLog}</td>" +
							'</tr>' +
							'</tbody>' +
							splitPage[3]
						} else {
							echo "Number of tables on page unexpected: ${splitPage.size()}"
						}
					} else {
						unstable "File ${perfResultsFilePath} not found"
					}
					break
					case 'ModuleSearchTest':
					// Split page at end of each relevant table
					def splitPage = pageContent.split('</tbody>',5)
					if (splitPage.size() == 5) {
						// add rows to Discovery Performance and File/Error Count tables
						pageContent = 	splitPage[0] +
						'</tbody>' +
						splitPage[1] +
						'</tbody>' +
						splitPage[2] +
						'</tbody>' +
						splitPage[3] +
						'<tr>' +
						"<td colspan=\"1\">${dateString}</td>" +
						"<td colspan=\"1\">${mxBuildVersion}</td>" +
						"<td style=\"text-align: right;\" colspan=\"1\">${duration}</td>" +
						'</tr>' +
						'</tbody>' +
						splitPage[4]
					} else {
						echo "Number of tables on page unexpected: ${splitPage.size()}"
					}
					break
					case 'DiscoverCodePerformanceTest':
					def countTotalFiles
					def timeDiscTotalFiles
					def timeDiscIncScanT2
					def timeDiscIncScanT3
					def timeDiscIncScan
					def performancePercentage
					def match

					def perfResultsFilePath = "${workDir}/${testcase}/RunResults/performance-metrics-discovery-code-incremental-scan.txt"
					if (fileExists(perfResultsFilePath)) {
						def perfData = readFile(file: perfResultsFilePath)

						echo "${perfData}"
						perfData.split('\n').each { line ->
							if (line.contains('Total files')) {
								match = getRegexMatch(line, ~/:\s(\d+[,?\d*]+)/)
								countTotalFiles = match.isEmpty() ? "n/a" : match.get(1).replaceAll(',','')
							} else if (line.contains('Discover Code(t1)')) {
								match = getRegexMatch(line, ~/(\d{2}:\d{2}:\d{2})/)
								timeDiscTotalFiles = match.isEmpty() ? "n/a" : timeToMinutes(match.get(1))
							} else if (line.contains('Discover Code(t2)')) {
								match = getRegexMatch(line, ~/(\d{2}:\d{2}:\d{2})/)
								timeDiscIncScanT2 = match.isEmpty() ? "n/a" : timeToMinutes(match.get(1))
							} 
						}
						//timeDiscIncScan = timeDiscIncScanT2 + timeDiscIncScanT3
						// Split page at end of each relevant table
						def splitPage = pageContent.split('</tbody>',6)
						if (splitPage.size() == 6) {
							// add rows to Discovery Performance and File/Error Count tables
							pageContent = 	splitPage[0] +
							'</tbody>' +
							splitPage[1] +
							'</tbody>' +
							splitPage[2] +
							'</tbody>' +
							splitPage[3] +
							'</tbody>' +
							splitPage[4] +
							'<tr>' +
							"<td colspan=\"1\">${dateString}</td>" +
							"<td colspan=\"1\">${mxBuildVersion}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${countTotalFiles}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${timeDiscTotalFiles}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${timeDiscIncScanT2}</td>" +
							'<td style=\"text-align: right;\" colspan=\"1\">null</td>' +
							'<td style=\"text-align: right;\" colspan=\"1\">null</td>' +
							'<td style=\"text-align: right;\" colspan=\"1\">null</td>' +
							'<td style=\"text-align: right;\" colspan=\"1\">null</td>' +
							'</tr>' +
							'</tbody>' +
							splitPage[5]
						} else {
							echo "Number of tables on page unexpected: ${splitPage.size()}"
						}
					} else {
						unstable "File ${perfResultsFilePath} not found"
					}
					break
					case 'DiscoverMetricsIncrementalScanTest':
					def countTotalFiles
					def countTotalFiles2
					def timeMetrics1
					def timeMetrics2
					def performancePercentage

					def perfResultsFilePath = "${workDir}/${testcase}/RunResults/performance-metrics-discover-metrics-incremental-scan.txt"
					if (fileExists(perfResultsFilePath)) {
						def perfData = readFile(file: perfResultsFilePath)

						echo "${perfData}"
						perfData.split('\n').each { line ->
							if (line.contains('Run 1')) {
								match = getRegexMatch(line, ~/:\s(\d+[,?\d*]+)/)
								countTotalFiles = match.isEmpty() ? "n/a" : match.get(1).replaceAll(',','')
							}else if (line.contains('Run 2')) {
								match = getRegexMatch(line, ~/:\s(\d+[,?\d*]+)/)
								countTotalFiles2 = match.isEmpty() ? "n/a" : match.get(1).replaceAll(',','')
							}else if (line.contains('1st run')) {
								match = getRegexMatch(line, ~/(\d{2}:\d{2}:\d{2})/)
								timeMetrics1 = match.isEmpty() ? "n/a" : timeToMinutes(match.get(1))
							}else if (line.contains('2nd run')) {
								match = getRegexMatch(line, ~/(\d{2}:\d{2}:\d{2})/)
								timeMetrics2 = match.isEmpty() ? "n/a" : timeToMinutes(match.get(1))
							}
						}
						// Split page at end of each relevant table
						def splitPage = pageContent.split('</tbody>',6)
						// Split 'splitPage[4]' at 'null' to replace the content null created in confluence report 'DiscoverCodePerformanceTest'
						def splitNull = splitPage[4].split('null', 5)
						def metricsPart = splitNull[0] + countTotalFiles + splitNull[1] + timeMetrics1 + splitNull[2] + countTotalFiles2 + splitNull[3] + timeMetrics2 + splitNull[4] 
						if (splitPage.size() == 6) {
							// add rows to Discovery Performance and File/Error Count tables
							pageContent = 	splitPage[0] +
							'</tbody>' +
							splitPage[1] +
							'</tbody>' +
							splitPage[2] +
							'</tbody>' +
							splitPage[3] +
							'</tbody>' +
							metricsPart +
							'</tbody>' +
							splitPage[5]
						} else {
							echo "Number of tables on page unexpected: ${splitPage.size()}"
						}
					} else {
						unstable "File ${perfResultsFilePath} not found"
					}
					break
					case 'LargeCallChainExportTest':
					def timecsvdownload
					def timezipdownload
					def timegraphmldownload
					def csvFileSize
					def csvFileSizeInZip
					def graphmlFileSize
                    def match

					def perfResultsFilePath = "${workDir}/${testcase}/RunResults/Large-Call-Chain-ExPortTest.txt"
					if (fileExists(perfResultsFilePath)) {
						def perfData = readFile(file: perfResultsFilePath)
						echo "${perfData}"
						perfData.split('\n').each { line ->
							if (line.contains('download') && line.contains('csv')) {
								match = getRegexMatch(line, ~/(\d{2}:\d{2}:\d{2})/)
								timecsvdownload = match.isEmpty() ? "n/a" : timeToSeconds(match.get(1))
							} else if (line.contains('download') && line.contains('zip')) {
								match = getRegexMatch(line, ~/(\d{2}:\d{2}:\d{2})/)
								timezipdownload = match.isEmpty() ? "n/a" : timeToSeconds(match.get(1))
							} else if (line.contains('download') && line.contains('GraphML')) {
								match = getRegexMatch(line, ~/(\d{2}:\d{2}:\d{2})/)
								timegraphmldownload = match.isEmpty() ? "n/a" : timeToSeconds(match.get(1))
							} else if (line.contains('csv file size') && ! line.contains('unzipped')) {
								match = getRegexMatch(line, ~/\s(\d+[,?\d]*\sKB)/)
								csvFileSize = match.isEmpty() ? "n/a" : match.get(1)
							} else if (line.contains('csv file size') && line.contains('unzipped')) {
								match = getRegexMatch(line, ~/\s(\d+[,?\d]*\sKB)/)
								csvFileSizeInZip = match.isEmpty() ? "n/a" : match.get(1)
							} else if (line.contains('graphml file size')) {
								match = getRegexMatch(line, ~/\s(\d+[,?\d]*\sKB)/)
								graphmlFileSize = match.isEmpty() ? "n/a" : match.get(1)
							}
						}
						// Split page at end of each relevant table
						def splitPage = pageContent.split('</tbody>',7)
						if (splitPage.size() == 7) {
							pageContent = 	splitPage[0] +
							'</tbody>' +
							splitPage[1] +
							'</tbody>' +
							splitPage[2] +
							'</tbody>' +
							splitPage[3] +
							'</tbody>' +
							splitPage[4] +
							'</tbody>' +
							splitPage[5] +
							'<tr>' +
							"<td colspan=\"1\">${dateString}</td>" +
							"<td colspan=\"1\">${mxBuildVersion}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${timecsvdownload}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${csvFileSize}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${timezipdownload}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${csvFileSizeInZip}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${timegraphmldownload}</td>" +
							"<td style=\"text-align: right;\" colspan=\"1\">${graphmlFileSize}</td>" +
							'</tr>' +
							'</tbody>' + 
							splitPage[6]
							} else {
							echo "Number of tables on page unexpected: ${splitPage.size()}"
							}
						} else {
							unstable "File ${perfResultsFilePath} not found"
							}
					break
					default: unstable "Reporting not supported for testcase ${testcase}."
				}

				// Push the new HTML back to the page
							def pageInfoJson = [
								"id":"${pageId}",
								"type": "page",
								"title":"${pageInfo.title}",
								"space":[
									"key":"${pageInfo.space.key}"
								],
								"body":[
									"storage":[
										"value":"${pageContent}",
										"representation":"storage"
									]
								],
								"version":[
									"number":pageInfo.version.number+1
								]
							]

							def parsedJson = groovy.json.JsonOutput.toJson(pageInfoJson)
							def exitCode = sh(script: "curl -u ${qmUserName}:\"${qmUserPw}\" -X PUT -H 'Content-Type: application/json' -d'${parsedJson}' ${miscUtils.getBaseConfluenceURL()}/rest/api/content/${pageId} -k", returnStatus: true)
							if (exitCode != 0) {
								unstable "Error while updating Confluence. Exit code: ${exitCode}"
							}
						}
					} catch (e) {
						unstable "Encountered error during report to confluence: ${e}"
					}
				}
			}

			// Returns a list of regex matches, or an empty list if no matches are found
			def List getRegexMatch(String line, java.util.regex.Pattern pattern) {
				return (line =~ pattern) ? (line =~ pattern)[0] : []
			}

			// Parses format hh:mm:sec to minutes
			def int timeToMinutes(String time) {
				def timeUnits = time.split(':')
				def hours = timeUnits[0] as int
				def minutes = timeUnits[1] as int
				def seconds = timeUnits[2] as int
				minutes += (seconds < 30) ? 0 : 1

				return hours*60+minutes
			}

			//parses format hh:mm:sec to seconds
			def int timeToSeconds(String time) {
				def timeUnits = time.split(':')
				def hours = timeUnits[0] as int
				def minutes = timeUnits[1] as int
				def seconds = timeUnits[2] as int
				seconds += (hours * 3600) + (minutes * 60)

				return seconds
			}

			/**
			 * Returns the duration of step05 of ModuleSearchTest. 
			 */
			def getExecutionTime(buildInformation) {
				def stepData = ''
				try {
					def buildResult = new RunWrapper(buildInformation, false)
					def testResultAction =  buildResult.rawBuild.getAction(AbstractTestResultAction.class)
					if (testResultAction != null) {
						echo "Tests: ${testResultAction.getFailCount()} failures of ${testResultAction.getTotalCount()}."
						def passedTestResults = testResultAction.getPassedTests()
						if (passedTestResults != null) {
							passedTestResults.each { passedTest ->
								if (passedTest.getTitle().contains('step06')) {
									echo "Module Search : ${passedTest.getDuration()}s"
									stepData += "${passedTest.getDuration()}"
								}
							}
						}
					}
					if(! stepData?.trim()){
						stepData = 'in progress'
					}
				} catch (e) {
					unstable "Test result eval failed: ${e}"
				}
				return stepData
			}
