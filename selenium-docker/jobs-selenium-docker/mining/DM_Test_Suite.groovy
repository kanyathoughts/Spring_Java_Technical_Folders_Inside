import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
@Library('TestUtils') _

/**
 * Compile and run all available test cases for mining
 * 
 * @param mxBuildVersion	 the trunk build version to use.
 * @param deploy 			 whether a deploy to Mining Staging is desired or not
 * @param rebuild_number	 rebuild suffix for artifact version
 */

node('DM') {
     
    buildName "#${env.BUILD_ID} - ${rebuild_number ? mxBuildVersion+'-'+rebuild_number : mxBuildVersion}"
    buildDescription "mxBuildVersion=${mxBuildVersion} deploy=${deploy}"

	def miscUtils = new MiscUtils()
    def workDir = pwd()
	def totalTestCount = 0
	def unstableStagesCount = 0
	def failedStagesCount = 0
    def totalTestCountMap = [:]
	def unstableStagesMap = [:]
	def failedPipelines = ''
	def abortedPipelines = ''
	
	// parse build descriptions, unstableStages are in format stageName:buildNumber
	// example "... totalTestCount=35 unstableStages=test.iam.test:75,test.plugin.test:76 ..."
    def parseBuildResults = { stageName, buildResult ->
    
    	def buildDesc = buildResult.getDescription()
    	if (buildResult.result == 'FAILURE') {
			failedPipelines = failedPipelines ?  failedPipelines+=", ${stageName}" : "${stageName}"
		} else if (buildResult.result == 'ABORTED') {
			abortedPipelines = abortedPipelines ?  abortedPipelines+=", ${stageName}" : "${stageName}"
			// exit method, because aborted pipelines do not contain stage info
			return
		}
    	
    	if (buildDesc) {
			if (buildDesc.contains('totalTestCount')) {
			    def totalTestCountForBuild = (buildDesc =~ /totalTestCount=(\d+)/)[0][1] as Integer
			    
			    if (totalTestCountForBuild) {
					totalTestCount += totalTestCountForBuild
					totalTestCountMap.put(stageName, totalTestCountForBuild)
			    }
			} else {
				echo 'totalTestCount label could not be found in build description'
			}
			
			if (buildDesc.contains('unstableStages')) {
			    def unstableStagesForBuild = (buildDesc =~ /unstableStages=([A-Za-z0-9:,\.]+)/)[0][1]
			    
			    if (unstableStagesForBuild) {
			        def unstableStages = unstableStagesForBuild.split(',')
					unstableStagesCount += unstableStages.size()
					unstableStagesMap.put(stageName + ' - unstable', unstableStages)
			    }
			} else {
				echo 'unstableStages label could not be found in build description'
			}
			
			if (buildDesc.contains('failedStages')) {
			    def failedStagesForBuild = (buildDesc =~ /failedStages=([A-Za-z0-9:,\.]+)/)[0][1]
			    
			    if (failedStagesForBuild) {
			        def failedStages = failedStagesForBuild.split(',')
					failedStagesCount += failedStages.size()
					unstableStagesMap.put(stageName + ' - failed', failedStages)
			    }
			} else {
				echo 'failedStages label could not be found in build description'
			}
	   }
	}
	
	stage ('check if already running') {
		if(currentBuild.previousBuild) {
			if (currentBuild.previousBuild.rawBuild.isBuilding() || currentBuild.previousBuild.previousBuild.rawBuild.isBuilding()) {
				error("A previous build is still running.")
			}
		}
	}
	
    stage ('clean up workspace') {
        deleteDir()
    }
     
    stage ('check if rebuild') {
        if (! rebuild_number.isEmpty()) {
            echo "rebuild number = ${rebuild_number}"
            mxBuildVersion = "${mxBuildVersion}-${rebuild_number}"
            echo "set mxBuildVersion to ${mxBuildVersion}"
        } else {
            echo 'no rebuild'
        }
    }
    
	stage ('run parallel pipelines') {
    	parallel(    
        	a: {
            	stage ('Mining - Plugin') {
					def buildResult = build job: 'DM_Tests_Plugin', propagate: false, parameters: [
						extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion),
						booleanParam(name: 'deploy', value: deploy)
					]
					miscUtils.evaluateBuildResult(buildResult)
					echo "Build result of Plugin: ${buildResult.result}"
					parseBuildResults('Mining - Plugin', buildResult)
		       }
            },
            
            b: {
	            stage ('Mining - Web UI & IAM') {
	            	sleep 60
					def buildResult = build job: 'DM_Tests_WebUI', propagate: false, parameters: [
						extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion)
					]
					miscUtils.evaluateBuildResult(buildResult)
					echo "Build result of Web UI & IAM: ${buildResult.result}"
					parseBuildResults('Mining - Web UI & IAM', buildResult)
	           }
            },

	        c: {
	            stage ('Mining - Discovery') {
	            	sleep 120
					def buildResult = build job: 'DM_Tests_Discovery', propagate: false, parameters: [
						extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion)
						]
					miscUtils.evaluateBuildResult(buildResult)
					echo "Build result of Discovery: ${buildResult.result}"
					parseBuildResults('Mining - Discovery', buildResult)
	            }
				
				stage ('Mining - UI Selenium') {
	            	sleep 120
					def buildResult = build job: 'DM_Tests_WebUI_Selenium', propagate: false, parameters: [
						extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion)
						]
					miscUtils.evaluateBuildResult(buildResult)
					echo "Build result of web UI - Selenium: ${buildResult.result}"
					parseBuildResults('Mining - UI Selenium', buildResult)
	            }
        	},
			
			d: {
	            stage ('Mining - Performance') {
	            	sleep 180
					def buildResult = build job: 'DM_Tests_Performance', propagate: false, parameters: [
						extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion)
						]
					miscUtils.evaluateBuildResult(buildResult)
					echo "Build result of Performance: ${buildResult.result}"
					parseBuildResults('Mining - Performance', buildResult)
	            }
				
				stage('Mining - AWS') {
					def buildResult = build job: 'DM_Tests_AWS_ASG', propagate: false, parameters: [
						string(name: 'mxBuildVersion', value: "${mxBuildVersion}")
						]
						miscUtils.evaluateBuildResult(buildResult)
						echo "Build result of AWS_Testsuite: ${buildResult.result}"
						parseBuildResults('Mining - AWS', buildResult)
						
						if (buildResult.result != 'ABORTED' && buildResult.result != 'FAILURE') {
							try {
								copyArtifacts(projectName: buildResult.projectName, selector: specific("${buildResult.number}"));					    
							} catch (e) {
								unstable "Problem retrieving artifacts from AWS pipeline: ${e}"
							}
						}
	            }
				
				stage ('Mining - CustomConfig') {
	            	sleep 60
					def buildResult = build job: 'DM_Tests_CustomConfig', propagate: false, parameters: [
						extendedChoice(name: 'mxBuildVersion', value: mxBuildVersion)
					]
					miscUtils.evaluateBuildResult(buildResult)
					echo "Build result of Mining - CustomConfig: ${buildResult.result}"
					parseBuildResults('Mining - CustomConfig', buildResult)
	           }
        	}/*,
			
	        e: {
	            stage('Mining - AWS') {
	            	sleep 180
					def buildResult = build job: 'DM_Tests_AWS_ASG', propagate: false, parameters: [
						string(name: 'mxBuildVersion', value: "${mxBuildVersion}")
						]
						miscUtils.evaluateBuildResult(buildResult)
						echo "Build result of AWS_Testsuite: ${buildResult.result}"
						parseBuildResults('Mining - AWS', buildResult)
						
						if (buildResult.result != 'ABORTED' && buildResult.result != 'FAILURE') {
							try {
								copyArtifacts(projectName: buildResult.projectName, selector: specific("${buildResult.number}"));					    
							} catch (e) {
								unstable "Problem retrieving artifacts from AWS pipeline: ${e}"
							}
						}
	            }
	        } */
        )
    }
    
    stage ('Sending mail notifications') {
		if (! currentBuild.currentResult.equals('FAILURE')) {
			def subject = "Pipeline: Test Summary - ${mxBuildVersion} - Test Suite Mining"
			if (unstableStagesMap || failedPipelines || abortedPipelines) {
				body = "Version: ${mxBuildVersion}<br/><br/>"
				body += "Link to Jenkins build: http://qef-linux1-us.deloitte.com:8085/job/DM_Test_Suite/${currentBuild.number}/<br/><br/>"
				
				def stablePercentage = 100 - (((unstableStagesCount + failedStagesCount) / totalTestCount) * 100)
				body += "Total executed tests: ${totalTestCount}<br/>"
				body += "Percentage of stable tests: ${String.format('%.2f',stablePercentage)}%<br/>"
				
				if (failedPipelines) {
					body += "<br/>The following pipelines failed: ${failedPipelines}<br/>"
				}
				if (abortedPipelines) {
					body += "<br/>The following pipelines were aborted: ${abortedPipelines}<br/>"
				}
				
				if (unstableStagesMap) {
					body += "<br/>The following ${unstableStagesCount+failedStagesCount} test(s) of the Mining Test Suite pipeline did not pass:<br/>"
					
					unstableStagesMap.each { buildName, unstableStages ->
						body += "<br/>Pipeline ${buildName} (${unstableStages.size()} of ${totalTestCountMap[buildName.substring(0, buildName.lastIndexOf('-')-1)]})<br/>"
						unstableStages.each { unstableStage ->
							def stageInfo = unstableStage.split(':')
							if (buildName.contains('Performance')) {
								body += "- ${stageInfo[0]}: http://qef-linux1-us.deloitte.com:8085/job/DM_LeanFT_Mining_Performance_Testjob/${stageInfo[1]}/<br/>"
							} else if (buildName.contains('Selenium')) {
								body += "- ${stageInfo[0]}: http://qef-linux1-us.deloitte.com:8085/job/DM_Selenium_Mining_Testjob/${stageInfo[1]}/<br/>"
							} else if (buildName.contains('Handshake')) {
								body += "- ${stageInfo[0]}: http://qef-linux1-us.deloitte.com:8085/view/mining/job/DM_Test_Version_Handshake/${stageInfo[1]}/<br/>"
							} else {
								body += "- ${stageInfo[0]}: http://qef-linux1-us.deloitte.com:8085/job/DM_LeanFT_Mining_Testjob/${stageInfo[1]}/<br/>"
							}
						}
					}
				}
				mail body: body, mimeType: 'text/html', subject: subject, to: miscUtils.getMailReceivers(currentBuild.projectName)
			} else {
				body = "Version: ${mxBuildVersion}<br/><br/>"
				body += "Link to Jenkins build: http://qef-linux1-us.deloitte.com:8085/job/Mining_Test_Suite/${currentBuild.number}/<br/><br/>"
				body += 'All tests passed :)<br/>'
				body += "\nTotal executed tests: ${totalTestCount}<br/>"
				body += "Percentage of stable tests: 100%"
				mail body: body, mimeType: 'text/html', subject: subject, to: miscUtils.getMailReceivers(currentBuild.projectName)
			}
		} else {
			echo "Build status is FAILURE - no mail notifications"
		}
	}
	
	stage ('Report to Confluence') {
		try {
			withCredentials([usernamePassword(credentialsId: 'USAppModQMUserSVC-at-deloitte-com-User-PW', passwordVariable: 'qmUserPw', usernameVariable: 'qmUserName')]) {
		        // Pull the Confluence page content from
		        // https://amiconfluence.deloitte.com/pages/viewpage.action?pageId=147000381
		        def pageId = 166887470
		        
		        def responseJson = sh(script: "curl -u ${qmUserName}:\"${qmUserPw}\" -X GET \"${miscUtils.getBaseConfluenceURL()}/rest/api/content/${pageId}?expand=body.storage,version,space\" -k", returnStdout: true).trim()
				def pageInfo = readJSON text: responseJson
				def pageContent = pageInfo.body.storage.value
				
				//echo "PageInfo: ${pageInfo}"		
				//echo "Content: ${pageContent}"
				
				// Parse the html retrieved above and extend it with
				// - a new row in the overview table
				// - a new entry for Discover Code graph
				// - a new entry for Discover Metrics graph
				
		        def dateMatch = (mxBuildVersion =~ /-20(\d{2})(\d{2})(\d{2})/)[0]
		        def date = "${dateMatch[1]}/${dateMatch[2]}/${dateMatch[3]}"
				
				def duration = currentBuild.duration
				
		        def minutes = (int) (duration / (1000*60)) % 60
		        def hours   = (int) (duration / (1000*60*60)) % 24
		        def time = "${hours.toString().padLeft( 2, '0' )}h${minutes.toString().padLeft( 2, '0' )}min"
		        
				def txDmvDiscoverCode = ''
				def txDmvDiscoverMetrics = ''
				def txDmvDiscoverCodeMap = [:]
				def txDmvDiscoverMetricsMap = [:]
				def txDmvCount = 0
				
				if (fileExists('PerformanceMeasures.csv')) {
					def perfData = readFile(file: "PerformanceMeasures.csv")
					
					// line format like discovery.TxDMVTest,21.4.0-alpha-202106260436-4,154,1,413.127,2143.477
					perfData.split('\n').each { line ->
						if (line.contains('TxDMVTest')) {
							def pattern = ".+?(\\d{1}),(\\d+\\.\\d+)?,(\\d+\\.\\d+)?"
							if (line ==~ /${pattern}/) {
								def regexMatch = (line =~ /${pattern}/)[0]
								def nodeText =  (regexMatch[1] == "1") ? "node" : "nodes"
								def dcSecs = regexMatch[2].substring(0, regexMatch[2].indexOf('.'))
								txDmvDiscoverCode += "<p>${regexMatch[1]} ${nodeText}: ${dcSecs.padLeft(4,'0')}s</p>"
								txDmvDiscoverCodeMap.put("${regexMatch[1]} ${nodeText}", "${dcSecs}")
								
								def dmSecs = regexMatch[3].substring(0, regexMatch[3].indexOf('.'))
								txDmvDiscoverMetrics += "<p>${regexMatch[1]} ${nodeText}: ${dmSecs.padLeft(4,'0')}s</p>"
								txDmvDiscoverMetricsMap.put("${regexMatch[1]} ${nodeText}", "${dmSecs}")
								txDmvCount ++
							}  else {
							echo "Line \"${line}\" does not match: ${pattern}"
							}
						}
					}
				}
				
				if (txDmvCount < 2) {
					txDmvDiscoverCode += '<p>in progress</p>'
					txDmvDiscoverMetrics += '<p>in progress</p>'
				} else {
					// fill chart data if all three TxDMV tests reported data
					// set date for each chart
					def pattern = "(.+Discover\\sCode\\sPerformance.+?Build.+?)(<\\/tr>.*)"
					if (pageContent ==~ /${pattern}/) {
						def regexMatch = (pageContent =~ /${pattern}/)[0]
						pageContent = regexMatch[1] +
										"<td colspan=\"1\">${date.substring(3,8)}</td>" +
										regexMatch[2] 
					} else {
						echo "Content does not match: ${pattern}"
					}
					
					pattern = "(.+Discover\\sMetrics\\sPerformance.+?Build.+?)(<\\/tr>.*)"
					if (pageContent ==~ /${pattern}/) {
						def regexMatch = (pageContent =~ /${pattern}/)[0]
						pageContent = regexMatch[1] +
										"<td colspan=\"1\">${date.substring(3,8)}</td>" +
										regexMatch[2] 
					} else {
						echo "Content does not match: ${pattern}"
					}
					
					// fill in performance data
					txDmvDiscoverCodeMap.each { column, dctime ->
						pattern = "(.+Discover\\sCode\\sPerformance.+?${column}.+?)(<\\/tr>.*)"
						if (pageContent ==~ /${pattern}/) {
							def regexMatch = (pageContent =~ /${pattern}/)[0]
							pageContent = regexMatch[1] +
											"<td colspan=\"1\">${dctime}</td>" +
											regexMatch[2] 
						} else {
							echo "Content does not match: ${pattern}"
						}
					}
					txDmvDiscoverMetricsMap.each { column, dmtime ->
						pattern = "(.+Discover\\sMetrics\\sPerformance.+?${column}.+?)(<\\/tr>.*)"
						if (pageContent ==~ /${pattern}/) {
							def regexMatch = (pageContent =~ /${pattern}/)[0]
							pageContent = regexMatch[1] +
											"<td colspan=\"1\">${dmtime}</td>" +
											regexMatch[2] 
						} else {
							echo "Content does not match: ${pattern}"
						}
					}
				}
				
				
				def percentage = String.format("%.2f",100-(((unstableStagesCount + failedStagesCount) / totalTestCount) * 100))
				def newRow = "<tr>" + 
				                "<td style=\"text-align: center;\" colspan=\"1\">${date}</td>" +
				                "<td style=\"text-align: center;\" colspan=\"1\">${time}</td>" +
				                "<td style=\"text-align: center;\" colspan=\"1\">" +
				                    "${txDmvDiscoverCode}" +
				                "</td>" +
				                "<td style=\"text-align: center;\" colspan=\"1\">" +
				                    "${txDmvDiscoverMetrics}" +
				                "</td>" +
				                "<td style=\"text-align: center;\" colspan=\"1\">${totalTestCount}</td>" +
				                "<td style=\"text-align: center;\" colspan=\"1\">${totalTestCount-unstableStagesCount}</td>" +
				                "<td style=\"text-align: center;\" colspan=\"1\">${unstableStagesCount}</td>" +
				                "<td style=\"text-align: center;\" colspan=\"1\">${failedStagesCount}</td>" +
				                "<td style=\"text-align: center;\" colspan=\"1\">${percentage}%</td>" +
				                "<td style=\"text-align: left;\" colspan=\"1\"><br /></td>" +
			                "</tr>"
				
				// Insert new row in page content		
				def splitContent = pageContent.split('</tbody>',2)
				pageContent = splitContent[0] + 
									newRow + 
									'</tbody>' + 
									splitContent[1]
				
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
