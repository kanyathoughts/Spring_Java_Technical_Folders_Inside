import org.jenkinsci.plugins.pipeline.modeldefinition.Utils
@Library('TestUtils') _
/**
 * This job runs the overall modernization test suite.
 * 
 * @param version	The version to use.
 */
node('MOD') {
	def miscUtils = new MiscUtils()
	def workDir = pwd()
	def totalTestCount = 0
	def unstableStagesCount = 0
	def failedStagesCount = 0
	def totalTestCountMap = [:]
	def unstableStagesMap = [:]
	def failedPipelines = ''
	def abortedPipelines = ''
	
	buildName "#${env.BUILD_ID} - ${version}"
	buildDescription "version=${version}"
	
	// parse build descriptions, unstableStages are in format stageName:buildNumber
	// example "... totalTestCount=35 unstableStages=test.DBCrawlerTest:75, ..."
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
		if (currentBuild.previousBuild.rawBuild.isBuilding() || currentBuild.previousBuild.previousBuild.rawBuild.isBuilding()) {
			error("A previous build is still running.")
		}
	}
	
	stage ('clean up workspace') {
		deleteDir()
	}
	
	stage ('DB Cutter Tests') {
		def buildResult = build job: 'Mod_Tests_DB_Cutter', propagate: false, parameters: [
			string(name: 'buildTag', value: version)
		]
		miscUtils.evaluateBuildResult(buildResult)
		echo "Build result of Modernization - DB Cutter Tests: ${buildResult.result}"	
		parseBuildResults('Modernization - DB Cutter', buildResult)
	}
	
	stage ('Monolith Cutter Tests') {
		def buildResult = build job: 'Mod_Tests_Monolith_Cutter', propagate: false, parameters: [
			string(name: 'buildTag', value: version)
		]
		miscUtils.evaluateBuildResult(buildResult)
		echo "Build result of Modernization - Monolith Cutter Tests: ${buildResult.result}"
		parseBuildResults('Modernization - Monolith Cutter', buildResult)
	}
	
	stage ('Sending mail notifications') {
		if (! currentBuild.currentResult.equals('FAILURE')) {
			def subject = "Pipeline: Test Summary - ${version} - Modernization Test Suite"
			if (unstableStagesMap || failedPipelines || abortedPipelines) {
				body = "Version: ${version}<br/><br/>"
				body += "Link to Jenkins build: http://qef-linux1-us.deloitte.com:8085/job/Mod_Test_Suite/${currentBuild.number}/<br/><br/>"
				
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
					body += "<br/>The following ${unstableStagesCount+failedStagesCount} test(s) of the Modernization Test Suite pipeline did not pass:<br/>"
					
					unstableStagesMap.each { buildName, unstableStages ->
						body += "<br/>Pipeline ${buildName} (${unstableStages.size()} of ${totalTestCountMap[buildName.substring(0, buildName.lastIndexOf('-')-1)]})<br/>"
						unstableStages.each { unstableStage ->
							def stageInfo = unstableStage.split(':')
							body += "- ${stageInfo[0]}: http://qef-linux1-us.deloitte.com:8085/job/Mod_Selenium_Testjob/${stageInfo[1]}/<br/>"
							//body += "- ${stageInfo[0]}<br/>"
						}
					}
				}
				mail body: body, mimeType: 'text/html', subject: subject, to: miscUtils.getMailReceivers(currentBuild.projectName)
			} else {
				body = "Version: ${version}<br/><br/>"
				body += "Link to Jenkins build: http://qef-linux1-us.deloitte.com:8085/job/Mod_Test_Suite/${currentBuild.number}/<br/><br/>"
				body += 'All tests passed.<br/>'
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
		        def pageId = 197859805
		        
		        def responseJson = sh(script: "curl -u ${qmUserName}:\"${qmUserPw}\" -X GET \"${miscUtils.getBaseConfluenceURL()}/rest/api/content/${pageId}?expand=body.storage,version,space\" -k", returnStdout: true).trim()
				def pageInfo = readJSON text: responseJson
				def pageContent = pageInfo.body.storage.value
				
				//echo "PageInfo: ${pageInfo}"		
				//echo "Content: ${pageContent}"
				
				// Parse the html retrieved above and extend it with
				// - a new row in the overview table
				// - a new entry for Discover Code graph
				// - a new entry for Discover Metrics graph

				def buildversion = "${version}"
		        def dateMatch = (version =~ /.20(\d{2})(\d{2})(\d{2})/)[0]
		        def date = "${dateMatch[1]}/${dateMatch[2]}/${dateMatch[3]}"
				
				def duration = currentBuild.duration

				
				
		        def minutes = (int) (duration / (1000*60)) % 60
		        def hours   = (int) (duration / (1000*60*60)) % 24
		        def time = "${hours.toString().padLeft( 2, '0' )}h${minutes.toString().padLeft( 2, '0' )}min"
				
				def percentage = String.format("%.2f",100-(((unstableStagesCount + failedStagesCount) / totalTestCount) * 100))
				def newRow = "<tr>" + 
				                "<td style=\"text-align: center;\" colspan=\"1\">${date}</td>" +
								 "<td style=\"text-align: center;\" colspan=\"1\">${buildversion}</td>" +
				                "<td style=\"text-align: center;\" colspan=\"1\">${time}</td>" +
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
