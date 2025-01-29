@Library('TestUtils') _

/**
 * This job creates the changelog for a given Jira query.
 * The output is either  Wordpress optimized HTML file, saved as job artifact.
 * 
 * @param jiraQuery The Jira query from which on to generate changelogs
 * @param isInternal  Creates spotlight placeolders and ticket IDs as links to their i.ris URL for internal changelogs.
 *                    Customer facing changelogs should have plaintext ticket IDs.
 */


node {
	
	def changeLogsUtils
	
	stage('Initialize') {
		if (jiraQuery == '') {
			error 'A valid jira filter query must be entered'
		}
		changeLogsUtils = new ChangeLogsUtils(this)
		changeLogsUtils.setIsInternal(isInternal)
		
		changeLogsUtils.setJiraQuery(jiraQuery)
		
		deleteDir()
	}
	
    stage('Query Jira') {
    	def jqlFilter = changeLogsUtils.createJqlFilterFromQuery()
       	def jiraResponseData = jiraJqlSearch jql: jqlFilter, site: 'IRIS', failOnError: false
       	if ( ! jiraResponseData.successful) {
       		error('Jira request failed!')
       	}
    			
    	final chapterOrder = ['General','Transformation','Legacy DevOps','Modernization','Mining']
    		
		def outputResult = changeLogsUtils.createHTMLForIssues(jiraResponseData.data.issues)
		def xmlText = changeLogsUtils.getHtmlBodyStart()
		
		def outputResultOrdered = [:]
		chapterOrder.each {
			chapterName ->
			if(outputResult.keySet().contains(chapterName)) {
				outputResultOrdered[chapterName] = outputResult[chapterName]	
			}
		}
		
		outputResultOrdered.keySet().each {
			categoryLevel1 ->
			xmlText += outputResult[categoryLevel1]
		}
		writeFile file: "Changelogs.html", text: xmlText
    }
    
    stage('Archive changelogs') {
		archiveArtifacts '*.html'
    }
    
}
