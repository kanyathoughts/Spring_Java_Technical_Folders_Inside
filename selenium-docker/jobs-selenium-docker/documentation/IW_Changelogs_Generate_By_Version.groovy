@Library('TestUtils') _

/**
 * This job creates the changelog for a set of maxenso versions.
 * The versions are specified as a range starVersion-endVersion, the edge versions being given by two job parameters.
 * The output is either  Wordpress optimized HTML file, saved as job artifact.
 * 
 * @param startVersion The start release version from which on to generate changelogs
 * @param endVersion The end release version up to which to generate changelogs
 *                   May be left empty, in which case the job sets endVersion to startVersion.
 * @param relevance The relevance considered for the changelogs. Hotfix changelogs include logs 
 * 					marked with relevance 'External' as well as 'Hotfix'.
 */


node {
	
	def changeLogsUtils = new ChangeLogsUtils(this)
	
	stage('Initialize') {
		if (endVersion == '') {
			endVersion = startVersion
			echo "endVersion was not set, therefore set to startVersion ${startVersion}"
		}
		buildName "#${BUILD_NUMBER} - ${startVersion} - ${endVersion}"
		
		echo "Parameter check. startVersion = ${startVersion}, endVersion = ${endVersion}"
		if (startVersion.contains('_') || endVersion.contains('_')) {
			error 'Alpha or beta versions not allowed.'
		}
		
		if (startVersion.count('.') != endVersion.count('.')) {
			// E.g. cannot compare 19.0.0.03 to 19.1.00
			error 'Cannot compare deprecated 4 digit version scheme with 3 digit scheme'
		}
			
		startVersionMajor = startVersion.substring(0, startVersion.lastIndexOf("."))
		endVersionMajor = endVersion.substring(0, endVersion.lastIndexOf("."))
		
		if (startVersionMajor != endVersionMajor) {
		    error 'Start and end version may differ in the build version (last fragment) only.'
		}
		def buildVersionStart = startVersion.substring(startVersion.lastIndexOf(".") + 1)
		def buildVersionEnd = endVersion.substring(endVersion.lastIndexOf(".") + 1)
		if (buildVersionStart > buildVersionEnd) {
		    error 'The end build version must be >= the start build version.'
		}

		if (relevance == 'External' || relevance == 'Hotfix') {
			changeLogsUtils.setRelevance(relevance)
		} else {
			error 'relevance is not valid. Please enter External or Hotfix.'
		}
		
		changeLogsUtils.setStartEndVersion(startVersion, endVersion)
		changeLogsUtils.setIsInternal(false)
		
		deleteDir()
	}
	
    stage('Query Jira') {
    	def jqlFilter = changeLogsUtils.createJqlFilterFromVersion()
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
