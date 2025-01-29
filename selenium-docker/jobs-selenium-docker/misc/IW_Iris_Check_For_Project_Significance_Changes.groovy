@Library('TestUtils') _

import groovy.json.JsonSlurper
import java.time.*
import java.text.SimpleDateFormat

/**
 * Collects all Tickets with changes in the last 24 hours.
 * If there are tickets, were the project significance changed, an email notification will be send.
 */
 
node('OS-Linux') {
	def miscUtils = new MiscUtils()
	def tickets = []
	def entriesTickets = []
	def entriesWDISAndWMINTickets = []
	def outputTickets = ''
	def outputWDISAndWMINTickets  = ''

    stage('query jira') {
		def jiraResponseData = jiraJqlSearch jql: '(project = WMEE OR project = WMDD OR project = WNDT OR project = WSC OR project = WBASE OR project = WMIN OR project = WDIS) and updated  >=  -24h', site: 'IRIS', failOnError: false
		if ( ! jiraResponseData.successful) {
			error('Jira request failed!')
		}
		
	  	for (int i = 0; i < jiraResponseData.data.issues.size(); i++) { 
	        tickets.add(jiraResponseData.data.issues[i].key)
	    }
		   
		tickets.each { ticketKey ->
			def curlOutput
			withCredentials([usernamePassword(credentialsId: 'USAppModQMUserSVC-at-deloitte-com-User-PW', passwordVariable: 'jiraPw', usernameVariable: 'jiraUser')]) {
				curlOutput = sh returnStdout: true, script: "curl -X GET https://iriseu.deloitte.com/rest/api/2/issue/${ticketKey}?expand=changelog --user '${jiraUser}:${jiraPw}' --header 'Accept: application/json'"
			}
			def ticketAsJson = new JsonSlurper().parseText(curlOutput)
			ticketAsJson.changelog.histories.each { history ->
				def format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
				if (format.parse(history.created) >= new Date() - 1) {
					history.items.each { item -> item.field 
						if (item.field == 'Project Significance') {
							if (getSignificanceCategory(item.fromString) != getSignificanceCategory(item.toString)) {
								def entry = [date: history.created, text: "Date: ${format.parse(history.created).format("MM-dd-yyyy HH:mm:ss")} Ticket: ${ticketAsJson.key} Summary: ${ticketAsJson.fields.summary} From: ${item.fromString} (${getSignificanceCategory(item.fromString)}) To: ${item.toString} (${getSignificanceCategory(item.toString)}) Priority: ${ticketAsJson.fields.priority.name} Assignee: ${checkAssignee(ticketAsJson.fields.assignee)}"]
								def project = ticketAsJson.key.split('-')
								if (project[0] == "WMIN" || project[0] == "WDIS") {
									entriesWDISAndWMINTickets.add(entry)
								} else {
									entriesTickets.add(entry)
								}
							}
						}
                    }
                }
			}
	    }
		def sortedWDISAndWMINTickets = sortAfterDate(entriesWDISAndWMINTickets)
		def sortedTickets = sortAfterDate(entriesTickets)
		sortedTickets.each { item ->
			outputTickets += item.text + '\n'
		}
		sortedWDISAndWMINTickets.each { item ->
			outputWDISAndWMINTickets += item.text + '\n'
		}
		echo outputTickets
		echo outputWDISAndWMINTickets
    }
	
	stage('send email notification') {
		if (outputTickets.length() > 0) {
			wrap([$class: 'BuildUser']) {
            	def subj = 'Project significance for the following tickets changed in the last 24 hours'
            	def body = outputTickets										
            	def to = miscUtils.getMailReceivers(currentBuild.projectName,'Tickets')
            	emailext subject: subj, body: body, to: to
        	}
		}
	}

	stage('send email notification for WMIN and WDIS') {
		if (outputWDISAndWMINTickets.length() > 0) {
			wrap([$class: 'BuildUser']) {
			 	def subj = 'Project significance for the following WDIS and WMIN tickets changed in the last 24 hours'
				def body = outputWDISAndWMINTickets
			 	def to = miscUtils.getMailReceivers(currentBuild.projectName,'WDISAndWMINTickets')
			 	emailext subject: subj, body: body, to: to
			}
		}
	}
}

/**
 * Gets value of the project significance field and categorize it 
 */
def getSignificanceCategory(significance) {
	if (significance == null) {
		return 'No category'
	}
	def signInt = Integer.parseInt(significance)
	if (signInt >= 0 && signInt <= 999) {
		return 'Not for this project or team'
	}
	if (signInt <= 1499) {
		return 'By go-live, end of project'
	}
	if (signInt <= 1749) {
		return 'By next milestone'
	}
	if (signInt <= 1899) {
		return 'Within the next 10 working days'
	}
	if (signInt <= 2000) {
		return 'As soon as possible'
	}
}

/**
 * Checks the value of the assignee field and gives back one of two elements.
 */
def checkAssignee(assignee) {
	if (assignee == null) {
		return 'No assignee'
	} else {
		return assignee.displayName
	}
}

/**
 * This method sorts all dates stored in the incoming array
 */
@NonCPS
def sortAfterDate(array) {
	return array.sort{ it['date'] }
}