@Library('TestUtils') _

/**
 * Search for issues with attachments in the specific project. Add a comment linking all attachments to make them available for external customers.
 * @param projectID         Search this project for issues with attachments.
 * @param jiraHost          Choose jira host.
 * @param limitTestResults  Limits results to 2 for testing purposes.
 */

node('OS-Linux') {
    def dockerUtils = new DockerUtils()
    timestamps {
        buildName "#${env.BUILD_ID} - ${projectID}"
        buildDescription "projectID=${projectID} jiraHost=${jiraHost} limitTestResults=${limitTestResults}"
        stage('Search and add attachments to comments') {
            def jiraResponseData
            if (Boolean.parseBoolean(limitTestResults)) {
                jiraResponseData = jiraJqlSearch failOnError: false, jql: "project = ${projectID} AND attachments is not EMPTY", maxResults: 2, site: jiraHost
            } else {
                jiraResponseData = jiraJqlSearch failOnError: false, jql: "project = ${projectID} AND attachments is not EMPTY", site: jiraHost
            }
            
            def tickets = []
            def attachmentNames = [:]
            def commentAuthors = [:]
            if ( ! jiraResponseData.successful) {
                error('Jira request failed!')
            }
            for (int i = 0; i < jiraResponseData.data.issues.size(); i++) { 
                def issue = jiraGetIssue idOrKey: jiraResponseData.data.issues[i].key, site: jiraHost
                tickets.add(jiraResponseData.data.issues[i].key)
                attachmentNames[jiraResponseData.data.issues[i].key] = []
                for (filename in issue.data.fields.attachment.filename) {
                    attachmentNames[jiraResponseData.data.issues[i].key].add(filename)
                }
                commentAuthors[jiraResponseData.data.issues[i].key] = []
                for (name in issue.data.fields.comment.comments.author.name) {
                    commentAuthors[jiraResponseData.data.issues[i].key].add(name)
                }
            }
            def listingAttachments
            tickets.each { ticketKey ->
                listingAttachments = '*Listing attachments:*\n'
                for (attachmentName in attachmentNames[ticketKey]) {
                    if (attachmentName ==~ /.+(\.png|\.jpg|\.jpeg)$/) {
                        listingAttachments += "!${attachmentName}|thumbnail!\n"
                    } else {
                        listingAttachments += "[^${attachmentName}]\n"
                    }
                }
                if (!commentAuthors[ticketKey].contains('USAppModQMUserSVC@deloitte.com')) {
                    jiraAddComment comment: listingAttachments, idOrKey: ticketKey, site: jiraHost, auditLog: false
                }
            }
        }
    }
}