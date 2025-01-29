@Library('TestUtils') _

/*
* Download a single file from sharepoint and upload it to corresponding jira ticket.
* @param relativePath (will be trimed) defines a relative path to a downloadable file on sharepoint, e.g. CDHS/CDHS_maxenso_19.2.04.zip. The root folder is Shared Documents/share/QM - Quality Management/customer-delivery/ on sharepoint.
*  Type: String parameter
* @param ticketNumber defines the corresponding jira ticket number for the file
*  Type: String parameter
*/
def fileName = ''

node('Tool-sharepoint') {
    def gitUtils = new GitUtils()
    def prodUtils = new ProductDeliveryUtils()

    stage('clean-up') {
        deleteDir()
    }

    stage('initialize') {
        relativePath = relativePath.trim()

        buildName "#${env.BUILD_ID} - ${ticketNumber}"
        buildDescription "relativePath=${relativePath} ticketNumber=${ticketNumber}"

        gitUtils.checkoutGitProject('.', "${gitUtils.getGitUrlQef()}/release-management/automated-sharepoint-upload.git", 'master')
    }

    stage('download-from-sharepoint') {
        (path, fileName) = prodUtils.getFilePathAndFileName("Shared Documents/share/QM - Quality Management/customer-delivery/${relativePath}")

        withCredentials([usernamePassword(credentialsId: 'USAppModQMUserSVC-at-deloitte-com-User-PW', passwordVariable: 'password', usernameVariable: 'username')]) {
            retry(5) {
                powershell script: "./downloadFromSharepoint.ps1 ${username} '${password}' '${path}/' '${fileName}' '${pwd()}\\'"
            }
        }

        stash includes: fileName, name: fileName
    }
}

node('OS-Linux') {
    stage('upload-to-jira') {
        unstash fileName

        jiraUploadAttachment file: fileName, idOrKey: ticketNumber, site: 'IRIS'
    }
}
