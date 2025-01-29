@Library('TestUtils') _

/**
 * Test of the ChangeLogsUtils
 */

node('OS-Linux') {
	
	stage('test') {
    	def svnUtils = new SvnUtils()
    	def resultComparisonUtils = new ResultComparisonUtils()
    	def changeLogsUtils = new ChangeLogsUtils(this)
    	
    	deleteDir()
    	
    	def svnUrl = "${svnUtils.getSvnUrlRm()}/Test_ChangeLogsUtils"
		withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'svnPw', usernameVariable: 'svnUser')]) {
			svnUtils.svnExportRecursive(svnUrl, '.', svnUser, svnPw)
		}
		def testIssues = readJSON file: 'testIssues.json'
		
		changeLogsUtils.setStartEndVersion('18.0.0.01', '18.0.0.09')
		def actualResult = changeLogsUtils.createDocbookForIssues(testIssues)
		
		sh 'mkdir actual'
		actualResult.keySet().each {
			category ->
			def docbook = actualResult[category]
			def fileName = "actual/${category}.xml"
			writeFile file: fileName, text: docbook
		}
		
		sh 'mkdir log'
		sh 'mkdir tmp'
		def rc = resultComparisonUtils.resultCompare('actual', 'expected', [], [], 'log', 'tmp')
		if (rc != 0) {
			unstable 'Written files are different than the expected results'
		}
		
		archiveArtifacts artifacts: '**/*'
	}
    
}
