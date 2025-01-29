@Library('TestUtils') _
/**
 * This job creates a monolith cutter environment of a specific version on the selected node.
 * 
 * @param buildTag		The db cutter version/tag to use.
 * @param executeOn		Node where to create the db cutter environment	
 */
node(executeOn) {
	def jobWorkspace = pwd()
	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def pdDockerRegistryUrl = "https://pd-gitlab.deloitte.com:8000"
	def registryAccessToken = "fL1ikGu9hKxQCsN87W2V"
	def trunkBuildIdentifierLength = 13
	def serverIPToRunOn = '10.241.173.102'
	
	buildName "#${env.BUILD_ID} - ${buildTag}"
	buildDescription "buildTag=${buildTag} executeOn=${executeOn}"
	
	stage('TBD') {
		//TODO
	}
}