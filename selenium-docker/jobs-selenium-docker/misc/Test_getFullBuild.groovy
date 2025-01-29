@Library('TestUtils') _

/**
 * Tests MxVersionUtils.getFullBuild()
 */

def mxVersionUtils = new MxVersionUtils()
def dockerUtils = new DockerUtils()
def miscUtils = new MiscUtils()

node('Docker-host') {

	docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside("-v jenkins-m2-repo:/var/m2_repo:rw") {
		stage('incomplete build 19.4.06') {
			def runtimeOnlyBuild = '19.4.06'
			def expectedFullBuild = '19.4.04'
			def actualFullBuild = mxVersionUtils.getFullBuild(runtimeOnlyBuild)
			if (actualFullBuild != expectedFullBuild) {
				unstable "full build for ${runtimeOnlyBuild} - expected ${expectedFullBuild}, but actual is ${actualFullBuild}"
			}
		}
	
		stage('incomplete build 19.4.03') {
			def runtimeOnlyBuild = '19.4.03'
			def expectedFullBuild = '19.4.00'
			def actualFullBuild = mxVersionUtils.getFullBuild(runtimeOnlyBuild)
			if (actualFullBuild != expectedFullBuild) {
				unstable "full build for ${runtimeOnlyBuild} - expected ${expectedFullBuild}, but actual is ${actualFullBuild}"
			}
		}
	
		stage('incomplete build trunk') {
			def runtimeOnlyBuild = '22.1.0-alpha-202201210406'
			def expectedFullBuild = '22.1.0-alpha-202201201146'
			def actualFullBuild = mxVersionUtils.getFullBuild(runtimeOnlyBuild)
			if (actualFullBuild != expectedFullBuild) {
				unstable "full build for ${runtimeOnlyBuild} - expected ${expectedFullBuild}, but actual is ${actualFullBuild}"
			}
		}
	
		stage('incomplete build 22.3.2') {
			def runtimeOnlyBuild = '22.3.2'
			def expectedFullBuild = '22.3.1'
			def actualFullBuild = mxVersionUtils.getFullBuild(runtimeOnlyBuild)
			if (actualFullBuild != expectedFullBuild) {
				unstable "full build for ${runtimeOnlyBuild} - expected ${expectedFullBuild}, but actual is ${actualFullBuild}"
			}
		}
		
		stage('full build 22.3.1') {
			def fullBuild = '22.3.1'
			def expectedFullBuild = '22.3.1'
			def actualFullBuild = mxVersionUtils.getFullBuild(fullBuild)
			if (actualFullBuild != expectedFullBuild) {
				unstable "full build for ${fullBuild} - expected ${expectedFullBuild}, but actual is ${actualFullBuild}"
			}
		}
	}
	
}
