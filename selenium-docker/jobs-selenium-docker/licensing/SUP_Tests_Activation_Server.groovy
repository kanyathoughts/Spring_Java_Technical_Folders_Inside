@Library('TestUtils') _

/**
 * Run tests to ensure the activation server is working as expected. Following test cases are executed:
 * - Test if local instance of the server listens to a certain port and passes the lifecheck
 * - Test if version deployed to production listens to a certain port and passes the lifecheck
 *
 * @param waitWhenEnvironmentIsRunning  If true the job stops and you can perform manual test on the environment. If false the job finishes after running the tests.
 *        type: boolean
 */

node('USLinux1') {
	
	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def lifecheckDir = 'base-license-lifecheck'
	def webappsDir = 'webapps'
	
	buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
	buildDescription "waitWhenEnvironmentIsRunning=${waitWhenEnvironmentIsRunning}"
	
	stage('prepare local environment') {
		def persistenceXmlFile = 'WEB-INF/classes/META-INF/persistence.xml'
		def activationServerJdbcUrlProd = 'jdbc:mysql://innowake-activation-production.cdbzmovegw6k.eu-central-1.rds.amazonaws.com:3306/production_innowake_activation?enabledTLSProtocols=TLSv1.2'
		
		deleteDir()
		docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside('-v jenkins-m2-repo:/var/m2_repo:rw') {
			dir(webappsDir) {
				withMaven(maven: 'Default') {
					sh "$MVN_CMD dependency:copy -Dartifact=innowake.base.license.server:base-license-server:${mxBuildVersion}:war -DoutputDirectory=."
				}
				sh 'mv base-license-server*.war activation.war'
				sh "unzip -d . activation.war ${persistenceXmlFile}"
				withCredentials([usernamePassword(credentialsId: 'LMP_MySQL_User_Prod', passwordVariable: 'mySQLPw', usernameVariable: 'mySQLUser')]) {
					sh "sed -i 's,<property name=\"javax.persistence.jdbc.driver\" value=\".*\" />,<property name=\"javax.persistence.jdbc.driver\" value=\"com.mysql.jdbc.Driver\" />,g' ${persistenceXmlFile}"
					sh "sed -i 's,<property name=\"javax.persistence.jdbc.url\" value=\".*\" />,<property name=\"javax.persistence.jdbc.url\" value=\"${activationServerJdbcUrlProd}\" />,g' ${persistenceXmlFile}"
					sh "sed -i 's,<property name=\"javax.persistence.jdbc.user\" value=\".*\" />,<property name=\"javax.persistence.jdbc.user\" value=\"prodwebuser_activation\" />,g' ${persistenceXmlFile}"
					sh "sed -i 's,<property name=\"javax.persistence.jdbc.password\" value=\".*\" />,<property name=\"javax.persistence.jdbc.password\" value=\"innoWakeSQLProdDB\" />,g' ${persistenceXmlFile}"
				}
				sh "zip -d activation.war ./${persistenceXmlFile}"
				sh "zip -u activation.war ./${persistenceXmlFile}"
			}
			dir(lifecheckDir) {
				withMaven(maven: 'Default') {
					sh "$MVN_CMD dependency:copy -Dartifact=innowake.base.license.lifecheck:base-license-lifecheck:${mxBuildVersion}:package.zip -DoutputDirectory=."
				}
				sh "mv ${lifecheckDir}*.package.zip ${lifecheckDir}.package.zip"
				sh "unzip ${lifecheckDir}.package.zip"
				sh 'chmod u=rwx start-en.sh'
			}
		}
	}
	
	stage('run tests against local environment') {
		def jobWorkspace = pwd()
		def hostname = miscUtils.getHostname()
		def testTomcatVersion = '9.0.37'
		def testActivationServerUrl = "http://${hostname}:9090/activation"
		
		docker.image("tomcat:${testTomcatVersion}-jdk8-openjdk").withRun("--name activation_server -v ${jobWorkspace}/${webappsDir}/activation.war:/usr/local/tomcat/webapps/activation.war -p 9090:8080") {
			docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside {
				// Wait until application is started
				sleep 10
				
				// Test case 1: use curl to get a response from the test instance
				def curlOutput = sh returnStdout: true, script: "curl ${testActivationServerUrl}"
				echo "curlOutput = ${curlOutput}"
				if (! (curlOutput.contains("Apache Tomcat/${testTomcatVersion}") && curlOutput.contains('HTTP Status 405'))) {
					unstable 'Test case 1 = UNSTABLE'
				}
				
				// Test case 2: use  base-license-lifecheck to check if the test activation server and the database are running
				dir(lifecheckDir) {
					def output = sh returnStdout: true, script:  "./start-en.sh ${testActivationServerUrl}"
					echo "output = ${output}"
					if (! output.contains('Activation server life check successful')) {
						unstable 'Test case 2 = UNSTABLE'
					}
				}
			}
		}
	}
	
	stage('run tests against productive environment') {
		docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside {
			def prodActivationServerUrl = 'https://innowake-activation.deloitte.com/activation'
			def prodTomcatVersion = '9.0.62'
			
			// Test case 3: use curl to get a response from the prod server
			def curlOutput = sh returnStdout: true, script: "curl ${prodActivationServerUrl}"
			echo "curlOutput = ${curlOutput}"
			if (! (curlOutput.contains("Apache Tomcat/${prodTomcatVersion}") && curlOutput.contains('HTTP Status 405'))) {
				unstable 'Test case 3 = UNSTABLE'
			}
			
			// Test case 4: use  base-license-lifecheck to check if the prod activation server and the database are running
			dir(lifecheckDir) {
				def output = sh returnStdout: true, script:  "./start-en.sh ${prodActivationServerUrl}"
				echo "output = ${output}"
				if (! output.contains("Activation server life check successful")) {
					unstable 'Test case 4 = UNSTABLE'
				}
			}
			
			if (Boolean.parseBoolean(waitWhenEnvironmentIsRunning)) {
				input 'Click on proceed when you do not longer need the environment'
			}
		}
	}
}