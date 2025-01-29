/*
 * This job enables or disables a feature on a target Mining environment. Therefore, following parameters have to be specified.
 * 
 * @param executeOn          The node of the target environment.
 * @param feature            The feature that needs to be enabled/disabled.
 * @param switchOnOrOff      The On or Off action on the feature.
 * 
 */
@Library('TestUtils') _
import groovy.json.JsonSlurper

node(executeOn) {
	def serverIPToRunOn = '10.25.44.122'
	def setState = false
	
	buildDescription "executeOn=${executeOn} feature=${feature} switchOnOrOff=${switchOnOrOff}"
	
	switch (executeOn) {
		case 'USLinux1':
		    serverIPToRunOn = '10.241.173.126'
			break 
		case 'USLinux3':
			serverIPToRunOn = '10.25.35.211'
			break
		case 'USLinux5-DM':
			serverIPToRunOn = '10.241.173.116'
			break
		case 'USLinux6-DM':
			serverIPToRunOn = '10.241.173.120'
			break
		case 'USLinux7-DM':
			serverIPToRunOn = '10.241.173.104'
			break
		case 'USLinux8-DM':
			serverIPToRunOn = '10.241.173.102'
			break
	}

	if (switchOnOrOff == 'On') {
		setState = true
	} else {
		setState = false
	}
	
	stage('Enable/Disable feature') {   
		def accessToken
		
		echo "Getting Keycloak token"
		
		def response = sh(script: "curl -d 'client_id=backend' -d 'username=first' -d 'password=first' -d 'grant_type=password' 'http://${serverIPToRunOn}:8180/auth/realms/mining/protocol/openid-connect/token'", returnStdout: true).trim()
		def responseJson = readJSON text: response
		if (responseJson.access_token) {
			accessToken = responseJson.access_token
		} else {
			error "Could not retrieve access token from Keycloak: ${responseJson}"
		}
			
		echo "Switching ${feature} to ${setState} on ${serverIPToRunOn}"
		sh "curl -X POST 'http://${serverIPToRunOn}:8080/api/v1/features/${feature}/toggle?state=${setState}' -H 'Authorization: Bearer ${accessToken}'"
	}
}
