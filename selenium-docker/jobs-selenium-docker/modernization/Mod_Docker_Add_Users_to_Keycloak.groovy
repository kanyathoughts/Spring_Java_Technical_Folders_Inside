@Library('TestUtils') _
/**
 * This job add users to a running keycloak container with pre-defined users
 * 
 * @param executeAgainst			Node where the db cutter environment is up and running
 */
node('USWindows10-Mod') {
	def jobWorkspace = pwd()
	def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def gitUtils = new GitUtils()
	def serverIP = miscUtils.getIpByNodeName(executeAgainst)
    def keycloakUserDataDir = 'KeycloakUserData'
	
	buildName "#${env.BUILD_ID}"
	buildDescription "executeAgainst=${executeAgainst}"

    stage('checkout user data JSON') {
        deleteDir()
		def gitlabProject = "${gitUtils.getGitUrlQef()}/infrastructure/modernization-keycloak.git"
		dir(keycloakUserDataDir) {
			git branch: 'master', credentialsId: 'USAppModQMUserSVC-User-PW', url: gitlabProject
		}
    }
	
	stage('add users to keycloak') {
		def userData = readJSON file: "${jobWorkspace}/${keycloakUserDataDir}/userData.json"
    	withCredentials([usernamePassword(credentialsId: 'modernization-keycloak-login', passwordVariable: 'keycloakPassword', usernameVariable: 'keycloakUser')]) {
			def cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat config credentials --server http://${serverIP}:28080/auth --realm master --user ${keycloakUser} --password ${keycloakPassword}"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testmanager1.username}\" -s firstName=\"${userData.users.testmanager1.firstName}\" -s lastName=\"${userData.users.testmanager1.lastName}\" -s email=${userData.users.testmanager1.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testmanager1.username} --new-password ${userData.users.testmanager1.password}"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testeditor1.username}\" -s firstName=\"${userData.users.testeditor1.firstName}\" -s lastName=\"${userData.users.testeditor1.lastName}\" -s email=${userData.users.testeditor1.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testeditor1.username} --new-password ${userData.users.testeditor1.password}"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testeditor2.username}\" -s firstName=\"${userData.users.testeditor2.firstName}\" -s lastName=\"${userData.users.testeditor2.lastName}\" -s email=${userData.users.testeditor2.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testeditor2.username} --new-password ${userData.users.testeditor2.password}"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testrolemanager1.username}\" -s firstName=\"${userData.users.testrolemanager1.firstName}\" -s lastName=\"${userData.users.testrolemanager1.lastName}\" -s email=${userData.users.testrolemanager1.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testrolemanager1.username} --new-password ${userData.users.testrolemanager1.password}"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testrolemanager2.username}\" -s firstName=\"${userData.users.testrolemanager2.firstName}\" -s lastName=\"${userData.users.testrolemanager2.lastName}\" -s email=${userData.users.testrolemanager2.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testrolemanager2.username} --new-password ${userData.users.testrolemanager2.password}"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testroleeditor1.username}\" -s firstName=\"${userData.users.testroleeditor1.firstName}\" -s lastName=\"${userData.users.testroleeditor1.lastName}\" -s email=${userData.users.testroleeditor1.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testroleeditor1.username} --new-password ${userData.users.testroleeditor1.password}"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testroleviewer1.username}\" -s firstName=\"${userData.users.testroleviewer1.firstName}\" -s lastName=\"${userData.users.testroleviewer1.lastName}\" -s email=${userData.users.testroleviewer1.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testroleviewer1.username} --new-password ${userData.users.testroleviewer1.password}"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testmanagerqaclient.username}\" -s firstName=\"${userData.users.testmanagerqaclient.firstName}\" -s lastName=\"${userData.users.testmanagerqaclient.lastName}\" -s email=${userData.users.testmanagerqaclient.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testmanagerqaclient.username} --new-password ${userData.users.testmanagerqaclient.password}"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testeditor1qaclient.username}\" -s firstName=\"${userData.users.testeditor1qaclient.firstName}\" -s lastName=\"${userData.users.testeditor1qaclient.lastName}\" -s email=${userData.users.testeditor1qaclient.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testeditor1qaclient.username} --new-password ${userData.users.testeditor1qaclient.password}"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testeditor2qaclient.username}\" -s firstName=\"${userData.users.testeditor2qaclient.firstName}\" -s lastName=\"${userData.users.testeditor2qaclient.lastName}\" -s email=${userData.users.testeditor2qaclient.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testeditor2qaclient.username} --new-password ${userData.users.testeditor2qaclient.password}"
            cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testmanagerqaviewerclient.username}\" -s firstName=\"${userData.users.testmanagerqaviewerclient.firstName}\" -s lastName=\"${userData.users.testmanagerqaviewerclient.lastName}\" -s email=${userData.users.testmanagerqaviewerclient.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testmanagerqaviewerclient.username} --new-password ${userData.users.testmanagerqaviewerclient.password}"
            cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testroleviewer1qaclient.username}\" -s firstName=\"${userData.users.testroleviewer1qaclient.firstName}\" -s lastName=\"${userData.users.testroleviewer1qaclient.lastName}\" -s email=${userData.users.testroleviewer1qaclient.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testroleviewer1qaclient.username} --new-password ${userData.users.testroleviewer1qaclient.password}"
            cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat create users -r Phaidon-realm -s username=\"${userData.users.testroleviewer2qaclient.username}\" -s firstName=\"${userData.users.testroleviewer2qaclient.firstName}\" -s lastName=\"${userData.users.testroleviewer2qaclient.lastName}\" -s email=${userData.users.testroleviewer2qaclient.email} -s enabled=true"
			cmdOutput = bat returnStdout: true, script: "E:/tools/keycloak-20.0.1/bin/kcadm.bat set-password -r Phaidon-realm --username ${userData.users.testroleviewer2qaclient.username} --new-password ${userData.users.testroleviewer2qaclient.password}"
		}
	}
}
