@Library('TestUtils') _

/**
 * Tests the Jenkins instance, that executes this job.
 * Items to be tested:
 * - Jenkins version
 * - plugins and plugin versions
 * - file system
 * - credentials
 * - managed configuration files
 * - Access to SVN, GitLab, Jira
 */

node('built-in') {
    timestamps {
        def dockerUtils = new DockerUtils()
        
        stage('Jenkins-version') {
            if (env.JENKINS_VERSION != '2.222.4') {
                unstable "FAILURE - Wrong Jenkins version. Should be 2.222.4, but is ${env.JENKINS_VERSION}"
            }
        }
        
        stage('Mounts') {
            ['/data', '/data/mxJars', '/media/untested', '/media/approved'].each {
                def stdout = sh returnStdout: true, script: "ls -la ${it} | wc -l"
                if (Integer.parseInt(stdout.trim()) <= 0) {
                    unstable "FAILURE - ${it} does not exist or is empty"
                }
            }
        }
        
        stage('Credentials') {
            ['User-QMSRV1-for-SVN', 'USAppModQMUserSVC-User-PW',
             'gip165agtv', 'gip165agtv2', 'gip165ofdan', 'gip184agtv',
             'LMP_MySQL_User_Dev', 'LMP_MySQL_User_Prod', 'LMP_MySQL_User_Stage',
             'Node-Logon-qmsrv1', 'Node-Logon-morpheus', 'Node-Logon-LinuxJenkins'
            ].each {
                credId ->
                try {
                    withCredentials([usernamePassword(credentialsId: credId, passwordVariable: 'pw', usernameVariable: 'user')]) {
                        echo "User/password credential ${credId} found"
                    }
                } catch (ex) {
                    unstable "FAILURE - credential retrieval for ${credId} failed, exception ${ex}"
                }
            }
            ['USAppModQMUserSVC-Access-Token'].each {
                credId ->
                try {
                    withCredentials([string(credentialsId: credId, variable: 'secretText')]) {
                        echo "Secret text credential ${credId} found"
                    }
                } catch (ex) {
                    unstable "FAILURE - credential retrieval for ${credId} failed, exception ${ex}"
                }
            }
        }
        
        stage('Managed-files') {
            ['global_maven_settings', 'linux_maven_settings', 'windows_maven_settings'].each {
                managedFileId ->
                try {
                    configFileProvider([configFile(fileId: managedFileId, targetLocation: "${managedFileId}.xml", variable: 'fileVar')]) {
                        echo "fileVar = ${fileVar}"
                        sh "ls -la ${managedFileId}.xml"
                    }
                } catch (ex) {
                    unstable "FAILURE - problem with configuration file ${managedFileId}, exception ${ex}"
                }
            }
        }
        
        stage('Nodes') {
            def expectedLinuxNodes = ['USLinux1', 'USLinux2', 'USLinux3']
            def actualLinuxNodes = nodesByLabel label: 'OS-Linux', offline: true
            if ( ! actualLinuxNodes.containsAll(expectedLinuxNodes)) {
                unstable "FAILURE - Not all expected Linux nodes available. Expected: ${expectedLinuxNodes}. Actual: ${actualLinuxNodes}."
            }
            def expectedWindowsNodes = ['USWindows1', 'USWindows2', 'USWindows3', 'USWindows4']
            def actualWindowsNodes = nodesByLabel label: 'OS-Windows', offline: true
            if ( ! actualWindowsNodes.containsAll(expectedWindowsNodes)) {
                unstable "FAILURE - Windows nodes deviate. Expected: ${expectedWindowsNodes}. Actual: ${actualWindowsNodes}."
            }
			def expectedLeanftNodes = ['USLeanft1', 'USLeanft2', 'USLeanft3', 'USLeanft4']
			def actualLeanftNodes = nodesByLabel label: 'Tool-leanft', offline: true
			if ( ! actualLeanftNodes.containsAll(expectedLeanftNodes)) {
				unstable "FAILURE - Leanft nodes deviate. Expected: ${expectedLeanftNodes}. Actual: ${actualLeanftNodes}."
			}
        }
        
        stage('Plugins') {
            try {
                /* https://plugins.jenkins.io/build-user-vars-plugin/ */
                wrap([$class: 'BuildUser']) {
                    echo "BUILD_USER = ${env.BUILD_USER}"
                    echo "BUILD_USER_FIRST_NAME = ${env.BUILD_USER_FIRST_NAME}"
                    echo "BUILD_USER_LAST_NAME = ${env.BUILD_USER_LAST_NAME}"
                    echo "BUILD_USER_ID = ${env.BUILD_USER_ID}"
                    echo "BUILD_USER_EMAIL = ${env.BUILD_USER_EMAIL}"
                }
            } catch (ex) {
                unstable 'FAILURE - build-user-vars-plugin failed, maybe it is not installed.'
            }
        }
        
        stage('Jira-access') {
            try {
                def jiraResponseData = jiraJqlSearch jql: 'project = WMEE', site: 'IRIS'
            } catch (ex) {
                unstable "FAILURE - Jira access failed, exception ${ex}"
            }
        }
        
        stage('SVN-access') {
            try {
                withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'pw', usernameVariable: 'user')]) {
                    checkout([
                        $class: 'SubversionSCM',
                        locations: [[
                            credentialsId: 'User-QMSRV1-for-SVN',
                            depthOption: 'empty',
                            local: '.',
                            remote: 'http://poseidon.innowake.hq/svn/innowake-qm'
                        ]],
                        workspaceUpdater: [$class: 'CheckoutUpdater']
                    ])
                }
            } catch (ex) {
                unstable "FAILURE - SVN checkout failed, exception ${ex}"
            }
        }
            
        stage('Git-access') {
            try {
                checkout([
                    $class: 'GitSCM',
                    branches: [[name: '*/master']],
                    userRemoteConfigs: [[
                        credentialsId: 'USAppModQMUserSVC-User-PW',
                        url: 'https://gitlab.consulting.sltc.com/appmod/qef/innowake-documentation/documentation.git'
                    ]]
                ])
            } catch (ex) {
                unstable "FAILURE - git checkout failed, exception ${ex}"
            }
        }
    }
}
