/*
* Generates the manual artifacts like PDFs or eclipse help bundles. The method executes the php script inside a docker container. 
* The generated artifacts are copied to the artifacts folder that is mounted from the wordpress container. The manual PDFs are
* immediately available in WordPress.
*
* @param version            list of all versions
* @param manuals            add here the number of manuals category from constants.php. If it is empty, the artifacts are created for all manuals. It is possible to add more numbers than one. 
*                           Just separate the numbers with a comma (for example: 26, 5, 45). 
*                           If isChangelog checked, you can add here the changelog id. It is only possible to generate one changelog PDF.
* @param isChangelog        if true only the changelog PDFs are created, otherwise only manual PDFs
* @param isEclipseArtifact  if true only the eclipse help bundles are created, otherwise only manual PDFs
*/
@Library('TestUtils') _

node('Documentation') {
    def jobWorkspace = pwd()
    def dockerUtils = new DockerUtils()
    def gitUtils = new GitUtils()

    buildName "#${env.BUILD_ID}"
    buildDescription "version=${version} isChangelog=${isChangelog} manuals=${manuals} isEclipseArtifact=${isEclipseArtifact}"

    if(Boolean.parseBoolean(isChangelog) && version == 'All') {
        error "It is not possible to use version = 'All' if 'isChangelog' is checked!"
    }

    if(Boolean.parseBoolean(isChangelog) && manuals.isEmpty()) {
        error "Please add the changslog ID to the field manuals!"
    }

     if(Boolean.parseBoolean(isChangelog) && Boolean.parseBoolean(isEclipseArtifact)) {
        error "It is not possible to generate Changelogs and Eclipse artifacts at the same time."
    }

    def allVersions = [
        'trunk', 
        'innowake-23',
        'innowake-23-ir', 
        'innowake-22', 
        'innowake-22-ir', 
        'innowake-22-ir2', 
        'innowake-22-ir3',
        'innowake-22-ir4', 
        'innowake-21', 
        'innowake-21-ir', 
        'innowake-21-ir2', 
        'innowake-21-ir3', 
        'innowake-21-ir4', 
        'innowake-21-ir5',
        'innowake-21-ir5',
        'innowake-21-ir6',
        'innowake-21-ir7',
        'innowake-19',
        'innowake-19-ir',
        'innowake-19-ir2',
        'innowake-19-ir3',
        'innowake-19-ir4', 
        'innowake-19-ir5'
    ]

    
    allVersions = (version == 'All') ? allVersions : [version]
    
    stage('clean up') {
        deleteDir()
    }

    docker.image(dockerUtils.pullJenkinsEnvironmentImage('java11')).inside {
        stage('checkout') {
            gitUtils.checkoutGitProject("${jobWorkspace}/wp-phpscripts", "${gitUtils.getGitUrlQef()}/innowake-documentation/wp-phpscripts.git", 'master')
        }
    }

    stage('generate artifacts') {
        def manualList = (manuals.isEmpty()) ? getManualsList("${jobWorkspace}/wp-phpscripts/phpscripts/constants.php") : manuals.tokenize(',')

        if(Boolean.parseBoolean(isChangelog)) {
            dir('wp-phpscripts') {
                echo "Generate Changelog ${version} and ${manuals}"
                waitUntilScriptFinished('docker build . -t generate-pdfs')
                waitUntilScriptFinished("docker run --rm --name generatepdfs -v ${jobWorkspace}/artifacts:/usr/src/phpscripts/artifacts generate-pdfs php /usr/src/phpscripts/generatePDF/scripts/generate_changelogs.php ${version} ${manuals}")
            }
        } else {
            dir('wp-phpscripts') {
                waitUntilScriptFinished('docker build . -t generate-pdfs')
                if (Boolean.parseBoolean(isEclipseArtifact)) {
                    generateArtifacts("${jobWorkspace}/artifacts:/usr/src/phpscripts/artifacts", "/usr/src/phpscripts/eclipse/generate_eclipse_artifacts.php", allVersions, manualList)
                } else {
                    generateArtifacts("${jobWorkspace}/artifacts:/usr/src/phpscripts/artifacts", "/usr/src/phpscripts/generatePDF/scripts/generate_manuals.php", allVersions, manualList)
                }
            }
        }
    }

    stage('copy artifacts to appmod') {
        waitUntilScriptFinished('cp -RT -u artifacts /data/appmod/wordpress/artifacts')
        if( !Boolean.parseBoolean(isEclipseArtifact)) {
            waitUntilScriptFinished("docker cp ${jobWorkspace}/wp-phpscripts/phpscripts/constants.php wordpress-wordpress-1:/var/www/html/wp-content/phpscripts/")
        }
    }

    stage('update artificats repository') {
        if(Boolean.parseBoolean(isChangelog)) {
            echo "The manual is a changelog. Skipping IW_Documentation_Update_Artifacts_Repo!"
        } else {
            build job: 'IW_Documentation_Update_Artifacts_Repo', wait: false
        }
        
    }
}

/**
* waits until the script finished  
*/
def waitUntilScriptFinished(scriptCommand) {
    timeout(60) {
        waitUntil {
            script {
                def res = sh script: scriptCommand, returnStatus: true
                return (res == 0)
            }
        }
    }
}

/**
* reads the php file to extract the manuals category number and returns them as list
*/
def List getManualsList(phpFile) {
    def list = []
    def fileData = readFile(file: phpFile)
    def manualsData = fileData.split('\\*/')[2]
    manualsData.split('\n').each { line ->
        if(line.contains("category")) {
            list.add((line =~ /[\d]+/)[0])
        }
    }
    return list
}

/**
* creates and starts docker container and executes php command 
*/
def generateArtifacts(artifactsVolume, script, versionList, manualList){
    versionList.each {
        version -> 
        manualList.each {
            manual->
            echo "Generate PDFs for ${version} and ${manual.trim()}"
            waitUntilScriptFinished("docker run --rm --name generatepdfs -v ${artifactsVolume} generate-pdfs php ${script} ${version} ${manual.trim()}")
        }
    }
}