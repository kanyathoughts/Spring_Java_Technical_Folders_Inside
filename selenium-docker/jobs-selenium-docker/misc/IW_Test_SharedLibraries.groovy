@Library('TestUtils') _

/**
* Trigger job for starting Util methods tests based on git branch name in pull request.
* This job is triggered by GitLab webhook from shared-libraries repo (https://gitlab.consulting.sltc.com/appmod/qef/infrastructure/shared-libraries/-/hooks)
* 
* @param branchName Name of the branch where changes are stored, default: master
* @param testAll Boolean flag indicating if all util tests should be executed
*/
timestamps {
    def gitUtils = new GitUtils()
    def dockerUtils = new DockerUtils()
    def miscUtils = new MiscUtils()

    node('OS-Linux') {
        def utils = ['ChangeLogUtils', 'DockerUtils', 'GitUtils', 'MiscUtils', 'MxVersionUtils', 'PerformanceUtils', 'ProductDeliveryUtils', 'ResultComparisonUtils', 'SharepointUtils', 'SvnUtils']
        //def testJobs = ['IW_Test_ChangeLogsUtils','IW_Test_DockerUtils', 'IW_Test_GitUtils','IW_Test_MiscUtils', 'IW_Test_MxVersionUtils','Test_MxVersionUtils_getFullBuild', 'IW_Test_PerformanceUtils', 'Test_PerformanceUtilsAverage','IW_Test_ProductDeliveryUtils', 'IW_Test_ResultComparisonUtils', 'IW_Test_Sharepoint_API', 'IW_Test_SVNUtils']
        def testJobs = ['IW_Test_ChangeLogsUtils','IW_Test_DockerUtils', 'IW_Test_GitUtils','IW_Test_MiscUtils', 'IW_Test_MxVersionUtils', 'IW_Test_PerformanceUtils','IW_Test_ProductDeliveryUtils', 'IW_Test_ResultComparisonUtils', 'IW_Test_Sharepoint_API', 'IW_Test_SVNUtils']
        def foundUtils = null
        def jobStarter = currentBuild.getBuildCauses()[0].userId == null ? 'GitLab Trigger' : currentBuild.getBuildCauses()[0].userId
        branchName = jobStarter == 'GitLab Trigger' ? getBranchName(currentBuild.getBuildCauses()[0].shortDescription) : branchName //branch where new changes are stored
        def workDir = pwd()
        testAll = (testAll == null ? false : Boolean.parseBoolean(testAll))

        buildDescription "jobStarter=${jobStarter} branch=${branchName}\ntestAll=${testAll}" 

        deleteDir()
        docker.image(dockerUtils.pullJenkinsEnvironmentImage(miscUtils.getDefaultJavaVersion())).inside {
            stage('Initialize'){
                if (jobStarter.equals('GitLab Trigger')) {
                    echo "Job started by Trigger\nReason: ${currentBuild.getBuildCauses()[0].shortDescription}"
                } else {
                    echo "Job started by User\nUsername: ${jobStarter}\nBranch is ${branchName}"
                }
                //Find util names in git diff
                foundUtils = []
                if (testAll) {
                    foundUtils = utils
                } else {
                    withCredentials([string(credentialsId: 'USAppModQMUserSVC-Access-Token', variable: 'gitlabToken')]) {
                        sh "git clone --branch master ${gitUtils.getGitUrlQef('USAppModQMUserSVC-Access-Token', gitlabToken)}/infrastructure/shared-libraries.git ."
                        try {
                            diffDump = sh(returnStdout: true, script: "git diff master origin/${branchName}")
                        }catch(ex) {
                            error "Could not determine branch diff between master and ${branchName}. Is ${branchName} present in GitLab?"
                        }

                        //Get total diff between master and current branch. In this diff, search for Util names. These names are the Utils which we want to test 
                        utils.collect { it + '.groovy' }.each{ 
                            if (diffDump.contains(it)) {
                                foundUtils.add(it)
                            }
                        } 
                    }
                    echo "Utils found by git diff ${foundUtils}"
                    if (foundUtils.isEmpty()) {
                        unstable "Could not determine affected Util classes in branch ${branchName}. Assuming to test all Util classes."
                        foundUtils = utils.collect{ it + '.groovy' }
                        testAll = true
                    }
                    //Find dependent utils. This also assures correct job execution order (e.g., if util A depends on util B, then the order should be B,A)
                    foundUtils = foundUtils.collect{ compilationOrderForFile(it) }.flatten().unique()
                    echo "Final Found utils ${foundUtils}"
                }
            }

            def jobsToExecute = foundUtils.collect{u -> getJobAndParameterForUtil(u, branchName)[0]}.flatten().unique() //assures that every util test job is executed once
            utils.each { util ->
                stage(util) {
                    when(foundUtils.contains(util), "This stage is not part of the util(s) under test, skipping..."){
                        (testJobNames, testJobParams) = getJobAndParameterForUtil(util, branchName)
                        if ( !testJobNames.isEmpty() && !testJobParams.isEmpty()) {
                            for(int i = 0; i < testJobNames.size();i++) {
                                def testJobName = testJobNames[i]
                                def jobOut = build job: testJobName, propagate: false, parameters: testJobParams[i]
                                miscUtils.evaluateBuildResult(jobOut)
                            }
                        } else {
                            unstable "Could not determine test job name or test job params for ${util2test}"
                        }        
                    }
                }
            }
        }    
    }
}

/**
* If job triggerd by Gitlab the description could look like e.g. 'Triggered by GitLab Merge Request #92: infrastructure/WQATF-1358 => master'
* @param description From the description parameter, this method exstracts the branch name from which the Pull Request was started (e.g. WQATF-1358)
    Type: String
* @return branch  If no branch name was found, it will always return 'master' otherwise the branch name from the pull request
    Type: String
*/
def getBranchName(description) {
    def branchName = 'master'
    catchError(buildResult: 'UNSTABLE', message: 'Unable to find branch name in description, assuming master') {
        //[0] = list of matches, [1] = first match on group (.*) 
        branchName = (description =~ /^Triggered by GitLab Merge Request .*: .*\/(.*) => .*$/)[0][1]
    }
    return branchName
}

/**
* From a Utils name, get the dependent util names, ordered according to their compilation order (e.g. MiscUtils depends on MxVersionUtils, so the compilation order is MxVersionUtils and then MiscUtils)
* @param name Name of the Utils class (ending with .groovy) which we want the compilation order for
    Type: String
* @return List of Util class names + the name of the searched util (without .groovy) ordered according to their dependencies to each other
    Type: list [String]
*/
def compilationOrderForFile(name) {
    name = name.minus('.groovy')
    switch(name) {
        case 'ChangeLogUtils':
            return ['ChangeLogUtils']
        case 'DockerUtils':
            return compilationOrderForFile('MxVersionUtils') + ['DockerUtils']
        case 'GitUtils':
            return ['GitUtils']
        case 'MiscUtils':
            return compilationOrderForFile('MxVersionUtils') + ['MiscUtils']           
        case 'MxVersionUtils':
            return ['MxVersionUtils']
        case 'PerformanceUtils':
            return compilationOrderForFile('MiscUtils') + ['PerformanceUtils']
        case 'ProductDeliveryUtils':
            return ['ProductDeliveryUtils']             
        case 'ResultComparisonUtils':
            return ['ResultComparisonUtils']
        case 'SharepointUtils':
            return ['SharepointUtils']   
        case 'SvnUtils':
            return ['SvnUtils']
        default:
            unstable "${name} compilation order is unknown! Assuming no order"
            return []                              
    }
}

/**
* Receives a util name and branch where changes to this util are stored and returns list of job names and corresponding job parameters.
* @param name utilName of the Utils class (ending with .groovy) which we want to test
    Type: String
* @param branchName Branch name where altered Util is stored
    Type: String
* @return List containing lists where Job names and their parameters are stored.
    Type: list [[String], [Parameters]]
*/
def getJobAndParameterForUtil(utilName, branchName) {
            def testJobNames = []
            def testJobParams = []
            switch(utilName.minus('.groovy')) {
                case 'ChangeLogUtils':
                    testJobNames = ['IW_Test_ChangeLogsUtils']
                    testJobParams = [[listGitBranches(name: 'branchName', value: branchName)]]
                    break
                case 'DockerUtils':
                    testJobNames = ['IW_Test_DockerUtils']
                    testJobParams = [[listGitBranches(name: 'branchName', value: branchName)]]
                    break
                case 'GitUtils':
                    testJobNames = ['IW_Test_GitUtils']
                    testJobParams = [[listGitBranches(name: 'branchName', value: branchName)]]
                    break
                case 'MiscUtils':
                    testJobNames = ['IW_Test_MiscUtils']
                    testJobParams = [[listGitBranches(name: 'branchName', value: branchName)]]
                    break         
                case 'MxVersionUtils':
                    //testJobNames = ['IW_Test_MxVersionUtils','Test_MxVersionUtils_getFullBuild']
                    //testJobParams = [[listGitBranches(name: 'branchName', value: branchName)], [listGitBranches(name: 'branch', value: branchName)]]
                    testJobNames = ['IW_Test_MxVersionUtils']
                    testJobParams = [[listGitBranches(name: 'branchName', value: branchName)]]
                    break
                case 'PerformanceUtils':
                    //testJobNames = ['IW_Test_PerformanceUtils', 'Test_PerformanceUtilsAverage']
                    //testJobParams = [[listGitBranches(name: 'branchName', value: branchName)],[listGitBranches(name: 'branch', value: branchName)]]
                    testJobNames = ['IW_Test_PerformanceUtils']
                    testJobParams = [[listGitBranches(name: 'branchName', value: branchName)]]
                    break
                case 'ProductDeliveryUtils':
                    testJobNames = ['IW_Test_ProductDeliveryUtils']
                    testJobParams = [[listGitBranches(name: 'branchName', value: branchName)]]
                    break             
                case 'ResultComparisonUtils':
                    testJobNames = ['IW_Test_ResultComparisonUtils']
                    //more params!
                    testJobParams = [[listGitBranches(name: 'branchName', value: branchName)]]
                    break
                case 'SharepointUtils':
                    testJobNames = ['IW_Test_Sharepoint_API']
                    testJobParams = [[listGitBranches(name: 'branchName', value: branchName)]]
                    break   
                case 'SvnUtils':
                    testJobNames = ['IW_Test_SVNUtils']
                    testJobParams = [[listGitBranches(name: 'branchName', value: branchName)]]
                    break
                default:
                    unstable "For ${utilName} no test job is known!"
                    break                 
            }
            return [testJobNames, testJobParams]
}
