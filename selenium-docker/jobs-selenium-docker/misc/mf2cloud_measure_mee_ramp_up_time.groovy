// @param mxBuildVersion

node('OS-Linux')  {
    
    deleteDir()
     
    def jobWorkspace = pwd()
    def durations = []
    def longestTime = 0
    
    stage('Checkout project, buildJar.xml, pom.xml & runtime-cloud-dist.jar on master') {
        checkout([$class: 'SubversionSCM', additionalCredentials: [], 
            excludedCommitMessages: '',
            excludedRegions: '', excludedRevprop: '', 
            excludedUsers: '', filterChangelog: false, 
            ignoreDirPropChanges: false, includedRegions: '', 
            locations: [[credentialsId: 'User-QMSRV1-for-SVN', depthOption: 'infinity', 
                ignoreExternalsOption: true,
                remote: 'https://poseidon.innowake.hq/svn/innowake/branches/features/WMEE-8580-runtime-startup-and-footprint']], 
                workspaceUpdater: [$class: 'UpdateUpdater']])
                
        checkout([$class: 'SubversionSCM', additionalCredentials: [], 
            excludedCommitMessages: '',
            excludedRegions: '', excludedRevprop: '', 
            excludedUsers: '', filterChangelog: false, 
            ignoreDirPropChanges: false, includedRegions: '', 
            locations: [[credentialsId: 'User-QMSRV1-for-SVN', depthOption: 'infinity', 
                ignoreExternalsOption: true,
                remote: 'https://poseidon.innowake.hq/svn/innowake-qm/qa/projects/maxenso/branches/99.9.9/mf2cloud/mf2cloud_measure_mee_ramp_up_time']], 
                workspaceUpdater: [$class: 'UpdateUpdater']])
     }
    
    stage('Build jar file on master') {
    
        sh "mv mf2cloud_measure_mee_ramp_up_time/buildJar.xml ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint" 
        //delete old pom
        sh "rm ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint/pom.xml"
        //delete original jar file
        sh "rm ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint/lib/innowake-maxenso-runtime-cloud-dist-99.9.99-TRUNK-SNAPSHOT.jar"
        //move addapted pom to project
        sh "mv ${jobWorkspace}/mf2cloud_measure_mee_ramp_up_time/pom.xml ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint" 
        //reaplace classpath entry
        sh "sed -i \"s|<versionToReplace>|${mxBuildVersion}|g\" ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint/.classpath"       
 
        withMaven(maven: 'Default') {
            dir('WMEE-8580-runtime-startup-and-footprint')   {
                sh "mvn dependency:resolve -DmxBuildVersion=${mxBuildVersion}"
                sh "ant -DmxBuildVersion=${mxBuildVersion} -debug -f buildJar.xml"
            }
        }

        sh "mv ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint/ramp-up-times.jar ${jobWorkspace}"
    }
    
    stage('run startup on master')  {
            
        for(int i = 0 ; i < 3; i++) {
            sh 'java -jar ramp-up-times.jar > tmpResult.out'
            def outPut = readFile 'tmpResult.out'
            outPut.trim()
            outPut = outPut.substring(19,22)
                
            outPut = outPut as Integer
                
            durations.add(outPut)
            
            if(i == 0)    {
                longestTime = outPut
            }else if(longestTime < outPut)    {
                longestTime = outPut
            }
        }
            
        print 'measured times on master: '+durations
        longestTime = longestTime as String
        writeFile encoding: 'UTF8', file: 'masterTimes.txt', text: longestTime
    }
    stage("measured startup times on master: " +durations[0]+', '+durations[1]+', '+durations[2]+' ms')   {
        buildName "#${env.BUILD_ID} longest startup time: ${longestTime} ms"
    }
    
    stage('provide built jar file')  {
        archiveArtifacts 'ramp-up-times.jar'
        stash includes: 'ramp-up-times.jar', name: 'jar'
        stash includes: 'masterTimes.txt', name:'masterTimes.txt'
    }
}

node('morpheus')  {
    
    deleteDir()
     
    def jobWorkspace = pwd()
    def durations = []
    def longestTime = 0
    
    stage('run startup on morpheus')  {
        
        unstash 'jar'
                
        for(int i = 0 ; i < 3; i++) {
            sh 'java -jar ramp-up-times.jar > tmpResult.out'
            def outPut = readFile 'tmpResult.out'
            outPut.trim()
            outPut = outPut.substring(19,22)
                
            outPut = outPut as Integer
                
            durations.add(outPut)
            
            if(i == 0)    {
                longestTime = outPut
            }else if(longestTime < outPut)    {
                longestTime = outPut
            }
        }
    }
    
    stage("measured startup times on morpheus: " +durations[0]+', '+durations[1]+', '+durations[2]+' ms')   {
        unstash 'masterTimes.txt'
        def timeMaster = readFile 'masterTimes.txt'
        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "longest startup time morpheus: ${longestTime} ms, master: ${timeMaster} ms"
    }
}