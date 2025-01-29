// @param mxBuildVersion

node('OS-Linux')  {
    
    deleteDir()
     
    def jobWorkspace = pwd()
    def measures = []
    def memoryPeak = 0
    
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
                remote: 'https://poseidon.innowake.hq/svn/innowake-qm/qa/projects/maxenso/branches/99.9.9/mf2cloud/mf2cloud_measure_mee_memory_footprint']], 
                workspaceUpdater: [$class: 'UpdateUpdater']])
     }
    
    stage('Build jar file on master') {
    
        sh "mv mf2cloud_measure_mee_memory_footprint/buildJar.xml ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint" 
        //delete original pom
        sh "rm ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint/pom.xml"
        //delete original Memory.java file
        sh "rm ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint/src/main/java/innowake/mee/performanceevalutation/benchmarks/Memory.java"
        //replace original class Memory.java
        sh "mv mf2cloud_measure_mee_memory_footprint/Memory.java ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint/src/main/java/innowake/mee/performanceevalutation/benchmarks"
        //delete original jar file
        sh "rm ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint/lib/innowake-maxenso-runtime-cloud-dist-99.9.99-TRUNK-SNAPSHOT.jar"
        //move addapted pom to project
        sh "mv ${jobWorkspace}/mf2cloud_measure_mee_memory_footprint/pom.xml ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint" 
        //reaplace classpath entry
        sh "sed -i \"s|<versionToReplace>|${mxBuildVersion}|g\" ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint/.classpath"       
 
        withMaven(maven: 'Default') {
            dir('WMEE-8580-runtime-startup-and-footprint')   {
                sh "mvn dependency:resolve -DmxBuildVersion=${mxBuildVersion}"
                sh "ant -DmxBuildVersion=${mxBuildVersion} -f buildJar.xml"
            }
        }

        sh "mv ${jobWorkspace}/WMEE-8580-runtime-startup-and-footprint/memory-footprint.jar ${jobWorkspace}"
    }
    
    stage('run memory on master')  {
            
        for(int i = 0 ; i < 3; i++) {
            sh 'java -jar memory-footprint.jar > tmpResult.out'
            def outPut = readFile 'tmpResult.out'
            outPut.trim()
            outPut = outPut.substring(0,9)
                
            outPut = outPut as BigDecimal
                
            //convert to megaByte    
            outPut = outPut/1000000 
            
            measures.add(outPut)
            
            if(i == 0)    {
                memoryPeak = outPut
            }else if(memoryPeak < outPut)    {
                memoryPeak = outPut
            }
        }
            
        print 'measured memory on master: '+measures
        memoryPeak = memoryPeak as String
        writeFile encoding: 'UTF8', file: 'masterMemory.txt', text: memoryPeak
    }
    stage("memory on master: " +measures[0]+', '+measures[1]+', '+measures[2]+' mb')   {
        buildName "#${env.BUILD_ID} largest memory peak: ${memoryPeak} mb"
    }
    
    stage('provide built jar file')  {
        archiveArtifacts 'memory-footprint.jar'
        stash includes: 'memory-footprint.jar', name: 'jar'
        stash includes: 'masterMemory.txt', name:'masterMemory.txt'
    }
}

node('morpheus')  {
    
    deleteDir()
     
    def jobWorkspace = pwd()
    def measures = []
    def memoryPeak = 0
    
    stage("run memory on morpheus")  {
        
        unstash 'jar'
                
        for(int i = 0 ; i < 3; i++) {
            sh 'java -jar memory-footprint.jar > tmpResult.out'
            def outPut = readFile 'tmpResult.out'
            outPut.trim()
            outPut = outPut.substring(0,9)
                
            outPut = outPut as BigDecimal
            
            //convert to megaByte
            outPut = outPut/1000000       
                
            measures.add(outPut)
            
            if(i == 0)    {
                memoryPeak = outPut
            }else if(memoryPeak < outPut)    {
                memoryPeak = outPut
            }
        }
    }
    
    stage("memory on morpheus: " +measures[0]+', '+measures[1]+', '+measures[2]+' mb')   {
        unstash 'masterMemory.txt'
        def memoryMaster = readFile 'masterMemory.txt'
        buildName "#${env.BUILD_ID} - ${mxBuildVersion}"
        buildDescription "largest memory peak morpheus: ${memoryPeak} mb, master: ${memoryMaster} mb"
    }
}