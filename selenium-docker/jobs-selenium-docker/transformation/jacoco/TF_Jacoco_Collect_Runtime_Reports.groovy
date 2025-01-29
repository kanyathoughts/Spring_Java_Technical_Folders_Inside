@Library('TestUtils') _

/**
 * Collects the Jacoco report files (.exec) from every runtime job which can be executed with code coverage. 
 * 
 * @param mxBuildVersion The build. To be selected from the available builds.
 *		  				 type: Extensible Choice -> File Choice Parameter
 */ 

nodeTF('Docker-host') {

	timestamps {
    
		def svnUtils = new SvnUtils()
		def dockerUtils = new DockerUtils()
		def workDir
		def antProperties
		def productionAntFile
		def mxJarsDir = "/data/mxJars/${mxBuildVersion}"
				
    	buildName "#${BUILD_NUMBER} - ${mxBuildVersion}"
    	
    	try {
			deleteDir()
    		docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside("-v ${mxJarsDir}:${mxJarsDir}:ro") {
    			workDir = pwd()
    			stage('initialisation') {
    				withCredentials([usernamePassword(credentialsId: 'User-QMSRV1-for-SVN', passwordVariable: 'svnPw', usernameVariable: 'svnUser')]) {
    					svnUtils.svnExportFiles("${svnUtils.getSvnUrlQaTools()}/CollectReports", workDir, svnUser, svnPw)
    				}
    				productionAntFile = "${workDir}/buildRuntimeReport.xml"
    				antProperties = "-DworkDir=${workDir} -DwithCodeCoverage='true' -DmxBuildVersion=${mxBuildVersion} -DmxJarsDir=${mxJarsDir}"
    			}
    
    			stage('Copy artifacts') {
    				//array with the names of the jobs 
    				def jobs = ['TF_Natural_Java_Test_KIDICAP_Mini_AGTV', 'TF_Natural_Java_Test_KIDICAP_Mini_OFDAN', 'TF_Natural_Java_Test_KIDICAP_Mini_OFDVE', 
    				            'TF_Natural_Java_Test_KIDICAP_165_AGTV', 'TF_Natural_Java_Test_KIDICAP_165_OFDAN', 'KIDICAP_ACI_Bufferplayer_OFDAN', 
    				            'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part0103', 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part0409', 'TF_Natural_Java_Test_KIDICAP_165_AGTV2Part1020', 
    				            'TF_Natural_Java_Test_KIDICAP_184_AGTV', 'TF_Natural_Java_Test_Mannheimer']
                
    					jobs.each {
    					if ("${it}" == "TF_Natural_Java_Test_KIDICAP_165_AGTV" || "${it}" == "TF_Natural_Java_Test_KIDICAP_165_OFDAN") {
							copyArtifacts([
								projectName: "${it}",
								filter: '**/'+"${it}"+'.exec',
								selector: lastSuccessful(),
								parameters: "withCodeCoverage=true,mxBuildVersion=${mxBuildVersion},reduceDbVolume=false",
								target: workDir,
								fingerprintArtifacts: true,
								flatten: true
							])
    					} else {
							copyArtifacts([
								projectName: "${it}",
								filter: '**/'+"${it}"+'.exec',
								selector: lastSuccessful(),
								parameters: "withCodeCoverage=true,mxBuildVersion=${mxBuildVersion}",
								target: workDir,
								fingerprintArtifacts: true,
								flatten: true
							])
    					}
    				}
    			}
    
    			stage('Test coverage reports') {
    				withAnt(installation: 'Default') {
    					sh "ant -buildfile ${productionAntFile} ${antProperties} overallReport"
    					sh "ant -buildfile ${productionAntFile} ${antProperties} naturalReport"
    					sh "ant -buildfile ${productionAntFile} ${antProperties} cobolReport"
    					sh "ant -buildfile ${productionAntFile} ${antProperties} jclReport"
    					sh "ant -buildfile ${productionAntFile} ${antProperties} generalReport"
    					sh "ant -buildfile ${productionAntFile} ${antProperties} vaadinReport"
    				}
    			}
    		}
    	} finally {
    		stage('Archive artifacts') {
    			archiveArtifacts 'coverage/**/*'
    		}
    	}
    }
}