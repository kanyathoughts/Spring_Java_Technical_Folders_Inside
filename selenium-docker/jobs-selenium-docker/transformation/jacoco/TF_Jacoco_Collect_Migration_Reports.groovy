@Library('TestUtils') _

/**
 * Collects the Jacoco report files (.exec) from every migration job. 
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
 					productionAntFile = "${workDir}/buildMigrationReport.xml"
    				antProperties = "-DworkDir=${workDir} -DwithCodeCoverage='true' -DmxBuildVersion=${mxBuildVersion} -DmxJarsDir=${mxJarsDir}"
    			}
    
    			stage('Copy artifacts') {
    				//array with the names of the jobs 
    				def jobs = ['TF_Natural_Java_Migration_KIDICAP_Mini', 'TF_Natural_Java_Migration_KIDICAP_165', 'TF_Natural_Java_Migration_KIDICAP_184']
                
    				jobs.each {
    					echo "${it}"
						copyArtifacts([
							projectName: "${it}",
							filter: '**/'+"${it}"+'.exec',
							selector: lastSuccessful(),
							parameters: "withCodeCoverage=true,mxBuildVersion=${mxBuildVersion},transformCopycodes=true",
							target: workDir,
							fingerprintArtifacts: true,
							flatten: true
						])
    				}
					copyArtifacts([
						projectName: 'TF_Natural_Java_Migration_Mannheimer',
						filter: '**/Mannheimer_nat2java.exec',
						selector: lastSuccessful(),
						parameters: "withCodeCoverage=true,mxBuildVersion=${mxBuildVersion}",
						target: workDir,
						fingerprintArtifacts: true,
						flatten: true
					])
    			}
    
    			stage('Test coverage report') {
    				withAnt(installation: 'Default') {
    					sh "ant -buildfile ${productionAntFile} ${antProperties} overallReport"
    					sh "ant -buildfile ${productionAntFile} ${antProperties} naturalReport"
    					sh "ant -buildfile ${productionAntFile} ${antProperties} cobolReport"
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