@Library('TestUtils') _

/**
 * Subtracts two Jacoco report files (.exec) from each other. 
 * 
 * @param mxBuildVersion The build. To be selected from the available builds.
 *						 Depending on the "reportOne" and "reportTwo" parameter, this parameter is used to search Jenkins job runs with the same parameter settings.
 *		  type: Extensible Choice -> File Choice Parameter
 * @param reportOne Choose the Jenkins job from which the Jacoco report will be taken.
 * @param reportTwo Choose the Jenkins job from which the Jacoco report will be taken.
 */ 

nodeTF('Docker-host') {
	
	def dockerUtils = new DockerUtils()
	def workDir
    def mxJarsDir = "/data/mxJars/${mxBuildVersion}"
    def sourcesJar = ("${reportOne}".contains('_Natural_Java_Migration_')) ? 'mee-source-migration-dist.jar' : 'innowake-maxenso-runtime-dist.jar'
	
    buildName "#${BUILD_NUMBER} - ${mxBuildVersion}"
    buildDescription "reportOne=${reportOne} reportTwo=${reportTwo}"
    
    try {
		deleteDir()
    	docker.image(dockerUtils.pullJenkinsEnvironmentImage('java8')).inside("-v ${mxJarsDir}:${mxJarsDir}:ro") {
    		workDir = pwd()
    		stage('Copy artifacts') {
				copyArtifacts([
					projectName: reportOne,
					filter: "**/${reportOne}.exec",
					selector: lastSuccessful(),
					parameters: "withCodeCoverage=true,mxBuildVersion=${mxBuildVersion}",
					target: "${workDir}/reportsToSubtract",
					fingerprintArtifacts: true,
					flatten: true
				])
				copyArtifacts([
					projectName: reportTwo,
					filter: "**/${reportTwo}.exec",
					selector: lastSuccessful(),
					parameters: "withCodeCoverage=true,mxBuildVersion=${mxBuildVersion}",
					target: "${workDir}/reportsToSubtract",
					fingerprintArtifacts: true,
					flatten: true
				])
				copyArtifacts([
					projectName: 'TF_Jacoco_Build_Subtract_Reports',
					filter: '**/*.jar',
					selector: lastSuccessful(),
					target: workDir,
					fingerprintArtifacts: true,
					flatten: true
				])
    			
    			sh "mkdir -p ${workspace}/classes/"
    			sh "cp ${mxJarsDir}/${sourcesJar} ${workspace}/classes/" 
    		}
    		
    		stage('Subtract reports') {
    			sh "java -jar SubtractReports.jar ${sourcesJar} ${reportOne}.exec ${reportTwo}.exec"
    			sh "mv report ${reportOne}_without_${reportTwo}"
    			sh "java -jar SubtractReports.jar ${sourcesJar} ${reportTwo}.exec ${reportOne}.exec"
    			sh "mv report ${reportTwo}_without_${reportOne}"
    		}
    	}
    } finally {
    	stage('Archive artifacts') {
    		archiveArtifacts "${reportOne}_without_${reportTwo}/**/*, ${reportTwo}_without_${reportOne}/**/*"
    	}
    }
}