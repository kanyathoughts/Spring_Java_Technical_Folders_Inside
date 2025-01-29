@Library('TestUtils') _

import java.text.SimpleDateFormat

/**
 * This jobs analyzes all builds of a given job if they are older than a given number of days. If that's the case these builds can optionally also be deleted with the given parameters.
 *
 * @param jobName The job whose builds are to be analyzed
 *        type: String
 * @param deleteAlphaBuilds If checked the job will delete alpha builds that are older than 14 days. Leave it unchecked if you just want to analyze the builds without deleting them.
 *        type: boolean
 * @param deleteBetaBuilds If checked the job will delete beta builds that are older than 14 days. Leave it unchecked if you just want to analyze the builds without deleting them.
 *        type: boolean
 * @param numberOfDays This specifies the number of days in the past the builds remain. 14 means that all builds from the last 14 days will not be deleted
 *        type: String
 */
 
node('built-in') {
	
	buildName "#${env.BUILD_ID} - ${jobName}"
	buildDescription "deleteAlphaBuilds=${deleteAlphaBuilds} deleteBetaBuilds=${deleteBetaBuilds} numberOfDays=${numberOfDays}"
    
    stage('run') {
        def buildsFolder = "/var/jenkins_builds/jobs/${jobName}/builds"
        def folders = sh returnStdout: true, script: "ls ${buildsFolder}"
        def problematicBuilds = [:]
        (folders.split() as List).findAll {
            buildId ->
            if ( ! (buildId.startsWith('legacyIds') || buildId.startsWith('permalinks'))) {
                try {
                    def mxBuildVersion = getVersionNumberFromBuild("${buildsFolder}/${buildId}")
                    if ((mxBuildVersion.contains('alpha') || mxBuildVersion.contains('beta')) && isOlderThanXDays(mxBuildVersion)) {
                        echo "Build #${buildId} with mxBuildVersion=${mxBuildVersion} is older than ${numberOfDays} days and can be deleted"
                        if ((Boolean.parseBoolean(deleteAlphaBuilds) && mxBuildVersion.contains('alpha'))
                            || (Boolean.parseBoolean(deleteBetaBuilds) && mxBuildVersion.contains('beta'))) {
                            echo "Deleting build #${buildId} with mxBuildVersion=${mxBuildVersion}"
                            sh "rm -rf ${buildsFolder}/${buildId}"
                        }
                    } else {
                        echo "Build #${buildId} with mxBuildVersion=${mxBuildVersion} will remain"
                    }
                } catch (Exception e) {
                    echo e.toString()
                    echo "Problem with build #${buildId}. See above error for details"
                    problematicBuilds.put(buildId, "${env.JENKINS_URL}job/${jobName}/${buildId}/")
                }
            }
        }
        if ( ! problematicBuilds.isEmpty()) {
            unstable 'There are problematic builds:'
		    echo problematicBuilds.toString()
        }
        
    }
}

/**
 * Extracts the version number from the build.xml that can be found in each build folder
 * @param pathToBuild The path to the build 
 * @return The mxBuildVersion in known format 
 */
def getVersionNumberFromBuild(def pathToBuild) {
    def buildXmlString = readFile "${pathToBuild}/build.xml"
	def match = buildXmlString =~ /<name>mxBuildVersion<\/name>\s*(?:<description>.*<\/description>\s*)?<value>(.*)<\/value>/
	if ( ! match.find()) {
		error "Unable to extract build version from ${pathToBuild}/build.xml"
	}
    def mxBuildVersion = match.group(1)
    if (mxBuildVersion ==~ /(\d+\.\d+(\.\d+)*\.\d*.-(alpha|beta)-\d+(-\d+)?)|(\d+\.\d+(\.\d+)*\.\d+)/) {
        return mxBuildVersion
    } else {
        error "There is something wrong with the build version: ${mxBuildVersion} from ${pathToBuild}/build.xml"
    }
}

/**
 * Checks if a mxBuildVersion contains a timestamp older than 14 days
 * 
 * @return true if the mxBuildVersion contains a timestamp older than 14 days, false if not
 */
boolean isOlderThanXDays(def mxBuildVersion) {
    def mxBuildVersionDate = mxBuildVersion.tokenize('-')[2]
    def dateString = new SimpleDateFormat("yyyyMMddHHmm").format(new Date() - Integer.parseInt(numberOfDays.trim()))
    return (mxBuildVersionDate as Long) < (dateString as Long)
}
