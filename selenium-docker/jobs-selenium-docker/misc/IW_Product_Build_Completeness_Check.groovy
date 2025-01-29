@Library('TestUtils') _

/**
 * Compare the two PD builds and notify if new jars and folders appear or artifacts are missing.
 *
 * @param mxBuildVersion The 1st mxVersion selected from the untested or approved builds to compare.
 * @param mxBuildVersionToCompare The 2nd mxVersion selected from the untested or approved builds to compare.
 * @param usePreviousVersionToCompare By default it will take the selected mxBuildversionToCompare to compare.  
 * 		If usePreviousVersionToCompare is set to true it will choose the previous mxBuildVersion of the selected mxBuildVersion
 * 
 */
 
node('OS-Linux && Docker-host') {
    def miscUtils = new MiscUtils()
	 
	if (Boolean.parseBoolean(usePreviousVersionToCompare)) {			 
	    mxBuildVersionToCompare = getPreviousVersion(mxBuildVersion)
	}
		
	buildDescription "mxBuildVersion=${mxBuildVersion} mxBuildVersionToCompare=${mxBuildVersionToCompare}"
	
	stage('completeness check') {		    	    
		deleteDir()
		getVersionContent(mxBuildVersion)
		getVersionContent(mxBuildVersionToCompare)
		
		def mxVersion = mxBuildVersion.tokenize('/');   
        def mxVersionToCompare = mxBuildVersionToCompare.tokenize('/'); 					      
		        	            																									
		testOutput = sh (script: "comm -3 ${mxVersion[1]} ${mxVersionToCompare[1]}", returnStdout:true).trim()
		if (testOutput.size() != 0) {	
		    unstable "These are the different files: ${testOutput}"
		    def subj = "build ${currentBuild.number} of Completeness check for build artifacts"
		    def body = "${currentBuild.absoluteUrl} is unstable, the differences in ${mxBuildVersion} and ${mxBuildVersionToCompare} are : \n ${testOutput}"
		    def to = miscUtils.getMailReceivers(currentBuild.projectName)
		    emailext subject: subj, body: body, to: to			
		}
    }
}
 
/**
 * Selects the previous version of selected mxBuildVersion. 
 * @return mxBuildVersionToCompare.
 */
def getPreviousVersion(mxBuildVersion) {
    def mxBuild = ('find /media/untested /media/approved -mindepth 1 -maxdepth 1'.execute().text.split('\n') as List).findAll {
                  	(it ==~ /.+\d{1,2}\.\d{1,2}\.\d{1,2}\..*/ ) || (it ==~ /.+\d{1,2}\.\d{1,2}\..*/ )
                  }.collect {
                  	it.replace('/media/', '')
                  }
    return mxBuild.get(mxBuild.indexOf(mxBuildVersion) - 1)
}

/**
 * Store the contents of the selected mxBuildVersion in a file.   
 * @return Display the contents of the file.
 */
def getVersionContent(mxBuildVersion) {
    def mxBuildSplit = mxBuildVersion.tokenize('/');
    sh "ls -R '/media/${mxBuildVersion}' > ${mxBuildSplit[1]}"
    sh "grep -v '${mxBuildVersion}' ${mxBuildSplit[1]} > tmp && mv tmp ${mxBuildSplit[1]}"
    sh "grep . ${mxBuildSplit[1]} > tmp && mv tmp ${mxBuildSplit[1]}"
    sh "sed -e s/-'${mxBuildSplit[1]}'// ${mxBuildSplit[1]} > tmp && mv tmp ${mxBuildSplit[1]}"
    sh "sed -e s/_'${mxBuildSplit[1]}'// ${mxBuildSplit[1]} > tmp && mv tmp ${mxBuildSplit[1]}"
    sh "sort ${mxBuildSplit[1]} > tmp && mv tmp ${mxBuildSplit[1]}" 
    sh "cat ${mxBuildSplit[1]}"      
}