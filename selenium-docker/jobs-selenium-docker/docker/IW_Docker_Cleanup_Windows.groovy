@Library('TestUtils') _
/**
 * This job removed all nuget caches from all Windows Server.  
 * 
 * IW_Docker_Cleanup_Windows is triggered periodically once a month.
 */

node('OS-Windows') {
    timestamps {
        def onlineNodes = nodesByLabel 'OS-Windows' 
		def allNodes = nodesByLabel label: 'OS-Windows', offline: true
		def offlineNodes = allNodes - onlineNodes
			
		onlineNodes.each { nodeName ->
			stage("${nodeName} cleanup") {
				node(nodeName) {
					echo nodeName
					bat 'nuget locals all -clear'
				}
			}
		}
		
		if ( ! offlineNodes.isEmpty()) {
			unstable "Cleanup on following nodes not possible, as the nodes are currently offline: ${offlineNodes}"
		}
    }
}

       