
package innowake.mining.server.discovery.source.iris.WDIS537;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.logging.Logging;

/**
 * Sample Class to check lines of code and lines of comments in Java File
 */
public class LinesOfCodeLinesOfComments {
	
	public final static testMethod() {
		/* info to check the log information in a file*/
		//Test Inline Comment
		LOG.info("This is a test Method");
		int var = 2 ; /*edge test cases 
		to check with line starting with code and then comment
		end of comment */ System.out.println(var); 
	}
}