package innowake.mining.extensions.export.table;

import java.io.Closeable;
import java.io.IOException;
import java.util.List;

public interface TableLineBuilder extends Closeable {
	
	/**
	 * Builds a standard table line.
	 * F.e. in Confluence Markup it would look like this:
	 * "|MMRS370|COBOL|300|"
	 * 
	 * @param elements elements that will be inserted in the line cells
	 * @throws IOException if writing goes wrong
	 */
	public void buildStandardLine(final List<String> elements) throws IOException;
	
	/**
	 * Builds a header table line.
	 * 
	 * @param elements elements that will be inserted in the line cells
	 * @throws IOException if writing goes wrong
	 */
	public void buildHeaderLine(final List<String> elements) throws IOException;
		
}
