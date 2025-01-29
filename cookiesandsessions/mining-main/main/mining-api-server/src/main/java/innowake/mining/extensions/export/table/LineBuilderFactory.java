package innowake.mining.extensions.export.table;

import java.io.OutputStream;

public interface LineBuilderFactory {
	
	/**
	 * Instantiates a new LineBuilder that is able to write lines to the specified output stream.
	 * 
	 * @param out stream the LineBuilder will write to
	 * @return newly instantiated LineBuilder
	 */
	public TableLineBuilder getTableLineBuilder(OutputStream out);
}
