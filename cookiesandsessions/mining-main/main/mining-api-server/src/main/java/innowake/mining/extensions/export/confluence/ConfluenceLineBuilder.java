package innowake.mining.extensions.export.confluence;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.List;

import innowake.mining.extensions.export.table.TableLineBuilder;

/**
 * Class that writes Confluence Table Markup lines using a Writer instance
 * 
 * @author jsimianer
 *
 */
public class ConfluenceLineBuilder implements TableLineBuilder {
	private static final String TABLE_DELIMITER = "|";
	private static final String TABLE_HEADER_DELIMITER = "||";
	private static final String NOT_AVAILABLE_TEXT = "N/A";
	
	private final OutputStreamWriter outputStreamWriter;
	
	public ConfluenceLineBuilder(final OutputStream out) {
		outputStreamWriter = new OutputStreamWriter(out, StandardCharsets.UTF_8);
	}
	
	/**
	 * Writes a standard table line in Confluence Markup.
	 * F.e. "|MMRS370|COBOL|300|"
	 * 
	 * @param elements elements that will be inserted in the line cells
	 * @throws IOException if outputStreamWriter throws it
	 */
	@Override
	public void buildStandardLine(final List<String> elements) throws IOException {
		writeLine(elements, TABLE_DELIMITER);
	}
	
	/**
	 * Writes a header table line in Confluence Markup.
	 * 
	 * @param elements elements that will be inserted in the line cells
	 * @throws IOException if outputStreamWriter throws it
	 */
	@Override
	public void buildHeaderLine(final List<String> elements) throws IOException {
		writeLine(elements, TABLE_HEADER_DELIMITER);
	}
	
	void writeLine(final List<String> line, final String tableDelimiter) throws IOException {
		outputStreamWriter.write(tableDelimiter);
		for (String s : line) {
			if (s.isEmpty()) {
				outputStreamWriter.write(NOT_AVAILABLE_TEXT);
			} else {
				while (s.endsWith("\n")) { /* Remove all linebreaks at the end because they would mess up the table */
					s = s.substring(0, s.length() - 1);
				}
				outputStreamWriter.write(s);
			}
			outputStreamWriter.write(tableDelimiter);
		}
		outputStreamWriter.write('\n');
	}
	
	@Override
	public void close() throws IOException {
		outputStreamWriter.close();
	}

}
