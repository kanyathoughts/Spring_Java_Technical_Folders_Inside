package innowake.mining.extensions.export.csv;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.List;

import com.opencsv.CSVWriter;

import innowake.mining.extensions.export.table.TableLineBuilder;

/**
 * Builds CSV lines using a CSVWriter. The lines can be used for exporting tables to CSV.
 * 
 * @author jsimianer
 *
 */
public class CSVLineBuilder implements TableLineBuilder {
	private final CSVWriter csvWriter;
	
	public CSVLineBuilder(final OutputStream out) {
		csvWriter = new CSVWriter(new OutputStreamWriter(out, StandardCharsets.UTF_8));
	}

	/**
	 * Writes standard CSV line.
	 */
	@Override
	public void buildStandardLine(final List<String> elements) {
		csvWriter.writeNext(elements.toArray(new String[0]));
	}

	/**
	 * For CSV we don't make a difference for headers so this method calls the standard line method.
	 */
	@Override
	public void buildHeaderLine(final List<String> elements) {
		buildStandardLine(elements);
	}

	@Override
	public void close() throws IOException {
		csvWriter.close();
	}

}
