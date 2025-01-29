package innowake.mining.extensions.export.csv;

import java.io.OutputStream;

import innowake.mining.extensions.export.table.LineBuilderFactory;
import innowake.mining.extensions.export.table.TableLineBuilder;

public class CSVLineBuilderFactory implements LineBuilderFactory {

	@Override
	public TableLineBuilder getTableLineBuilder(final OutputStream out) {
		return new CSVLineBuilder(out);
	}
}
