package innowake.mining.extensions.export.confluence;

import java.io.OutputStream;

import innowake.mining.extensions.export.table.LineBuilderFactory;
import innowake.mining.extensions.export.table.TableLineBuilder;

public class ConfluenceLineBuilderFactory implements LineBuilderFactory {

	@Override
	public TableLineBuilder getTableLineBuilder(final OutputStream out) {
		return new ConfluenceLineBuilder(out);
	}
}
