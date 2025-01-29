/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.util.Map;

import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.io.WorkbookDefinition;

/**
 * Creates the Errors sheet of a Discovery CSV file.
 */
@Service
public class CsvErrorsGenerator extends ErrorsGenerator {

	@Nullable
	private CsvGenerator csvGenerator;

	void createCsv(final EntityId projectId, final Map<Long, String> moduleMapping,
			final DiscoveryExportOptions options, final CsvGenerator csvGenerator, final boolean sorted) throws IOException {
		this.csvGenerator = csvGenerator;
		csvGenerator.createHeader(WorkbookDefinition.SHEET_ERRORS);
		process(projectId, moduleMapping, options, sorted);
	}

	@Override
	public void createRow(final Object... values) throws IOException {
		assertNotNull(csvGenerator).createRow(values);
	}
}
