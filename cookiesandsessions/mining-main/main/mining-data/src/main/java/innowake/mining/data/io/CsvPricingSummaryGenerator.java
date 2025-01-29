/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;

import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.io.EffortSummaryWorkbookDefinition;
import innowake.mining.shared.model.EffortSummaryType;

/**
 * Creates the Pricing summary sheet for the Effort Summary CSV file.
 */
@Service
public class CsvPricingSummaryGenerator extends EffortSummaryGenerator {

	@Nullable
	private CsvGenerator csvGenerator;

	/**
	 * Creates the CSV content for the pricing summary.
	 *
	 * @param projectId the project ID
	 * @throws IOException if an error occurs
	 */
	void createCsv(final EntityId projectId, final CsvGenerator csvGenerator) throws IOException {
		this.csvGenerator = csvGenerator;
		csvGenerator.createHeader(EffortSummaryWorkbookDefinition.SHEET_PRICING_SUMMARY, true);
		process(projectId, EffortSummaryType.PRICING);
	}

	@Override
	public void createRow(final Object... values) throws IOException {
		assertNotNull(csvGenerator).createRow(values);
	}
}
