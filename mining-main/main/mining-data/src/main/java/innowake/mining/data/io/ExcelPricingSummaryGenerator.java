/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;

import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.io.EffortSummaryWorkbookDefinition;
import innowake.mining.shared.io.WorkbookDefinition.Sheet;
import innowake.mining.shared.model.EffortSummaryType;

/**
 * Creates the Pricing summary sheet for the Effort Summary Excel workbook.
 */
@Service
public class ExcelPricingSummaryGenerator extends EffortSummaryGenerator {

	@Value("${export.excel.maxRowsPerSheet:1048570}")
	protected int maxRowsPerSheet;

	@Nullable
	private SheetGenerator sheetGenerator;

	void createSheet(final SXSSFWorkbook workbook, final EntityId projectId) throws IOException {
		final Sheet sheetDefinition = EffortSummaryWorkbookDefinition.SHEETS.get(EffortSummaryWorkbookDefinition.SHEET_PRICING_SUMMARY);
		sheetGenerator = new SheetGenerator(workbook, sheetDefinition, maxRowsPerSheet);
		process(projectId, EffortSummaryType.PRICING);
	}

	@Override
	public void createRow(final Object... values) throws IOException {
		assertNotNull(sheetGenerator).createRow(values);
	}
}
