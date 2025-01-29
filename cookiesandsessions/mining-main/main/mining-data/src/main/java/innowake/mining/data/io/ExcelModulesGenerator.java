/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.util.Map;

import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.io.WorkbookDefinition;
import innowake.mining.shared.io.WorkbookDefinition.Sheet;

/**
 * Creates the Modules sheet of a Discovery Excel workbook.
 */
@Service
public class ExcelModulesGenerator extends ModulesGenerator {

	@Nullable
	private SheetGenerator sheetGenerator;
	
	@Value("${export.excel.maxRowsPerSheet:1048570}")
	protected int maxRowsPerSheet;

	Map<Long, String> createSheet(final SXSSFWorkbook workbook,
			final EntityId projectId,
			final DiscoveryExportOptions options,
			final boolean sorted) throws IOException {
		final Sheet sheetDefinition = WorkbookDefinition.SHEETS.get(WorkbookDefinition.SHEET_MODULES);
		sheetGenerator = new SheetGenerator(workbook, sheetDefinition, maxRowsPerSheet);
		return process(projectId, options, sorted);
	}

	@Override
	public void createRow(final Object... values) throws IOException {
		assertNotNull(sheetGenerator).createRow(values);
	}
}
