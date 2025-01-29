/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.extensions.export.discovery;

import innowake.mining.data.io.DiscoveryCsvExportService;
import innowake.mining.data.io.DiscoveryExcelExportService;
import innowake.mining.data.io.DiscoveryExportOptions;
import innowake.mining.data.io.ExcelImportService;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.importer.csv.CSVImportService;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojo;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static innowake.lib.core.lang.Assert.fail;

/**
* Test the import and export of Errors sheet and tests whether backward compatibility works for {@link ErrorMarker}.
*/
class DiscoveryImportAndExportForErrorMarkerTest extends BaseDiscoveryTest {

	private static final String DUMP_FILE_CSV = "Discovery_Error_Marker_Exported.csv";
	private static final String DUMP_FILE_EXCEL = "Discovery_Error_Marker_Excel_Exported.xlsx.dump";
	private static final Path DUMP_FILE_CSV_PATH = Paths.get("./test-resources/innowake/mining/server/integration/import-discovery-WMIN-8321.csv");
	private static final Path DUMP_FILE_EXCEL_PATH = Paths.get("./test-resources/innowake/mining/server/integration/import-discovery-excel-WMIN-8321.xlsx");

	@Autowired
	private DiscoveryCsvExportService csvExportService;
	@Autowired
	private CSVImportService csvImportService;
	@Autowired
	private DiscoveryExcelExportService excelExportService;
	@Autowired
	private ExcelImportService excelImportService;
	
	/**
	 * Tests the backward compatibility of the {@link CSVImportService} and  {@link CSVExportService} for Errors sheet.
	 */
	@Test
	void testExportAndImportForErrorCsv() {
		try {
			final Path expectedFile = getExpectedFile(DUMP_FILE_CSV);
			final EntityId projectId = this.projectId = createProject().identity();

			csvImportService.importCsv(projectId, DUMP_FILE_CSV, Files.newInputStream(DUMP_FILE_CSV_PATH));

			final String actualCsv;
			try (final ByteArrayOutputStream out = new ByteArrayOutputStream()) {
				csvExportService.exportCsv(projectId, out, new DiscoveryExportOptions());
				actualCsv = out.toString(getCharset());
			}
			if (isWriteExpected()) {
				writeExpected(expectedFile, actualCsv);
				return;
			}
			DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), actualCsv);
		} catch (Exception e) {
			fail("Fail to import and export CSV for the ErrorMarker. Exception occured : " + e.getMessage());
		}
	}
	
	/**
	 * Tests the backward compatibility of the {@link excelImportService} and {@link excelExportService} for Errors sheet.
	 */
	@Test
	void testExportAndImportForErrorExcel() {
		try {
			final Path expectedFile = getExpectedFile(DUMP_FILE_EXCEL);
			final ProjectPojo projectPojo = createProject();
			
			excelImportService.importExcel(projectPojo.identity(), DUMP_FILE_EXCEL, Files.newInputStream(DUMP_FILE_EXCEL_PATH));

			final byte[] exportData;
			try (final ByteArrayOutputStream out = new ByteArrayOutputStream()) {
				excelExportService.exportExcel(projectPojo.identity(), out, new DiscoveryExportOptions());
				exportData = out.toByteArray();
			}
			final Workbook workbook = WorkbookFactory.create(new ByteArrayInputStream(exportData));
			final String contents = DiscoveryExcelUtil.dumpWorkbook(workbook);
			if (isWriteExpected()) {
				writeExpected(expectedFile, contents);
			} else {
				DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), contents);
			}
		} catch (Exception e) {
			fail("Fail to import and export Excel for the ErrorMarker. Exception occured : " + e.getMessage());
		}
	}

	@Override
	protected String getTestFolder() {
		return "WMIN8321";
	}
}
