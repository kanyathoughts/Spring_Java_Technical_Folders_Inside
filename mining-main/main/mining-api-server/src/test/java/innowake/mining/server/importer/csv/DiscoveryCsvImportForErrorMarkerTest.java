/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.importer.csv;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.data.io.ExcelImportService;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ErrorMarkerPojo;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Test the import of Errors sheet with both old and current sheets for both EXCEL and CSV files 
 */
class DiscoveryCsvImportForErrorMarkerTest extends DatabaseResettingTest {
	
	private static final EntityId PROJECT_ID = EntityId.of(1l);
	private static final String OLD_ERROR_SHEET_FILE_CSV = "import-discovery-errors-old.csv";
	private static final Path OLD_ERROR_SHEET_CSV = Paths.get("./test-resources/innowake/mining/server/integration/WMIN7071", OLD_ERROR_SHEET_FILE_CSV);
	
	private static final String NEW_ERROR_SHEET_FILE_CSV = "import-discovery-errors-new.csv";
	private static final Path NEW_ERROR_SHEET_CSV = Paths.get("./test-resources/innowake/mining/server/integration/WMIN7071", NEW_ERROR_SHEET_FILE_CSV);
    
	private static final String OLD_ERROR_SHEET_FILE_EXCEL = "import-discovery-errors-old.xlsx";
	private static final Path OLD_ERROR_SHEET_EXCEL = Paths.get("./test-resources/innowake/mining/server/integration/WMIN7071", OLD_ERROR_SHEET_FILE_EXCEL);
	
	private static final String NEW_ERROR_SHEET_FILE_EXCEL = "import-discovery-errors-new.xlsx";
	private static final Path NEW_ERROR_SHEET_EXCEL = Paths.get("./test-resources/innowake/mining/server/integration/WMIN7071", NEW_ERROR_SHEET_FILE_EXCEL);

	@Autowired
	private CSVImportService importCsvService;	
	@Autowired
	private ExcelImportService excelImportService;	
	@Autowired
	private ModuleService moduleService;
	
	/**
	 * Tests CSV with new errors sheet import.
	 */
	@Test
	void importCsv() {
		try {
			importExcelCsvAssert(NEW_ERROR_SHEET_FILE_CSV, NEW_ERROR_SHEET_CSV, false, false);
		} catch (final IOException e) {
			fail("Failed to read " + OLD_ERROR_SHEET_CSV.toString() + ". Exception: " + e.getMessage());
		}
	}
	
	/**
	 * Tests the backwards compatibility of the {@link CSVImportService} for Errors sheet.
	 * In WMIN-7071, 2 new columns (Offset, Length) were added and removed Line column from Error sheet
	 */
	@Test
	void testBackwardsCompatibilityErrorsImportCsv() {
		try {
			importExcelCsvAssert(OLD_ERROR_SHEET_FILE_CSV, OLD_ERROR_SHEET_CSV, false, true);
		} catch (final IOException e) {
			fail("Failed to read file " + NEW_ERROR_SHEET_CSV.toString() + ". Exception: " + e.getMessage());
		}
	}
	
	/**
	 * Tests Excel with new errors sheet import.
	 */
	@Test
	void importExcel() {
		try {
			importExcelCsvAssert(NEW_ERROR_SHEET_FILE_EXCEL, NEW_ERROR_SHEET_EXCEL, true, false);		
		} catch (final IOException e) {
			fail("Failed to read " + OLD_ERROR_SHEET_CSV.toString() + ". Exception: " + e.getMessage());
		}
	}
	
	/**
	 * Tests the backwards compatibility of the {@link CSVImportService} for Errors sheet.
	 * In WMIN-7071, 2 new columns (Offset, Length) were added and removed Line column from Error sheet
	 */
	@Test
	void testBackwardsCompatibilityErrorsImportExcel() {
		try {
			importExcelCsvAssert(OLD_ERROR_SHEET_FILE_EXCEL, OLD_ERROR_SHEET_EXCEL, true, true);
		} catch (final IOException e) {
			fail("Failed to read file " + NEW_ERROR_SHEET_CSV.toString() + ". Exception: " + e.getMessage());
		}
	}

	private void importExcelCsvAssert(final String fileName, final Path filePath, final boolean isExcelImport, final boolean isBackward) throws IOException {
		final long errorMarkersCountBeforeImport = moduleService.countErrorMarkers(q -> q.ofProject(PROJECT_ID));
		assertEquals(0, errorMarkersCountBeforeImport);
		importCsvExcel(fileName, filePath, isExcelImport);
		assertResults(isBackward);
	}

	private void assertResults(final boolean isBackward) {
		final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(PROJECT_ID).withPath("src/cobol/programs/WMIN7071.cbl"))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for path: src/cobol/programs/WMIN7071.cbl"));
		/* As we are getting the Error markers with Module's id , that means there is an edge created between module and ErrorMaker i.e HasErrorMarker.*/
		final List<ErrorMarkerPojo> errorMarkers = moduleService.findErrorMarkers(q -> q.ofProject(PROJECT_ID).ofModule(module.identity()));
		assertEquals(3, errorMarkers.size());
		final var moduleLocation = assertNotNull(errorMarkers.get(0).getLocation());
		assertTrue(moduleLocation.getAssembledOffset().isPresent());
		assertTrue(moduleLocation.getAssembledLength().isPresent());
		if (isBackward) {
			assertEquals(-1, moduleLocation.getAssembledOffset().get());
			assertEquals(-1, moduleLocation.getAssembledLength().get());
		} else {
			assertTrue(moduleLocation.getAssembledOffset().get() > 0, "The Offset of ErrorMarker should be greater than 0");
			assertTrue(moduleLocation.getAssembledLength().get() > 0, "The Length of ErrorMarker should be greater than 0");
		}
	}

	private void importCsvExcel(final String fileName, final Path filePath, final boolean isExcelImport) throws IOException {
		if (isExcelImport) {
			excelImportService.importExcel(PROJECT_ID, fileName, Files.newInputStream(filePath));
		} else {
			importCsvService.importCsv(PROJECT_ID, fileName, Files.newInputStream(filePath));
		}
	}
}
