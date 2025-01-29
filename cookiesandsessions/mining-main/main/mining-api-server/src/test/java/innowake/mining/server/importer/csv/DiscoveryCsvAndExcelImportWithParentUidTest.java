/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.importer.csv;

import static innowake.lib.core.lang.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.data.io.ExcelImportService;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.Type;

/**
 * Tests import of discovery XLSX and CSV files with new column ParentUid and also ensures whether the contains module edge is created or not. 
 */
class DiscoveryCsvAndExcelImportWithParentUidTest extends DatabaseResettingTest {
	
	private static final Long PROJECT_ID = Long.valueOf(1);
	private static final String DISCOVERY_FILE_CSV = "import-discovery-csv.csv";
	private static final Path  DISCOVERY_FILE_CSV_PATH = Paths.get("./test-resources/innowake/mining/server/integration/WMIN8401", DISCOVERY_FILE_CSV);
	
	private static final String DISCOVERY_FILE_CSV1 = "import-discovery-csv1.csv";
	private static final Path  DISCOVERY_FILE_CSV_PATH1 = Paths.get("./test-resources/innowake/mining/server/integration/WMIN8401", DISCOVERY_FILE_CSV1);
    
	private static final String DISCOVERY_FILE_EXCEL = "import-discovery-excel.xlsx";
	private static final Path DISCOVERY_FILE_EXCEL_PATH = Paths.get("./test-resources/innowake/mining/server/integration/WMIN8401", DISCOVERY_FILE_EXCEL);

	@Autowired
	private CSVImportService importCsvService;	
	@Autowired
	private ExcelImportService excelImportService;	
	@Autowired
	private ModuleService moduleService;
	
	/**
	 * Tests CSV file with new column Parent Uid.
	 */
	@Test
	void importCsv() {
		try {
			assertEquals(0, moduleService.countModules(b -> b.ofProject(EntityId.of(PROJECT_ID))));
			importCsvService.importCsv(EntityId.of(PROJECT_ID), DISCOVERY_FILE_CSV, Files.newInputStream(DISCOVERY_FILE_CSV_PATH));
			assertImportedResults();
		} catch (final IOException e) {
			fail("Failed to read " + DISCOVERY_FILE_CSV.toString() + ". Exception: " + e.getMessage());
		}
	}
	
	/**
	 * Tests excel file with new column Parent Uid.
	 */
	@Test
	void importExcel() {
		try {
			assertEquals(0, moduleService.countModules(b -> b.ofProject(EntityId.of(PROJECT_ID))));
			excelImportService.importExcel(EntityId.of(PROJECT_ID), DISCOVERY_FILE_EXCEL, Files.newInputStream(DISCOVERY_FILE_EXCEL_PATH));
			assertImportedResults();
		} catch (final IOException e) {
			fail("Failed to read " + DISCOVERY_FILE_CSV.toString() + ". Exception: " + e.getMessage());
		}
	}
	
	/**
	 * Tests CSV file with new column Parent Uid.
	 */
	@Test
	void importCsv1() {
		try {
			assertEquals(0, moduleService.countModules(b -> b.ofProject(EntityId.of(PROJECT_ID))));
			importCsvService.importCsv(EntityId.of(PROJECT_ID), DISCOVERY_FILE_CSV1, Files.newInputStream(DISCOVERY_FILE_CSV_PATH1));
			final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(EntityId.of(PROJECT_ID)).withType(Type.TYPE));
			assertEquals(2, modules.size());
			final Optional<ModulePojo> module1 = modules.stream()
					.filter(module -> {
						final var containsPath = module.getParentPath();
						return containsPath.isPresent() && containsPath.get().equals("src/java/test1/Test.java");
					}).findFirst();
			assertTrue(module1.isPresent(), String.format("Module with Name %s and with parent as %s should be present", "Test", "src/java/test1/Test.java"));
			final String module1LinkHash = module1.get().getLinkHash();
			final Optional<ModulePojo> module2 = modules.stream()
					.filter(module -> {
						final var containsPath = module.getParentPath();
						return containsPath.isPresent() && containsPath.get().equals("src/java/test2/Test.java");
					}).findFirst();
			assertTrue(module2.isPresent(), String.format("Module with Name %s and with parent as %s should be present", "Test", "src/java/test1/Test.java"));
			final String module2LinkHash = module2.get().getLinkHash();
			assertNotEquals(module1LinkHash, module2LinkHash, "Link hash should not be the same for both modules");
		} catch (final IOException e) {
			fail("Failed to read " + DISCOVERY_FILE_CSV.toString() + ". Exception: " + e.getMessage());
		}
	}
	
	private void assertImportedResults() {
		assertEquals(6, moduleService.countModules(b -> b.ofProject(EntityId.of(PROJECT_ID))));
		final String moduleName1 = "testcases.discovery.MiningProjectSpecificSettingsTest1";
		final String moduleName2 = "testcases.discovery1.MiningProjectSpecificSettingsTest2";
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(EntityId.of(PROJECT_ID)).withNames(Arrays.asList(moduleName1, moduleName2)));
		assertEquals(2, modules.size());
		final Optional<ModulePojo> module1 = modules.stream().filter(module -> module.getName().equals(moduleName1)).findFirst();
		assertTrue(module1.isPresent(), String.format("Module with Name %s should be present", moduleName1));
		assertEquals("src/java/MiningProjectSpecificSettingsTest1.java", module1.get().getParentPath().orElseThrow());
		
		final Optional<ModulePojo> module2 = modules.stream().filter(module -> module.getName().equals(moduleName2)).findFirst();
		assertTrue(module2.isPresent(), String.format("Module with Name %s should be present", moduleName2));
		assertEquals("src/java/MiningProjectSpecificSettingsTest2.java", module2.get().getParentPath().orElseThrow());
	}
}
