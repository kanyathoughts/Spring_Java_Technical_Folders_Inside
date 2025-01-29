/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.importer.csv;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.data.io.ExcelImportService;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.RelationshipField;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;

/**
 * Tests import of discovery CSV and XLSX files with new column Relationship.
 */
class DiscoveryCsvAndExcelImportWithRelationshipTest extends DatabaseResettingTest {

	private static final EntityId PROJECT_ID = EntityId.of(1l);
	private static final String moduleName = "MMRS00C.A.LOADLIB";

	@Autowired
	private CSVImportService importCsvService;

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private ExcelImportService excelImportService;

	@Test
	void testImportCsvWithoutRelationship() {
		doTest(moduleName, "import-discovery-WMIN8830A.csv",
				Paths.get("./test-resources/innowake/mining/server/integration/WMIN8830/import-discovery-WMIN8830A.csv"), RelationshipType.ACCESSES, false);
	}

	@Test
	void testImportCsvWithDifferentRelationship() {
		doTest(moduleName, "import-discovery-WMIN8830B.csv",
				Paths.get("./test-resources/innowake/mining/server/integration/WMIN8830/import-discovery-WMIN8830B.csv"), RelationshipType.CALLS, false);
	}

	@Test
	void testImportCsvWithWrongRelationship() {
		doTest(moduleName, "import-discovery-WMIN8830C.csv",
				Paths.get("./test-resources/innowake/mining/server/integration/WMIN8830/import-discovery-WMIN8830C.csv"), RelationshipType.ACCESSES, false);
	}

	@Test
	void testImportExcelWithoutRelationship() {
		doTest(moduleName, "import-discovery-WMIN8830A.xlsx",
				Paths.get("./test-resources/innowake/mining/server/integration/WMIN8830/import-discovery-WMIN8830A.xlsx"), RelationshipType.ACCESSES, true);
	}

	@Test
	void testImportExcelWithDifferentRelationship() {
		doTest(moduleName, "import-discovery-WMIN8830B.xlsx",
				Paths.get("./test-resources/innowake/mining/server/integration/WMIN8830/import-discovery-WMIN8830B.xlsx"), RelationshipType.CALLS, true);
	}

	@Test
	void testImportExcelWithWrongRelationship() {
		doTest(moduleName, "import-discovery-WMIN8830C.xlsx",
				Paths.get("./test-resources/innowake/mining/server/integration/WMIN8830/import-discovery-WMIN8830C.xlsx"), RelationshipType.ACCESSES,
				true);
	}

	private void doTest(final String moduleName, final String fileName, final Path path, final RelationshipType target, final boolean excelImport) {
		try {
			assertEquals(0, moduleService.countModules(q -> q.ofProject(PROJECT_ID)));
			if (excelImport) {
				excelImportService.importExcel(PROJECT_ID, fileName, Files.newInputStream(path));
			} else {
				importCsvService.importCsv(PROJECT_ID, fileName, Files.newInputStream(path));
			}
			assertEquals(8, moduleService.countModules(q -> q.ofProject(PROJECT_ID)));
			final List<ModulePojo> moduleList = moduleService.findModules(q -> q.ofProject(PROJECT_ID).withName(moduleName));
			assertEquals(1, moduleList.size());
			final List<ModuleRelationshipPojo> referenceList = moduleService.findRelationship(q -> q.ofProject(PROJECT_ID)
																				.ofModuleInDirection(moduleList.iterator().next().identity(), RelationshipDirection.BOTH)
																				.withTypes(RelationshipType.DEPENDENCY_TYPES)
																				.distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION));
			assertEquals(1, referenceList.size());
			assertEquals(target, referenceList.get(0).getRelationship());
		} catch (final IOException e) {
			throw new AssertionError("Failed to read " + fileName.toString() + ". Exception: " + e.getMessage(), e);
		}
	}
}
