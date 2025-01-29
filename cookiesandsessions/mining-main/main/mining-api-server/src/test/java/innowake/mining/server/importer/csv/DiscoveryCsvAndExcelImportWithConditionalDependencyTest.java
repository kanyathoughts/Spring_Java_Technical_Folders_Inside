/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.importer.csv;

import static innowake.lib.core.lang.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
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
 * Tests import of discovery CSV and XLSX files with new column Conditional Dependency.
 */
class DiscoveryCsvAndExcelImportWithConditionalDependencyTest extends DatabaseResettingTest{

	private static final EntityId PROJECT_ID = EntityId.of(1l);
	private static final String MODULE_NAME = "TEST.FILE3";
	private static final String MODULE_NAME1 = "PROD.FILE2";

	@Autowired
	private CSVImportService importCsvService;

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private ExcelImportService excelImportService;

	@Test
	void testImportCsvWithConditionalDependency() {
		doTest("import-discovery-WMIN8885A.csv",
				Paths.get("./test-resources/innowake/mining/server/integration/WMIN8885/import-discovery-WMIN8885A.csv"), false, true);
	}
	
	@Test
	void testImportExcelWithConditionalDependency() {
		doTest("import-discovery-WMIN8885B.xlsx",
				Paths.get("./test-resources/innowake/mining/server/integration/WMIN8885/import-discovery-WMIN8885B.xlsx"), true, true);
	}
	
	@Test
	void testImportExcelWithoutConditionalDependency() {
		doTest("import-discovery-WMIN8885C.xlsx",
				Paths.get("./test-resources/innowake/mining/server/integration/WMIN8885/import-discovery-WMIN8885C.xlsx"), true, false);
	}
	
	@Test
	void testImportCsvWithoutConditionalDependency() {
		doTest("import-discovery-WMIN8885D.csv",
				Paths.get("./test-resources/innowake/mining/server/integration/WMIN8885/import-discovery-WMIN8885D.csv"), false, false);
	}

	private void doTest(final String fileName, final Path path, final boolean excelImport, final boolean isConditionalDependencyExist) {
		try {
			assertEquals(0, moduleService.countModules(q -> q.ofProject(PROJECT_ID)));
			if (excelImport) {
				excelImportService.importExcel(PROJECT_ID, fileName, Files.newInputStream(path));
			} else {
				importCsvService.importCsv(PROJECT_ID, fileName, Files.newInputStream(path));
			}
			assertEquals(9, moduleService.countModules(q -> q.ofProject(PROJECT_ID)));
			final List<EntityId> moduleList = moduleService.findModuleIds(q -> q.ofProject(PROJECT_ID).withName(MODULE_NAME));
			assertEquals(1, moduleList.size());
			final List<EntityId> moduleList1 = moduleService.findModuleIds(q -> q.ofProject(PROJECT_ID).withName(MODULE_NAME1));
			assertEquals(1, moduleList1.size());

			final List<ModuleRelationshipPojo> referenceList = moduleService.findRelationship(q -> q.ofModuleInDirection(moduleList.iterator().next(), RelationshipDirection.BOTH)
																									.ofProject(PROJECT_ID)
																									.withTypes(RelationshipType.DEPENDENCY_TYPES)
																									.distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION));
			assertEquals(1, referenceList.size());
			final List<ModuleRelationshipPojo> referenceList1 = moduleService.findRelationship(q -> q.ofModuleInDirection(moduleList1.iterator().next(), RelationshipDirection.BOTH)
																									.ofProject(PROJECT_ID)
																									.distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION));
			assertEquals(1, referenceList1.size());
			final List<UUID> referenceModuleIds = referenceList.get(0).getValidIfReachedFrom();
			Collections.sort(referenceModuleIds);
			final List<UUID> reference1ModuleIds = referenceList1.get(0).getValidIfReachedFrom();
			if (isConditionalDependencyExist) {
				assertEquals(2, referenceModuleIds.size());
				final ModulePojo prodModule = moduleService.getModule(EntityId.of(referenceModuleIds.get(0)));
				final ModulePojo prod1Module = moduleService.getModule(EntityId.of(referenceModuleIds.get(1)));
				MatcherAssert.assertThat(Arrays.asList(prodModule.getName(), prod1Module.getName()), Matchers.containsInAnyOrder("PROD", "PROD1"));
				assertEquals(1, reference1ModuleIds.size());
				final ModulePojo prod2Module = moduleService.getModule(EntityId.of(reference1ModuleIds.get(0)));
				assertEquals("PROD1", prod2Module.getName());
			} else {
				assertTrue(referenceModuleIds.isEmpty());
				assertTrue(reference1ModuleIds.isEmpty());
			}
		} catch (final IOException e) {
			fail("Failed to read " + fileName.toString() + ". Exception: " + e.getMessage());
		}
	}
}
