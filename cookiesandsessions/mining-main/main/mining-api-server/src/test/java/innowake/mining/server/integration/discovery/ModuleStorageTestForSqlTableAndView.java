/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.data.io.ExcelImportService;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.importer.csv.CSVImportService;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.model.Storage;

/**
 * Tests for WMIN-8139: Ensure the Storage of SQL_TABLE  and SQL_VIEW is set to DATABASE
 */
@WithMockUser
class ModuleStorageTestForSqlTableAndView extends BaseDiscoveryTest {

	private static final String DUMP_FILE_XLSX = "discovery.xlsx.dump";
	private static final String DUMP_FILE_CSV = "discovery.csv";
	private static final Path DUMP_FILE_XLSX_PATH = Paths.get("./test-resources/innowake/mining/server/integration/WMIN8139/storage_discovery.xlsx");
	private static final Path DUMP_FILE_CSV_PATH = Paths.get("./test-resources/innowake/mining/server/integration/WMIN8139/storage_discovery.csv");
	@Autowired
	private ExcelImportService excelImportService;
	@Autowired
	private CSVImportService csvImportService;

	@Override
	protected String getTestFolder() {
		return "WMIN8139";
	}

	/**
	 * Tests the storage of module with technology as SQL and type as TABLE and VIEW through discovery.
	 */
	@Test
	void testStorageOfModuleThroughDiscovery() {
		final ProjectPojo project = assertNotNull(createProject());
		final EntityId projectId = project.identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
		testStorage(projectId);
	}

	/**
	 * Tests the storage of module with technology as SQL and type as TABLE and VIEW through import.
	 */
	@Test
	void testStorageOfModuleThroughimportExcel() {
		try {
			final ProjectPojo project = createProject();
			excelImportService.importExcel(project.identity(), DUMP_FILE_XLSX, Files.newInputStream(DUMP_FILE_XLSX_PATH));
			testStorage(project.identity());
		} catch (final IOException e) {
			fail("Fail to import the Excel. Exception occured : " + e.getMessage());
		}
	}
	
	/**
	 * Tests the storage of module with technology as SQL and type as TABLE and VIEW through import.
	 */
	@Test
	void testStorageOfModuleThroughimportCsv() {
		try {
			final ProjectPojo project = createProject();
			csvImportService.importCsv(project.identity(), DUMP_FILE_CSV, Files.newInputStream(DUMP_FILE_CSV_PATH));
			testStorage(project.identity());
		} catch (final IOException e) {
			fail("Fail to import the Excel. Exception occured : " + e.getMessage());
		}
	}
	
	private void testStorage(final EntityId projectId) {
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(projectId).withTechnology(Technology.SQL));
		for (final ModulePojo module : modules) {
			final Type type = module.getType();
			if (type.equals(Type.TABLE) || type.equals(Type.VIEW)) {
				assertEquals(Storage.DATABASE, module.getStorage());
			}
		}
	}

}
