/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration;

import innowake.mining.data.io.DiscoveryExcelExportService;
import innowake.mining.data.io.DiscoveryExportOptions;
import innowake.mining.data.io.ExcelImportService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.io.WorkbookDefinition;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Tests to verify 
 * Excel Export {@link DiscoveryExcelExportService} able to create new sheets when rowIndex exceeds maxRowsPerSheet.
 * Excel Import {@link ExcelImportService} able to import multiple sheets.
 * 
 * Note : export.excel.maxRowsPerSheet property is overridden to improve the test case performance.
 */
@SpringBootTest("export.excel.maxRowsPerSheet=20")
class ExcelExportMultiSheetTest extends DatabaseRelatedTest {

	private static final EntityId CLIENT_ID = EntityId.of(1L);

	/* Number of modules to be created in database for excel export. */
	private static final int NUMBER_OF_MODULES = 30;

	private static final String TEMP_SNAPSHOT = "snapshot";

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private DiscoveryExcelExportService excelExportService;

	@Autowired
	private ExcelImportService excelImportService;

	@Test
	void shouldExportAndImportExcelWithMultiSheetIndex() {
		final ProjectPojo testProjectExport = projectService.create(new ProjectPojoPrototype()
				.setName("TEST PROJECT EXPORT")
				.setClient(CLIENT_ID)
				.setNatures(Collections.emptySet()));
		createTestModule(testProjectExport);

		final ProjectPojo testProjectImport = projectService.create(new ProjectPojoPrototype()
				.setName("TEST PROJECT IMPORT")
				.setClient(CLIENT_ID)
				.setNatures(Collections.emptySet()));

		try {
			/* Export Excel*/
			final byte[] workbookBytes;
			try (final ByteArrayOutputStream out = new ByteArrayOutputStream()) {
				excelExportService.exportExcel(testProjectExport.identity(), out, new DiscoveryExportOptions());
				workbookBytes = out.toByteArray();
			}
			final Workbook workbook = WorkbookFactory.create(new ByteArrayInputStream(workbookBytes));

			/* Import Excel*/
			excelImportService.importExcel(testProjectImport.identity(), TEMP_SNAPSHOT, new ByteArrayInputStream(workbookBytes));

			/* Validate Records in Exported Excel*/
			final List<String> sheetNames = new ArrayList<String>();
			for (int i = 0; i < workbook.getNumberOfSheets(); i++) {
				sheetNames.add(workbook.getSheetName(i));
			}
			
			final int numberOfModules = getNumberOfRecordsInSheet(workbook, WorkbookDefinition.SHEET_MODULES);
			final int numberOfDependencies = getNumberOfRecordsInSheet(workbook, WorkbookDefinition.SHEET_DEPENDENCIES);
			assertEquals(NUMBER_OF_MODULES, numberOfModules, "number of modules in excel should match the number of modules created");
			assertEquals(NUMBER_OF_MODULES - 1, numberOfDependencies, "number of dependencies in excel should match the number of dependencies created");
			validateSheetCount(sheetNames, WorkbookDefinition.SHEET_MODULES, 2);
			validateSheetCount(sheetNames, WorkbookDefinition.SHEET_DEPENDENCIES, 2);

			/* Validate Records in database*/
			final List<ModulePojo> modulesOfExportProject = moduleService.findModules(builder -> builder.ofProject(testProjectExport.identity()));
			final List<ModulePojo> modulesOfImportProject = moduleService.findModules(builder -> builder.ofProject(testProjectImport.identity()));
			
			assertEquals(NUMBER_OF_MODULES, modulesOfExportProject.size(), "number of modules exported should match the number of modules created");
			assertEquals(NUMBER_OF_MODULES, modulesOfImportProject.size(), "number of modules imported should match the number of modules created");

			final List<ModuleRelationshipPojo> dependenciesOfExportProject = moduleService.findRelationship(q -> q.ofProject(testProjectExport.identity())
																												.withTypes(RelationshipType.DEPENDENCY_TYPES));
			final List<ModuleRelationshipPojo> dependenciesOfImportProject = moduleService.findRelationship(q -> q.ofProject(testProjectImport.identity())
																												.withTypes(RelationshipType.DEPENDENCY_TYPES));

			assertEquals(NUMBER_OF_MODULES - 1, dependenciesOfExportProject.size(), "number of dependencies exported should match the number of dependencies created");
			assertEquals(NUMBER_OF_MODULES - 1, dependenciesOfImportProject.size(), "number of dependencies imported should match the number of dependencies created");
		} catch (final IOException e) {
			fail("Fail to export and import multi sheet excel. Exception occured : " + e.getMessage(), e);
		}
	}

	private void createTestModule(final ProjectPojo project) {
		EntityId previousId = null;
		for (int i = 0; i < NUMBER_OF_MODULES; i++) {
			final ModulePojoPrototype cobolProgram = new ModulePojoPrototype();
			cobolProgram.setProject(project.identity());
			cobolProgram.setName("TESTCOB");
			cobolProgram.setTechnology(Technology.COBOL);
			cobolProgram.setType(Type.PROGRAM);
			cobolProgram.setOrigin(Origin.CUSTOM);
			cobolProgram.setStorage(Storage.FILE);
			cobolProgram.setIdentification(Identification.IDENTIFIED);
			cobolProgram.setPath("src/cobol/TESTCOB" + i + ".cbl");
			cobolProgram.setCreator(Creator.DISCOVERY);
			final EntityId moduleId = moduleService.create(cobolProgram);

			/* Create dependencies */
			if (previousId != null) {
				final var reference = new ModuleRelationshipPojoPrototype();
				reference.setSrcModule(moduleId)
						 .setDstModule(previousId)
						 .setRelationship(RelationshipType.CALLS);
				moduleService.createRelationship(reference);
			}
			previousId = moduleId;
		}
	}

	private int getNumberOfRecordsInSheet(final Workbook workbook, final String sheetName) {
		int rowCount = 0;
		for (int sheetIndex = 0; sheetIndex < workbook.getNumberOfSheets(); sheetIndex++) {
			if (workbook.getSheetName(sheetIndex).startsWith(sheetName))
				rowCount += (workbook.getSheetAt(sheetIndex).getPhysicalNumberOfRows() - 1);
		}
		return rowCount;
	}

	private void validateSheetCount(final List<String> sheetNames, final String sheetName, final long expectedCount) {
		final long actualCount = sheetNames.stream()
				.filter(sheet -> sheet.startsWith(sheetName))
				.count();

		assertEquals(expectedCount, actualCount, String.format("%s sheet counts are not matching", sheetName));
	}
}
