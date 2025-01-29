/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;


import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.shared.TestResourceUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Test for legacy-compatible Excel/CSV export.
 */
class LegacyExportTest extends DatabaseResettingTest {

	private static final String EXPECTED_FOLDER = "innowake/mining/server/discovery/expected/WMIN2057/";
	
	/* use an empty project */
	private static final EntityId PROJECT_ID = EntityId.of(4l);
	
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private DiscoveryExcelExportService excelExportService;
	
	@Autowired
	private DiscoveryCsvExportService csvExportService;
	
	@Test
	void testExportLegacyExcel() throws IOException {
		projectService.resetConfiguration(PROJECT_ID);
		createTestModules();

		final byte[] exportData;
		try (final ByteArrayOutputStream out = new ByteArrayOutputStream()) {
			excelExportService.exportExcel(PROJECT_ID, out, new DiscoveryExportOptions());
			exportData = out.toByteArray();
		}
		final Workbook workbook = WorkbookFactory.create(new ByteArrayInputStream(exportData));
		final String contents = DiscoveryExcelUtil.dumpWorkbook(workbook);
		if (BaseDiscoveryTest.isWriteExpected()) {
			TestResourceUtil.write(EXPECTED_FOLDER + "legacy-excel.xlsx.dump", contents, Charset.forName("Cp1252"));
		} else {
			TestResourceUtil.getContent(EXPECTED_FOLDER + "legacy-excel.xlsx.dump", Charset.forName("Cp1252"));
		}
	}
	
	@Test
	void testExportLegacyCsv() throws IOException {
		projectService.resetConfiguration(PROJECT_ID);
		createTestModules();

		final String contents;
		try (final ByteArrayOutputStream out = new ByteArrayOutputStream()) {
			csvExportService.exportCsv(PROJECT_ID, out, new DiscoveryExportOptions());
			contents = out.toString(StandardCharsets.UTF_8);
		}
		if (BaseDiscoveryTest.isWriteExpected()) {
			TestResourceUtil.write(EXPECTED_FOLDER + "legacy-csv.csv", contents, Charset.forName("Cp1252"));
		} else {
			TestResourceUtil.getContent(EXPECTED_FOLDER + "legacy-csv.csv", Charset.forName("Cp1252"));
		}
	}
	
	private void createTestModules() {
		final var cobolProgram = new ModulePojoPrototype();
		final var storedProcedure = new ModulePojoPrototype();
		final var table = new ModulePojoPrototype();
		/* these fields are required and the default is null ... */
		final ModuleLocation fakeLocation = new ModuleLocation();
		fakeLocation.setOffset(Integer.valueOf(0));
		fakeLocation.setLength(Integer.valueOf(0));
	
		cobolProgram.setName("TESTCOB");
		cobolProgram.setTechnology(Technology.COBOL);
		cobolProgram.setType(Type.PROGRAM);
		cobolProgram.setStorage(Storage.FILE);
		cobolProgram.setOrigin(Origin.CUSTOM);
		cobolProgram.setProject(PROJECT_ID);
		cobolProgram.setCreator(Creator.API);
		cobolProgram.setIdentification(Identification.IDENTIFIED);
		final var cProg = moduleService.create(cobolProgram);
		
		storedProcedure.setName("TESTPROC");
		storedProcedure.setTechnology(Technology.SQL);
		storedProcedure.setStorage(Storage.DATABASE);
		storedProcedure.setOrigin(Origin.CUSTOM);
		storedProcedure.setType(Type.STORED_PROCEDURE);
		storedProcedure.setCreator(Creator.API);
		storedProcedure.setProject(PROJECT_ID);
		storedProcedure.setIdentification(Identification.IDENTIFIED);
		final var storedProc = moduleService.create(storedProcedure);
		
		table.setName("TESTTABLE");
		table.setTechnology(Technology.SQL);
		table.setType(Type.TABLE);
		table.setStorage(Storage.DATABASE);
		table.setOrigin(Origin.CUSTOM);
		table.setCreator(Creator.API);
		table.setProject(PROJECT_ID);
		table.setIdentification(Identification.IDENTIFIED);
		final var tab = moduleService.create(table);
		
		/* Cobol Program calls Stored Procedure */
		var reference = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(cProg)
				.setDstModule(storedProc)
				.setSrcLocation(fakeLocation)
				.setDstLocation(fakeLocation);
		moduleService.createRelationship(reference);
	
		/* Cobol Program reads/writes Table */
		reference = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(cProg)
				.setDstModule(tab)
				.setSrcLocation(fakeLocation)
				.setDstLocation(fakeLocation);
		moduleService.createRelationship(reference);
	}
}
