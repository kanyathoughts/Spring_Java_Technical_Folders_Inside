/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.shared.TestResourceUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Test for WMIN-2057.
 * <p>
 * Excel export shouldn't crash when a Statement exists for a utility module.
 * <p>
 * A very contrived test but I wasn't able to reproduce this particular constellation using a "real" Discover Metrics run.
 */
class ExportUtilityTest extends DatabaseResettingTest {

	private static final String EXPECTED_FILE = "innowake/mining/server/discovery/expected/WMIN2057/discovery.xlsx.dump";
	
	/* use an empty project */
	private static final EntityId PROJECT_ID = EntityId.of(4l);

	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private DiscoveryCsvExportService csvExportService;
	
	@Test
	void testExportUtility() throws IOException {
		projectService.resetConfiguration(PROJECT_ID);
		
		/* create "UTILITY" module */
		final var moduleId = createTestModule();
		/* create a "STATEMENT" for the utility */

		final var statement = new StatementPojoPrototype()
				.setModule(moduleId)
				.setTechnology(Technology.UNKNOWN)
				.setType(StatementType.CALL)
				.setText("FAKE STATEMENT");

		final var result = moduleService.createStatement(statement, false);
		assertTrue(result.isPresent(), "Statement must have been created");

		final String contents;
		try (final ByteArrayOutputStream out = new ByteArrayOutputStream()) {
			csvExportService.exportCsv(PROJECT_ID, out, new DiscoveryExportOptions());
			contents = out.toString(Charset.forName("cp1252"));
		}
		if (BaseDiscoveryTest.isWriteExpected()) {
			TestResourceUtil.write(EXPECTED_FILE, contents, Charset.forName("Cp1252"));
		} else {
			DiscoveryExcelUtil.compareIgnoreOrder(TestResourceUtil.getContent(EXPECTED_FILE, Charset.forName("Cp1252")), contents);
		}
	}

	private EntityId createTestModule() {
		final ModulePojoPrototype utility = new ModulePojoPrototype();

		utility.setProject(PROJECT_ID);
		utility.setName("ABEND");
		utility.setTechnology(Technology.UNKNOWN);
		utility.setType(Type.UTILITY);
		utility.setStorage(Storage.FILE);
		utility.setOrigin(Origin.CUSTOM);
		utility.setIdentification(Identification.MISSING);
		utility.setCreator(Creator.DISCOVERY);

		return moduleService.create(utility);
	}
}
