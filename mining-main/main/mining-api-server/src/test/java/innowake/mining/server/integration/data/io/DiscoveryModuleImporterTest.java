/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.data.io;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Instant;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.data.io.DiscoveryModuleImporter;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for {@code DiscoveryModuleImporter}
 */
class DiscoveryModuleImporterTest extends DatabaseResettingTest {
	
	private static final EntityId TEST_PROJECT_ID = EntityId.of(4L);
	
	@Autowired
	private DiscoveryModuleImporter moduleImporter;
	
	@Autowired
	private ModuleService moduleService;
	
	@Test
	void testImportModuleWithoutName() {
		final ModuleParameters moduleParameters = new ModuleParameters(Instant.now());
		final SourceMetrics sourceMetrics = new SourceMetrics();
		sourceMetrics.setCodeLines(Integer.valueOf(24));
		sourceMetrics.setCommentLines(Integer.valueOf(25));
		sourceMetrics.setComplexityMcCabe(Integer.valueOf(26));
		sourceMetrics.setPhysicalLines(Integer.valueOf(27));

		final EntityId importedModuleId = moduleImporter.importModule(TEST_PROJECT_ID, "", "", Technology.UNKNOWN, Type.UNKNOWN, Storage.UNDEFINED,
				sourceMetrics, Representation.VIRTUAL, null, moduleParameters);

		final ModulePojo importedModule = moduleService.getModule(importedModuleId);

		assertEquals("<missing name>", importedModule.getName());
		assertEquals(TEST_PROJECT_ID.getNid(), importedModule.getProject().getNid());
		assertTrue(importedModule.getPath().isEmpty());
		assertEquals(Technology.UNKNOWN, importedModule.getTechnology());
		assertEquals(Type.UNKNOWN, importedModule.getType());
		assertEquals(Storage.UNDEFINED, importedModule.getStorage());
		assertEquals(Representation.VIRTUAL, importedModule.getRepresentation().get());
		assertTrue(importedModule.getLocation().isEmpty());
		assertEquals(moduleParameters.getMetricsDate().getEpochSecond(), importedModule.getMetricsDate().get().getEpochSecond());
		assertEquals(moduleParameters.getMetricsDate().getEpochSecond(), importedModule.getModifiedDate().get().getEpochSecond());

		final SourceMetricsPojo actual = importedModule.getSourceMetrics().get();
		assertEquals(sourceMetrics.getCodeLines(), actual.getCodeLines());
		assertEquals(sourceMetrics.getCommentLines(), actual.getCommentLines());
		assertEquals(sourceMetrics.getComplexityMcCabe(), actual.getComplexityMcCabe());
		assertEquals(sourceMetrics.getDeadCodeLines(), actual.getDeadCodeLines());
		assertEquals(sourceMetrics.getPhysicalLines(), actual.getPhysicalLines());
	}
}
