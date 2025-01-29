/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for WMIN-6537: Ensure Modules with creator "API" and their edges and other linked entities are deleted only when requested.
 */
@WithMockUser
@Disabled("Disabling these tests until WMIN-8699 is completed. After postgres module migration, this test class will be modified.")
class ModuleDaoDeleteAllTest extends BaseDiscoveryTest {

	@Override
	protected String getTestFolder() {
		return "WMIN6537";
	}

	/**
	 * Tests that Discover Metrics deletes all modules with {@code creator = DISCOVERY}. Modules with {@code creator = API} must not be deleted.
	 */
	@Test
	@DisplayName("Test that Discover Metrics does not delete Api Modules")
	void testDiscoveryDoesNotDeleteApiModules() {
		final EntityId projectId = createProject().identity();
		this.projectId = projectId;

		final ModulePojo moduleA = createModule("A", Creator.API, projectId);
		final ModulePojo moduleD = createModule("D", Creator.DISCOVERY, projectId);

		/* Submit discover code job */
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));

		/* Submit discover metrics job */
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));

		/* Test that ModuleA with creator API was not deleted */
		final var modA = moduleService.findAnyModule(b -> b.ofProject(projectId).byId(moduleA.identity()));

		assertTrue(modA.isPresent(), "Module with creator API must not be deleted by Discover Metrics");
		assertEquals(modA.get().identity(), moduleA.identity());

		/* Test that ModuleD with creator DISCOVERY was deleted */
		assertFalse(moduleService.findAnyModule(b -> b.ofProject(projectId)
													.byId(moduleD.identity()))
									.isPresent(), "Module with creator DISCOVERY must be deleted by Discover Metrics");
	}

	/**
	 * Tests that Incremental Discover deletes all modules with {@code creator = DISCOVERY}. Modules with {@code creator = API} must not be deleted.
	 */
	@Test
	@DisplayName("Test that Incremental Discovery does not delete Api Modules")
	void testIncrementalDiscoveryDoesNotDeleteApiModules() {
		final EntityId projectId = createProject().identity();
		this.projectId = projectId;

		/* Submit discover code job */
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));

		/* Submit discover metrics job */
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, true));

		final ModulePojo moduleA = createModule("A", Creator.API, projectId);
		final ModulePojo moduleD = createModule("D", Creator.DISCOVERY, projectId);

		/* Submit incremental discover metrics job */
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, true));

		/* Test that ModuleA with creator API was not deleted */
		final var modA = moduleService.findAnyModule(b -> b.ofProject(projectId).byId(moduleA.identity()));

		assertTrue(modA.isPresent(), "Module with creator API must not be deleted by Discover Metrics");
		assertEquals(modA.get().identity(), moduleA.identity());

		/* Test that ModuleD with creator DISCOVERY was deleted */
		assertFalse(moduleService.findAnyModule(b -> b.ofProject(projectId)
													.byId(moduleD.identity()))
									.isPresent(), "Module with creator DISCOVERY must be deleted by Discover Metrics");
	}

	private ModulePojo createModule(final String suffix, final Creator creator, final EntityId projectId) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setName("MOD" + suffix);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setOrigin(Origin.CUSTOM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setPath("src/cobol/TESTCOBA" + suffix + ".cbl");
		module.setCreator(creator);

		return moduleService.findAnyModule(q -> q.byId(moduleService.create(module)))
				.orElseThrow(() -> new MiningEntityNotFoundException("Created test module must exist."));
	}
}
