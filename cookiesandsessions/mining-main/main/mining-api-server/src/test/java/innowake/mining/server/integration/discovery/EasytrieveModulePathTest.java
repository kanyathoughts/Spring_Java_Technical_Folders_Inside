/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.nio.file.Paths;
import java.util.List;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.Technology;

/**
 * Tests to validate Easytrieve Modules creates null Path in DB
 */
@WithMockUser
class EasytrieveModulePathTest extends BaseDiscoveryTest {

	private static final String TEST_FOLDER = Paths.get("iris/WMIN1587").toString();
	private static final String MODULE_IN_TEST = "AP10E60B";

	/**
	 * Method to create project and run Discover Code and Discover Metrics
	 */
	@BeforeEach
	void createAndDiscover() {
		projectId = assertNotNull(createProject()).identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(assertNotNull(projectId)));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(assertNotNull(projectId), false));
	}

	/**
	 * Test to check if Module path is null
	 */
	@Test
	void testIfEasytrieveModulePathIsNull() {
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(assertNotNull(projectId)).withName(MODULE_IN_TEST));
		assertNotNull(modules);
		for (final ModulePojo module : modules) {
			if (module.getTechnology().equals(Technology.EASYTRIEVE)) {
				Assert.assertTrue(module.getPath().isEmpty());
			}
		}
	}

	/**
	 * Get the test folder for the discovery to execute
	 */
	@Override
	protected String getTestFolder() {
		return TEST_FOLDER;
	}

}
