/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.ProjectNature;

/**
 * Tests whether the dependency hash created while calculating the metrics and should be same for two runs.
 */
@WithMockUser
class DiscoveryMetricsWithDependencyHashTest extends BaseDiscoveryTest {

	@Override
	protected String getTestFolder() {
		return "WMIN12196";
	}

	/**
	 * Tests whether each module has a dependency hash and created as a part of discover metrics and verifies whether
	 * dependency hash on same modules on different project is same or not.
	 */
	@Test
	void testForDependencyHashForModules() {
		final Map<String, ModulePojo> modulesInFirstRun = calculateMetricsAndAssert("WMIN12196A").stream()
				.collect(Collectors.toMap(ModulePojo::getName, module -> module));
		final List<ModulePojo> modulesInSecondRun = calculateMetricsAndAssert("WMIN12196B");
		modulesInSecondRun.forEach(module2 -> {
			final Optional<String> dependencyHash2 = module2.getDependencyHash();
			final Optional<String> dependencyHash1 = modulesInFirstRun.get(module2.getName()).getDependencyHash();
			assertEquals(dependencyHash1.get(), dependencyHash2.get(),
					String.format("Dependency hash for modules in both the runs should be same for %s", module2.getName()));
		});
	}

	private List<ModulePojo> calculateMetricsAndAssert(final String projectName) {
		final EntityId projectId = createProject(projectName).identity();
		assertEquals(0, moduleService.findModuleIds(q -> q.ofProject(projectId)).size());
		sourceService.resetCaches();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
		final List<ModulePojo> modules = moduleService.findModules(q -> q.ofProject(projectId).withNames(Arrays.asList("TEST", "TESTJOBA", "CC1", "TESTPRC")));
		assertFalse("Modules should be present", modules.isEmpty());
		for (final ModulePojo module: modules) {
			assertTrue(String.format("Dependency hash should be present for %s", module.getName()), module.getDependencyHash().isPresent());
		}
		return modules;
	}
	
	protected ProjectPojo createProject(final String projectName) {
		return projectService.create(new ProjectPojoPrototype()
				.setName(projectName)
				.setClient(EntityId.of(Long.valueOf(1)))
				.setNatures(new HashSet<>(Arrays.asList(ProjectNature.DISCOVERY))));
	}
}
