/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.model.Type;

/**
 * Test class to test the creation of contains module edge for java virtual modules.
 */
@WithMockUser
class JavaContainsModuleTest extends BaseDiscoveryTest {

	@Override
	protected DiscoverMetricsJob createDiscoverMetricsJob(final EntityId projectId) {
		return new DiscoverMetricsJob(projectId, true);
	}
	
	/**
	 * Tests whether there is a ContainsModule edge from every virtual JAVA Module to a physical one.
	 */
	@Test
	void testIsContainedInMapSet() {
		final EntityId projectId = performDiscovery();
		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
		assertEquals(3, modules.size());

		final Set<Long> physicalModuleIds = modules.stream()
				.filter(m -> Representation.PHYSICAL == m.getRepresentation().orElse(null))
				.map(m -> m.getId())
				.collect(Collectors.toSet());
		assertEquals(1, physicalModuleIds.size());
		for (final ModulePojo module : modules) {
			if (Representation.VIRTUAL == module.getRepresentation().orElse(null)) {
				if (module.getType() == Type.PACKAGE) {
					assertTrue(module.getParent().isEmpty(), "Parent module must not be present for Java package module");
				} else {
					assertTrue(module.getParent().isPresent(), "Parent module must be present for Java virtual module");
					assertTrue(physicalModuleIds.contains(module.getParent().orElseThrow().getNid()));
				}
			}
		}
	}

	public EntityId performDiscovery() {
		sourceService.resetCaches();
		final EntityId projectId = createProject().identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, createDiscoverMetricsJob(projectId));
		return projectId;
	}

	@Override
	public String getTestFolder() {
		return "javaContainsModule";
	}

}
