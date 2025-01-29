/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.Type;

@WithMockUser
class CsdContainsModuleTest extends BaseDiscoveryTest {

	/**
	 * Tests that transactions and resource files in a CSD list have the proper ContainsModule edges.
	 */
	@Test
	void test() {
		final EntityId projectId = performDiscovery();
		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));

		final List<ModulePojo> csdLists = modules.stream().filter( m -> m.getType() == Type.LIST ).collect(Collectors.toList());
		assertEquals(1, csdLists.size());
		final ModulePojo csdList = csdLists.get(0);

		final List<ModulePojo> transactions = modules.stream().filter( m -> m.getType() == Type.TRANSACTION ).collect(Collectors.toList());
		assertEquals(4, transactions.size());
		for (final ModulePojo transaction : transactions) {
			final Optional<ModuleRelationshipPojo> containsModule = moduleService
					.findAnyRelationship(q -> q.ofSource(csdList.identity()).ofDestination(transaction.identity()));
			assertTrue("Missing ContainsModule edge!", containsModule.isPresent());
		}
	}

	public EntityId performDiscovery() {
		sourceService.resetCaches();
		final EntityId entityId = createProject().identity();
		uploadResources(entityId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(entityId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(entityId, false));
		return entityId;
	}

	@Override
	public String getTestFolder() {
		return "csd";
	}
}
