/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;


import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Type;

/**
 * Tests whether correct storage type is set to the JCL control cards
 */
@WithMockUser
class JclControlCardStorageTypeTest extends BaseDiscoveryTest {

	@Test
	void testStorageType() {
		final Long projectId = performDiscovery();
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(EntityId.of(projectId)).withName("CCARD"));
		assertEquals(1, modules.size());
		assertEquals(Type.CONTROLCARD, modules.get(0).getType());
		assertEquals(Storage.FILE_SECTION, modules.get(0).getStorage());
	}

	public Long performDiscovery() {
		sourceService.resetCaches();
		final EntityId projectId = createProject().identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
		return projectId.getNid();
	}

	@Override
	public String getTestFolder() {
		return "JclStorageType";
	}
}
