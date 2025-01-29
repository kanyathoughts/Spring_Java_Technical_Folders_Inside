/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.data.io.DiscoveryReferenceImporter;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Test case to verify whether the linkHash for modules is set correctly in {@linkplain DiscoveryReferenceImporter} during discover metrics.
 */
@WithMockUser
class LinkHashTest extends BaseDiscoveryTest {

	@Test
	void testLinkHash() {
		final Long projectId = performDiscovery();

		final List<ModulePojo> virtuals = moduleService.findModules(b -> b.ofProject(EntityId.of(projectId)).withName("CREATURE"));
		assertEquals(1, virtuals.size());
		assertEquals("13cOy1BkHxtrAmOff3RCSW", virtuals.get(0).getLinkHash());

		final List<ModulePojo> physicals = moduleService.findModules(b -> b.ofProject(EntityId.of(projectId)).withName("DEAD1"));
		assertEquals(1, physicals.size());
		assertEquals("5IWbHhYKXeRQlFeU0qHeQy", physicals.get(0).getLinkHash());
	}

	private Long performDiscovery() {
		sourceService.resetCaches();
		final EntityId projectId = createProject().identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, createDiscoverMetricsJob(projectId));
		return projectId.getNid();
	}

	@Override
	public String getTestFolder() {
		return "WMIN4689";
	}
}
