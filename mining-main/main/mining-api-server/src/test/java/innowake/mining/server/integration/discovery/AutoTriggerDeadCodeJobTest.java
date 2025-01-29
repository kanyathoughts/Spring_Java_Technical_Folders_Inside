/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.job.deadcode.IdentifyDeadCodeJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.SourceMetricsPojo;

/**
 * Tests for dead code calculation using {@link IdentifyDeadCodeJob} as apart of discover metrics.
 */
@WithMockUser
class AutoTriggerDeadCodeJobTest extends BaseDiscoveryTest {

	@Override
	protected String getTestFolder() {
		return "WMIN11656";
	}

	/**
	 * Tests the dead code calculation through {@link IdentifyDeadCodeJob} as a part of discover metrics.
	 */
	@Test
	void testDeadCodeLineCountCalculation() {
		projectId = assertNotNull(createProject()).identity();
		final EntityId nonNullProjectId = assertNotNull(projectId);
		
		sourceService.resetCaches();
		uploadResources(assertNotNull(projectId));
		submitJob(jobManager, tracer, new DiscoverCodeJob(nonNullProjectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(nonNullProjectId, false));
		
		final Optional<ModulePojo> module = moduleService.findAnyModule(q -> q.ofProject(nonNullProjectId).withPath("src/cobol/WMIN11656/programs/DEAD10.cbl"));
		assertTrue(module.isPresent());
		final Optional<SourceMetricsPojo> sourceMetrics = assertNotNull(module.get().getSourceMetrics());
		assertEquals(5, sourceMetrics.get().getDeadCodeLines());
	}
}
