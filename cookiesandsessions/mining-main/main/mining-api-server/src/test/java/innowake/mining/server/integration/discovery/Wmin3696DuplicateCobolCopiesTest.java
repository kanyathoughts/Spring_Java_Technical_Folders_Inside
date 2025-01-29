/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;

/**
 * Tests that Cobol copies are collected case insensitive and that no duplicates are created if the same copy
 * is included once lower case and once upper case.
 */
@WithMockUser
class Wmin3696DuplicateCobolCopiesTest extends BaseDiscoveryTest {

	/**
	 * Collects the {@code LNCY270.cbl} and {@code LNCY290.cbl} Cobol modules which both include the same copy, one with name is lower case and one with the
	 * name in upper case.
	 * <p>Tests that the discovery creates only one module for the copy.</p>
	 */
	@Test
	void duplicatedCopybooks() {
		final EntityId projectId = createProject().identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));

		assertEquals(1, moduleService.countModules(b -> b.ofProject(projectId).withNames(List.of("lncvb070"), true)));
		assertEquals(1, moduleService.countModules(b -> b.ofProject(projectId).withNames(List.of("LNCVB070"), true)));
	}

	@Override
	protected String getTestFolder() {
		return "WMIN3696";
	}

}
