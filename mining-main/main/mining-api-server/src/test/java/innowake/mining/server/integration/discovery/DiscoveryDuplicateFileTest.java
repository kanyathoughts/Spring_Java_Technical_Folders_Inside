/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.fail;

import java.io.IOException;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;

/**
 * Tests if the duplicate files are being identified with their dependencies.
 */
@WithMockUser
class DiscoveryDuplicateFileTest extends BaseDiscoveryTest {
	/**
	 * Test to create project and run Discover Code twice and then validate dump.
	 *
	 * WMIN-2082 fixed this issue
	 */
	@Test
	void discoverCodeDuplicates() {
		final Path expectedFile = getExpectedFile(getExpectedFileName());
		final EntityId projectId = assertNotNull(createProject()).identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
		try {
			if (isWriteExpected()) {
				writeExpected(expectedFile, getMetricsContentAsCsv(projectId));
			} else {
				DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), getMetricsContentAsCsv(projectId));
			}
		} catch (final IOException e) {
			fail("Fail to export excel . Exception occured : " + e.getMessage());
			e.printStackTrace();
		}
	}

	@Override
	protected String getTestFolder() {
		return "WMIN2082";
	}

}
