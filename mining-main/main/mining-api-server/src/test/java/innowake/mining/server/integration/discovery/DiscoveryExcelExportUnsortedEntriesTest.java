/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;

/**
 * Tests if the discovery excel export has unsorted entries above a threshold value.
 */
@WithMockUser
@SpringBootTest("configuration.discoveryExportSortThreshold=1")
class DiscoveryExcelExportUnsortedEntriesTest extends BaseDiscoveryTest {
	
	/**
	 * The discovery exported excel should have unsorted entries and hence should not match with
	 * the discovery dump file (having sorted entries) if compared while preserving order and match otherwise.
	 */
	@Test
	void discoverExcelUnsorted() {
		/* The expected dump file is present with the sorted entries. */
		final Path expectedFile = getExpectedFile(getExpectedFileName());
		final EntityId projectId = assertNotNull(createProject()).identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
		try {
			final String actualWorkbook = getMetricsContentAsCsv(projectId);
			if (isWriteExpected()) {
				writeExpected(expectedFile, actualWorkbook);
			} else {
				/* The expected csv (sorted) should not be exact copy of the generated csv (unsorted) due to 
				 * threshold set as 1 (see property configuration.discoveryExportSortThreshold on test class).*/
				assertFalse(DiscoveryExcelUtil.comparePreservingOrder(read(expectedFile), actualWorkbook, Set.of("Uid", "Target Uid"), Collections.emptyMap()),
						"The actual generated excel is sorted despite low threshold. Expected an unsorted file.");
				DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), actualWorkbook);
			}
		} catch (final IOException e) {
			fail("Fail to export excel . Exception occured : " + e.getMessage());
			e.printStackTrace();
		}
	}

	@Override
	protected String getTestFolder() {
		return "WMIN2160";
	}
}
