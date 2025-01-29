/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.Technology;

/**
 * Test case to validate model artifacts creation even though resource collection fails.
 */
@WithMockUser
class DiscoveryMetricsErrorHandlerTest extends BaseDiscoveryTest {

	@Test
	void discoverError() {
		final Path expectedFile = getExpectedFile(getExpectedFileName());
		final EntityId projectId = assertNotNull(createProject()).identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		
		/* Modify the SourceObject in order to throw an exception during resource collector phase. */
		final List<SourcePojo> sourceObjects = sourceService.find(q -> q.ofProject(projectId));
		sourceService.update(new SourcePojoPrototype()
				.withId(sourceObjects.get(0).identity())
				.setTechnology(Technology.RESOURCE));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, true));
		try {
			if (isWriteExpected()) {
				writeExpected(expectedFile, getMetricsContentAsCsv(projectId));
			} else {
				DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), getMetricsContentAsCsv(projectId));
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}

	}

	@Override
	protected String getTestFolder() {
		return "WMIN2291";
	}

}
