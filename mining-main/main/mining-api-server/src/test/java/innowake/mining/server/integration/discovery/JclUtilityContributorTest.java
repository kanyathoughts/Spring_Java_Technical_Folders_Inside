/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JclContributorContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JclUtilityContributor;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;
import innowake.ndt.jcl.parser.model.StepExec;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;

import java.io.IOException;
import java.nio.file.Path;

import static innowake.lib.core.lang.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test class for testing JclUtilityContributor.
 */
@Import(JclUtilityContributorTest.JclDummyUtilityContributor.class)
@WithMockUser
class JclUtilityContributorTest extends BaseDiscoveryTest {

	@Override
	protected String getTestFolder() {
		return "JclUtilityContributor";
	}

	@Test
	void testForCustomContributor() {
		final Path expectedFile = getExpectedFile(getExpectedFileName());
		final Path expectedEffortFile = getExpectedFile(getExpectedEffortFileName());
		final EntityId projectId = createProject().identity();
		assertEquals(0, moduleService.countErrorMarkers(q -> q.ofProject(projectId)));
		sourceService.resetCaches();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
		
		try {
			final String actualWorkbook = getMetricsContentAsCsv(projectId);
			final String actualEffortContent = getEffortContentAsCsv(projectId);
			if (isWriteExpected()) {
				writeExpected(expectedFile, actualWorkbook);
				writeExpected(expectedEffortFile, actualEffortContent);
			} else {
				DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), actualWorkbook);
				DiscoveryExcelUtil.compareIgnoreOrder(read(expectedEffortFile), actualEffortContent);
			}
		} catch (final IOException e) {
			fail("Fail to export excel . Exception occured : " + e.getMessage());
		}
	}

	public static class JclDummyUtilityContributor implements JclUtilityContributor {

		@Override
		public boolean accept(final StepExec stepExec) {
			return "DUMMY-UTIL".equals(stepExec.getProperties().get("PGM"));
		}

		@Override
		public void contribute(final JclContributorContext context, final StepExec stepExec, final DiscoveryBuilder.ModuleBuilder stepModule) {
			stepModule.declareStatement(StatementType.UNKNOWN).setText("Random text : " + stepExec.getFullyQualifiedId()).setTechnology(Technology.JCL);
		}
	}
}
