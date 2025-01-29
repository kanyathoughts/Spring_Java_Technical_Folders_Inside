/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.number.OrderingComparison.greaterThan;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.BDDMockito.given;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.info.BuildProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;

@WithMockUser
class DiscoveryMetricsVersionTest extends BaseDiscoveryTest {
	
	private static final Integer ZERO = Integer.valueOf(0);
	private static final String SERVER_VERSION = "Metrics 3000";
	
	@MockBean
	@Nullable
	private BuildProperties buildProperties;
	
	@BeforeEach
	public void init() {
		mockServerVersion(SERVER_VERSION);
	}

	@Override
	protected DiscoverMetricsJob createDiscoverMetricsJob(final EntityId projectId) {
		return new DiscoverMetricsJob(projectId, true);
	}

	@Test
	void metricsVersionIsSetAfterSuccessfulMetricsRun() {
		final ProjectPojo projectBeforeDiscoverMetricsRun = setupProject();
		final String metricsVersionBeforeMetrics = projectBeforeDiscoverMetricsRun.getMetricsVersion();
		assertNull(metricsVersionBeforeMetrics);

		final Long projectId = projectBeforeDiscoverMetricsRun.getId();
		performDiscovery(EntityId.of(projectId));
		
		final ProjectPojo projectAfterDiscoveryMetrics = getProject(EntityId.of(projectId));
		final String metricsVersionAfterMetrics = projectAfterDiscoveryMetrics.getMetricsVersion();
		assertThat(metricsVersionAfterMetrics, is(SERVER_VERSION));
	}
	
	@Test
	void mismatchingVersionsTriggerFullScanOnDiscoveryMetrics() {
		final EntityId projectId = setupProject().identity();

		/* Initial discovery run */
		performDiscovery(projectId);

		/* All lines of code must be positive */
		for (final ModulePojo module : moduleService.findModules(builder -> builder.ofProject(projectId))) {
			final SourceMetricsPojo sourceMetrics = module.getSourceMetrics().orElseThrow();
			assertThat(sourceMetrics.getCodeLines(), is(greaterThan(ZERO)));

			/* Setting lines of code to zero */
			moduleService.putSourceMetrics(new SourceMetricsPojoPrototype()
					.setModule(module.identity())
					.setCodeLines(ZERO)
				);
		}
		
		/* Verify that all modules now have a line count of zero */
		for (final ModulePojo module : moduleService.findModules(builder -> builder.ofProject(projectId))) {
			assertThat(module.getSourceMetrics().orElseThrow().getCodeLines(), is(ZERO));
		}
		
		/* Change the version number */
		mockServerVersion("Version for second run of discovery");
		
		/* Re-run discovery with a different version has to re-discover all modules */
		performDiscovery(projectId);

		/* Now all modules must have been re-discovered and therefore the line numbers must not be zero again */
		for (final ModulePojo module : moduleService.findModules(builder -> builder.ofProject(projectId))) {
			assertThat(module.getSourceMetrics().orElseThrow().getCodeLines(), is(greaterThan(ZERO)));
		}
	}
	
	public void performDiscovery(final EntityId projectId) {
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, createDiscoverMetricsJob(projectId));
	}

	@Override
	public String getTestFolder() {
		return "metricsVersion";
	}
	
	private ProjectPojo setupProject() {
		sourceService.resetCaches();
		final EntityId projectId = assertNotNull(createProject()).identity();
		uploadResources(projectId);
		return getProject(projectId);
	}

	private ProjectPojo getProject(final EntityId projectId) {
		return assertNotNull(projectService.get(projectId));
	}

	private void mockServerVersion(final String version) {
		final BuildProperties buildProperties2 = buildProperties;
		if (buildProperties2 != null) {
			given(buildProperties2.getVersion()).willReturn(version);
		} else {
			fail("BuildProperties was not properly injected.");
		}
	}
}
