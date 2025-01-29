/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.Serializable;
import java.time.Instant;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;

import org.ff4j.FF4j;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.info.BuildProperties;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Span;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.TaskSource;
import innowake.mining.server.discovery.dawn.metrics.impl.service.DummyDiscoveryService;
import innowake.mining.server.discovery.metrics.MetricsCollector;
import innowake.mining.server.discovery.metrics.MetricsContributorProvider;
import innowake.mining.server.discovery.metrics.PersistMetricsModelService;
import innowake.mining.server.discovery.metrics.TaskHandler;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.configuration.GenericConfiguration;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.tags.DiscoveryTest;

/**
 * Test class for testing the {@link MetricsCollector}.
 */
@WithMockUser
@DiscoveryTest
class MetricsCollectorTest extends DatabaseResettingTest implements TaskHandler {
	@Autowired
	private transient ModuleService moduleService;
	@Autowired
	private transient PersistMetricsModelService persistMetricsModelService;
	@Autowired
	private transient FF4j ff4j;
	@Autowired
	private transient BuildProperties buildProperties;
	@Autowired
	private transient JobConfigurationProperties jobConfig;
	@Autowired
	private transient GenericConfiguration configProperties;
	@Autowired
	private transient SourceCachingService sourceService;
	@Autowired
	private transient GenericConfigProperties genericConfigProperties;
	@Autowired
	private transient MetricsContributorProvider contributorProvider;

	/**
	 * Tests that method {@link MetricsCollector#prepare(ProjectPojo, innowake.lib.job.api.ProgressMonitor, boolean)} returns an empty {@link Optional} for a full
	 * discovery and an {@link Optional} containing a {@link Set} for the incremental discovery.
	 */
	@Test
	void testMetaDataExport() {
		ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setClient(EntityId.of(1L));
		project.setName("MetricsCollectorTest.testMetaDataExport");
		project.setNatures(Collections.emptySet());

		final ProjectPojo projectPojo = projectService.create(project); 
		
		final ModuleParameters moduleParameters = new ModuleParameters(Instant.now());
		final MetricsCollector metricsCollector = new MetricsCollector(sourceService, moduleService, contributorProvider,
				persistMetricsModelService, this, buildProperties, moduleParameters, ff4j, jobConfig, 
				configProperties.isDiscoveryRestartEnabled(), genericConfigProperties, new DummyDiscoveryService());

		final Optional<Set<Long>> prepareFull = metricsCollector.prepare(projectPojo, new NullProgressMonitor(), false);
		assertFalse(prepareFull.isPresent());

		final Optional<Set<Long>> prepareIncremental = metricsCollector.prepare(projectPojo, new NullProgressMonitor(), true);
		assertTrue(prepareIncremental.isPresent());
	}

	@Override
	public JobMonitor getJobMonitor() {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public String getJobId() {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public Span getSpan() {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public <R extends Serializable> void forkJobTasks(TaskSource<R> taskSource, ResultConsumer<R> resultConsumer) {
		throw new UnsupportedOperationException();
	}
}
