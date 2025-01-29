/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery.dna;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static innowake.mining.shared.access.ProjectService.PLACEHOLDER_XML_CONFIG_KEY;

import java.util.Collections;
import java.util.Date;
import java.util.Map;
import java.util.Optional;

import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.dna.FindCommunitiesJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.ConfigResources;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;

/**
 * Abstract class for testing Discover Dna exporter jobs
 */
@WithMockUser
public abstract class DiscoverDnaJobTest extends BaseDiscoveryTest {
	
	@Autowired
	protected DnaDataService dnaDataService;

	/**
	 * Method to create project and run Discover Code, Discover Metrics and Discover Dna.
	 */
	@BeforeEach
	void createAndDiscover() {
		projectId = EntityId.of(createProject().getId());
		uploadResources(assertNotNull(projectId));
		sourceService.resetCaches();
		submitJob(jobManager, tracer, new DiscoverCodeJob(assertNotNull(projectId)));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(assertNotNull(projectId), false));
		submitJob(jobManager, tracer, new FindCommunitiesJob(assertNotNull(projectId)));
	}
	
	/**
	 * Updates the similarity threshold and metrics date of the project and performs Discovery DNA on the project.
	 *
	 * @param similarityThreshold the {@link Similarity} threshold
	 * @param metricsDate the date when the metrics is performed
	 */
	protected void updateProjectConfigurationsAndRunDna(final String similarityThreshold, final Optional<Date> metricsDate) {
		final ProjectPojo project = projectService.get(assertNotNull(projectId));
		ProjectPojoPrototype updateProjectPojo = new ProjectPojoPrototype();
		updateProjectPojo.setNid(assertNotNull(project).getId());
		updateProjectPojo.setMetricsDate(project.getMetricsDate());

		if (metricsDate.isPresent()) {
			updateProjectPojo.setMetricsDate(metricsDate.get().toInstant());
		}
		final String dnaSimilarityConfigName = ConfigResources.DNA_SIMILARITY_PROCESSOR_CONFIG.getResourceName();
		final Map<String, Map<String, Object>> configurations = projectService.getConfigs(assertNotNull(projectId));
		if (configurations.containsKey(dnaSimilarityConfigName)) {
			projectService.putConfig(assertNotNull(projectId), dnaSimilarityConfigName, Collections.singletonMap(PLACEHOLDER_XML_CONFIG_KEY, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n" + "<properties>\r\n"
					+ "    <property key=\"similarity threshold\" value=\"" + similarityThreshold + "\" title=\"Similarity Threshold\">\r\n"
					+ "        <comment>Define the minimum similarity for the dna to be processed by the community detection. Range 0.0-1.0</comment>\r\n"
					+ "    </property>\r\n" + "</properties>"));
		}
		projectService.update(updateProjectPojo);
		BaseDiscoveryTest.submitJob(jobManager, tracer, new FindCommunitiesJob(assertNotNull(projectId)));
	}

	protected void assertCount(final long dnaSnapshotCount, final long dnaStringCount, final long dnaStringElementCount, final long dnaSimilarityCount,
			final long dnaCommunityCount) {
		assertEquals(dnaSnapshotCount, getDnaSnapshotCount());
		assertEquals(dnaStringCount, getDnaStringCount());
		assertEquals(dnaStringElementCount, getDnaStringElementCount());
		assertEquals(dnaSimilarityCount, getDnaSimilarityCount());
		assertEquals(dnaCommunityCount, getDnaCommunityCountOfLatestSnapshot());
	}


	private long getDnaSnapshotCount() {
		return dnaDataService.findSnapshots(builder -> builder.ofProject(assertNotNull(projectId))).size();
	}

	private long getDnaStringCount() {
		return dnaDataService.getDnaStringCount(builder -> builder.ofProject(assertNotNull(projectId)));
	}

	private long getDnaStringElementCount() {
		return dnaDataService.getDnaStringElementCount(builder -> builder.ofProject(assertNotNull(projectId)));
	}

	private long getDnaSimilarityCount() {
		return dnaDataService.getDnaSimilarityCount(builder -> builder.ofProject(assertNotNull(projectId)));
	}

	private long getDnaCommunityCountOfLatestSnapshot() {
		return dnaDataService.latestSnapshot(assertNotNull(projectId))
					.map(dnaSnapshot -> Long.valueOf(dnaDataService.findCommunities(builder -> builder.ofSnapshot(dnaSnapshot.getId())).size()))
					.orElse(Long.valueOf(0))
					.longValue();
	}
}
