/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.community;

import java.util.List;
import java.util.Objects;
import java.util.function.UnaryOperator;
import java.util.UUID;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.dna.community.louvain.LouvainRunner;
import innowake.mining.server.discovery.dna.model.Cluster;
import innowake.mining.server.util.ProgressMonitorThrottle;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.dna.DnaClusterAlgorithm;
import innowake.mining.shared.entities.dna.DnaCommunityPojoPrototype;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSimilarityPojo;

/**
 * Managing class to use similarity information and calculate communities.
 */
public class CommunityDetector {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.DNA);
	private final UnaryOperator<String> configProvider;
	private final Long projectId;
	private final DnaDataService dnaDataService;
	private final ProgressMonitor monitor;

	/**
	 * Create a new instance of the community detector with project access to load the configuration.
	 * @param configProvider Function providing configuration data by configuration name.
	 * @param projectId ID of the Project to operate on.
	 * @param dnaDataService DNA data access service
	 * @param monitor The monitor to show progress and react on user cancel.
	 */
	public CommunityDetector(final UnaryOperator<String> configProvider, final Long projectId,
			final DnaDataService dnaDataService, final ProgressMonitor monitor) {
		this.configProvider = configProvider;
		this.projectId = projectId;
		this.dnaDataService = dnaDataService;
		this.monitor = monitor;

	}

	/**
	 * Execute the community detection.
	 *
	 * @param dnaSimilarities List of {@link DnaSimilarityPojo DnaSimilarityPojos} used to compute the communities.
	 * @param snapshotId ID of the  DNA Snapshot
	 */
	public void detect(final List<DnaSimilarityPojo> dnaSimilarities, final UUID snapshotId) {
		final DnaSimilarityPojo firstDnaSimilarity = dnaSimilarities.get(0);
		final DnaSequencer sequencerId = firstDnaSimilarity.getSequencer();
		final DnaSimilarityAlgorithm similarityId = firstDnaSimilarity.getSimilarityAlgorithm();

		if (monitor.isCanceled()) {
			LOG.info("Community Detection Cancelled");
			return;
		}
		createCommunities(snapshotId, dnaSimilarities, sequencerId, similarityId);
	}

	private void createCommunities(final UUID snapshotId, final List<DnaSimilarityPojo> similarities,
			final DnaSequencer sequencerId, final DnaSimilarityAlgorithm similarityId) {
		if (similarities.isEmpty()) {
			LOG.info("No DnaSimilarities found for {} with {}", sequencerId, similarityId);
			return;
		}
		try {
			final var runner = new LouvainRunner(configProvider);
			final String logMessage = "Find communities for " + sequencerId + " with " + similarityId;
			LOG.info(() -> logMessage);
			ProgressMonitorThrottle.throttleStepDescription(logMessage, monitor);

			final List<Cluster> clusters = runner.execute(similarities, monitor);

			LOG.info(() -> "Now persisting communities into database. Total clusters: " + clusters.size());
			ProgressMonitorThrottle.throttleStepDescription("Now persisting communities into database", monitor);

			for (final Cluster cluster : clusters) {
				final var dnaCommunity = createDnaCommunity(snapshotId, sequencerId, similarityId, cluster.getTitle(), cluster.getClusterNumber() + 1);
				final List<UUID> moduleUnitRids = cluster.getIdentifiers();
				final var communityId = dnaDataService.createCommunity(dnaCommunity, true);
				dnaDataService.putCommunityModules(communityId, moduleUnitRids);

				monitor.worked(moduleUnitRids.size());
			}
			if ( ! clusters.isEmpty()) {
				final List<UUID> moduleUnitRids = dnaDataService.getUnassignedModules(EntityId.of(Objects.requireNonNull(projectId)), sequencerId, snapshotId);
				if ( ! moduleUnitRids.isEmpty()) {
					/* Creating a DNA community for unassigned modules, and this should be created, once all the DNA communities created
					 * for a particular sequencerId.
					 * */
					final var dnaCommunity = createDnaCommunity(snapshotId, sequencerId, similarityId, "Unassigned Modules", -1);
					final var communityId = dnaDataService.createCommunity(dnaCommunity, true);
					dnaDataService.putCommunityModules(communityId, moduleUnitRids);

					monitor.worked(moduleUnitRids.size());
				}
			}
			LOG.info(() -> "Persisted communities into database");
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}

	private DnaCommunityPojoPrototype createDnaCommunity(final UUID snapshotId, final DnaSequencer sequencerId, final DnaSimilarityAlgorithm similarityId,
														 final String title, final int clusterIndex) {
		final var dnaCommunity = new DnaCommunityPojoPrototype();
		dnaCommunity.setSnapshot(snapshotId);
		dnaCommunity.setSequencerId(sequencerId);
		dnaCommunity.setSimilarityId(similarityId);
		dnaCommunity.setClusterAlgorithmId(DnaClusterAlgorithm.LOUVAIN);
		dnaCommunity.setTitle(title);
		dnaCommunity.setClusterIndex(Integer.valueOf(clusterIndex));

		return dnaCommunity;
	}
}
