/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.discovery.dna;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.dna.DnaNeighboringSimilarity;
import innowake.mining.server.discovery.Logging;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSnapshotPojo;

/**
 * Generates export files for Dna neighboring modules.
 */
@Service
public class NeighboringModulesGenerator implements DiscoverDnaGenerator{
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.DNA);
	
	@Autowired
	private transient DnaDataService dnaDataService;
	
	@Override
	public List<Tuple2<String, byte[]>> build(final EntityId projectId) {
		final List<Tuple2<String, byte[]>> neighboringModules = new ArrayList<>();
		final Optional<DnaSnapshotPojo> snapshotId = dnaDataService.latestSnapshot(projectId);
		if (snapshotId.isEmpty()) {
			LOG.error(() -> "No Dna snapshot exist for current project");
			return neighboringModules;
		}
		for (final DnaSimilarityAlgorithm similarityId : DnaSimilarityAlgorithm.values()) {
			for (final DnaSequencer sequencerId : DnaSequencer.values()) {
				final Optional<Tuple2<String, byte[]>> neighboringSimilarities
						= getNeighboringSimilarities(similarityId, sequencerId, snapshotId.get().getId());
				if (neighboringSimilarities.isPresent()) {
					neighboringModules.add(neighboringSimilarities.get());
				}
			}
		}
		return neighboringModules;
	}
	
	private Optional<Tuple2<String, byte[]>> getNeighboringSimilarities(final DnaSimilarityAlgorithm similarityId, final DnaSequencer sequencerId,
			final UUID snapshotId) {
		LOG.info(() -> "Generating Neighboring modules for " + sequencerId.getId() + ", " + similarityId.getId());
		final Map<UUID, Integer> clusterIndexAndModuleRid = dnaDataService.getClusterIndexAndModuleRidBySnapshot(similarityId, sequencerId, snapshotId);
		final List<DnaNeighboringSimilarity> result = clusterIndexAndModuleRid.entrySet().parallelStream()
				.map(entry -> DnaNeighboringSimilarity.create(dnaDataService.findNeighboringModules(entry.getKey(), sequencerId, similarityId), entry.getValue()))
				.filter(Objects::nonNull)
				.collect(Collectors.toList());
		
		if (result.isEmpty()) {
			LOG.info(() -> "No Neighboring modules found for " + sequencerId.getId() + ", " + similarityId.getId());
			return Optional.empty();
		}
		LOG.info(() -> "Completed Generating Neighboring modules for " + sequencerId.getId() + ", " + similarityId.getId());
		return Optional.of((new Tuple2<>("Modules_in_" + sequencerId + "_Cluster.csv", generateCsv(result))));
	}
	
	private byte[] generateCsv(final List<DnaNeighboringSimilarity> result) {
		final StringBuilder sb = new StringBuilder();
		sb.append("Cluster Index, Module,First Neighbor,First Neighbor Similarity,Second Neighbor,Second Neighbor Similarity,Third Neighbor,"
				+ "Third Neighbor Similarity,Avg of Top 3 Similarity Weights\n");
		sb.append(result.stream()
				.sorted(Comparator.comparing(DnaNeighboringSimilarity::getClusterIndex).thenComparing(DnaNeighboringSimilarity::getModulePath))
				.map(DnaNeighboringSimilarity::toString)
				.collect(Collectors.joining("\n")));
		return sb.toString().getBytes(StandardCharsets.UTF_8);
	}
}
