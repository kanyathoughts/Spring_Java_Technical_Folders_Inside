/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.dna.export;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import innowake.mining.data.model.discovery.dna.DnaConfig;
import innowake.mining.data.model.discovery.dna.ModelCluster;
import innowake.mining.data.model.discovery.dna.ModelClustering;
import innowake.mining.data.model.discovery.dna.ModelDna;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.dna.DnaClusterAlgorithm;
import innowake.mining.shared.entities.dna.DnaCommunityPojo;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSnapshotPojo;

/**
 * Builds Dna Model.
 */
public class DnaModelBuilder {

	private final DnaDataService dnaData;

	/**
	 * create an instance of {@link DnaModelBuilder}.
	 *
	 * @param dnaData the {@link DnaDataService}
	 */
	public DnaModelBuilder(final DnaDataService dnaData) {
		this.dnaData = dnaData;
	}

	/**
	 * Retrieves the {@link ModelDna} with the given DnaSnapshot.
	 *
	 * @param dnaSnapshot the DnaSnapshot
	 * @return the {@linkModelDna} for the given DnaSnapshot
	 */
	public ModelDna buildModel(final DnaSnapshotPojo dnaSnapshot) {
		final ModelDna modelDna = new ModelDna(dnaSnapshot.getTotalModuleCount());
		for (final DnaSequencer sequencerId : DnaSequencer.values()) {
			for (final DnaSimilarityAlgorithm similarityId : DnaSimilarityAlgorithm.values()) {
				final List<DnaCommunityPojo> dnaCommunities = dnaData
						.findCommunities(q -> q.ofSnapshot(dnaSnapshot.getId()).ofSequencer(sequencerId).withSimilarityAlgorithm(similarityId)
													.sortCount(SortDirection.DESCENDING));
				if ( ! dnaCommunities.isEmpty()) {
					final Map<DnaClusterAlgorithm, List<DnaCommunityPojo>> clusterLine = dnaCommunities.stream()
							.collect(Collectors.groupingBy(DnaCommunityPojo::getClusterAlgorithm));
					addClusteringsToModelDna(sequencerId, similarityId, clusterLine, DnaConfig.fromMap(dnaSnapshot.getDnaConfig()), modelDna);
				}
			}
		}
		return modelDna;
	}

	private void addClusteringsToModelDna(final DnaSequencer sequencerId, final DnaSimilarityAlgorithm similarityId,
			final Map<DnaClusterAlgorithm, List<DnaCommunityPojo>> clusterLine, final DnaConfig dnaConfig, final ModelDna modelDna) {
		for (final Map.Entry<DnaClusterAlgorithm, List<DnaCommunityPojo>> clusters : clusterLine.entrySet()) {
			final ModelClustering clustering = new ModelClustering();
			modelDna.getClusterings().add(clustering);
			clustering.getAlgorithm().put("Sequencer", sequencerId.getId());
			clustering.getAlgorithm().put("Similarity", similarityId.getId());
			clustering.getAlgorithm().put("Clustering", clusters.getKey().getId());
			clustering.setOptions(DnaConfig.getDnaConfigurationOptions(dnaConfig));
			clusters.getValue().forEach(c -> {
				final int moduleCount = c.getModuleCount();
				final String title = c.getTitle() == null ? "" : c.getTitle();
				final String clusterDescription = c.getDescription() == null ? "" : c.getDescription();
				if (moduleCount > 0) {
					final var modelCluster = new ModelCluster(moduleCount, clusterDescription , c.getClusterIndex(), String.valueOf(c.getId()), 
							title);
					clustering.getClusters().add(modelCluster);
				}
			});
		}
	}
}
