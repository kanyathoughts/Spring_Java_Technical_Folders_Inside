/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dna;

import java.util.UUID;

import innowake.mining.shared.Definable;
import innowake.mining.shared.entities.PojoPrototype;

/**
 * {@code dna_community} entity request class.
 */
public class DnaCommunityPojoPrototype implements PojoPrototype {
	
	public final Definable<UUID> id = new Definable<>(false, "DnaCommunity.id");
	public final Definable<UUID> snapshot = new Definable<>(false, "DnaCommunity.snapshot");
	public final Definable<DnaSequencer> sequencerId = new Definable<>(false, "DnaCommunity.sequencerId");
	public final Definable<DnaSimilarityAlgorithm> similarityId = new Definable<>(false, "DnaCommunity.similarityId");
	public final Definable<DnaClusterAlgorithm> clusterAlgorithmId = new Definable<>(false, "DnaCommunity.clusterAlgorithmId");
	public final Definable<Integer> clusterIndex = new Definable<>(false, "DnaCommunity.clusterIndex");
	public final Definable<String> title = new Definable<>(true, "DnaCommunity.title");
	public final Definable<String> description = new Definable<>(true, "DnaCommunity.description");

	/**
	 * Sets the ID of the {@code dna_community}.
	 *
	 * @param id the {@link UUID}
	 * @return this instance for method chaining
	 */
	public DnaCommunityPojoPrototype setId(final UUID id) {
		this.id.set(id);
		return this;
	}

	/**
	 * Sets the ID of the {@code dna_snapshot}.
	 *
	 * @param snapshot the snapshot {@link UUID}
	 * @return this instance for method chaining
	 */
	public DnaCommunityPojoPrototype setSnapshot(final UUID snapshot) {
		this.snapshot.set(snapshot);
		return this;
	}

	/**
	 * Sets the ID of the sequencer that was used for this {@code dna_community}.
	 *
	 * @param sequencerId the {@link DnaSequencer}
	 * @return this instance for method chaining
	 */
	public DnaCommunityPojoPrototype setSequencerId(final DnaSequencer sequencerId) {
		this.sequencerId.set(sequencerId);
		return this;
	}

	/**
	 * Sets the {@link DnaSimilarityAlgorithm} that was used for this {@code dna_community}.
	 *
	 * @param similarityId the {@link DnaSimilarityAlgorithm}
	 * @return this instance for method chaining
	 */
	public DnaCommunityPojoPrototype setSimilarityId(final DnaSimilarityAlgorithm similarityId) {
		this.similarityId.set(similarityId);
		return this;
	}

	/**
	 * Sets the {@link DnaClusterAlgorithm} that was used for this {@code dna_community}.
	 *
	 * @param clusterAlgorithmId the {@link DnaClusterAlgorithm}
	 * @return this instance for method chaining
	 */
	public DnaCommunityPojoPrototype setClusterAlgorithmId(final DnaClusterAlgorithm clusterAlgorithmId) {
		this.clusterAlgorithmId.set(clusterAlgorithmId);
		return this;
	}

	/**
	 * Sets the cluster index of this {@code dna_community}.
	 *
	 * @param clusterIndex the cluster index
	 * @return this instance for method chaining
	 */
	public DnaCommunityPojoPrototype setClusterIndex(final Integer clusterIndex) {
		this.clusterIndex.set(clusterIndex);
		return this;
	}

	/**
	 * Sets the title of this {@code dna_community}.
	 *
	 * @param title the title
	 * @return this instance for method chaining
	 */
	public DnaCommunityPojoPrototype setTitle(final String title) {
		this.title.set(title);
		return this;
	}

	/**
	 * Sets the description of this {@code dna_community}.
	 *
	 * @param description the description
	 * @return this instance for method chaining
	 */
	public DnaCommunityPojoPrototype setDescription(final String description) {
		this.description.set(description);
		return this;
	}
}
