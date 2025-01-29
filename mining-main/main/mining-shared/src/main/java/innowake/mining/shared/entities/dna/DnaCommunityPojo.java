/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dna;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;

/**
 * {@code community} entity class.
 */
public class DnaCommunityPojo {
	
	private final UUID id;
	private final UUID snapshot;
	private final DnaSequencer sequencerId;
	private final DnaSimilarityAlgorithm similarityAlgorithm;
	private final DnaClusterAlgorithm clusterAlgorithm;
	private final int clusterIndex;
	private final Instant updatedTime;
	@Nullable
	private final String title;
	@Nullable
	private final String description;
	private final List<EntityId> modules;

	/**
	 * Constructor
	 * 
	 * @param id the ID of the {@code dna_community}
	 * @param snapshot the ID of the {@code dna_snapshot}
	 * @param sequencerId the ID of the sequencer that was used for this {@code dna_community}
	 * @param similarityAlogrithm the {@link DnaSimilarityAlgorithm} that was used for this {@code dna_community}
	 * @param clusterAlgorithm the {@link DnaClusterAlgorithm} that was used for this {@code dna_community}
	 * @param clusterIndex the cluster index of this {@code dna_community}
	 * @param title the title of this {@code dna_community}
	 * @param description the description of this {@code dna_community}
	 * @param modules the modules that were contained in this {@code dna_community}
	 * @param updatedTime the Updated time of the snapshots
	 */
	@JsonCreator
	public DnaCommunityPojo(@JsonProperty final UUID id,
							@JsonProperty final UUID snapshot,
							@JsonProperty final DnaSequencer sequencerId,
							@JsonProperty final DnaSimilarityAlgorithm similarityAlogrithm,
							@JsonProperty final DnaClusterAlgorithm clusterAlgorithm,
							@JsonProperty final int clusterIndex,
							@JsonProperty final @Nullable String title,
							@JsonProperty final @Nullable String description,
							@JsonProperty final List<EntityId> modules,
							@JsonProperty final Instant updatedTime) {
		super();
		this.id = id;
		this.snapshot = snapshot;
		this.sequencerId = sequencerId;
		this.similarityAlgorithm = similarityAlogrithm;
		this.clusterAlgorithm = clusterAlgorithm;
		this.clusterIndex = clusterIndex;
		this.title = title;
		this.description = description;
		this.modules = modules;
		this.updatedTime = updatedTime;
	}

	/**
	 * @return the ID of the {@code dna_community}
	 */
	public UUID getId() {
		return id;
	}

	/**
	 * @return the ID of the {@code dna_snapshot}
	 */
	public UUID getSnapshot() {
		return snapshot;
	}

	/**
	 * @return the ID of the sequencer that was used for this {@code dna_community}
	 */
	public DnaSequencer getSequencerId() {
		return sequencerId;
	}

	/**
	 * @return the {@link DnaSimilarityAlgorithm} that was used for this {@code dna_community}
	 */
	public DnaSimilarityAlgorithm getSimilarityAlgorithm() {
		return similarityAlgorithm;
	}

	/**
	 * @return the {@link DnaClusterAlgorithm} that was used for this {@code dna_community}
	 */
	public DnaClusterAlgorithm getClusterAlgorithm() {
		return clusterAlgorithm;
	}

	/**
	 * @return the cluster index of this {@code dna_community}
	 */
	public int getClusterIndex() {
		return clusterIndex;
	}

	/**
	 * @return the title of this {@code dna_community}
	 */
	@Nullable
	public String getTitle() {
		return title;
	}

	/**
	 * @return the description of this {@code dna_community}
	 */
	@Nullable
	public String getDescription() {
		return description;
	}

	/**
	 * @return the modules that were contained in this {@code dna_community}
	 */
	public List<EntityId> getModules() {
		return modules;
	}

	/**
	 * @return the number of modules that were contained in this {@code dna_community}
	 */
	public int getModuleCount() {
		return modules.size();
	}
	
	/**
	 * @return the timestamp when this {@code dna_community} got updated
	 */
	public Instant getUpdated() {
		return updatedTime;
	}
	
}
