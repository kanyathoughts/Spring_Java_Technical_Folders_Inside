/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dna;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.access.EntityId;

/**
 * {@code dna_similarity} entity class which represent a similarity entry between two DNA strings with a double similarity score between 0-1.
 */
public class DnaSimilarityPojo {

	private final EntityId aModule;
	private final EntityId bModule;
	private final DnaSequencer sequencer;
	private final DnaSimilarityAlgorithm similarityAlgorithm;
	private final double similarity;

	/**
	 * Constructor.
	 * 
	 * @param aModule the ID of the first {@code module} that was compared.
	 * @param bModule the ID of the second {@code module} that was compared.
	 * @param sequencer the {@linkplain DnaSequencer}
	 * @param similarityAlgorithm the name of the used DNA similarity algorithm
	 * @param similarity the similarity score for {@code aModule} and {@code bModule}
	 */
	@JsonCreator
	public DnaSimilarityPojo(@JsonProperty final EntityId aModule,
							 @JsonProperty final EntityId bModule,
							 @JsonProperty final DnaSequencer sequencer,
							 @JsonProperty final DnaSimilarityAlgorithm similarityAlgorithm,
							 @JsonProperty final double similarity) {
		this.aModule = aModule;
		this.bModule = bModule;
		this.sequencer = sequencer;
		this.similarityAlgorithm = similarityAlgorithm;
		this.similarity = similarity;
	}

	/**
	 * @return the ID of the first {@code module} that was compared.
	 */
	@JsonProperty("a_module")
	public EntityId getAModule() {
		return aModule;
	}

	/**
	 * @return the ID of the second {@code module} that was compared.
	 */
	@JsonProperty("b_module")
	public EntityId getBModule() {
		return bModule;
	}

	/**
	 * @return the {@linkplain DnaSequencer}
	 */
	public DnaSequencer getSequencer() {
		return sequencer;
	}

	/**
	 * @return the {@linkplain DnaSimilarityAlgorithm}
	 */
	@JsonProperty("similarity_algo")
	public DnaSimilarityAlgorithm getSimilarityAlgorithm() {
		return similarityAlgorithm;
	}

	/**
	 * @return the similarity score value
	 */
	public double getSimilarity() {
		return similarity;
	}
}
