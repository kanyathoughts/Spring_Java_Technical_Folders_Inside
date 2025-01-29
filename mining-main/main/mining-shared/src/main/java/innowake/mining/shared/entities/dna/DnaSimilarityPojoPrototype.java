/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dna;

import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.PojoPrototype;

/**
 * {@code dna_similarity} entity request class, which represent a similarity entry between two DNA strings with a double similarity score between 0-1.
 */
public class DnaSimilarityPojoPrototype implements PojoPrototype {

	public final Definable<EntityId> aModule = new Definable<>(false, "DnaSimilarity.a_module");
	public final Definable<EntityId> bModule = new Definable<>(false, "DnaSimilarity.b_module");
	public final Definable<DnaSequencer> sequencer = new Definable<>(false, "DnaSimilarity.sequencer");
	public final Definable<DnaSimilarityAlgorithm> similarityAlgorithm = new Definable<>(false, "DnaSimilarity.similarity_algo");
	public final Definable<Double> similarity = new Definable<>(false, "DnaSimilarity.similarity");

	/**
	 * Sets the ID of the first {@code module} that was compared.
	 *
	 * @param aModule the ID of the first {@code module}
	 * @return this instance for method chaining
	 */
	@JsonProperty("a_module")
	public DnaSimilarityPojoPrototype setAModule(final EntityId aModule) {
		this.aModule.set(aModule);
		return this;
	}

	/**
	 * Sets the ID of the second {@code module} that was compared.
	 *
	 * @param bModule the ID of the second {@code module}
	 * @return this instance for method chaining
	 */
	@JsonProperty("b_module")
	public DnaSimilarityPojoPrototype setBModule(final EntityId bModule) {
		this.bModule.set(bModule);
		return this;
	}

	/**
	 * Sets the DNA sequencer name
	 *
	 * @param sequencer the name of the DNA sequencer
	 * @return this instance for method chaining
	 */
	public DnaSimilarityPojoPrototype setSequencer(final DnaSequencer sequencer) {
		this.sequencer.set(sequencer);
		return this;
	}

	/**
	 * Sets the name of the used DNA similarity algorithm
	 *
	 * @param similarityAlgorithm the name of the used DNA similarity algorithm
	 * @return this instance for method chaining
	 */
	@JsonProperty("similarity_algo")
	public DnaSimilarityPojoPrototype setSimilarityAlgorithm(final DnaSimilarityAlgorithm similarityAlgorithm) {
		this.similarityAlgorithm.set(similarityAlgorithm);
		return this;
	}

	/**
	 * Sets the similarity score value
	 *	
	 * @param similarity the similarity score value
	 * @return this instance for method chaining
	 */
	public DnaSimilarityPojoPrototype setSimilarity(final double similarity) {
		this.similarity.set(Double.valueOf(similarity));
		return this;
	}
}
