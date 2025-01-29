/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dna;

import java.time.Instant;

import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.PojoPrototype;

/**
 * {@code dna_string} entity request class.
 */
public class DnaStringPojoPrototype implements PojoPrototype {

	public final Definable<EntityId> module = new Definable<>(false, "DnaString.module");
	public final Definable<DnaSequencer> sequencer = new Definable<>(false, "DnaString.sequencer");
	public final Definable<Instant> generated = new Definable<>(false, "DnaString.generated");
	public final Definable<byte[]> contentHash = new Definable<>(false, "DnaString.content_hash");

	/**
	 * Sets the ID of the  {@code Module}
	 *
	 * @param module the module ID
	 * @return this instance for method chaining
	 */
	public DnaStringPojoPrototype setModule(final EntityId module) {
		this.module.set(module);
		return this;
	}

	/**
	 * Sets the DNA sequencer name
	 *
	 * @param sequencer the {@linkplain DnaSequencer}
	 * @return this instance for method chaining
	 */
	public DnaStringPojoPrototype setSequencer(final DnaSequencer sequencer) {
		this.sequencer.set(sequencer);
		return this;
	}

	/**
	 * Sets the timestamp of generation
	 *
	 * @param generated the timestamp of generation
	 * @return this instance for method chaining
	 */
	public DnaStringPojoPrototype setGenerated(final Instant generated) {
		this.generated.set(generated);
		return this;
	}

	/**
	 * Sets the content hash of the {@code Module}
	 *
	 * @param contentHash the content hash of the {@code Module}
	 * @return this instance for method chaining
	 */
	@JsonProperty("content_hash")
	public DnaStringPojoPrototype setContentHash(final byte[] contentHash) {
		this.contentHash.set(contentHash);
		return this;
	}
}
