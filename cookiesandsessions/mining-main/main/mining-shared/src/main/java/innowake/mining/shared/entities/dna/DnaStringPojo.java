/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dna;

import java.time.Instant;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.access.EntityId;

/**
 * {@code dna_string} entity class.
 */
public class DnaStringPojo {

	private final EntityId module;
	private final DnaSequencer sequencer;
	private final Instant generated;
	private final byte[] contentHash;

	/**
	 * Constructor.
	 * 
	 * @param module the ID of the {@code Module}
	 * @param sequencer the name of the DNA sequencer
	 * @param generated the timestamp of generation
	 * @param contentHash the content hash of the {@code Module}
	 */
	@JsonCreator
	public DnaStringPojo(@JsonProperty final EntityId module,
						 @JsonProperty final DnaSequencer sequencer,
						 @JsonProperty final Instant generated,
						 @JsonProperty final byte[] contentHash) {
		this.module = module;
		this.sequencer = sequencer;
		this.generated = generated;
		this.contentHash = contentHash;
	}

	/**
	 * @return the ID of the {@code Module}
	 */
	public EntityId getModule() {
		return module;
	}

	/**
	 * @return the {@linkplain DnaSequencer}
	 */
	public DnaSequencer getSequencer() {
		return sequencer;
	}

	/**
	 * @return the timestamp of generation
	 */
	public Instant getGenerated() {
		return generated;
	}

	/**
	 * @return the content hash of the {@code Module}
	 */
	@JsonProperty("content_hash")
	public byte[] getContentHash() {
		return contentHash;
	}
}
