/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dna;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleLocation;

/**
 * {@code dna_string} entity class.
 */
public class DnaStringElementPojo {

	private final EntityId module;
	private final DnaSequencer sequencer;
	private final int index;
	private final ModuleLocation location;
	private final String value;

	/**
	 * Constructor.
	 * 
	 * @param module the ID of the {@code module}
	 * @param sequencer the {@linkplain DnaSequencer}
	 * @param index the index of the element in the DNA string
	 * @param location the {@linkplain ModuleLocation} of the element
	 * @param value the value of the DNA string element
	 */
	@JsonCreator
	public DnaStringElementPojo(@JsonProperty final EntityId module,
								@JsonProperty final DnaSequencer sequencer,
								@JsonProperty final int index,
								@JsonProperty final ModuleLocation location,
								@JsonProperty final String value) {
		this.module = module;
		this.sequencer = sequencer;
		this.index = index;
		this.location = location;
		this.value = value;
	}

	/**
	 * @return the ID of the {@code module}
	 */
	public EntityId getModule() {
		return module;
	}

	/**
	 * @return the name of the DNA sequencer
	 */
	public DnaSequencer getSequencer() {
		return sequencer;
	}

	/**
	 * @return the index of this element in the DNA string
	 */
	public int getIndex() {
		return index;
	}

	/**
	 * @return the {@linkplain ModuleLocation} of the element
	 */
	public ModuleLocation getLocation() {
		return location;
	}

	/**
	 * @return the value of the element
	 */
	public String getValue() {
		return value;
	}
}
