/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dna;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.PojoPrototype;
import innowake.mining.shared.model.ModuleLocation;

/**
 * {@code dna_string} entity request class.
 */
public class DnaStringElementPojoPrototype implements PojoPrototype {

	public final Definable<EntityId> module = new Definable<>(false, "DnaStringElement.module");
	public final Definable<String> sequencer = new Definable<>(false, "DnaStringElement.sequencer");
	public final Definable<Integer> index = new Definable<>(false, "DnaStringElement.index");
	public final Definable<ModuleLocation> location = new Definable<>(false, "DnaStringElement.location");
	public final Definable<String> value = new Definable<>(false, "DnaStringElement.value");

	/**
	 * Sets the ID of the {@code module}
	 *
	 * @param module the module ID
	 * @return this instance for method chaining
	 */
	public DnaStringElementPojoPrototype setModule(final EntityId module) {
		this.module.set(module);
		return this;
	}

	/**
	 * Sets the DNA sequencer name
	 *
	 * @param sequencer the name of the DNA sequencer
	 * @return this instance for method chaining
	 */
	public DnaStringElementPojoPrototype setSequencer(final String sequencer) {
		this.sequencer.set(sequencer);
		return this;
	}

	/**
	 * Sets the index of this element in the DNA string
	 *
	 * @param index the index of this element in the DNA string
	 * @return this instance for method chaining
	 */
	public DnaStringElementPojoPrototype setIndex(final int index) {
		this.index.set(Integer.valueOf(index));
		return this;
	}

	/**
	 * Sets the {@linkplain ModuleLocation} of the element
	 *
	 * @param location the {@linkplain ModuleLocation} of the element
	 * @return this instance for method chaining
	 */
	public DnaStringElementPojoPrototype setLocation(final ModuleLocation location) {
		this.location.set(location);
		return this;
	}

	/**
	 * Sets the value of the element
	 *
	 * @param value the value of the element
	 * @return this instance for method chaining
	 */
	public DnaStringElementPojoPrototype setValue(final String value) {
		this.value.set(value);
		return this;
	}
}
