/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Optional;

import innowake.ndt.core.parsing.ILocationMultiline;

/**
 * Represent one dna entry. If possible the dna source location is set.
 * It is not always possible to trace back the location.
 */
public class DNAItem {
	
	private final Optional<ILocationMultiline> location;
	private final DNAValueProvider provider;
	
	/**
	 * Create a new dna item with a function providing the dna value.
	 * The function is only called when a consumer is interested in the dna value (or {@link #toString()} is called.)
	 * This constructor set the location to {@link Optional#empty()}.
	 * 
	 * @param primitiveCallback The callback function to create the dna value.
	 */
	public DNAItem(final DNAValueProvider primitiveCallback) {
		this.provider = assertNotNull(primitiveCallback);
		this.location  = Optional.empty();
	}
	
	/**
	 * Create a new dna item with a function providing the dna value.
	 * The function is only called when a consumer is interested in the dna value (or {@link #toString()} is called.)
	 * 
	 * @param primitiveCallback The callback function to create the dna value.
	 * @param location The source location of the dna value.
	 */
	public DNAItem(final DNAValueProvider primitiveCallback, final Optional<ILocationMultiline> location) {
		this.provider = assertNotNull(primitiveCallback);
		this.location  = location;
	}

	/**
	 * Get the dna value provider instance.
	 *
	 * @return The dna value provider.
	 */
	public DNAValueProvider getProvider() {
		return provider;
	}
	
	/**
	 * Get the dna value by executing the provider
	 *
	 * @return The dna value
	 */
	public String getValue() {
		return provider.getValue();
	}
	
	/**
	 * Get the dna source location.
	 *
	 * @return The dna source location or {@link Optional#empty()} if not available.
	 */
	public Optional<ILocationMultiline> getLocation() {
		return location;
	}
	
	@Override
	public String toString() {
		return getValue() + ":" + location;
	}

}
