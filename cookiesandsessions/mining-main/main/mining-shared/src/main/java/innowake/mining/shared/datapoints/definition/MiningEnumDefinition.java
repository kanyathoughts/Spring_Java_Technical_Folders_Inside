/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition;

import java.util.List;

import innowake.lib.core.api.lang.Nullable;

/**
 * Definition of an enumeration type.
 */
public class MiningEnumDefinition extends MiningSchemaClass {

	private final List<String> values;
	@Nullable
	private final Class<? extends Enum<?>> representedBy;
	
	public MiningEnumDefinition(final String name, @Nullable final Class<? extends Enum<?>> representedBy, final List<String> values) {
		super(name, representedBy);
		this.representedBy = representedBy;
		this.values = values;
	}
	
	/**
	 * Returns the values of this enum.
	 *
	 * @return the enum values
	 */
	public List<String> getValues() {
		return values;
	}

	@Nullable
	@SuppressWarnings("unchecked")
	public <T extends Enum<T>> Class<T> getRepresentedBy() {
		return (Class<T>) representedBy;
	}

	@Override
	public String toString() {
		return "MiningEnumDefinition [name=" + name + ", values=" + values + "]";
	}

}
