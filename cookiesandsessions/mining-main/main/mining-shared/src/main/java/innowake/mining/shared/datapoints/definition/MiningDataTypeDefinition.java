/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition;

import innowake.lib.core.api.lang.Nullable;

/**
 * Definition of a mining data type containing {@link MiningDataPointDefinition}.
 */
public class MiningDataTypeDefinition extends MiningSchemaClass {

	public MiningDataTypeDefinition(final String name) {
		this(name, null);
	}

	public MiningDataTypeDefinition(final String name, @Nullable final Class<?> representedBy) {
		super(name, representedBy);
	}

	@Override
	public String toString() {
		return "MiningTypeDefinition [name=" + name + ", className=" + className + "]";
	}
}
