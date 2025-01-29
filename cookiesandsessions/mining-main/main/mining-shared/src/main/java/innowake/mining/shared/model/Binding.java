/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

/** 
 * Enum class for Specifying the binding type.
 * LATE bindings are dependencies detected at runtime
 * EARLY bindings are direct dependencies to static entries
 */
public enum Binding {
	EARLY, 
	LATE, 
	UNKNOWN;

	private static final Logger LOGGER = LoggerFactory.getLogger(Type.class);
	
	/**
	 * Returns the Binding for given a name.
	 * <p>
	 * The comparison is done case-insensitive.
	 *
	 * @param name the name the Binding is associated with.
	 * @return the type mapped with the name or {@link #UNKNOWN} if no match is found
	 */
	public static Binding fromName(final String name) {
		for (final Binding value : values()) {
			if (value.name().equalsIgnoreCase(name)) {
				return value;
			}
		}

		LOGGER.error("No enum constant " + name + " available as Binding, so it is being defaulted to UNKNOWN.");
		return UNKNOWN;
	}
}