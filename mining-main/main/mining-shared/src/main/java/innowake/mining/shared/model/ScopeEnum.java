/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * Represents available scopes for shared saved searches
 */
@Entity(name = "ScopeEnum")
@MiningDataType(name = "ScopeEnum")
public enum ScopeEnum {

	/**
	 * The saved search only available to the author
	 */
	INDIVIDUAL,
	/**
	 * The saved search available to all team members within the project
	 */
	PROJECT,
	/**
	 * The saved search available to all team members for all projects under a client
	 */
	CLIENT,
	/**
	 * The saved search available to all mining instances
	 */
	GLOBAL;
	
	/**
	 * Returns the scope given a name.
	 * <p>
	 * The comparison is case-sensitive.
	 *
	 * @param name the name of the scope
	 * @return the scope
	 */
	public static ScopeEnum scope(final String name) {
		for (final ScopeEnum value : values()) {
			if (value.name().equals(name)) {
				return value;
			}
		}
		throw new IllegalArgumentException(String.format("No enum constant %s available", name));
	}
}
 