/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.security;

import java.util.Arrays;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Representation of the different project natures. 
 */
public enum NatureType {

	/**
	 * Nature value representing DISCOVERY-LIGHT.
	 */
	DISCOVERY_LIGHT("discovery-light"),

	/**
	 * Nature value representing DISCOVERY.
	 */
	DISCOVERY("discovery"),

	/**
	 * Nature value representing MINING.
	 */
	MINING("mining");

	public static final Set<String> VALUE_SET = Collections.unmodifiableSet(Arrays.stream(values()).map(NatureType::getValue).collect(Collectors.toSet()));

	private final String value;

	private NatureType(final String value) {
		this.value = value;
	}

	private NatureType() {
		value = name().toLowerCase();
	}

	/**
	 * @return the value used in the raw Keycloak role
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Returns the type given a name.
	 * <p>
	 * The comparison is done case-insensitive.
	 *
	 * @param name the name the type is associated with.
	 * @return the type
	 */
	public static NatureType fromName(final String name) {
		for (final NatureType type : values()) {
			if (type.name().equalsIgnoreCase(name) || type.getValue().equalsIgnoreCase(name)) {
				return type;
			}
		}
		throw new IllegalArgumentException("No enum constant " + name + " available");
	}

}
