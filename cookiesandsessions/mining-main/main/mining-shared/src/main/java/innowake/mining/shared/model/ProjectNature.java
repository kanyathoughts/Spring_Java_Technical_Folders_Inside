/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * The nature associated with a Project.
 * It specifies the Project Natures the user has access to.
 */
@MiningDataType(name = "ProjectNature")
public enum ProjectNature {

	DISCOVERY,
	DISCOVERY_LIGHT,
	MINING,
	DB_CUTTER;

	/**
	 * Returns the {@link ProjectNature} for corresponding name.
	 *
	 * @param name The name associated with the {@link ProjectNature}
	 * @return The corresponding {@link ProjectNature} value
	 * @throws IllegalArgumentException if an invalid name is provided
	 */
	public static ProjectNature fromName(final String name) {
		for (final ProjectNature value : values()) {
			if (value.name().equalsIgnoreCase(name)) {
				return value;
			}
		}
		throw new IllegalArgumentException("Invalid value for ProjectNature: " + name);
	}
}
