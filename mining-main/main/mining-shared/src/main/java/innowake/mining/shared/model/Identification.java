/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * Represents the status of a module.
 */
@Entity(name = "IdentificationEnum")
@MiningDataType(name = "Identification")
public enum Identification {

	/**
	 * The module was found in the project
	 */
	IDENTIFIED,
	
	/**
	 * The module was found as a dependency but is not available in the project
	 */
	MISSING;
}
