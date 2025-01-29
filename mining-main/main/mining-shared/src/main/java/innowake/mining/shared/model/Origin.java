/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * The origin of a module.
 */
@Entity(name = "OriginEnum")
@MiningDataType(name = "Origin")
public enum Origin {

	/**
	 * Customer specific modules.
	 */
	CUSTOM,
	
	/**
	 * Modules which are provided by the system, like JCL built-ins.
	 */
	ENVIRONMENT

}
