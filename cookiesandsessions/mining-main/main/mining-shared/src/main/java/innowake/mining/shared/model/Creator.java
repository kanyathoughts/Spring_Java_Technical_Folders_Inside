/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Describes the creator of a Module.
 */
@MiningDataType(name = "Creator")
public enum Creator {
	/**
	 * The Module was created by a Discovery contributor.
	 */
	DISCOVERY,
	/**
	 * The Module was created manually through the REST-API or via an import extension.
	 */
	API,
	/**
	 * The Module was created from the scheduler import information.
	 */
	SCHEDULER_INFO
}
