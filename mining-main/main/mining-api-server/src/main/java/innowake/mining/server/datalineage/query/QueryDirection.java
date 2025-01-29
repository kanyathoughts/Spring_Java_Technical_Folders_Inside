/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.query;

/**
 * Which direction to follow the data flow from the start nodes or start module.
 */
public enum QueryDirection {
	/**
	 * Will trace all write accesses to data fields, in order to trace
	 * the flow of data back to its origin.
	 */
	UPSTREAM,
	/**
	 * Will trace all read accesses to data fields, in order to trace
	 * the flow of data to its destination.
	 */
	DOWNSTREAM,
	/**
	 * Will trace both {@link #UPSTREAM} and {@link #DOWNSTREAM}.
	 */
	BOTH
}
