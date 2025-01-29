/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

/**
 * Enum for types of functional block links.
 */
public enum FunctionalBlockLinkType {

	/**
	 * Links the access module with the lower bound, to allow to associate access modules with lower bound modules
	 * without having to examine the call chain.
	 */
	RA_ACCESS,

	/**
	 * "RA_SHARED_RESOURCE" indicates that the link was created because of a shared resource (access to the same file or table)
	 */
	RA_SHARED_RESOURCE,
	/**
	 * "RA_FROM_SCHEDULER_INFO" indicates that the link was created using the information from scheduler import
	 */
	RA_FROM_SCHEDULER_INFO,
	DIRECTED,
	CALL_CHAIN
}
