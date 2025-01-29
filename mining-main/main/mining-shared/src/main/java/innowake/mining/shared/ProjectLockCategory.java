/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared;

/**
 * Defines categories for locks that can be acquired on project level.
 * Only one task can acquire the lock of each category on each project.
 */
public enum ProjectLockCategory {

	/**
	 * Discovery (Code, Metrics, DNA) is currently being executed on the project so no other Discovery task can be run.
	 */
	DISCOVERY,
	/**
	 * Annotations are currently being modified (e.g. by bulk import/export) so modification of Annotations is locked.
	 */
	ANNOTATIONS,
	/**
	 * Taxonomies are currently being modified (e.g. by bulk import/export) so modification of Taxonomies is locked.
	 */
	TAXONOMIES,
	/**
	 * Data Dictionary Entries are currently being modified (e.g. by bulk import/export) so modification of Data Dictionary Entries is locked.
	 */
	DATA_DICTIONARIES,
	
	/**
	 * Modules are currently being modified so modification of Modules is locked.
	 */
	MODULES,
	
	/**
	 * MetaModel are currently being modified so modification of MetaModel is locked.
	 */
	METAMODEL,
	
	/**
	 * References are currently being modified so modification of Reference is locked.
	 */
	REFERENCE,
	
	/**
	 * Data Schemas are currently being modified so modification of Reference is locked.
	 */
	DATA_SCHEMA,
	
	/**
	 * Member for a Keycloak User are currently being modified so modification of Member is locked.
	 */
	MEMBER
}
