/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.job;

import java.util.Set;

import innowake.mining.shared.access.EntityId;

/**
 * Provides the results of a parsing operation.
 */
public interface ParsingSummary {

	/**
	 * Returns a collection of successfully processed modules.
	 * @return the successful modules.
	 */
	Set<EntityId> getSuccessfulModules();
	
	/**
	 * Returns a collection of unsuccessfully processed modules.
	 * @return the unsuccessful modules.
	 */
	Set<EntityId> getUnsuccessfulModules();
	
	/**
	 * Returns a collection of unsupported modules.
	 * @return the unsupported modules.
	 */
	Set<EntityId> getUnsupportedModules();
}
