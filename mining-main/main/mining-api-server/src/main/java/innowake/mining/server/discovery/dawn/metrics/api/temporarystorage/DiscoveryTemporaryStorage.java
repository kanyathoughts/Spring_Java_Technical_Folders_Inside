/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.temporarystorage;

import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.server.discovery.dawn.metrics.api.model.DeferredActionDefinition;
import innowake.mining.shared.access.EntityId;

import java.util.Collection;

/**
 * Interface for storing objects to the temporary storage.
 */
public interface DiscoveryTemporaryStorage {
	
	/**
	 * Stores the unanchored contributor result to the temporary storage.
	 * 
	 * @param jobId id of the job in which the discovery is executed
	 * @param result the {@link ContributorResult}
	 */
	void storeUnanchoredContributorResult(String jobId, ContributorResult result);

	/**
	 * Gets the contributor results and removes them from temporary storage.
	 *
	 * @param jobId id of the job in which the discovery is executed
	 * @return {@link ContributorResult}s
	 */
	Collection<ContributorResult> getAndRemoveUnanchoredContributorResults(String jobId);
	
	/**
	 * Stores the {@linkplain DeferredActionDefinition} to the temporary storage.
	 *
	 * @param jobId id of the job in which the discovery is executed
	 * @param moduleId the Module Id.
	 * @param actionDefinition the {@link DeferredActionDefinition}
	 */
	void storeDeferredAction(String jobId, EntityId moduleId, DeferredActionDefinition actionDefinition);

	/**
	 * Checks if there are modules with deferred action for the given job
	 *
	 * @param jobId The id of the job
	 * @return true if there are modules with deferred action for the given job else false
	 */
	boolean hasModulesWithDeferredActions(String jobId);

	/**
	 * Gets the Module's primary key which consists of {@linkplain DeferredActionDefinition}s from the temporary storage.
	 *
	 * @param jobId id of the job in which the discovery is executed
	 * @return Module Id's of those with {@link DeferredActionDefinition}
	 */
	Collection<EntityId> getAndRemoveModulesWithDeferredActions(String jobId);
	
	/**
	 * Gets the {@linkplain DeferredActionDefinition}s and removes them from the temporary storage.
	 *
	 * @param jobId id of the job in which the discovery is executed
	 * @param moduleId the primary key of the module
	 * @return {@link DeferredActionDefinition}s for the given moduleId
	 */
	Collection<DeferredActionDefinition> getAndRemoveDeferredActions(String jobId, EntityId moduleId);
}
