/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.temporarystorage;

import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.server.discovery.dawn.metrics.api.model.DeferredActionDefinition;
import innowake.mining.server.discovery.dawn.metrics.api.temporarystorage.DiscoveryTemporaryStorage;
import innowake.mining.shared.access.EntityId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation for {@linkplain DiscoveryTemporaryStorage}
 */
@Service
public class DiscoveryTemporaryStorageImpl implements DiscoveryTemporaryStorage {
	
	public static final String UNACHOR_KEY = "CONTRIBUTOR_RESULT_UNANCHORED";
	public static final String DEFERRED_ACTIONS_MODULEIDS = "DEFERRED_ACTIONS_MODULEIDS";
	public static final String DEFERRED_ACTIONS_FOR_MODULE = "DEFERRED_ACTIONS_FOR_MODULEID-";
	
	@Autowired
	private DiscoveryCache discoveryCache;

	@Override
	public void storeUnanchoredContributorResult(final String jobId, final ContributorResult result) {
		discoveryCache.putMultiValue(jobId, UNACHOR_KEY, result);
	}

	@Override
	public Collection<ContributorResult> getAndRemoveUnanchoredContributorResults(final String jobId) {
		final Collection<ContributorResult> contributorResults = discoveryCache.getMultiValue(jobId, UNACHOR_KEY).stream()
				.map(ContributorResult.class::cast).collect(Collectors.toList());
		discoveryCache.removeMultiValue(jobId, UNACHOR_KEY);
		return contributorResults;
	}

	@Override
	public void storeDeferredAction(final String jobId, final EntityId moduleId, final DeferredActionDefinition actionDefinition) {
		discoveryCache.putMultiValue(jobId, DEFERRED_ACTIONS_FOR_MODULE + moduleId, actionDefinition);
		discoveryCache.putMultiValue(jobId, DEFERRED_ACTIONS_MODULEIDS, moduleId);
	}

	@Override
	public boolean hasModulesWithDeferredActions(final String jobId) {
		return discoveryCache.hasMultiValue(jobId, DEFERRED_ACTIONS_MODULEIDS);
	}

	@Override
	public Collection<EntityId> getAndRemoveModulesWithDeferredActions(final String jobId) {
		final List<EntityId> modulesWithDeferredActions = discoveryCache.getMultiValue(jobId, DEFERRED_ACTIONS_MODULEIDS).stream()
				.map(EntityId.class::cast)
				.collect(Collectors.toList());
		discoveryCache.removeMultiValue(jobId, DEFERRED_ACTIONS_MODULEIDS);
		return modulesWithDeferredActions;
	}

	@Override
	public Collection<DeferredActionDefinition> getAndRemoveDeferredActions(final String jobId, final EntityId moduleId) {
		final Collection<DeferredActionDefinition> deferredActions = discoveryCache.getMultiValue(jobId,
				DEFERRED_ACTIONS_FOR_MODULE + moduleId).stream()
				.map(DeferredActionDefinition.class::cast).collect(Collectors.toList());
		discoveryCache.removeMultiValue(jobId, DEFERRED_ACTIONS_FOR_MODULE + moduleId);
		return deferredActions;
	}
}
