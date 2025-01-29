/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.cache.invalidation;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.event.ModulesModifiedEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event handling service that notifies {@link CacheInvalidationStrategy} if the modules
 * are modified.
 */
@Component
public class ModulesChangeHandler {
	
	private static final Logger LOG = LoggerFactory.getLogger(ModulesChangeHandler.class);
	
	private CacheInvalidationStrategy cacheInvalidationStrategy;
	
	/**
	 * Creates {@code ModulesChangeHandler}.
	 *
	 * @param cacheInvalidationStrategy the CacheInvalidationStrategy that clears the cache
	 */
	@Autowired
	public ModulesChangeHandler(final CacheInvalidationStrategy cacheInvalidationStrategy) {
		this.cacheInvalidationStrategy = cacheInvalidationStrategy;
	}
	
	/**
	 * Event listener that is notified when modules are modified. It then calls
	 * the CacheInvalidationStrategy that clears specific caches.
	 *
	 * @param event the {@code ModulesModifiedEvent}
	 */
	@EventListener
	public void onModulesModified(final ModulesModifiedEvent event) {
		LOG.trace("invalidating 'aggregatedValues', 'hotSpots', 'moduleStatistics' and 'latestModelDna' because Modules are modified");
		final Optional<EntityId> projectId = event.getProjectId();
		if (projectId.isPresent()) {
			cacheInvalidationStrategy.clearAggregatedValues(projectId.get());
			cacheInvalidationStrategy.clearHotSpots(projectId.get());
			cacheInvalidationStrategy.clearModuleStatistics(projectId.get());
			cacheInvalidationStrategy.clearLatestModelDna(projectId.get());
			cacheInvalidationStrategy.clearSavedSearch(projectId.get());
			cacheInvalidationStrategy.clearTaxonomyAggregation(projectId.get());
			cacheInvalidationStrategy.clearTaxonomyCategoryAggregation(projectId.get());
		}
	}
}
