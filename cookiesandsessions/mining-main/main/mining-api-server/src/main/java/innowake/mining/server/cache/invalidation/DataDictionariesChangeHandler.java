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
import innowake.mining.server.event.DataDictionariesModifiedEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event handling service that notifies {@link CacheInvalidationStrategy} if the Data Dictionaries
 * are modified.
 */
@Component
public class DataDictionariesChangeHandler {
	
	private static final Logger LOG = LoggerFactory.getLogger(DataDictionariesChangeHandler.class);
	
	private CacheInvalidationStrategy cacheInvalidationStrategy;
	
	/**
	 * Creates {@code DataDictionariesChangeHandler}.
	 *
	 * @param cacheInvalidationStrategy the CacheInvalidationStrategy that clears the cache
	 */
	@Autowired
	public DataDictionariesChangeHandler(final CacheInvalidationStrategy cacheInvalidationStrategy) {
		this.cacheInvalidationStrategy = cacheInvalidationStrategy;
	}
	
	/**
	 * Event listener that is notified when Data Dictionaries are modified. It then calls
	 * the CacheInvalidationStrategy that clears specific caches.
	 *
	 * @param event the {@code DataDictionariesChangeHandler}
	 */
	@EventListener
	public void onDataDictionariesModified(final DataDictionariesModifiedEvent event) {
		LOG.trace("invalidating 'aggregatedValues', 'hotSpots' because Data Dictionaries are modified");
		final Optional<EntityId> projectId = event.getProjectId();
		if (projectId.isPresent()) {
			cacheInvalidationStrategy.clearAggregatedValues(projectId.get());
			cacheInvalidationStrategy.clearHotSpots(projectId.get());
			cacheInvalidationStrategy.clearSavedSearch(projectId.get());
		}
	}
}
