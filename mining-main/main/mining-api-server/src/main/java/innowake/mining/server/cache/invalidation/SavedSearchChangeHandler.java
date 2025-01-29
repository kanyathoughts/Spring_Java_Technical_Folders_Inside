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
import innowake.mining.server.event.SavedSearchModifiedEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event handling service that notifies {@link CacheInvalidationStrategy} if the savedSearches are modified.
 */
@Component
public class SavedSearchChangeHandler {

	private static final Logger LOG = LoggerFactory.getLogger(SavedSearchChangeHandler.class);
	
	private final CacheInvalidationStrategy cacheInvalidationStrategy;
	
	/**
	 * Creates {@code SavedSearchChangeHandler}.
	 *
	 * @param cacheInvalidationStrategy the CacheInvalidationStrategy that clears the cache
	 */
	@Autowired
	public SavedSearchChangeHandler(final CacheInvalidationStrategy cacheInvalidationStrategy) {
		this.cacheInvalidationStrategy = cacheInvalidationStrategy;
	}
	
	/**
	 * Event listener that is notified when saved searches are modified. It then calls
	 * the CacheInvalidationStrategy that clears specific caches.
	 *
	 * @param event the {@code SavedSearchModifiedEvent}
	 */
	@EventListener
	public void onSavedSearch(final SavedSearchModifiedEvent event) {
		LOG.trace("invalidating 'SavedSearchAggregation' because SavedSearches are modified");
		final Optional<EntityId> projectId = event.getProjectId();
		if (projectId.isPresent()) {
			cacheInvalidationStrategy.clearSavedSearch(projectId.get());
		}
	}
	
}
