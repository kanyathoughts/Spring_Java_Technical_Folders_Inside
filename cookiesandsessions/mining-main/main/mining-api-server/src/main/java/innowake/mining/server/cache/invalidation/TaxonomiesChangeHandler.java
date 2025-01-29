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
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event handling service that notifies {@link CacheInvalidationStrategy} if the taxonomies
 * are modified.
 */
@Component
public class TaxonomiesChangeHandler {
	
	private static final Logger LOG = LoggerFactory.getLogger(TaxonomiesChangeHandler.class);
	
	private final CacheInvalidationStrategy cacheInvalidationStrategy;
	
	/**
	 * Creates {@code TaxonomiesChangeHandler}.
	 *
	 * @param cacheInvalidationStrategy the CacheInvalidationStrategy that clears the cache
	 */
	@Autowired
	public TaxonomiesChangeHandler(final CacheInvalidationStrategy cacheInvalidationStrategy) {
		this.cacheInvalidationStrategy = cacheInvalidationStrategy;
	}
	
	/**
	 * Event listener that is notified when taxonomies are modified. It then calls
	 * the CacheInvalidationStrategy that clears specific caches.
	 *
	 * @param event the {@code TaxonomiesModifiedEvent}
	 */
	@EventListener
	public void onTaxonomiesModified(final TaxonomiesModifiedEvent event) {
		LOG.trace("invalidating 'aggregatedValues' 'hotSpots' 'taxonomyAggregation' and 'taxonomyCategoryAggregation' because Taxonomies are modified");
		final Optional<EntityId> projectId = event.getProjectId();
		if (projectId.isPresent()) {
			cacheInvalidationStrategy.clearAggregatedValues(projectId.get());
			cacheInvalidationStrategy.clearHotSpots(projectId.get());
			cacheInvalidationStrategy.clearSavedSearch(projectId.get());
			cacheInvalidationStrategy.clearTaxonomyAggregation(projectId.get());
			cacheInvalidationStrategy.clearTaxonomyCategoryAggregation(projectId.get());
		}
	}
}
