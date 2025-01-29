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
import innowake.mining.server.event.AnnotationEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event handling service that notifies {@link CacheInvalidationStrategy} if the Annotations
 * are modified.
 */
@Component
public class AnnotationsChangeHandler {
	
	private static final Logger LOG = LoggerFactory.getLogger(AnnotationsChangeHandler.class);
	
	private CacheInvalidationStrategy cacheInvalidationStrategy;
	
	/**
	 * Creates {@code AnnotationsChangeHandler}.
	 *
	 * @param cacheInvalidationStrategy the CacheInvalidationStrategy that clears the cache
	 */
	@Autowired
	public AnnotationsChangeHandler(final CacheInvalidationStrategy cacheInvalidationStrategy) {
		this.cacheInvalidationStrategy = cacheInvalidationStrategy;
	}
	
	/**
	 * Event listener that is notified when Annotations are modified. It then calls
	 * the CacheInvalidationStrategy that clears specific caches.
	 *
	 * @param event the {@code AnnotationsModifiedEvent}
	 */
	@EventListener
	public void onAnnotationsModified(final AnnotationEvent event) {
		LOG.trace("invalidating 'aggregatedValues', 'hotSpots' because Annotations are modified");
		final Optional<EntityId> projectId = event.getProjectId();
		if (projectId.isPresent()) {
			cacheInvalidationStrategy.clearAggregatedValues(projectId.get());
			cacheInvalidationStrategy.clearHotSpots(projectId.get());
			cacheInvalidationStrategy.clearSavedSearch(projectId.get());
		}
	}
}
