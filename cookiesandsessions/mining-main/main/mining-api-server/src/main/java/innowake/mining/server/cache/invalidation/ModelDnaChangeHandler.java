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
import innowake.mining.server.event.ModelDnaModifiedEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event handling service that notifies {@link CacheInvalidationStrategy} if the Model DNA
 * is modified.
 */
@Component
public class ModelDnaChangeHandler {
	
	private static final Logger LOG = LoggerFactory.getLogger(ModelDnaChangeHandler.class);
	
	private CacheInvalidationStrategy cacheInvalidationStrategy;
	
	/**
	 * Creates {@code ModelDnaChangeHandler}.
	 *
	 * @param cacheInvalidationStrategy the CacheInvalidationStrategy that clears the cache
	 */
	@Autowired
	public ModelDnaChangeHandler(final CacheInvalidationStrategy cacheInvalidationStrategy) {
		this.cacheInvalidationStrategy = cacheInvalidationStrategy;
	}
	
	/**
	 * Event listener that is notified when Model DNA are modified. It then calls
	 * the CacheInvalidationStrategy that clears specific caches.
	 *
	 * @param event the {@code ModelDnaModifiedEvent}
	 */
	@EventListener
	public void onModelDnaModified(final ModelDnaModifiedEvent event) {
		LOG.trace("invalidating 'latestModelDna' because Model Dna are modified");
		final Optional<EntityId> projectId = event.getProjectId();
		if (projectId.isPresent()) {
			cacheInvalidationStrategy.clearLatestModelDna(projectId.get());
		}
	}
}
