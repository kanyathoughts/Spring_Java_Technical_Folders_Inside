/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.cache.invalidation;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Component;

import innowake.mining.server.cache.MiningCacheConfig;
import innowake.mining.server.util.CacheNameUtil;
import innowake.mining.shared.access.EntityId;

@Component
public class CacheInvalidationStrategy {
	
	@Autowired
	private CacheManager cacheManager;
	@Autowired
	private MiningCacheConfig cacheConfig;

	/**
	 * Creates {@code CacheInvalidationStrategy}.	
	 *
	 * @param cacheManager the CacheManager for evicting all including caches
	 */
	public CacheInvalidationStrategy(final CacheManager cacheManager) {
		this.cacheManager = cacheManager;
	}

	/**
	 * Clear all the values from project specific aggregatedValues cache.
	 * 
	 * @param projectId the id of the project for cache eviction
	 */
	public void clearAggregatedValues(final EntityId projectId) {
		evictAllCacheValues(CacheNameUtil.getProjectCacheName(MiningCacheConfig.AGGREGATION_CACHE, cacheConfig.ensureProjectNid(projectId)));
	}

	/**
	 * Clear all the values from project specific hotSpots cache.
	 * 
	 * @param projectId the id of the project for cache eviction
	 */
	public void clearHotSpots(final EntityId projectId) {		
		evictAllCacheValues(CacheNameUtil.getProjectCacheName(MiningCacheConfig.HOTSPOT_CACHE, cacheConfig.ensureProjectNid(projectId)));
	}
	
	/**
	 * Clear all the values from project specific moduleStatistics cache.
	 * 
	 * @param projectId the id of the project for cache eviction
	 */
	public void clearModuleStatistics(final EntityId projectId) {
		evictAllCacheValues(CacheNameUtil.getProjectCacheName(MiningCacheConfig.MODULE_STATISTICS, cacheConfig.ensureProjectNid(projectId)));
	}
	
	/**
	 * Clear all the values from project specific TaxonomyAggregation cache.
	 * 
	 * @param projectId the id of the project for cache eviction
	 */
	public void clearTaxonomyAggregation(final EntityId projectId) {
		evictAllCacheValues(CacheNameUtil.getProjectCacheName(MiningCacheConfig.TAXONOMY_AGGREGATION_CACHE, cacheConfig.ensureProjectNid(projectId)));
	}
	
	/**
	 * Clear all the values from project specific TaxonomyCategoryAggregation cache.
	 * 
	 * @param projectId the id of the project for cache eviction
	 */
	public void clearTaxonomyCategoryAggregation(final EntityId projectId) {
		evictAllCacheValues(CacheNameUtil.getProjectCacheName(MiningCacheConfig.TAXONOMY_CATEGORY_AGGREGATION_CACHE, cacheConfig.ensureProjectNid(projectId)));
	}
	
	/**
	 * Clear all the values from project specific latestModelDna cache.
	 * 
	 * @param projectId the id of the project for cache eviction
	 */
	public void clearLatestModelDna(final EntityId projectId) {
		evictAllCacheValues(CacheNameUtil.getProjectCacheName(MiningCacheConfig.LATEST_MODEL_DNA, cacheConfig.ensureProjectNid(projectId)));
	}
	
	/**
	 * Clear all the values from project specific savedSearch cache.
	 * 
	 * @param projectId the id of the project for cache eviction
	 */
	public void clearSavedSearch(final EntityId projectId) {
		evictAllCacheValues(CacheNameUtil.getProjectCacheName(MiningCacheConfig.SAVED_SEARCH_AGGREGATION, cacheConfig.ensureProjectNid(projectId)));
	}
	
	private void evictAllCacheValues(final String cacheName) {
		final Cache cache = cacheManager.getCache(cacheName);

		if (cache != null) {
			cache.clear();
		}
	}
}
