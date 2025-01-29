/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.cache;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.cache.Cache;
import org.springframework.cache.Cache.ValueWrapper;
import org.springframework.cache.interceptor.SimpleKey;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.context.ActiveProfiles;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.model.discovery.dna.ModelDna;
import innowake.mining.server.cache.invalidation.CacheInvalidationStrategy;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.event.AnnotationEvent;
import innowake.mining.server.event.DataDictionariesModifiedEvent;
import innowake.mining.server.event.ModelDnaModifiedEvent;
import innowake.mining.server.event.ModulesModifiedEvent;
import innowake.mining.server.event.SavedSearchModifiedEvent;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.util.CacheNameUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.SavedSearchPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.model.HotSpot;
import innowake.mining.shared.model.ModuleFieldName;
import innowake.mining.shared.model.ModuleStatisticsResponse;
import innowake.mining.shared.model.ScopeEnum;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;

/**
 * Test validating the behavior of {@linkplain CacheInvalidationStrategy}
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false)
class MiningCacheInvalidationTest extends DatabaseRelatedTest {
	
	final static EntityId PROJECT_ONE = EntityId.of(Long.valueOf(1L));
	final static SimpleKey HOTSPOT_CACHE_KEY = new SimpleKey(PROJECT_ONE, "REFERENCES", null);
	final static SimpleKey PROJECT_KEY = new SimpleKey(PROJECT_ONE);
	
	@Autowired
	private GuavaCacheManager cacheManager;
	
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	@BeforeEach
	void init(){
		/* Creating Aggregation cache and adding to cache */
		final Object aggregationKey = getAggregationCacheKey();
		createAggregationCache(aggregationKey);
		/* Creating hotspot cache and adding to cache */
		createHotspotsCache(HOTSPOT_CACHE_KEY);
		/* Creating Module Statistics cache and adding to cache*/
		createModuleStatisticsCache(PROJECT_KEY);
		/* Creating Latest Model Dna cache and adding to cache*/
		createModelDnaCache(PROJECT_KEY);
		/* Creating Saved Search cache and adding to cache*/
		createSavedSearchCache(PROJECT_KEY);
		/* Creating Taxonomy Aggregation cache and adding to cache */
		createTaxonomyAggregationCache(PROJECT_KEY);
		/* Creating Taxonomy Category Aggregation cache and adding to cache */
		createTaxonomyCategoryAggregationCache(PROJECT_KEY);
	}
	
	/**
	 * Tests cache invalidation based on {@linkplain ModulesModifiedEvent} trigger
	 */
	@Test
	void testModulesChangeCacheInvalidation() {
		final Cache aggretationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.AGGREGATION_CACHE, PROJECT_ONE));
		final ValueWrapper aggregationValue = Assert.assertNotNull(aggretationCache).get(getAggregationCacheKey());
		assertNotNull(aggregationValue, "Aggregation result is not in cache");
		
		final Cache hotspotsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.HOTSPOT_CACHE, PROJECT_ONE));
		final ValueWrapper hotspotValue = Assert.assertNotNull(hotspotsCache).get(HOTSPOT_CACHE_KEY);
		assertNotNull(hotspotValue, "Hotspot result is not in cache");
		
		final Cache moduleStatisticsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.MODULE_STATISTICS, PROJECT_ONE));
		final ValueWrapper moduleStatisticsValue = Assert.assertNotNull(moduleStatisticsCache).get(PROJECT_KEY);
		assertNotNull(moduleStatisticsValue, "Module Statistics result is not in cache");
		
		final Cache latestDnaSnapshotCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.LATEST_MODEL_DNA, PROJECT_ONE));
		final ValueWrapper latestDnaSnapshotValue = Assert.assertNotNull(latestDnaSnapshotCache).get(PROJECT_KEY);
		assertNotNull(latestDnaSnapshotValue, "Latest Dna Snapshot result is not in cache");
		
		final Cache savedSearchAggregationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.SAVED_SEARCH_AGGREGATION, PROJECT_ONE));
		final ValueWrapper savedSearchAggregationCacheValue = Assert.assertNotNull(savedSearchAggregationCache).get(PROJECT_KEY);
		assertNotNull(savedSearchAggregationCacheValue, "savedSearchAggregation result is not in cache");
		
		/* Triggering event to clear all module related project specific caches */
		eventPublisher.publishEvent(new ModulesModifiedEvent(PROJECT_ONE, Optional.empty()));
		
		final ValueWrapper aggregationInvalidatedValue = Assert.assertNotNull(aggretationCache).get(getAggregationCacheKey());
		assertNull(aggregationInvalidatedValue, "Aggregation result should be invalidated");
		
		final ValueWrapper hotspotsInvalidatedValue = Assert.assertNotNull(hotspotsCache).get(HOTSPOT_CACHE_KEY);
		assertNull(hotspotsInvalidatedValue, "Hotspots result should be invalidated");
		
		final ValueWrapper moduleStatisticsInvalidatedValue = Assert.assertNotNull(moduleStatisticsCache).get(PROJECT_KEY);
		assertNull(moduleStatisticsInvalidatedValue, "Module Statistics result should be invalidated");
		
		final ValueWrapper latestDnaSnapshotInvalidatedValue = Assert.assertNotNull(latestDnaSnapshotCache).get(PROJECT_KEY);
		assertNull(latestDnaSnapshotInvalidatedValue, "Latest Model Dna result should be invalidated");	
		
		final ValueWrapper savedSearcgAggregationInvalidatedValue = Assert.assertNotNull(savedSearchAggregationCache).get(PROJECT_KEY);
		assertNull(savedSearcgAggregationInvalidatedValue, "SavedSearchAggregation result should be invalidated");
	}
	
	/**
	 * Tests cache invalidation based on {@linkplain TaxonomiesModifiedEvent} trigger
	 */
	@Test
	void testTaxonomiesChangeCacheInvalidation() {
		final Cache aggretationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.AGGREGATION_CACHE, PROJECT_ONE));
		final ValueWrapper aggregationValue = Assert.assertNotNull(aggretationCache).get(getAggregationCacheKey());
		assertNotNull(aggregationValue, "Aggregation result is not in cache");
		
		final Cache hotspotsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.HOTSPOT_CACHE, PROJECT_ONE));
		final ValueWrapper hotspotValue = Assert.assertNotNull(hotspotsCache).get(HOTSPOT_CACHE_KEY);
		assertNotNull(hotspotValue, "Hotspot result is not in cache");
		
		final Cache moduleStatisticsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.MODULE_STATISTICS, PROJECT_ONE));
		final ValueWrapper moduleStatisticsValue = Assert.assertNotNull(moduleStatisticsCache).get(PROJECT_KEY);
		assertNotNull(moduleStatisticsValue, "Module Statistics result is not in cache");
		
		final Cache latestDnaSnapshotCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.LATEST_MODEL_DNA, PROJECT_ONE));
		final ValueWrapper latestDnaSnapshotValue = Assert.assertNotNull(latestDnaSnapshotCache).get(PROJECT_KEY);
		assertNotNull(latestDnaSnapshotValue, "Latest Dna Snapshot result is not in cache");
		
		final Cache taxonomyAggregationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.TAXONOMY_AGGREGATION_CACHE, PROJECT_ONE));
		final ValueWrapper taxonomyAggregationCacheValue = Assert.assertNotNull(taxonomyAggregationCache).get(PROJECT_KEY);
		assertNotNull(taxonomyAggregationCacheValue, "taxonomyAggregation result is not in cache");
		
		final Cache taxonomyCategoryAggregationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.TAXONOMY_AGGREGATION_CACHE, PROJECT_ONE));
		final ValueWrapper taxonomyCategoryAggregationCacheValue = Assert.assertNotNull(taxonomyCategoryAggregationCache).get(PROJECT_KEY);
		assertNotNull(taxonomyCategoryAggregationCacheValue, "taxonomyCategoryAggregation result is not in cache");

		final Cache savedSearchAggregationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.SAVED_SEARCH_AGGREGATION, PROJECT_ONE));
		final ValueWrapper savedSearchAggregationCacheValue = Assert.assertNotNull(savedSearchAggregationCache).get(PROJECT_KEY);
		assertNotNull(savedSearchAggregationCacheValue, "savedSearchAggregation result is not in cache");
		
		/* Triggering event to clear all taxonomies related project specific caches */
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(PROJECT_ONE));
		
		final ValueWrapper aggregationInvalidatedValue = Assert.assertNotNull(aggretationCache).get(getAggregationCacheKey());
		assertNull(aggregationInvalidatedValue, "Aggregation result should be invalidated");
		
		final ValueWrapper hotspotsInvalidatedValue = Assert.assertNotNull(hotspotsCache).get(HOTSPOT_CACHE_KEY);
		assertNull(hotspotsInvalidatedValue, "Hotspots result should be invalidated");
		
		final ValueWrapper moduleStatisticsInvalidatedValue = Assert.assertNotNull(moduleStatisticsCache).get(PROJECT_KEY);
		assertNotNull(moduleStatisticsInvalidatedValue, "Module Statistics result should not be invalidated");
		
		final ValueWrapper latestDnaSnapshotInvalidatedValue = Assert.assertNotNull(latestDnaSnapshotCache).get(PROJECT_KEY);
		assertNotNull(latestDnaSnapshotInvalidatedValue, "Latest Model Dna result should not be invalidated");
	
		final ValueWrapper savedSearcgAggregationInvalidatedValue = Assert.assertNotNull(savedSearchAggregationCache).get(PROJECT_KEY);
		assertNull(savedSearcgAggregationInvalidatedValue, "SavedSearchAggregation result should be invalidated");
		final ValueWrapper taxonomyAggregationInvalidatedValue = Assert.assertNotNull(taxonomyAggregationCache).get(PROJECT_KEY);
		assertNull(taxonomyAggregationInvalidatedValue, "taxonomyAggregation result should be invalidated");
		
		final ValueWrapper taxonomyCategoryAggregationInvalidatedValue = Assert.assertNotNull(taxonomyCategoryAggregationCache).get(PROJECT_KEY);
		assertNull(taxonomyCategoryAggregationInvalidatedValue, "taxonomyCategoryAggregation result should be invalidated");
	}
	
	/**
	 * Tests cache invalidation based on {@linkplain ModelDnaModifiedEvent} trigger
	 */
	@Test
	void testModelDnaChangeCacheInvalidation() {
		final Cache aggretationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.AGGREGATION_CACHE, PROJECT_ONE));
		final ValueWrapper aggregationValue = Assert.assertNotNull(aggretationCache).get(getAggregationCacheKey());
		assertNotNull(aggregationValue, "Aggregation result is not in cache");
		
		final Cache hotspotsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.HOTSPOT_CACHE, PROJECT_ONE));
		final ValueWrapper hotspotValue = Assert.assertNotNull(hotspotsCache).get(HOTSPOT_CACHE_KEY);
		assertNotNull(hotspotValue, "Hotspot result is not in cache");
		
		final Cache moduleStatisticsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.MODULE_STATISTICS, PROJECT_ONE));
		final ValueWrapper moduleStatisticsValue = Assert.assertNotNull(moduleStatisticsCache).get(PROJECT_KEY);
		assertNotNull(moduleStatisticsValue, "Module Statistics result is not in cache");
		
		final Cache latestDnaSnapshotCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.LATEST_MODEL_DNA, PROJECT_ONE));
		final ValueWrapper latestDnaSnapshotValue = Assert.assertNotNull(latestDnaSnapshotCache).get(PROJECT_KEY);
		assertNotNull(latestDnaSnapshotValue, "Latest Dna Snapshot result is not in cache");
		
		final Cache savedSearchAggregationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.SAVED_SEARCH_AGGREGATION, PROJECT_ONE));
		final ValueWrapper savedSearchAggregationCacheValue = Assert.assertNotNull(savedSearchAggregationCache).get(PROJECT_KEY);
		assertNotNull(savedSearchAggregationCacheValue, "savedSearchAggregation result is not in cache");
		
		/* Triggering event to clear all model dna related project specific caches */
		eventPublisher.publishEvent(new ModelDnaModifiedEvent(PROJECT_ONE));
		
		final ValueWrapper aggregationInvalidatedValue = Assert.assertNotNull(aggretationCache).get(getAggregationCacheKey());
		assertNotNull(aggregationInvalidatedValue, "Aggregation result should not be invalidated");
		
		final ValueWrapper hotspotsInvalidatedValue = Assert.assertNotNull(hotspotsCache).get(HOTSPOT_CACHE_KEY);
		assertNotNull(hotspotsInvalidatedValue, "Hotspots result should not be invalidated");
		
		final ValueWrapper moduleStatisticsInvalidatedValue = Assert.assertNotNull(moduleStatisticsCache).get(PROJECT_KEY);
		assertNotNull(moduleStatisticsInvalidatedValue, "Module Statistics result should not be invalidated");
		
		final ValueWrapper latestDnaSnapshotInvalidatedValue = Assert.assertNotNull(latestDnaSnapshotCache).get(PROJECT_KEY);
		assertNull(latestDnaSnapshotInvalidatedValue, "Latest Model Dna result should be invalidated");	
	
		final ValueWrapper savedSearcgAggregationInvalidatedValue = Assert.assertNotNull(savedSearchAggregationCache).get(PROJECT_KEY);
		assertNotNull(savedSearcgAggregationInvalidatedValue, "SavedSearchAggregation result should not be invalidated");
	}
	
	/**
	 * Tests cache invalidation based on {@linkplain DataDictionariesModifiedEvent} trigger
	 */
	@Test
	void testDataDictionariesChangeCacheInvalidation() {
		final Cache aggretationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.AGGREGATION_CACHE, PROJECT_ONE));
		final ValueWrapper aggregationValue = Assert.assertNotNull(aggretationCache).get(getAggregationCacheKey());
		assertNotNull(aggregationValue, "Aggregation result is not in cache");
		
		final Cache hotspotsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.HOTSPOT_CACHE, PROJECT_ONE));
		final ValueWrapper hotspotValue = Assert.assertNotNull(hotspotsCache).get(HOTSPOT_CACHE_KEY);
		assertNotNull(hotspotValue, "Hotspot result is not in cache");
		
		final Cache moduleStatisticsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.MODULE_STATISTICS, PROJECT_ONE));
		final ValueWrapper moduleStatisticsValue = Assert.assertNotNull(moduleStatisticsCache).get(PROJECT_KEY);
		assertNotNull(moduleStatisticsValue, "Module Statistics result is not in cache");
		
		final Cache latestDnaSnapshotCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.LATEST_MODEL_DNA, PROJECT_ONE));
		final ValueWrapper latestDnaSnapshotValue = Assert.assertNotNull(latestDnaSnapshotCache).get(PROJECT_KEY);
		assertNotNull(latestDnaSnapshotValue, "Latest Dna Snapshot result is not in cache");
		
		final Cache savedSearchAggregationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.SAVED_SEARCH_AGGREGATION, PROJECT_ONE));
		final ValueWrapper savedSearchAggregationCacheValue = Assert.assertNotNull(savedSearchAggregationCache).get(PROJECT_KEY);
		assertNotNull(savedSearchAggregationCacheValue, "savedSearchAggregation result is not in cache");
		
		/* Triggering event to clear all taxonomies related project specific caches */
		eventPublisher.publishEvent(new DataDictionariesModifiedEvent(PROJECT_ONE));
		
		final ValueWrapper aggregationInvalidatedValue = Assert.assertNotNull(aggretationCache).get(getAggregationCacheKey());
		assertNull(aggregationInvalidatedValue, "Aggregation result should be invalidated");
		
		final ValueWrapper hotspotsInvalidatedValue = Assert.assertNotNull(hotspotsCache).get(HOTSPOT_CACHE_KEY);
		assertNull(hotspotsInvalidatedValue, "Hotspots result should be invalidated");
		
		final ValueWrapper moduleStatisticsInvalidatedValue = Assert.assertNotNull(moduleStatisticsCache).get(PROJECT_KEY);
		assertNotNull(moduleStatisticsInvalidatedValue, "Module Statistics result should not be invalidated");
		
		final ValueWrapper latestDnaSnapshotInvalidatedValue = Assert.assertNotNull(latestDnaSnapshotCache).get(PROJECT_KEY);
		assertNotNull(latestDnaSnapshotInvalidatedValue, "Latest Model Dna result should not be invalidated");	
	
		final ValueWrapper savedSearcgAggregationInvalidatedValue = Assert.assertNotNull(savedSearchAggregationCache).get(PROJECT_KEY);
		assertNull(savedSearcgAggregationInvalidatedValue, "SavedSearchAggregation result should be invalidated");
	}
	
	/**
	 * Tests cache invalidation based on {@linkplain AnnotationEvent} trigger
	 */
	@Test
	void testAnnotationsChangeCacheInvalidation() {
		final Cache aggretationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.AGGREGATION_CACHE, PROJECT_ONE));
		final ValueWrapper aggregationValue = Assert.assertNotNull(aggretationCache).get(getAggregationCacheKey());
		assertNotNull(aggregationValue, "Aggregation result is not in cache");
		
		final Cache hotspotsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.HOTSPOT_CACHE, PROJECT_ONE));
		final ValueWrapper hotspotValue = Assert.assertNotNull(hotspotsCache).get(HOTSPOT_CACHE_KEY);
		assertNotNull(hotspotValue, "Hotspot result is not in cache");
		
		final Cache moduleStatisticsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.MODULE_STATISTICS, PROJECT_ONE));
		final ValueWrapper moduleStatisticsValue = Assert.assertNotNull(moduleStatisticsCache).get(PROJECT_KEY);
		assertNotNull(moduleStatisticsValue, "Module Statistics result is not in cache");
		
		final Cache latestDnaSnapshotCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.LATEST_MODEL_DNA, PROJECT_ONE));
		final ValueWrapper latestDnaSnapshotValue = Assert.assertNotNull(latestDnaSnapshotCache).get(PROJECT_KEY);
		assertNotNull(latestDnaSnapshotValue, "Latest Dna Snapshot result is not in cache");
		
		final Cache savedSearchAggregationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.SAVED_SEARCH_AGGREGATION, PROJECT_ONE));
		final ValueWrapper savedSearchAggregationCacheValue = Assert.assertNotNull(savedSearchAggregationCache).get(PROJECT_KEY);
		assertNotNull(savedSearchAggregationCacheValue, "savedSearchAggregation result is not in cache");
		
		/* Triggering event to clear all taxonomies related project specific caches */
		eventPublisher.publishEvent(new AnnotationEvent(PROJECT_ONE));
		
		final ValueWrapper aggregationInvalidatedValue = Assert.assertNotNull(aggretationCache).get(getAggregationCacheKey());
		assertNull(aggregationInvalidatedValue, "Aggregation result should be invalidated");
		
		final ValueWrapper hotspotsInvalidatedValue = Assert.assertNotNull(hotspotsCache).get(HOTSPOT_CACHE_KEY);
		assertNull(hotspotsInvalidatedValue, "Hotspots result should be invalidated");
		
		final ValueWrapper moduleStatisticsInvalidatedValue = Assert.assertNotNull(moduleStatisticsCache).get(PROJECT_KEY);
		assertNotNull(moduleStatisticsInvalidatedValue, "Module Statistics result should not be invalidated");
		
		final ValueWrapper latestDnaSnapshotInvalidatedValue = Assert.assertNotNull(latestDnaSnapshotCache).get(PROJECT_KEY);
		assertNotNull(latestDnaSnapshotInvalidatedValue, "Latest Model Dna result should not be invalidated");
		
		final ValueWrapper savedSearcgAggregationInvalidatedValue = Assert.assertNotNull(savedSearchAggregationCache).get(PROJECT_KEY);
		assertNull(savedSearcgAggregationInvalidatedValue, "SavedSearchAggregation result should be invalidated");
	}
	
	/**
	 * Tests cache invalidation based on {@linkplain SavedSearchModifiedEvent} trigger
	 */
	@Test
	void testSavedSearchChangeCacheInvalidation() {
		final Cache aggretationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.AGGREGATION_CACHE, PROJECT_ONE));
		final ValueWrapper aggregationValue = Assert.assertNotNull(aggretationCache).get(getAggregationCacheKey());
		assertNotNull(aggregationValue, "Aggregation result is not in cache");
		
		final Cache hotspotsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.HOTSPOT_CACHE, PROJECT_ONE));
		final ValueWrapper hotspotValue = Assert.assertNotNull(hotspotsCache).get(HOTSPOT_CACHE_KEY);
		assertNotNull(hotspotValue, "Hotspot result is not in cache");
		
		final Cache moduleStatisticsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.MODULE_STATISTICS, PROJECT_ONE));
		final ValueWrapper moduleStatisticsValue = Assert.assertNotNull(moduleStatisticsCache).get(PROJECT_KEY);
		assertNotNull(moduleStatisticsValue, "Module Statistics result is not in cache");
		
		final Cache latestDnaSnapshotCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.LATEST_MODEL_DNA, PROJECT_ONE));
		final ValueWrapper latestDnaSnapshotValue = Assert.assertNotNull(latestDnaSnapshotCache).get(PROJECT_KEY);
		assertNotNull(latestDnaSnapshotValue, "Latest Dna Snapshot result is not in cache");
		
		final Cache savedSearchAggregationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.SAVED_SEARCH_AGGREGATION, PROJECT_ONE));
		final ValueWrapper savedSearchAggregationCacheValue = Assert.assertNotNull(savedSearchAggregationCache).get(PROJECT_KEY);
		assertNotNull(savedSearchAggregationCacheValue, "savedSearchAggregation result is not in cache");
		
		/* Triggering event to clear all savedSearch related project specific caches */
		eventPublisher.publishEvent(new SavedSearchModifiedEvent(PROJECT_ONE));
		
		final ValueWrapper savedSearcgAggregationInvalidatedValue = Assert.assertNotNull(savedSearchAggregationCache).get(PROJECT_KEY);
		assertNull(savedSearcgAggregationInvalidatedValue, "SavedSearchAggregation result should be invalidated");
		
		final ValueWrapper aggregationInvalidatedValue = Assert.assertNotNull(aggretationCache).get(getAggregationCacheKey());
		assertNotNull(aggregationInvalidatedValue, "Aggregation result should not be invalidated");
		
		final ValueWrapper hotspotsInvalidatedValue = Assert.assertNotNull(hotspotsCache).get(HOTSPOT_CACHE_KEY);
		assertNotNull(hotspotsInvalidatedValue, "Hotspots result should not be invalidated");
		
		final ValueWrapper moduleStatisticsInvalidatedValue = Assert.assertNotNull(moduleStatisticsCache).get(PROJECT_KEY);
		assertNotNull(moduleStatisticsInvalidatedValue, "Module Statistics result should not be invalidated");
		
		final ValueWrapper latestDnaSnapshotInvalidatedValue = Assert.assertNotNull(latestDnaSnapshotCache).get(PROJECT_KEY);
		assertNotNull(latestDnaSnapshotInvalidatedValue, "Latest Model Dna result should not be invalidated");	
	}

	private Object getAggregationCacheKey() {
		final Set<ModuleFieldName> groupBy = Set.of(ModuleFieldName.TECHNOLOGY);
		final Map<ModuleFieldName, AggregationOperator> fields = Map.of(ModuleFieldName.ID, AggregationOperator.COUNT);
		var cachingKeyPart = new AggregationRequest<ModuleFieldName>();
		cachingKeyPart.setFilterObject(Map.of(ModuleFieldName.PROJECT_ID, Map.of("eq", EntityId.of(1L))));
		cachingKeyPart.setGroupBy(groupBy);
		cachingKeyPart.setFields(fields);
		return new SimpleKey(EntityId.of(1L), cachingKeyPart);
	}
	
	private void createAggregationCache(final Object key) {
		final Map<ModuleFieldName, Object> fieldValue = new HashMap<>();
		fieldValue.put(ModuleFieldName.ID, Long.valueOf(2L));
		final Map<ModuleFieldName, Object> groupValue = new HashMap<>();
		groupValue.put(ModuleFieldName.TECHNOLOGY, "COBOL");
		final AggregationResult<ModuleFieldName> aggregation = new AggregationResult<>();
		aggregation.setFields(fieldValue);
		aggregation.setGroup(groupValue);
		final List<AggregationResult<ModuleFieldName>> aggregationValue = new ArrayList<>();
		aggregationValue.add(aggregation);
		
		/* Creation of cache and adding to cache */
		final Cache aggretationCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.AGGREGATION_CACHE, PROJECT_ONE));
		assertNotNull(aggretationCache, "The aggregate result cache should be created.");
		aggretationCache.put(key, aggregationValue);
	}

	private void createHotspotsCache(final Object key) {
		final List<HotSpot> hotspotValue = Arrays.asList(new HotSpot(new ModulePojoDummy().build(), 1), new HotSpot(new ModulePojoDummy().build(), 2));
		
		/* Creation of cache and adding to cache */
		final Cache hotspotsCache = cacheManager.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.HOTSPOT_CACHE, PROJECT_ONE));
		assertNotNull(hotspotsCache, "The hotspot result cache should be created.");
		hotspotsCache.put(key, hotspotValue);
	}
	
	private void createModuleStatisticsCache(final Object key) {
		final ModuleStatisticsResponse moduleStatisticsValue = new ModuleStatisticsResponse();
		moduleStatisticsValue.setCount(Long.valueOf(2L));
		moduleStatisticsValue.setMissingCount(Long.valueOf(5));
		
		/* Creation of cache and adding to cache */
		final Cache moduleStatisticsCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.MODULE_STATISTICS, PROJECT_ONE));
		assertNotNull(moduleStatisticsCache, "The moduleStatistics result cache should be created.");
		moduleStatisticsCache.put(key, moduleStatisticsValue);
	}
	
	private void createModelDnaCache(final Object key) {
		final ModelDna modelDnaValue = new ModelDna(12);
		
		/*Creation of cache and adding to cache */
		final Cache latestDnaSnapshotCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.LATEST_MODEL_DNA, PROJECT_ONE));
		assertNotNull(latestDnaSnapshotCache, "The latestModelDna result cache should be created.");
		latestDnaSnapshotCache.put(key, modelDnaValue);
	}
	
	private void createSavedSearchCache(final Object key) {
		final SavedSearchPojoPrototype savedSearch = newSavedSearch("Global Saved Search Project 0", "miningUi.modulesTable", EntityId.of(0l),
				"saved search", Collections.emptyList(), null, ScopeEnum.GLOBAL, null);
		
		/*Creation of cache and adding to cache */
		final Cache savedSearchCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.SAVED_SEARCH_AGGREGATION, PROJECT_ONE));
		assertNotNull(savedSearchCache);
		savedSearchCache.put(key, savedSearch);
	}
	
	private void createTaxonomyAggregationCache(final Object key) {
		final TaxonomyTypePojoPrototype taxonomyType = new TaxonomyTypePojoPrototype();
		
		/* Creation of cache and adding to cache */
		final Cache taxonomyTypeCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.TAXONOMY_AGGREGATION_CACHE, PROJECT_ONE));
		assertNotNull(taxonomyTypeCache);
		taxonomyTypeCache.put(key, taxonomyType);
	}
	
	private void createTaxonomyCategoryAggregationCache(final Object key) {
		final TaxonomyCategoryPojoPrototype taxonomyCategory = new TaxonomyCategoryPojoPrototype().setName("Business type").setProject(PROJECT_ONE);
		
		/* Creation of cache and adding to cache */
		final Cache taxonomyCategoryCache = cacheManager
				.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.TAXONOMY_CATEGORY_AGGREGATION_CACHE, PROJECT_ONE));
		assertNotNull(taxonomyCategoryCache);
		taxonomyCategoryCache.put(key, taxonomyCategory);
	}

	private SavedSearchPojoPrototype newSavedSearch(final String name, final String usage, @Nullable final EntityId projectId, final String savedSearch,
			final List<String> modifiers, @Nullable final String createdByUserId, final ScopeEnum scope, @Nullable final EntityId clientId) {
		final SavedSearchPojoPrototype prototype = new SavedSearchPojoPrototype()
				.setName(name)
				.setUsage(usage)
				.setSavedSearch(savedSearch)
				.setModifiers(modifiers)
				.setScope(scope);

		if (clientId != null) {
			prototype.setClient(clientId);
		}
		if (projectId != null) {
			prototype.setProject(projectId);
		}
		if (createdByUserId != null) {
			prototype.setCreatedByUserId(createdByUserId);
		}

		return prototype;
	}
}
