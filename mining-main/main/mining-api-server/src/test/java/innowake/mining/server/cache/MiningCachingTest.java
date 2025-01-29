/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.cache;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.Instant;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.cache.Cache;
import org.springframework.cache.Cache.ValueWrapper;
import org.springframework.cache.interceptor.SimpleKey;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.lang.Assert;
import innowake.mining.data.model.discovery.dna.DnaConfig;
import innowake.mining.data.model.discovery.dna.ModelDna;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.HotSpotController;
import innowake.mining.server.controller.SavedSearchController;
import innowake.mining.server.controller.discovery.DiscoveryController;
import innowake.mining.server.controller.module.ModuleController;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.util.CacheNameUtil;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SavedSearchPojo;
import innowake.mining.shared.entities.dna.DnaSnapshotPojoPrototype;
import innowake.mining.shared.model.HotSpot;
import innowake.mining.shared.model.HotSpot.FilterType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleFieldName;
import innowake.mining.shared.model.ModuleStatisticsResponse;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.SavedSearchCountResponse;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;

/**
 * Test validating the behavior of Mining Caching Implementation
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class MiningCachingTest extends DatabaseRelatedTest {
	
	final static EntityId PROJECT_ONE = EntityId.of(1L);
	
	@Autowired
	private MockMvc mvc;
	
	@Autowired
	private ObjectMapper objectMapper;
	
	@Autowired
	private DnaDataService dnaService;
	
	@Autowired
	private GuavaCacheManager cacheManager;
	
	@BeforeAll
	void init() throws Exception {	
		createModule(PROJECT_ONE, "Module1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED,
				Origin.CUSTOM);
		createModule(PROJECT_ONE, "Module2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED,
				Origin.CUSTOM);
		createDnaSnapshot();
	}
	
	/**
	 * Tests creation of aggregation cache and validated cache results
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@SuppressWarnings("unchecked")
	@Test
	void testAggregationCache() throws Exception {
		/* Aggregation request */
		final AggregationRequest<ModuleFieldName> aggregationRequest = new AggregationRequest<>();
		final Set<ModuleFieldName> groupBy = new HashSet<>();
		groupBy.add(ModuleFieldName.TECHNOLOGY);
		aggregationRequest.setGroupBy(groupBy);
		final Map<ModuleFieldName, AggregationOperator> fields = new LinkedHashMap<>();
		fields.put(ModuleFieldName.ID, AggregationOperator.COUNT);
		aggregationRequest.setFields(fields);
		/* Aggregation response */
		final MvcResult result = mvc.perform(post("/api" + ModuleController.AGGREGATIONS_URL, PROJECT_ONE.getNid())
				.content(PojoMapper.jsonWriter().writeValueAsString(aggregationRequest)).contentType(MediaType.APPLICATION_JSON_VALUE)).andDo(print())
				.andExpect(status().isOk()).andReturn();
		final List<AggregationResult<ModuleFieldName>> aggregationResult = objectMapper.readValue(result.getResponse().getContentAsString(),
				new TypeReference<>() {});
		/* Cache validation */
		final Cache cache = cacheManager.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.AGGREGATION_CACHE, PROJECT_ONE));
		assertNotNull(cache, "The aggregate result cache should be created");
		/* key for retrieving value from aggregation cache" */
		var cachingKeyPart = new AggregationRequest<ModuleFieldName>();
		cachingKeyPart.setFilterObject(Map.of(ModuleFieldName.PROJECT_ID, Map.of("eq", EntityId.of(1L))));
		cachingKeyPart.setGroupBy(groupBy);
		cachingKeyPart.setFields(fields);
		final Object key = new SimpleKey(EntityId.of(1L), cachingKeyPart);

		final ValueWrapper valueWrapper = Assert.assertNotNull(cache).get(key);
		assertNotNull(valueWrapper, "Cache should contain key and value for aggregation result");
		final List<AggregationResult<ModuleFieldName>> cachedValue = (List<AggregationResult<ModuleFieldName>>) Assert.assertNotNull(valueWrapper).get();
		assertEquals(Long.valueOf(aggregationResult.get(0).getFields().get(ModuleFieldName.ID).toString()),
					 Assert.assertNotNull(cachedValue).get(0).getFields().get(ModuleFieldName.ID),
					 "Aggregation result and cached result fields should be same");
		assertEquals(aggregationResult.get(0).getGroup().get(ModuleFieldName.TECHNOLOGY),
					 Assert.assertNotNull(cachedValue).get(0).getGroup().get(ModuleFieldName.TECHNOLOGY),
					 "Aggregation result and cached result group should be same");
	}
	
	/**
	 * Tests creation of hotspots cache and validated cache results
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@SuppressWarnings("unchecked")
	@Test
	void testHotSpotsCache() throws Exception {
		/* Hotspot response */
		final MvcResult result = mvc.perform(get("/api" + HotSpotController.HOTSPOT_COLLECTION_URL, PROJECT_ONE.getNid(), "REFERENCES")
				.contentType(MediaType.APPLICATION_JSON_VALUE)).andDo(print())
				.andExpect(status().isOk()).andReturn();
		final List<HotSpot> hotSpotResult = objectMapper.readValue(result.getResponse().getContentAsString(), new TypeReference<>() {});
		/* Cache validation */
		final Cache cache = cacheManager.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.HOTSPOT_CACHE, PROJECT_ONE));
		assertNotNull(cache, "The hotspot result cache should be created");
		/* key for retrieving value from hotSpot cache" */
		final Object key = new SimpleKey(PROJECT_ONE, FilterType.REFERENCES, null);
		final ValueWrapper valueWrapper = Assert.assertNotNull(cache).get(key);
		assertNotNull(valueWrapper, "Cache should contain key and value for hotSpot result");
		final List<HotSpot> cacheValue = (List<HotSpot>) Assert.assertNotNull(valueWrapper).get();
		
		assertEquals(hotSpotResult.get(0).getCount(), Assert.assertNotNull(cacheValue).get(0).getCount(),
					"Aggregation result and cached result count should be same");
		assertEquals(hotSpotResult.get(1).getCount(), Assert.assertNotNull(cacheValue).get(1).getCount(),
					"Aggregation result and cached result count should be same");
		
		assertEquals(hotSpotResult.get(0).getModule(), Assert.assertNotNull(cacheValue).get(0).getModule(),
					"Aggregation result and cached result module should be same");
		assertEquals(hotSpotResult.get(1).getModule(), Assert.assertNotNull(cacheValue).get(1).getModule(),
					"Aggregation result and cached result module should be same");
	}
	
	/**
	 * Tests creation of module statistics cache and validated cache results
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void testModuleStatisticsCache() throws Exception {
		/* ModuleStatistics response */
		final MvcResult result = mvc.perform(get("/api" + ModuleController.MODULE_STATISTICS, PROJECT_ONE.getNid())
				.contentType(MediaType.APPLICATION_JSON_VALUE)).andDo(print())
				.andExpect(status().isOk()).andReturn();
		final ModuleStatisticsResponse moduleStatisticsResult = objectMapper.readValue(result.getResponse().getContentAsString(),
				new TypeReference<>() {});
		/* Cache validation */
		final Cache cache = cacheManager.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.MODULE_STATISTICS, PROJECT_ONE));
		assertNotNull(cache, "The moduleStatistics result cache should be created");
		final ValueWrapper valueWrapper = Assert.assertNotNull(cache).get(PROJECT_ONE);
		assertNotNull(valueWrapper, "Cache should contain key and value for moduleStatistics result");
		final ModuleStatisticsResponse cacheValue = (ModuleStatisticsResponse) Assert.assertNotNull(valueWrapper).get();
		
		assertEquals(moduleStatisticsResult.getCount(), Assert.assertNotNull(cacheValue).getCount(),
					"ModuleStatistics result and cached result count should be same");
	}
	
	/**
	 * Tests creation of model dna cache and validated cache results
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void testLatestModelDnaCache() throws Exception {
		/* LatestDnaSnapshot response */
		final MvcResult result = mvc.perform(
				get("/api" + DiscoveryController.DNA_SNAPSHOT_FOR_LATEST_TIMESTAMP_URL, PROJECT_ONE.getNid()).secure(true)
				.contentType(MediaType.APPLICATION_JSON_VALUE)).andDo(print())
				.andExpect(status().isOk()).andReturn();
		final ModelDna latestDnaSnapshotResult = objectMapper.readValue(result.getResponse().getContentAsString(), new TypeReference<>() {});
		/* Cache validation */
		final Cache cache = cacheManager.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.LATEST_MODEL_DNA, PROJECT_ONE));
		assertNotNull(cache, "The latestModelDna result should be created");
		final ValueWrapper valueWrapper = cache.get(PROJECT_ONE);
		assertNotNull(valueWrapper, "Cache should contain key and value for latestModelDna result");
		final ModelDna cacheValue = (ModelDna) valueWrapper.get();
		assertEquals(latestDnaSnapshotResult.getModuleCount(), Assert.assertNotNull(cacheValue).getModuleCount(),
					"LatestDnaSnapshot result and cached result module count should be same");
	}
	
	@SuppressWarnings("unchecked")
	@Test
	void testSavedSearchCache() throws Exception {
		final MvcResult result = mvc.perform(
				get("/api" + SavedSearchController.SAVED_SEARCH_COUNTS_BY_PROJECT_ID, PROJECT_ONE.getNid()).secure(true))
				.andDo(print())
				.andExpect(status().isOk()).andReturn();
		final List<SavedSearchCountResponse> cacheValue = objectMapper.readValue(result.getResponse().getContentAsString(), new TypeReference<>() {});
		/* Cache validation */
		final Cache cache = cacheManager.getCache(CacheNameUtil.getProjectCacheName(MiningCacheConfig.SAVED_SEARCH_AGGREGATION, PROJECT_ONE));
		assertNotNull(cache, "The savedSearchAggregation result should be created");
		final ValueWrapper valueWrapper = cache.get(PROJECT_ONE);
		assertNotNull(valueWrapper, "Cache should contain key and value for savedSearchAggregation result");
		final List<SavedSearchCountResponse> savedSearchCountResponse = (List<SavedSearchCountResponse>) valueWrapper.get();
		assertNotNull(savedSearchCountResponse, "Saved search count response must not be null");
		final SavedSearchPojo expected = savedSearchCountResponse.get(0).getSavedSearch();
		assertNotNull(expected, "Expected saved search must not be null");
		final SavedSearchPojo actual = cacheValue.get(0).getSavedSearch();
		assertNotNull(actual, "Actual saved search must not be null");
		assertEquals(expected.getId(), actual.getId(), "SavedSearch result and cached result should be same");
	}

	private void createModule(final EntityId projectId, final String name, final Technology technology, final Type type, final Storage storage,
			final Identification identification, final Origin origin) throws Exception {
		final var module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setName(name);
		module.setTechnology(technology);
		module.setType(type);
		module.setStorage(storage);
		module.setIdentification(identification);
		module.setOrigin(origin);
		
		mvc.perform(post("/api" + ModuleController.MODULE_COLLECTION_URL, PROJECT_ONE.getNid())
				.contentType("application/json")
				.content(PojoMapper.jsonWriter().writeValueAsString(module)))
			.andDo(print()).andExpect(status().isCreated());
	}
	
	private void createDnaSnapshot() {
		final Optional<ProjectPojo> projectDb = projectService.find(PROJECT_ONE);
		assertTrue(projectDb.isPresent(), "Project with projectId 1 must exist");

		projectService.update(new ProjectPojoPrototype()
				.withId(projectDb.get().identity())
				.setMetricsDate(Instant.now()));

		dnaService.createSnapshot(new DnaSnapshotPojoPrototype()
				.setProjectId(PROJECT_ONE)
				.setTotalModuleCount(Integer.valueOf(12))
				.setDnaConfig(new DnaConfig().toMap()));
	}
}
