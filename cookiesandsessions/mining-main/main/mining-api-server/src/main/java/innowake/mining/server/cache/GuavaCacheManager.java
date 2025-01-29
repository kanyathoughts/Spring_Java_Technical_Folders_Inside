/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.cache;

import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;

import com.google.common.cache.CacheBuilder;

import innowake.lib.core.api.lang.NonNull;
import innowake.mining.server.properties.GenericConfigProperties;

public class GuavaCacheManager implements CacheManager {
	
	private final ConcurrentMap<String, Cache> cacheMap = new ConcurrentHashMap<>(16);
	
	@Autowired
	private GenericConfigProperties configProperties;

	@Override
	public Cache getCache(@NonNull final String name) {
		return cacheMap.computeIfAbsent(name, this::createCache);
	}

	@Override
	@NonNull
	public Collection<String> getCacheNames() {
		return Collections.unmodifiableSet(cacheMap.keySet());
	}

	protected Cache createCache(final String name) {
		final CacheBuilder<Object, Object> builder = CacheBuilder.newBuilder();

		if (name.contains(MiningCacheConfig.AGGREGATION_CACHE)) {
			builder.maximumSize(configProperties.getMiningAggregationCacheSize());
		} else if (name.contains(MiningCacheConfig.HOTSPOT_CACHE)) {
			builder.maximumSize(configProperties.getMiningHotspotCacheSize());
		} else if (name.contains(MiningCacheConfig.MODULE_STATISTICS)) {
			builder.maximumSize(configProperties.getMiningModuleStatisticsCacheSize());
		} else if (name.contains(MiningCacheConfig.LATEST_MODEL_DNA)) {
			builder.maximumSize(configProperties.getDiscoveryModelDnaCacheSize());
		} else if (name.contains(MiningCacheConfig.TAXONOMY_AGGREGATION_CACHE)) {
			builder.maximumSize(configProperties.getMiningTaxonomyAggregationCacheSize());
		} else if (name.contains(MiningCacheConfig.TAXONOMY_CATEGORY_AGGREGATION_CACHE)) {
			builder.maximumSize(configProperties.getMiningTaxonomyCategoryAggregationCacheSize());
		} else if (name.contains(MiningCacheConfig.SAVED_SEARCH_AGGREGATION)) {
			builder.maximumSize(configProperties.getSavedSearchChacheSize());
		} else if (name.contains(MiningCacheConfig.KEYCLOAK_USER_NAME_LOOKUP)) {
			builder.expireAfterWrite(configProperties.getKeycloakUserNameCacheDuration(), TimeUnit.SECONDS);
		} else if (name.contains(MiningCacheConfig.UTILITY_AGGREGATION_CACHE)) {
			builder.maximumSize(configProperties.getMiningUtilityAggregationCacheSize());
		} else if (name.contains(MiningCacheConfig.PROMPT_CACHE)) {
			builder.maximumSize(configProperties.getPromptCacheSize())
					.expireAfterWrite(configProperties.getPromptCacheDuration(), TimeUnit.SECONDS);
		} else {
			builder.maximumSize(configProperties.getGenericMiningCacheSize());
		}
		
		return new GuavaCache(name, builder);
	}
}
