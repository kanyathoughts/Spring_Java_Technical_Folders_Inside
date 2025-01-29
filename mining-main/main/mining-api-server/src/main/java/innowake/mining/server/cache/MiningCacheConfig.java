/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.cache;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.CachingConfigurerSupport;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.interceptor.CacheResolver;
import org.springframework.cache.interceptor.KeyGenerator;
import org.springframework.cache.interceptor.SimpleKeyGenerator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;

@EnableCaching
@Configuration
public class MiningCacheConfig extends CachingConfigurerSupport {
	
	/* Some of these constants are used in mining-data too. If you change them here you must scan all mining projects to check
	 * if you have to apply the same changes there as well! */
	
	public static final String AGGREGATION_CACHE = "aggregatedValues";
	public static final String HOTSPOT_CACHE = "hotSpots";
	public static final String MODULE_STATISTICS = "moduleStatistics";
	public static final String LATEST_MODEL_DNA = "latestModelDna";
	public static final String SAVED_SEARCH_AGGREGATION = "savedSearchAggregation";
	public static final String KEYCLOAK_USER_NAME_LOOKUP = "keycloakUserNameLookup";
	public static final String TAXONOMY_AGGREGATION_CACHE = "taxonomyAggregation";
	public static final String TAXONOMY_CATEGORY_AGGREGATION_CACHE = "taxonomyCategoryAggregation";
	public static final String UTILITY_AGGREGATION_CACHE = "utilityAggregation";
	public static final String PROJECT_KEY_GENERATOR = "projectKeyGenerator";
	public static final String PROMPT_CACHE = "promptCache";
	private static final KeyGenerator SIMPLE_KEY_GENERATOR = new SimpleKeyGenerator();

	@Autowired
	private ProjectService projectService;

	@Primary
	@Bean
	@Override
	public CacheManager cacheManager() {
		return new GuavaCacheManager();
	}
	
	@Bean
	@Override
	public CacheResolver cacheResolver() {
		return new ProjectAwareCacheResolver(cacheManager(), this);
	}
	
	@Bean("projectKeyGenerator")
	public KeyGenerator projectKeyGenerator() {
		return (target, method, params) -> SIMPLE_KEY_GENERATOR.generate(target, method, mapEntityIds(params));
	}

	private Object[] mapEntityIds(final Object... params) {
		if (params.length != 0) {
			final var copy = new Object[params.length];
			for (var i = 0; i < params.length; i++) {
				copy[i] = params[i] instanceof EntityId ? ensureProjectNid((EntityId) params[i]) : params[i];
			}

			return copy;
		}

		return params;
	}

	public EntityId ensureProjectNid(final EntityId param) {
		if (param.hasNid()) {
			return param;
		}
		return EntityId.of(param.getUid(), projectService.getNid(param));
	}
	
}
