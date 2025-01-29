/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import java.util.regex.Pattern;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.properties.GenericConfigProperties;

/**
 * Service for singleton artifact cache used in {@code DbModuleRepository}.
 */
@Service
class ArtifactCacheService {

	/** The pattern cache for artifact paths */
	final LoadingCache<String, Pattern> pathPatternCache;
	
	/** The artifact cache instance. */
	final Cache<String, Object> cache;
	
	/** The maximum size of the artifact cache. */
	final int maxCacheSize;

	@Autowired
	public ArtifactCacheService(final GenericConfigProperties configProperties) {
		maxCacheSize = configProperties.getDiscoveryModuleRepositoryCacheSize();
		CacheBuilder<Object, Object> builder = CacheBuilder.newBuilder().maximumSize(maxCacheSize);
		if ( ! configProperties.isDiscoveryCacheStrongReferences()) {
			builder = builder.softValues();
		}
		cache = builder.build();

		/* For now max size is not configurable. According to JProfiler the memory footprint of cached path patterns was 0.03 GB for
		 * 10_000 x src/pl1/DEAD6/%d/programs/[^/]* or 
		 * 10_000 x src/pl1/DEAD6/%d/programs/programs/programs/programs/programs/programs/programs/programs/programs/[^/]*  */
		CacheBuilder<Object, Object> patternCache = CacheBuilder.newBuilder().maximumSize(10_000);
		if ( ! configProperties.isDiscoveryCacheStrongReferences()) {
			patternCache = patternCache.softValues();
		}
		pathPatternCache = patternCache.build(new CacheLoader<String, Pattern>() {

			@Override
			@Nullable
			public Pattern load(@Nullable final String regEx) {
				return Pattern.compile(regEx);
			}
		});
	}
}
