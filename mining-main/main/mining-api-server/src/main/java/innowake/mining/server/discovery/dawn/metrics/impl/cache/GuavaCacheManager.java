/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.cache;

import com.google.common.cache.CacheBuilder;
import innowake.lib.core.api.lang.NonNull;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;

import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class GuavaCacheManager implements CacheManager {
	
	@Value("${configuration.dawn-fetch-module-cache-size}")
	private int fetchModuleCacheSize;
	
	@Value("${configuration.dawn-find-module-cache-size}")
	private int findModuleCacheSize;

	private final ConcurrentMap<String, Cache> cacheMap = new ConcurrentHashMap<>(16);

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
		final int cacheSize = name.equals(DawnCacheConfiguration.FIND_MODULES_CACHE) ? findModuleCacheSize : fetchModuleCacheSize;
		return new GuavaCache(name, CacheBuilder.newBuilder().maximumSize(cacheSize));
	}
}
