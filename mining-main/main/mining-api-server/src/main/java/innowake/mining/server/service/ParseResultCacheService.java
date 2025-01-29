/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Service class to cache the parse result.
 */
@Service
public class ParseResultCacheService {

	/** To evict the data from cache if the job status is inactive */
	private static final int CHECK_INTERVAL = 60;

	/** Cache keys set is maintained to evict its data from cache once the job is inactive. */
	private final Map<String, Set<String>> cacheKeySet = new ConcurrentHashMap<>();
	
	private final int maximumCacheSize;

	private final Cache<String, Object> cache;

	@Autowired
	private JobManager jobManager;

	/**
	 * Constructor.
	 * 
	 * Sets up the parse result cache using the given configuration properties.
	 * 
	 * @param configProperties the {@link GenericConfigProperties}
	 */
	@Autowired
	public ParseResultCacheService(final GenericConfigProperties configProperties) {
		maximumCacheSize = configProperties.getDiscoveryParseResultCacheSize();
		final CacheBuilder<Object, Object> builder = CacheBuilder.newBuilder().maximumSize(maximumCacheSize);
		if ( ! configProperties.isDiscoveryCacheStrongReferences()) {
			builder.softValues();
		}
		cache = builder.build();
	}

	/**
	 * Checks each jobId registered in {@code cacheKeySet} and evicts corresponding data from the cache if the job is finished.
	 */
	@Scheduled(fixedDelay = CHECK_INTERVAL, timeUnit = TimeUnit.SECONDS)
	public void pollFinishedJobs() {
		for (final String jobId : new HashSet<>(cacheKeySet.keySet())) {
			@Nullable
			final JobMonitor jobMonitor = assertNotNull(jobManager).getJobMonitor(jobId);
			if (jobMonitor == null || ! JobStatus.isActive(jobMonitor.getStatus())) {
				evictDataFromCache(jobId);
			}
		}
	}

	/**
	 * Returns the parse result from the cache based on the source object key.
	 * 
	 * @param jobId The @link {@link Job} ID 
	 * @param <P> the parse result
	 * @param key The key to check in cache
	 * @return the parse result from the cache based on the source object key
	 */
	@SuppressWarnings("unchecked")
	@Nullable
	public <P> P getCached(final String jobId, final String key) {
		if (maximumCacheSize == 0) {
			return null;
		}
		return (P) cache.getIfPresent(getKey(jobId, key));
	}

	/**
	 * Sets the parse result to the cache.
	 * 
	 * @param <P> the parse result
	 * @param jobId The @link {@link Job} ID
	 * @param key The key for the cache will be prefixed with jobId
	 * @param parseResult The parse result
	 */
	public <P> void cache(final String jobId, final String key, final P parseResult) {
		if (maximumCacheSize != 0) {
			cache.put(getKey(jobId, key), parseResult);
			/** Cache keys and Job IDs are maintained in a set to evict its data when the job is inactive. */
			cacheKeySet.computeIfAbsent(jobId, k -> ConcurrentHashMap.newKeySet()).add(getKey(jobId, key));
		}
	}

	/**
	 * Returns the cache key which is prefixed with JobId so that to evict it from cache once the job is completed 
	 *
	 * @param jobId The @link {@link Job} ID 
	 * @param key The key to check in cache
	 * @return cache key prefixed with the jobId
	 */
	private String getKey(final String jobId, final String key) {
		return jobId + "@" + key;
	}

	/**
	 * Evicts all the data from the cache for the inactive JobId. 
	 *
	 * @param jobId The @link {@link Job} ID
	 */
	private void evictDataFromCache(final String jobId) {
		cache.invalidateAll(cacheKeySet.get(jobId));
		cacheKeySet.remove(jobId);
	}
}