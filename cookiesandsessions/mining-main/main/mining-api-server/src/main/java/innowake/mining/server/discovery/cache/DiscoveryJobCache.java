/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.cache;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.hazelcast.map.IMap;
import com.hazelcast.multimap.MultiMap;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.server.config.DiscoveryJobConfig;

/**
 * Distributed cache that can be used to store intermediate results.
 */
@Component
public class DiscoveryJobCache implements DiscoveryCache {

	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryJobCache.class);
	
	@Value("${configuration.discovery-lock-debug-timeout: 10}")
	private long lockTimeout;
	
	@Autowired
	@Qualifier(DiscoveryJobConfig.CACHE_INDEX_MAP)
	private MultiMap<String, String> cacheIndex;

	@Autowired
	@Qualifier(DiscoveryJobConfig.CACHE_MULTI_MAP)
	private MultiMap<String, Object> discoveryMultiMapCache;
	
	@Autowired
	@Qualifier(DiscoveryJobConfig.CACHE_MAP)
	private IMap<String, Object> discoveryCache;

	private final ThreadLocal<ArrayDeque<List<String>>> currentlyHeldLocks = ThreadLocal.withInitial(ArrayDeque::new);
	private final ThreadLocal<ArrayDeque<List<Long>>> previousAcquireTimes = ThreadLocal.withInitial(ArrayDeque::new);

	@Override
	public void putMultiValue(final String jobId, final String key, final Object value) {
		final String completeKey = jobId + key;
		/* we store every cache key in an additional index map, so that we can properly clean up after the job is done. */
		cacheIndex.put(jobId, completeKey);
		discoveryMultiMapCache.put(completeKey, value);
	}

	@Override
	public Set<Object> getMultiValue(final String jobId, final String key) {
		return (Set<Object>) discoveryMultiMapCache.get(jobId + key);
	}
	
	@Override
	public void removeMultiValue(final String jobId, final String key) {
		discoveryMultiMapCache.delete(jobId + key);
		/* Unfortunately MultiMap doesn't allow to directly execute code on the machine where the map key resides. Therefore not deleting the key from
		 * the index map, to avoid the expensive complete deserialization and serialization of the possible large nested list. It's only used for the final
		 * cleanup anyways. */
	}

	@Override
	public boolean hasMultiValue(final String jobId, final String key) {
		return discoveryMultiMapCache.containsKey(jobId + key);
	}

	@Override
	public void putValue(final String jobId, final String key, final Object value) {
		final String completeKey = jobId + key;
		/* we store every cache key in an additional index map, so that we can properly clean up after the job is done. */
		cacheIndex.put(jobId, completeKey);
		discoveryCache.set(completeKey, value);
	}
	
	@Override
	public void removeValue(final String jobId, final String key) {
		discoveryCache.delete(jobId + key);
		/* Unfortunately MultiMap doesn't allow to directly execute code on the machine where the map key resides. Therefore not deleting the key from
		 * the index map, to avoid the expensive complete deserialization and serialization of the possible large nested list. It's only used for the final
		 * cleanup anyways. */
	}
	
	@Override
	public Object computeValueIfAbsent(final String jobId, final String key, final Callable<? extends Object> callable) throws Exception {
		final String completeKey = jobId + key;
		
		/* optimistic get */
		Object value = discoveryCache.get(completeKey);
		if (value != null) {
			return value;
		}
		
		try {
			createLocks(jobId, key);
			value = discoveryCache.get(completeKey);
			if (value != null) {
				return value;
			}
			final Object result = callable.call();
			/* we store every cache key in an additional index map, so that we can properly clean up after the job is done. */
			cacheIndex.put(jobId, completeKey);
			discoveryCache.set(completeKey, result);
			return result;
		} finally {
			releaseLocks(jobId, key);
		}
	}
	
	@Override
	@Nullable
	public Object getValue(final String jobId, final String key) {
		return discoveryCache.get(jobId + key);
	}
	
	@Override
	public void createLocks(final String jobId, final String... keys) {
		createLocks(jobId, Arrays.asList(keys));
	}
	
	@Override
	public void createLocks(final String jobId, final List<String> keys) {
		if (LOG.isTraceEnabled()) {
			createLocksWithDebugLog(jobId, keys);
		} else {
			for (final String key : keys) {
				discoveryCache.lock(jobId + key);
			}
		}
	}
	
	private void createLocksWithDebugLog(final String jobId, final List<String> keys) {
		final List<String> acquiredLocks = new ArrayList<>(keys.size());
		final List<Long> acquireTimes = new ArrayList<>(keys.size());
		currentlyHeldLocks.get().push(acquiredLocks);
		previousAcquireTimes.get().push(acquireTimes);
		final long timeBeforeAllLocks = System.nanoTime();
		for (final String key : keys) {
			try {
				final long timeBeforeLock = System.nanoTime();
				while ( ! discoveryCache.tryLock(jobId + key, lockTimeout, TimeUnit.SECONDS)) {
					final long timeAfterWaiting = System.nanoTime();
					final long waitedSeconds = TimeUnit.SECONDS.convert(timeAfterWaiting - timeBeforeLock, TimeUnit.NANOSECONDS);
					LOG.trace(() -> String.format("LOCK-WAITING: waiting %d seconds for lock %s - already acquired locks: %s", waitedSeconds, key, acquiredLocks));
				}
				final long timeAfterLock = System.nanoTime();
				acquiredLocks.add(key);
				acquireTimes.add(timeAfterLock);
				final long elapsedSeconds = TimeUnit.SECONDS.convert(timeAfterLock - timeBeforeLock, TimeUnit.NANOSECONDS);
				if (elapsedSeconds > lockTimeout) {
					LOG.trace(() -> String.format("LOCK-ACQUIRED-SINGLE: waited %d seconds for lock %s - already acquired locks: %s", elapsedSeconds, key, acquiredLocks));
				}
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
				LOG.error(() -> "LOCK-ERROR: interrupted while attempting to acquire lock", e);
				throw new IllegalStateException("Interrupted while attempting to acquire lock", e);
			}
		}
		final long timeAfterAllLocks = System.nanoTime();
		final long elapsedSeconds = TimeUnit.SECONDS.convert(timeAfterAllLocks - timeBeforeAllLocks, TimeUnit.NANOSECONDS);
		if (elapsedSeconds > lockTimeout) {
			LOG.trace(() -> String.format("LOCK-ACQUIRED: waited %d seconds for locks: %s", elapsedSeconds, keys));
		}
	}
	
	@Override
	public void releaseLocks(final String jobId, final String... keys) {
		releaseLocks(jobId, Arrays.asList(keys));
	}
	
	@Override
	public void releaseLocks(final String jobId, final List<String> keys) {
		if (LOG.isTraceEnabled()) {
			releaseLocksWithDebugLog(jobId, keys);
		} else {
			for (final String key : keys) {
				discoveryCache.unlock(jobId + key);
			}
		}
		
	}
	
	private void releaseLocksWithDebugLog(final String jobId, final List<String> keys) {
		final List<String> acquiredLocks = currentlyHeldLocks.get().pop();
		final List<Long> acquireTimes = previousAcquireTimes.get().pop();
		
		/* assert that released locks are currently held by thread */
		if ( ! keys.equals(acquiredLocks)) {
			LOG.error(() -> String.format("LOCK-UNLOCK-ERROR: unlocking %s but current thread holds %s", keys, acquiredLocks));
			throw new IllegalStateException("Attempting to release different locks than were previously acquired");
		}
		
		for (int i = 0; i < keys.size(); i++) {
			final String key = keys.get(i);
			discoveryCache.unlock(jobId + key);
			final long timeAfterRelease = System.nanoTime();
			final long acquireTime = acquireTimes.get(i);
			final long secondsUntilRelease = TimeUnit.SECONDS.convert(timeAfterRelease - acquireTime, TimeUnit.NANOSECONDS);
			if (secondsUntilRelease > lockTimeout) {
				LOG.trace(() -> String.format("LOCK-RELEASE: released lock %s after %d seconds - other held locks: %s", key, secondsUntilRelease, acquiredLocks));
			}
		}
	}
	
	@Override
	public void clearDiscoveryJobCache(final String jobId) {
		final Collection<String> indexValues = cacheIndex.get(jobId);
		indexValues.forEach(value -> {
			try {
				discoveryMultiMapCache.delete(value);
				discoveryCache.delete(value);
			} catch (final Exception e) {
				LOG.error(() -> "unable to clear cache entry: " + value, e);
			}
		});
		cacheIndex.remove(jobId);
	}
}
