/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.locking;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.cp.CPSubsystem;
import com.hazelcast.cp.lock.FencedLock;
import innowake.lib.core.api.lang.NonNull;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.internal.JobConfiguration;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import java.util.concurrent.TimeUnit;

/**
 * Service for obtaining and caching distributed Hazelcast locks.
 * @see FencedLock
 */
@Service
public class LockManager {

	private static final Logger LOG = LoggerFactory.getLogger(LockManager.class);

	private final LoadingCache<String, FencedLock> lockCache;

	public LockManager(@Qualifier(JobConfiguration.HAZELCAST_INSTANCE) final HazelcastInstance hz) {
		lockCache = CacheBuilder.newBuilder()
				.expireAfterAccess(30, TimeUnit.MINUTES)
				.build(new CacheLoader<>() {
					@Override
					@NonNull
					public FencedLock load(@Nullable final String name) {
						if (name == null) {
							throw new IllegalArgumentException("name must not be null");
						}
						LOG.debug(() -> "acquiring new Hazelcast FencedLock " + name);
						return hz.getCPSubsystem().getLock(name);
					}
				});
	}

	/**
	 * Gets the Hazelcast FencedLock with the given name. The LockManager caches the lock instance so the caller does not need to cache
	 * the returned lock instance.
	 * @param name name of the lock
	 * @return a cached FencedLock instance
	 * @see CPSubsystem#getLock(String)
	 */
	public FencedLock getLock(final String name) {
		return lockCache.getUnchecked(name);
	}
}
