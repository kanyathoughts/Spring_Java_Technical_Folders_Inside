/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import com.hazelcast.config.MapConfig;
import com.hazelcast.config.MultiMapConfig;
import com.hazelcast.config.MultiMapConfig.ValueCollectionType;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.map.IMap;
import com.hazelcast.multimap.MultiMap;

import innowake.lib.job.internal.JobConfiguration;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;

/**
 * Configuration for the discovery.
 */
@Configuration
@AutoConfigureAfter(JobConfiguration.class)
public class DiscoveryJobConfig {

	/** Name of the hazelcast cache index map. */
	public static final String CACHE_INDEX_MAP = "Discovery-Cache-Index";
	/** Name of the hazelcast discovery job {@link MultiMap} cache. */
	public static final String CACHE_MULTI_MAP = "Discovery-Job-MultiMap-Cache";
	public static final String CACHE_MAP = "Discovery-Job-Cache";

	private final HazelcastInstance hz;

	/**
	 * Constructor.
	 *
	 * @param hz the {@link HazelcastInstance}
	 */
	@Autowired
	public DiscoveryJobConfig(@Qualifier(JobConfiguration.HAZELCAST_INSTANCE) final HazelcastInstance hz) {
		this.hz = hz;
		hz.getConfig().addMultiMapConfig(new MultiMapConfig().setName(CACHE_INDEX_MAP).setValueCollectionType(ValueCollectionType.LIST));
		hz.getConfig().addMultiMapConfig(new MultiMapConfig().setName(CACHE_MULTI_MAP).setValueCollectionType(ValueCollectionType.SET));
		hz.getConfig().addMapConfig(new MapConfig().setName(CACHE_MAP).setMaxIdleSeconds(0).setTimeToLiveSeconds(0));
	}

	/**
	 * @return the index {@link MultiMap} that holds all keys created in the actual specific cache maps per jobId
	 */
	@Bean(name = CACHE_INDEX_MAP)
	public MultiMap<String, String> discoveryCacheIndexMap() {
		return hz.getMultiMap(CACHE_INDEX_MAP);
	}

	/**
	 * Provides a {@link MultiMap} whose keys are associated with a Set of values. It's main purpose is to be used by {@link DiscoveryJobCache}
	 * to cache any temporary multi valued discovery results.
	 * 
	 * @return the {@link MultiMap} used as cache for multi valued discovery temporary results
	 */
	@Bean(name = CACHE_MULTI_MAP)
	public MultiMap<String, Object> getDiscoveryJobMultiMapCache() {
		return hz.getMultiMap(CACHE_MULTI_MAP);
	}
	
	/**
	 * Provides a {@link IMap} primarily to be used by {@link DiscoveryJobCache} to cache any temporary single valued discovery results
	 * and for distributed locks.
	 * 
	 * @return the {@link IMap} used as a cache for single valued discovery temporary results and distributed locks
	 */
	@Bean(name = CACHE_MAP)
	public IMap<String, Object> getDiscoveryJobMapCache() {
		return hz.getMap(CACHE_MAP);
	}

	/**
	 * @return the {@link DiscoveryJobCache} to cache intermediate discovery results
	 */
	@Bean
	@Primary
	public DiscoveryJobCache discoveryJobCache() {
		return new DiscoveryJobCache();
	}
}
