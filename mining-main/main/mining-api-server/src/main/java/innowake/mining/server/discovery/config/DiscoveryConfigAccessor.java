/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.config;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.concurrent.ExecutionException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import innowake.mining.shared.entities.ProjectPojo;

/**
 * Provides access to discovery config for a given job. It is intended to be used by Discovery Tasks to provide fast access to the config.
 * <p>
 * This class uses a cache so multiple tasks of the same job operating on the same project can retrieve the config quickly.
 */
@Service
public class DiscoveryConfigAccessor {
	
	private static final int MAXIMUM_NUMBER_OF_CACHED_CONFIGS = 5;
	
	@Autowired
	private ProjectService projectService;
	
	private final LoadingCache<Tuple2<EntityId, String>, Config> configCache = CacheBuilder.newBuilder()
			.maximumSize(MAXIMUM_NUMBER_OF_CACHED_CONFIGS)
			.build(new CacheLoader<>() {
				@Override
				@Nullable
				public Config load(@Nullable final Tuple2<EntityId, String> keyOrNull) {
					final EntityId projectId = assertNotNull(keyOrNull).a;
					try {
						return loadConfig(projectId);
					} catch (final DiscoveryException e) {
						throw new IllegalStateException("Unable to load Discovery configuration for project " + projectId, e);
					}
				}
				
				private Config loadConfig(final EntityId projectId) throws DiscoveryException {
					final Config config = Config.loadConfig(projectService, projectId);
					
					if (config.getUtilityExclusionEnabled()) {
						config.setUtilityList(UtilityList.loadUtilityList(projectService, projectId));
					}
					
					return config;
				}
			});
	
	private final LoadingCache<Tuple2<EntityId, String>, SearchOrders> searchOrderCache = CacheBuilder.newBuilder()
			.maximumSize(MAXIMUM_NUMBER_OF_CACHED_CONFIGS)
			.build(new CacheLoader<>() {
				@Override
				@Nullable
				public SearchOrders load(@Nullable final Tuple2<EntityId, String> keyOrNull) {
					final EntityId projectId = assertNotNull(keyOrNull).a;
					return loadSearchOrders(projectId);
				}
				
				private SearchOrders loadSearchOrders(final EntityId projectId) {
					final ProjectPojo project = projectService.get(projectId);
					return new SearchOrders(project.getSearchOrders());
				}
			});
	
	/**
	 * Gets the discovery config for the given project.
	 * <p>
	 * The jobId must also be given as it is used as a cache key:
	 * it is expected that the config can change between jobs, but for the same job the same config is always returned from cache.
	 * 
	 * @param projectId the project id
	 * @param jobId the job id
	 * @return the discovery config for the given project, valid for tasks of the given job
	 */
	public Config getConfig(final EntityId projectId, final String jobId) {
		try {
			return configCache.get(new Tuple2<>(projectId, jobId));
		} catch (final ExecutionException e) {
			throw new IllegalStateException("Unable to load Discovery configuration for project " + projectId, e);
		}
	}
	
	public SearchOrders getSearchOrders(final EntityId projectId, final String jobId) {
		try {
			return searchOrderCache.get(new Tuple2<>(projectId, jobId));
		} catch (final ExecutionException e) {
			throw new IllegalStateException("Unable to load Discovery SearchOrders for project " + projectId, e);
		}
	}
	
}
