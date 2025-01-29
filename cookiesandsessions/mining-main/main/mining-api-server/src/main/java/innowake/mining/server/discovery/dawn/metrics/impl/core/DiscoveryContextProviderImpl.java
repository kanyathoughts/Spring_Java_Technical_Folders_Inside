/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.core;

import java.util.EnumMap;
import java.util.Map;

import org.ff4j.FF4j;
import org.springframework.stereotype.Service;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import brave.Span;
import brave.Tracer;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.discovery.PersistingSourceObjectResolver;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.config.DiscoveryConfigAccessor;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContextProvider;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleParameters;

/**
 * Default provider for DiscoveryContext.
 */
@Service
public class DiscoveryContextProviderImpl implements DiscoveryContextProvider {

	private final SourceCachingService sourceService;
	private final DiscoveryConfigAccessor configAccessor;
	private final Tracer tracer;
	private final FF4j ff4j;
	private LoadingCache<String, Map<FeatureId, Boolean>> featureMapCache;
	
	/**
	 * Creates a new discovery context provider.
	 * 
	 * @param sourceService required to build the {@linkplain DiscoveryContext#getSourceObjectResolver() source object resolver}
	 * @param configAccessor required to access the {@linkplain DiscoveryContext#getConfig() configuration}
	 * @param tracer required to build the {@linkplain DiscoveryContext#getTimedWorker() timed worker}
	 * @param ff4j required to load the current FF4j feature settings
	 */
	public DiscoveryContextProviderImpl(final SourceCachingService sourceService, final DiscoveryConfigAccessor configAccessor, final Tracer tracer,
			final FF4j ff4j) {
		this.sourceService = sourceService;
		this.configAccessor = configAccessor;
		this.tracer = tracer;
		this.ff4j = ff4j;
		this.featureMapCache = CacheBuilder.newBuilder()
				.maximumSize(10)
				.build(new CacheLoader<String, Map<FeatureId, Boolean>>() {
					@Override
					public Map<FeatureId, Boolean> load(final String key) throws Exception {
						return getFeatureMap();
					}
				});
	}

	@Override
	public DiscoveryContext createContext(final EntityId projectId, final String jobId, final int iteration, final ModuleParameters moduleParameters,
										  final ProgressMonitor progressMonitor, final Span span) {
		final Config config = configAccessor.getConfig(projectId, jobId);
		final SearchOrders searchOrders = configAccessor.getSearchOrders(projectId, jobId);
		final SourceObjectResolver sourceObjectResolver = new PersistingSourceObjectResolver(sourceService, searchOrders);

		return new DiscoveryContextImpl(projectId, jobId, iteration, progressMonitor, config, searchOrders, sourceObjectResolver, moduleParameters, tracer, span,
				featureMapCache.getUnchecked(jobId));
	}

	private Map<FeatureId, Boolean> getFeatureMap() {
		final Map<FeatureId, Boolean> featureMap = new EnumMap<>(FeatureId.class);
		for (final FeatureId feature : FeatureId.values()) {
			featureMap.put(feature, Boolean.valueOf(ff4j.getFeature(feature.getId()).isEnable()));
		}
		return featureMap;
	}
}
