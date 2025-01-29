/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.entities.SourcePojo;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;

import java.util.Collections;
import java.util.List;

/**
 * Base class that provides {@link SourceObjectDependency} for {@link SourcePojo}s.
 * 
 * @param <D> the actual {@link SourceObjectDependency} type 
 * @param <P> the actual parse result type 
 */
public abstract class AbstractSourceObjectManager<D extends SourceObjectDependency, P> implements SourceObjectManager<D> {

	protected static final Logger LOG = LoggerFactory.getLogger(Logging.OBJECT_RESOLVING);
	
	protected final SourceObjectResolver sourceObjectResolver;
	
	private final MultiValuedMap<Long, D> sourceObjectToDependencies = new ArrayListValuedHashMap<>();
	private final LoadingCache<SourcePojo, P> parseResultCache;
	
	/**
	 * Constructor.
	 * 
	 * @param sourceObjectResolver instance of {@link SourceObjectResolver}
	 * @param configProperties instance of {@link GenericConfigProperties}
	 */
	protected AbstractSourceObjectManager(final SourceObjectResolver sourceObjectResolver, final GenericConfigProperties configProperties) {
		this.sourceObjectResolver = sourceObjectResolver;
		final CacheLoader<SourcePojo, P> loader = new CacheLoader<SourcePojo, P>() {
			
			@Override
			public P load(@Nullable final SourcePojo key) {
				return parse(Assert.assertNotNull(key));
			}
		};
		final CacheBuilder<Object, Object> builder = CacheBuilder.newBuilder().maximumSize(configProperties.getDiscoveryLightweightParseResultCacheSize());
		if ( ! configProperties.isDiscoveryCacheStrongReferences()) {
			builder.weakKeys().softValues();
		}
		parseResultCache = builder.build(loader);
	}
	
	/**
	 * Provides a list of outgoing {@link SourceObjectDependency} for the given {@link SourcePojo}.
	 *
	 * @param sourceObject the {@link SourcePojo} to provide the dependencies for
	 * @return the list of outgoing {@link SourceObjectDependency} for the given {@link SourcePojo}
	 */
	@Override
	public List<D> getOutgoingDependencies(final SourcePojo sourceObject) {
		if ( ! sourceObjectToDependencies.containsKey(sourceObject.getId())) {
			calculateDependencies(sourceObject, getParseResult(sourceObject));
		}
		return Collections.unmodifiableList((List<D>) sourceObjectToDependencies.get(sourceObject.getId()));
	}
	
	/**
	 * Parses the given {@link SourcePojo} and returns the parse result.
	 *
	 * @param sourceObject the {@link SourcePojo}
	 * @return the parse result
	 */
	protected abstract P parse(final SourcePojo sourceObject);
	
	/**
	 * Calculates the {@link SourceObjectDependency} for the given {@link SourcePojo} and parse result.
	 *
	 * @param sourceObject the {@link SourcePojo}
	 * @param parseResult the parse result
	 */
	protected abstract void calculateDependencies(final SourcePojo sourceObject, final P parseResult);
	
	/**
	 * Provides the parse result for a given {@link SourcePojo}.
	 *
	 * @param sourceObject the {@link SourcePojo}
	 * @return the parse result for the given {@link SourcePojo}
	 */
	protected P getParseResult(final SourcePojo sourceObject) {
		return parseResultCache.getUnchecked(sourceObject);
	}
	
	/**
	 * Adds an outgoing dependency for a {@link SourcePojo} to this manager.
	 *
	 * @param sourceObject the {@link SourcePojo}
	 * @param dependency the outgoing {@link SourceObjectDependency}
	 */
	protected void registerOutgoingDependency(final SourcePojo sourceObject, final D dependency) {
		sourceObjectToDependencies.put(sourceObject.getId(), dependency);
	}
}	
