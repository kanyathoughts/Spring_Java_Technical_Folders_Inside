/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.cache;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.cache.interceptor.CacheOperationInvocationContext;
import org.springframework.cache.interceptor.CacheResolver;

import innowake.lib.core.api.lang.NonNull;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.annotation.ProjectIdArgument;
import innowake.mining.server.util.CacheNameUtil;
import innowake.mining.shared.access.EntityId;

public class ProjectAwareCacheResolver implements CacheResolver {

	private static final Logger LOG = LoggerFactory.getLogger(ProjectAwareCacheResolver.class);
	
	private final CacheManager cacheManager;
	private final MiningCacheConfig miningCacheConfig;

	public ProjectAwareCacheResolver(final CacheManager cacheManager, final MiningCacheConfig miningCacheConfig) {
		this.cacheManager = cacheManager;
		this.miningCacheConfig = miningCacheConfig;
	}

	@Override
	@NonNull
	public Collection<? extends Cache> resolveCaches(final CacheOperationInvocationContext<?> context) {
		if (context.getOperation().getCacheNames().isEmpty()) {
			throw new IllegalArgumentException("at least one cache name must be provided");
		}

		return context.getOperation().getCacheNames().stream()
				.map(cacheName -> {
					final var projectId = resolveProjectId(context.getMethod(), context.getArgs());
					final String finalCacheName;
					if ( ! projectId.isEmpty()) {
						finalCacheName = CacheNameUtil.getProjectCacheName(cacheName, projectId.get());
					} else {
						finalCacheName = cacheName;
					}
					LOG.trace(() -> "Resolved cache " + finalCacheName + " for operation " + context.getOperation());
					return finalCacheName;
				})
				.map(cacheManager::getCache)
				.collect(Collectors.toList());
	}

	private Optional<EntityId> resolveProjectId(final Method method, final Object[] args) {
		final Annotation[][] annotationMatrix = method.getParameterAnnotations();
		for (int i = 0; i < args.length; i++) {
			final Annotation[] annotations = annotationMatrix[i];
			for (final Annotation annotation : annotations) {
				if (annotation.annotationType() == ProjectIdArgument.class) {
					return Optional.of(miningCacheConfig.ensureProjectNid(args[i] instanceof EntityId ? (EntityId) args[i] : EntityId.of(args[i].toString())));
				}
			}
		}
		
		return Optional.empty();
	} 
}
