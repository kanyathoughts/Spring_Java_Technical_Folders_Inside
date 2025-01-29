/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.cache;

import com.orientechnologies.orient.core.id.ORecordId;
import com.orientechnologies.orient.core.record.OVertex;
import innowake.lib.core.api.lang.NonNull;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.cache.concurrent.ConcurrentMapCache;
import org.springframework.cache.support.NoOpCache;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

/**
 * Provides request-scoped caches for entity proxies. Caching is disabled (i.e. returns {@link NoOpCache}) when no request is active.
 * <p>
 *     This manager provides the following caches:
 *     <ul>
 *         <li>{@value #VERTEX_CACHE_NAME} caches {@link IEntityProxy} instances using {@link OVertex} instance as key</li>
 *         <li>{@value #RECORD_CACHE_NAME} caches {@link IEntityProxy} instances using {@link ORecordId} as key</li>
 *     </ul>
 * </p>
 */
public class OrientRequestCacheManager implements CacheManager {

	public static final String VERTEX_CACHE_NAME = "vertexBasedLazyEntityCache";
	public static final String RECORD_CACHE_NAME = "recordBasedLazyEntityCache";

	private static final String NAME_PREFIX = "orientRequestCache:";

	@Override
	public Cache getCache(@NonNull final String name) {
		final RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
		if (requestAttributes == null) {
			return new NoOpCache(name);
		}
		Cache cache = (Cache) requestAttributes.getAttribute(NAME_PREFIX + name, RequestAttributes.SCOPE_REQUEST);
		if (cache == null) {
			cache = createCache(name, requestAttributes);
		}
		return cache;
	}

	@Override
	@NonNull
	public Collection<String> getCacheNames() {
		if (RequestContextHolder.getRequestAttributes() == null) {
			/* outside a request, this CacheManager does not know any caches */
			return Collections.emptyList();
		} else {
			return Arrays.asList(VERTEX_CACHE_NAME, RECORD_CACHE_NAME);
		}
	}

	private Cache createCache(final String name, final RequestAttributes requestAttributes) {
		final Cache cache = new ConcurrentMapCache(name);
		requestAttributes.setAttribute(NAME_PREFIX + name, cache, RequestAttributes.SCOPE_REQUEST);
		requestAttributes.registerDestructionCallback(NAME_PREFIX + name, cache::clear, RequestAttributes.SCOPE_REQUEST);
		return cache;
	}
}
