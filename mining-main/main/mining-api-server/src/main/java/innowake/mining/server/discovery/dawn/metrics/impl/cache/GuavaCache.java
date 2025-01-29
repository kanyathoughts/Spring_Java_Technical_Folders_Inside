/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.cache;


import com.google.common.cache.CacheBuilder;
import innowake.lib.core.api.lang.NonNull;
import org.springframework.cache.Cache;
import org.springframework.cache.support.SimpleValueWrapper;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicBoolean;

public class GuavaCache implements Cache {

	private final String name;
	private final com.google.common.cache.Cache<Object, Object> cache;

	public GuavaCache(final String name, final CacheBuilder<Object, Object> builder) {
		this.name = name;
		cache = builder.build();
	}

	@Override
	@NonNull
	public String getName() {
		return name;
	}

	@Override
	@NonNull
	public Object getNativeCache() {
		return cache;
	}

	@Override
	public ValueWrapper get(@NonNull final Object key) {
		final Object value = cache.getIfPresent(key);
		return value != null ? new SimpleValueWrapper(value) : null;
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T> T get(@NonNull final Object key, final Class<T> type) {
		final Object value = cache.getIfPresent(key);
		if (value == null) {
			return null;
		} else if (type != null) {
			return type.cast(value);
		} else {
			return (T) value;
		}
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T> T get(@NonNull final Object key, @NonNull final Callable<T> valueLoader) {
		try {
			return (T) cache.get(key, valueLoader);
		} catch (final ExecutionException e) {
			throw new IllegalStateException(e);
		}
	}

	@Override
	public void put(@NonNull final Object key, final Object value) {
		cache.put(key, value);
	}

	@Override
	public ValueWrapper putIfAbsent(@NonNull final Object key, final Object value) {
		final AtomicBoolean valueComputed = new AtomicBoolean(false);
		final Object newValue = get(key, () -> {
			valueComputed.set(true);
			return value;
		});
		if (valueComputed.get()) {
			return null;
		}
		return newValue != null ? new SimpleValueWrapper(newValue) : null;
	}

	@Override
	public void evict(@NonNull final Object key) {
		cache.invalidate(key);
	}

	@Override
	public void clear() {
		cache.invalidateAll();
	}
}

