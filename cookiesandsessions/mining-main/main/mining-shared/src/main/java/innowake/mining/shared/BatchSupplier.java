/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.lib.core.api.lang.Nullable;

/**
 * Provides lazy loading for a batch of keyed values.
 * After creation, keys may be added by calling {@link #supply(Object)}.
 * When {@link #apply(Object)} or {@link #getMap()} or any of the provided Suppliers is invoked for the first time the entire batch is loaded.
 * After that, no new keys can be added.
 * @param <K> Type of the keys.
 * @param <V> Type of the values.
 */
public class BatchSupplier<K, V> implements Function<K, V> {
	
	private final Stream.Builder<K> keys;
	private final CachingSupplier<Map<K, V>> values;
	
	/**
	 * @param loader Function retrieving the values for a batch of keys.
	 */
	public BatchSupplier(final Function<List<K>, Map<K, V>> loader) {
		keys = Stream.builder();
		values = new CachingSupplier<>(() -> loader.apply(keys.build().collect(Collectors.toList())));
	}
	
	@Override
	public V apply(@Nullable final K key) {
		return values.get().get(key);
	}
	
	/**
	 * Adds a key to the batch and returns a Supplier for its value. 
	 * @param key Identifier of a value.
	 * @return Supplier for the value associated with the key.
	 * @throws IllegalStateException If the batch has already been loaded.
	 */
	public Supplier<V> supply(final K key) {
		keys.accept(key);
		return () -> apply(key);
	}
	
	/**
	 * @return Entire batch of keys with the loaded values.
	 */
	public Map<K, V> getMap() {
		return Collections.unmodifiableMap(values.get());
	}
	
}
