/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import innowake.mining.shared.CachingFunction;

/**
 * Simple cache using a HashMap.
 * @param <K> Type of keys.
 * @param <V> Type of values.
 */
public class HashCache<K, V> extends HashMap<K, V> implements CachingFunction<K, V> {
	
	@Override
	public V apply(final K key, final Function<K, V> loader) {
		return this.computeIfAbsent(key, loader);
	}
	
	@Override
	public List<V> apply(final List<K> key, final Function<List<K>, List<V>> loader) {
		if (! keySet().containsAll(key)) {
			final var value = loader.apply(key);
			for (int n = 0; n < key.size(); n++) {
				put(key.get(n), value.get(n));
			}
		}
		return key.stream().map(this::get).collect(Collectors.toList());
	}
	
	public void putAll(final Collection<V> values, final Function<V, K> keyMapper) {
		values.stream().forEach(v -> put(keyMapper.apply(v), v));
	}
	
}
