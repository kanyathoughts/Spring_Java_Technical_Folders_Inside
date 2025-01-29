/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared;

import java.util.List;
import java.util.function.Function;

/**
 * Interface for a function maintaining a cache. The function receives a key and a value Supplier.
 * It may then supply the value from a cache or call the provided supplier and store the returned value under the respective key.
 * @param <K> Type of the key.
 * @param <V> Type of the value.
 */
public interface CachingFunction<K, V> {
	
	/**
	 * Applies the caching function.
	 * @param key Key by which to find a value in the cache.
	 * @param loader Supplier for the value, if it is not in the cache yet.
	 * @return The corresponding value, retrieved either form the cache or via the loader.
	 */
	public V apply(K key, Function<K, V> loader);
	
	/**
	 * Applies the caching function.
	 * @param key Keys for which to find values in the cache.
	 * @param loader Supplier for all values, not yet in the cache.
	 * @return The corresponding value, retrieved either form the cache or via the loader.
	 */
	public List<V> apply(List<K> key, Function<List<K>, List<V>> loader);
	
}
