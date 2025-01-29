/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared;

import java.util.Optional;
import java.util.function.Supplier;

import com.fasterxml.jackson.annotation.JsonCreator;

import innowake.lib.core.api.lang.Nullable;

/**
 * Supplier for the value of a key.
 * @param <K> Type of the key.
 * @param <V> Type of the value.
 */
public class KeyedSupplier<K, V> implements Supplier<V> {
	
	private final K key;
	private final Optional<Supplier<V>> supplier;
	
	/**
	 * Creates a keyed Supplier with a Function to retrieve the value for the key.
	 * @param key Key of the value.
	 * @param supplier Function providing the value.
	 */
	public KeyedSupplier(final K key, @Nullable final Supplier<V> supplier) {
		this.key = key;
		this.supplier = Optional.ofNullable(supplier);
	}
	
	@JsonCreator
	public KeyedSupplier(final K key) {
		this(key, (Supplier<V>) null);
	}
	
	public K getKey() {
		return key;
	}
	
	public Optional<Supplier<V>> getSupplier() {
		return supplier;
	}
	
	@Override
	public V get() {
		return supplier.map(Supplier::get).orElseThrow();
	}
	
	@Override
	public String toString() {
		return super.toString() + ": " + key;
	}
	
}
