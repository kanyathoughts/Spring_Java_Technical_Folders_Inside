/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;

/**
 * Helper for creating EnumMaps.
 * @param <K> Key type.
 * @param <V> Value type.
 */
public class EnumMapBuilder<K extends Enum<K>, V> {
	
	public static class Factory<K extends Enum<K>> {
		private final Class<K> type;
		
		private Factory(final Class<K> keyType) {
			this.type = keyType;
		}
		
		/**
		 * Create a new builder for mapping Enum entries to values. 
		 * @param <V> Value type.
		 * @return new EnumMapBuilder
		 */
		public <V> EnumMapBuilder<K, V> create() {
			return new EnumMapBuilder<>(type);
		}
	}
	
	/**
	 * Create an EnumMapBuilder factory.
	 * @param <K> Key type.
	 * @param keyType Enum class.
	 * @return EnumMapBuilder factory for the specified Enum.
	 */
	public static <K extends Enum<K>> Factory<K> of(final Class<K> keyType) {
		return new Factory<>(keyType);
	}
	
	private final Class<K> type;
	private final EnumMap<K, V> map;
	
	private EnumMapBuilder(final Class<K> keyType) {
		type = keyType;
		map = new EnumMap<>(keyType);
	}
	
	public EnumMapBuilder<K, V> put(final K key, final V value) {
		map.put(key, value);
		return this;
	}
	
	public Map<K, V> build() {
		if (map.size() != type.getEnumConstants().length) {
			throw new IllegalStateException("Enum mapping incomplete");
		}
		return Collections.unmodifiableMap(map);
	}
	
}
