/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.model.discovery;

import java.io.Serializable;
import java.util.Collection;
import java.util.Comparator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;

/**
 * A map for holding dynamic attributes. 
 * 
 * Keys to the map should be constant and placed in {@link innowake.mining.data.model.discovery.attribute.ModelAttributeKey} so that the map can be easily accessed
 * in other areas of discovery.
 * Values for the do can be dynamic but if they are constant they can be placed in {@link innowake.mining.data.model.discovery.attribute.ModelAttributeValue}.
 *
 * @param <V> Values for the map.
 */
public class ModelAttributeMap<V> implements Map<ModelAttributeKey, V>, Serializable {
	
	private static final Gson GSON = new GsonBuilder()
			.disableHtmlEscaping()
			.create();

	private final TreeMap<ModelAttributeKey, V> attributeMap;
	
	public ModelAttributeMap() {
		attributeMap = new TreeMap<>();
	}
	
	public ModelAttributeMap(Comparator<ModelAttributeKey> comparator) {
		attributeMap = new TreeMap<>(comparator);
	}

	@Override
	public int size() {
		return attributeMap.size();
	}

	@Override
	public boolean isEmpty() {
		return attributeMap.isEmpty();
	}

	@Override
	public boolean containsKey(@Nullable final Object key) {
		return attributeMap.containsKey(key);
	}

	@Override
	public boolean containsValue(@Nullable final Object value) {
		return attributeMap.containsValue(value);
	}

	@Override
	public V get(@Nullable final Object key) {
		return attributeMap.get(key);
	}

	@Override
	public V put(@Nullable final ModelAttributeKey key, @Nullable final V value) {
		return attributeMap.put(key, value);
	}
	
	@Override
	public V remove(@Nullable final Object key) {
		return attributeMap.remove(key);
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public void putAll(@Nullable final Map m) {
		attributeMap.putAll(m);
	}

	@Override
	public void clear() {
		attributeMap.clear();
	}

	@Override
	public Set<ModelAttributeKey> keySet() {
		return attributeMap.keySet();
	}

	@Override
	public Collection<V> values() {
		return attributeMap.values();
	}

	@Override
	public Set<Map.Entry<ModelAttributeKey, V>> entrySet() {
		return attributeMap.entrySet();
	}
	
	/**
	 * Converts the map into a JSON string using GSON library.
	 */
	@Override
	public String toString() {
		return GSON.toJson(attributeMap);
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}

		if (obj == null) {
			return false;
		}

		return attributeMap.equals(obj);
	}

	@Override
	public int hashCode() {
		return attributeMap.hashCode();
	}

	/**
	 * Converts a JSON String into a ModelAttributeMap
	 * 
	 * @param jsonMapString The JSON string to convert to a ModelAttributeMap.
	 * @return A ModelAttributeMap with the save keys and values in the jsonMapString.
	 */
	@SuppressWarnings("unchecked")
	public static <V extends Object> ModelAttributeMap<V> fromString(final String jsonMapString) {
		if ("{}".equals(jsonMapString)) {
			return new ModelAttributeMap<>();
		}

		/* WMIN-1961: Don't use new TypeToken<ModelAttributeMap<V>>(){}.getType() as it causes an NPE in gson */
		return GSON.fromJson(jsonMapString, ModelAttributeMap.class);
	}
}
