/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;

/**
 * View for a Map containing string-keyed properties and possibly other Maps with more properties or Maps.
 */
public class NestedMap implements Map<String, Object> {
	
	private final Map<String, Object> map;
	
	/**
	 * Creates a nested view for an new {@link HashMap}.
	 */
	public NestedMap() {
		this.map = new HashMap<>();
	}
	
	/**
	 * Creates a nested view for a Map.
	 * @param map Any kind of Map to be wrapped. A {@code null} object corresponds to an immutable empty Map.
	 */
	public NestedMap(@Nullable final Map<String, Object> map) {
		this.map = map == null ? Collections.emptyMap() : map;
	}
	
	private NestedMap wrap(@Nullable final Map<String, Object> map) {
		if (map instanceof NestedMap) {
			return (NestedMap) map;
		}
		return new NestedMap(map);
	}
	
	/**
	 * Sets the value for a property on the current level.
	 * @param key Name of the property.
	 * @param value New value for the property.
	 * @return This Map.
	 */
	public NestedMap set(final String key, final Object value) {
		map.put(key, value);
		return this;
	}
	
	/**
	 * Sets the value for a property on the next lower level.
	 * @param parent Name of the properties group.
	 * @param key Name of the property.
	 * @param value New value for the property.
	 * @return This Map.
	 */
	@SuppressWarnings("unchecked")
	public NestedMap set(final String parent, final String key, final Object value) {
		((Map<String, Object>) map.computeIfAbsent(parent, k -> new HashMap<String, Object>())).put(key, value);
		return this;
	}
	
	/**
	 * Gets the value for a property on the current level.
	 * @param <T> Type of the property.
	 * @param key Name of the property.
	 * @return Value of the property.
	 * @throws NoSuchElementException If the property does not exist or the Map entry is set to {@code null}.
	 */
	public <T> T getValue(final String key) {
		return this.<T>getOptional(key).orElseThrow(() -> new NoSuchElementException(key));
	}
	
	/**
	 * Gets the value for a property on the current level if it is define and not {@code null}.
	 * @param <T> Type of the property.
	 * @param key Name of the property.
	 * @return Value of the property.
	 */
	@SuppressWarnings("unchecked")
	public <T> Optional<T> getOptional(final String key) {
		return Optional.ofNullable((T) map.get(key));
	}
	
	/**
	 * Gets the value for a property on the next lower level.
	 * @param parent Name of the properties group.
	 * @param key Name of the property.
	 * @return Value of the property.
	 * @throws NoSuchElementException If the property or its parent does not exist or the respective Map entry is set to {@code null}.
	 */
	public <T> T getValue(final String parent, final String key) {
		return this.<T>getOptional(parent, key).orElseThrow(() -> new NoSuchElementException(parent + "." + key));
	}
	
	/**
	 * Gets the value for a property on the next lower level.
	 * @param parent Name of the properties group.
	 * @param key Name of the property.
	 * @return Value of the property.
	 * @throws NoSuchElementException If the property or its parent does not exist or the respective Map entry is set to {@code null}.
	 */
	@SuppressWarnings("unchecked")
	public <T> Optional<T> getOptional(final String parent, final String key) {
		return this.<Map<String, Object>>getOptional(parent).map(sub -> (T) sub.get(key));
	}
	
	@SuppressWarnings("unchecked")
	/**
	 * Gets the value for a property at the specified path.
	 * @param <T> Type of the property.
	 * @param path Sequence of sub-element keys.
	 * @return Value of the last property if none of the properties on the path is {@code null} and none of the parent properties is not a Map.
	 */
	public <T> Optional<T> getOptionalAt(final String... path) {
		final Iterator<String> pathNodes = Arrays.stream(path).iterator();
		Object current = map;
		while (pathNodes.hasNext()) {
			if (! (current instanceof Map)) {
				return Optional.empty();
			}
			current = ((Map<String, Object>) current).get(pathNodes.next());
		}
		return Optional.ofNullable((T) current);
	}
	
	@SuppressWarnings("unchecked")
	protected Map<String, Object> subMap(final String key) {
		final Object sub = map.get(key);
		if (sub == null) {
			throw new NoSuchElementException(key);
		}
		if (! (sub instanceof Map)) {
			throw new IllegalAccessError("Element is not a Map: " + key);
		}
		return (Map<String, Object>) sub;
	}
	
	/**
	 * Retrieves a Map on the next lower level.
	 * @param key Name of the element on the current level containing the Map.
	 * @return Map of properties on a lower level.
	 * @throws NoSuchElementException If the key does not exist or is {@code null}.
	 * @throws IllegalAccessError If the key is defined but not a Map.
	 */
	public NestedMap getSub(final String key) {
		return wrap(subMap(key));
	}
	
	@SuppressWarnings("unchecked")
	@Nullable
	protected Map<String, Object> subMapAt(final String... path) {
		return getOptionalAt(path).map(m -> m instanceof Map ? (Map<String, Object>) m : null).orElse(null);
	}
	
	/**
	 * Retrieves a Map at the specified path.
	 * @param path Sequence of sub-element keys.
	 * @return Map at the last path element or an empty immutable Map if any parent elements or the element itself is {@code null} or not a Map.
	 */
	public NestedMap getSubAt(final String... path) {
		Map<String, Object> sub = subMapAt(path);
		if (sub instanceof NestedMap) {
			return (NestedMap) sub;
		}
		return new NestedMap(sub);
	}
	
	@Override
	public int size() {
		return map.size();
	}
	
	@Override
	public boolean isEmpty() {
		return map.isEmpty();
	}
	
	@Override
	public boolean containsKey(@Nullable final Object key) {
		return map.containsKey(key);
	}
	
	@Override
	public boolean containsValue(@Nullable final Object value) {
		return map.containsValue(value);
	}	

	@Override
	public Object get(@Nullable final Object key) {
		return map.get(key);
	}
	
	@Override
	public Object put(@Nullable final String key, @Nullable final Object value) {
		return map.put(key, value);
	}
	
	@Override
	public Object remove(@Nullable final Object key) {
		return map.remove(key);
	}
	
	@Override
	public void putAll(@Nullable final Map<? extends String, ? extends Object> m) {
		map.putAll(m);
	}
	
	@Override
	public void clear() {
		map.clear();
	}
	
	@Override
	public Set<String> keySet() {
		return map.keySet();
	}
	
	@Override
	public Collection<Object> values() {
		return map.values();
	}
	
	@Override
	public Set<Entry<String, Object>> entrySet() {
		return map.entrySet();
	}
	
	@SuppressWarnings("unchecked")
	public Set<Entry<String, Map<String, Object>>> subSet() {
		return map.entrySet().stream().filter(e -> e.getValue() instanceof Map)
				.map(e -> (Entry<String, Map<String, Object>>) (Entry<String, ?>) e).collect(Collectors.toSet());
	}
	
}
