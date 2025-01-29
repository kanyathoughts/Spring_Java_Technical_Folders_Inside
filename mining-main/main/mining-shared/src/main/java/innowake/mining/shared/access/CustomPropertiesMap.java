/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collections;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonCreator;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.lang.NestedMap;

/**
 * Wrapper for a Map of custom properties.
 * Effectively an immutable {@link NestedMap}.
 */
public class CustomPropertiesMap extends NestedMap {
	
	private static final CustomPropertiesMap EMPTY = new CustomPropertiesMap(null);
	
	/**
	 * Creates an immutable view of custom properties using the specified Map.
	 * @param map Map of custom property entries. A {@code null} value is treated as an empty Map.
	 */
	@JsonCreator
	public CustomPropertiesMap(@Nullable final Map<String, Object> map) {
		super(map != null ? Collections.unmodifiableMap(map) : Collections.emptyMap());
	}
	
	/**
	 * Returns an empty an immutable custom properties Map.
	 * @return Empty CustomPropertiesMap.
	 */
	public static CustomPropertiesMap empty() {
		return EMPTY;
	}
	
	private CustomPropertiesMap wrapCustomPropertiesMap(@Nullable final Map<String, Object> map) {
		if (map instanceof CustomPropertiesMap) {
			return (CustomPropertiesMap) map;
		}
		return new CustomPropertiesMap(map);
	}
	
	@Override
	public CustomPropertiesMap getSub(final String key) {
		return wrapCustomPropertiesMap(subMap(key));
	}
	
	@Override
	public CustomPropertiesMap getSubAt(String... path) {
		return wrapCustomPropertiesMap(subMapAt(path));
	}
	
}
