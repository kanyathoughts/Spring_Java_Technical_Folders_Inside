/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import innowake.lib.core.api.lang.Nullable;

/**
 * Provides access to a defined property of a Map.
 * @param <T> Type of the property.
 */
public class MapPropertyAccessor<T> extends AbstractMapAccessor<T, T> {
	
	/**
	 * Define access to a property of a Map.
	 * @param key Name under which the property's value is stored in the Map.
	 */
	public MapPropertyAccessor(final String key) {
		super(key);
	}
	
	@Override
	@Nullable
	protected T outputValue(@Nullable final T value) {
		return value;
	}
	
	@Override
	@Nullable
	protected T inputValue(@Nullable final T value) {
		return value;
	}
	
}
