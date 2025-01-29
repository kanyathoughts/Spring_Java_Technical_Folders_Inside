/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.function.Function;

import innowake.lib.core.api.lang.Nullable;

/**
 * Provides access to a defined property of a Map, allowing to convert it as it is read or written.
 * @param <T> Outside type of the property.
 * @param <U> Type of the property inside the Map.
 */
public class MapPropertyConverter<T, U> extends AbstractMapAccessor<T, U> {
	
	private final Function<U, T> getter;
	private final Function<T, U> setter;
	
	/**
	 * Define access to a property of a Map.
	 * @param key Name under which the property's value is stored in the Map.
	 * @param getter Function converting the value type inside the Map to the value type used by the application.
	 * @param setter Function converting the application type of the value to the value type used inside the Map.
	 */
	public MapPropertyConverter(final String key, final Function<U, T> getter, final Function<T, U> setter) {
		super(key);
		this.getter = getter;
		this.setter = setter;
	}
	
	@Nullable
	@Override
	protected U inputValue(@Nullable final T value) {
		return value != null ? setter.apply(value) : null; 
	}
	
	@Nullable
	@Override
	protected T outputValue(@Nullable final U value) {
		return value != null ? getter.apply(value) : null;
	}
	
}
