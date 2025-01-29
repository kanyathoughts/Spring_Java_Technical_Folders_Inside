/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;

/**
 * Provides access to a property of a Map.
 * @param <T> Type of the property.
 * @param <U> Type inside the Map, if different.
 */
public abstract class AbstractMapAccessor<T, U> {
	
	public final String key;
	
	protected AbstractMapAccessor(final String key) {
		this.key = key;
	}
	
	@Nullable
	protected abstract T outputValue(@Nullable U value);
	
	@Nullable
	protected abstract U inputValue(@Nullable T value);
	
	/***
	 * Retrieves the value of the property from a Map.
	 * @param map Map in which to lookup the property under its defined key.
	 * @return Value of the property which may be {@code null}.
	 */
	@Nullable
	@SuppressWarnings("unchecked")
	public T getFrom(@Nullable final Map<String, ?> map) {
		if (map != null) {
			return outputValue((U) map.get(key));
		}
		return null;
	}
	
	/***
	 * Retrieves the value of the property from a Map as an Optional.
	 * @param map Map in which to lookup the property under its defined key.
	 * @return Optional that contains the value of the property or is empty if the value is {@code null} or its key is not present on the Map.
	 */
	public Optional<T> optionalFrom(@Nullable final Map<String, ?> map) {
		return Optional.ofNullable(getFrom(map));
	}
	
	/***
	 * Retrieves the value of the property from a Map, requiring it to be present.
	 * @param map Map in which to lookup the property under its defined key.
	 * @return Value of the property.
	 * @throws NoSuchElementException If the value is {@code null} or its key is not present on the Map.
	 */
	public T requireFrom(@Nullable final Map<String, ?> map) {
		return optionalFrom(map).orElseThrow();
	}
	
	/**
	 * Associates the specified value with the property's defined key in a Map.
	 * @param map Map to put the value in.
	 * @param value Value for the property.
	 * @return Previous value of the property in the Map, if any.
	 */
	@Nullable
	@SuppressWarnings("unchecked")
	public T setIn(final Map<String, ?> map, @Nullable final T value) {
		return outputValue(((Map<String, U>) map).put(key, inputValue(value)));
	}
	
	/**
	 * Sets the property's defined key in a Map to the value of the provided Optional
	 * or removes it from the Map in case the Optional is empty.
	 * @param map Map to modify.
	 * @param value Value for the property.
	 * @return Previous value of the property in the Map, if any.
	 */
	@Nullable
	public T optionalTo(final Map<String, ?> map, final Optional<T> value) {
		if (value.isPresent()) {
			return setIn(map, value.get());
		}
		return removeFrom(map);
	}
	
	/**
	 * Sets the property's defined key in a Map to the value of the provided value
	 * or removes it from the Map in case the value is {@code null}.
	 * @param map Map to modify.
	 * @param value Value for the property.
	 * @return Previous value of the property in the Map, if any.
	 */
	@Nullable
	public T nonNullTo(final Map<String, ?> map, @Nullable final T value) {
		if (value != null) {
			return setIn(map, value);
		}
		return removeFrom(map);
	}
	
	/**
	 * Removes the property's defined key from a Map.
	 * @param map Map to modify.
	 * @return Previous value of the property in the Map, if any.
	 */
	@Nullable
	@SuppressWarnings("unchecked")
	public T removeFrom(@Nullable final Map<String, ?> map) {
		if (map != null) {
			return outputValue((U) map.remove(key));
		}
		return null;
	}
	
}
