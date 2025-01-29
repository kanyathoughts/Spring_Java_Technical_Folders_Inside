/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.shared;

import java.io.Serializable;
import java.util.AbstractMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import innowake.lib.core.api.lang.Nullable;

/**
 * Container for a value that may be defined or not and may additionally be nullable.
 * This is similar to an entry in a {@link java.util.Map} where a key may not be present/defined,
 * but may also be explicitly set to {@code null}, in contrast to an {@link java.util.Optional} which can either be empty or not {@code null}.
 * This is intended to be used in data update processes to determine if a value should be
 * changed, or not be modified at all. While changing the value may include setting it to {@code null}.
 * @param <T> Type of the contained value.
 */
public class Definable<T> implements Serializable {
	
	public static class ValueException extends RuntimeException {
		protected final transient Definable<?> ref;
		
		protected ValueException(final Definable<?> ref) {
			super(ref.description);
			this.ref = ref;
		}
		
		/**
		 * Gets the Definable the Exception originated from.
		 * @return The Definable the Exception refers to.
		 */
		public Definable<?> getReference() {
			return ref;
		}
	}
	
	public static class ValueNotNullableException extends ValueException {
		protected ValueNotNullableException(final Definable<?> ref) {
			super(ref);
		}
	}
	
	public static class ValueIsNullException extends ValueException {
		protected ValueIsNullException(final Definable<?> ref) {
			super(ref);
		}
	}
	
	public static class ValueNotDefinedException extends ValueException {
		protected ValueNotDefinedException(final Definable<?> ref) {
			super(ref);
		}
	}
	
	public static class DefinitionNotAllowedException extends ValueException {
		protected DefinitionNotAllowedException(final Definable<?> ref) {
			super(ref);
		}
	}
	
	@Nullable
	private T value;
	private boolean defined;
	private final boolean nullable;
	private final String description;
	@Nullable
	private transient Map.Entry<Definable<?>, Supplier<T>> proxy = null;
	
	@Nullable
	private T currentValue() {
		if (proxy != null) {
			return proxy.getValue().get();
		}
		return value;
	}
	
	/**
	 * Determines if the value of the container is defined.
	 * @return If a value has been set.
	 */
	public boolean isDefined() {
		if (proxy != null) {
			return proxy.getKey().isDefined();
		}
		return defined;
	}
	
	/**
	 * @return {@code true}, if a non-{@code null} value is present, otherwise {@code false}
	 */
	public boolean isPresent() {
		return isDefined() && currentValue() != null;
	}
	
	/**
	 * Determines if the contained value may be {@code null}.
	 * @return If the value is nullable.
	 */
	public boolean isNullable() {
		return nullable;
	}
	
	private String getDesciption() {
		if (proxy != null) {
			return description + "*" + proxy.getKey().getDesciption();
		}
		return description;
	}
	
	/**
	 * Overrides another container with this one. It will reflect this container status and value.
	 * Set operations can only performed on containers if they are not overridden.
	 * @param <P> Value type of the other container.
	 * @param other Container to be overridden.
	 * @param value Function providing the value for the other container.
	 * 				The returned value must correspond with the others's nullability. 
	 * @return This container.
	 */
	public <P> Definable<T> overrides(final Definable<P> other, final Function<Definable<T>, P> value) {
		other.proxy = new AbstractMap.SimpleEntry<>(this, () -> value.apply(this));
		return this;
	}
	
	/**
	 * Overrides another container with this one. It will reflect this container status and value.
	 * Set operations can only performed on containers if they are not overridden.
	 * @param <P> Value type of the other container.
	 * @param other Container to be overridden.
	 * @param value Function providing the value for the other container.
	 * 				The returned value must correspond with the others's nullability. 
	 * @return This container.
	 */
	public <P> Definable<T> overrides(final Definable<P> other, final Supplier<P> value) {
		this.defined = true;
		other.proxy = new AbstractMap.SimpleEntry<>(this, value);
		return this;
	}
	
	private void failOnProxy() {
		if (proxy != null) {
			throw new IllegalStateException("Definable " + this.description + " is proxy to " + proxy.getKey().description);
		}
	}
	
	/**
	 * Creates a new value container.
	 * @param nullable If the value may be defined as {@code null}.
	 * @param description Arbitrary string to identify Exceptions thrown by access methods of this Definable.
	 */
	public Definable(final boolean nullable, final String description) {
		this.nullable = nullable;
		this.description = description;
	}
	
	/**
	 * Defines the value.
	 * @param value The value to be set.
	 * @throws ValueNotNullableException If the value is {@code null} but the container is not nullable. See {@link #Definable(boolean, String)}
	 */
	public void set(@Nullable final T value) {
		failOnProxy();
		if (! nullable && value == null) {
			throw new ValueNotNullableException(this);
		}
		this.value = value;
		defined = true;
	}
	
	/**
	 * Clears the value.
	 */
	public void unset() {
		failOnProxy();
		this.defined = false;
		this.value = null;
	}
	
	/**
	 * Returns this container, checking if it's value is defined.
	 * @param isRequired If an Exception shall be thrown in case the value is not defined.
	 * @return This Definable.
	 * @throws ValueNotDefinedException If the value is required but not defined.
	 */
	public Definable<T> required(final boolean isRequired) {
		if (isRequired && ! isDefined()) {
			throw new ValueNotDefinedException(this);
		}
		return this;
	}
	
	/**
	 * Returns this container, checking if a value is present.
	 * @param isAllowed If the value may be defined.
	 * @return This Definable.
	 * @throws DefinitionNotAllowedException If a value is defined but not allowed to be present.
	 */
	public Definable<T> allowed(boolean isAllowed) {
		if (! isAllowed && isDefined()) {
			throw new DefinitionNotAllowedException(this);
		}
		return this;
	}
	
	/**
	 * Returns this container, allowing a value to be present only if it is required.
	 * @param isRequired If the value must or must not be defined.
	 * @return This Definable.
	 * @throws ValueNotDefinedException If a value is required but not present.
	 * @throws DefinitionNotAllowedException If a value is present but not required.
	 */
	public Definable<T> exclusive(final boolean isRequired) {
		if (isRequired ^ isDefined()) {
			throw isDefined() ? new DefinitionNotAllowedException(this) : new ValueNotDefinedException(this);
		}
		return this;
	}
	
	/**
	 * Retrieves the value.
	 * @return The current value, if defined.
	 * @throws ValueNotDefinedException In case the value has not been set.
	 */
	@Nullable
	public T get() {
		return required(true).currentValue();
	}
	
	/**
	 * Retrieves the value or returns a default if it is not defined.
	 * @param value Value to return if the container has no value set.
	 * @return The defined value or the default.
	 */
	@Nullable
	public T orElse(@Nullable final T value) {
		if (isDefined()) {
			return currentValue();
		}
		return value;
	}
	
	/**
	 * Retrieves the value if present and not {@code null} or sets and returns the provided value.
	 * @param valueIfAbsent Supplier for a new value in case none is present.
	 * @return Either the current or the new value.
	 */
	public T getOrSet(final Supplier<T> valueIfAbsent) {
		return orElseNonNull(() -> {
			final T newValue = valueIfAbsent.get();
			set(newValue);
			return newValue;
		});
	}
	
	/**
	 * Returns an Optional of this Definable, not distinguishing between undefined and null.
	 * @return Optional of the Definable.
	 */
	public Optional<T> optional() {
		return Optional.ofNullable(orElse(null));
	}
	
	/**
	 * Retrieves the value, expecting it not to be {@code null}.
	 * @return The current value, if defined and not {@code null}.
	 * @throws ValueIsNullException In case the value is set to {@code null}.
	 * @throws ValueNotDefinedException In case the value has not been set.
	 */
	public T getNonNull() {
		final T definedValue = required(true).currentValue();
		if (definedValue == null) {
			throw new ValueIsNullException(this);
		}
		return definedValue;
	}
	
	/**
	 * Retrieves the value or returns a default if it is not defined or {@code null}.
	 * @param defaultValue Value to return if the container has no value set.
	 * @return The defined value or the default.
	 */
	public T orElseNonNull(final T defaultValue) {
		return optional().orElse(defaultValue);
	}
	
	/**
	 * Retrieves the value or returns a default if it is not defined or {@code null}.
	 * @param defaultValue Supplier of the value to return if none is present on the container.
	 * @return The defined value or the default.
	 */
	public T orElseNonNull(final Supplier<T> defaultValue) {
		return optional().orElseGet(defaultValue);
	}
	
	/**
	 * Performs an operation using the currently set value.
	 * @param action The operation to perform if the value is defined.
	 */
	public void ifDefined(final Consumer<T> action) {
		if (isDefined()) {
			action.accept(currentValue());
		}
	}
	
	@Override
	public String toString() {
		return "Definable<" + getDesciption() + ">" + (isDefined() ? "[" + currentValue() + "]" : "(undefined)");
	}
	
}
