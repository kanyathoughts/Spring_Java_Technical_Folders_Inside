/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;

import innowake.lib.core.api.lang.Nullable;

/**
 * Wrapper for a {@link Serializable} and {@link Comparable} value.
 * Equality, hash code and comparison order of the Wrapper is determined by the "identity" of the value which in the simplest case may be the value itself.
 * @param <T> Type of the value.
 * @param <I> Identity type.
 */
public class Wrapper<T extends Serializable, I extends Comparable<I>> implements Supplier<T>, Comparable<Wrapper<T, I>>, Serializable {
	
	private final T value;
	private final transient Function<T, I> identity;
	
	protected Wrapper(final T value, final Function<T, I> identity) {
		this.value = Objects.requireNonNull(value);
		this.identity = identity;
	}
	
	public static <T extends Serializable & Comparable<T>> Wrapper<T, T> of(final T value) {
		return new Wrapper<>(value, i -> i);
	}
	
	public static <T extends Serializable, I extends Comparable<I>> Wrapper<T, I> of(final T value, final Function<T, I> identity) {
		return new Wrapper<>(value, identity);
	}
	
	public I identity() {
		return identity.apply(value);
	}
	
	@Override
	public int hashCode() {
		return identity().hashCode();
	}
	
	@Override
	public boolean equals(@Nullable final Object obj) {
		return identity().equals(obj);
	}
	
	@Override
	public String toString() {
		return value.toString();
	}
	
	@Override
	public T get() {
		return value;
	}
	
	@Override
	public int compareTo(@Nullable final Wrapper<T, I> o) {
		return identity().compareTo(o != null ? o.identity() : null);
	}
	
}
