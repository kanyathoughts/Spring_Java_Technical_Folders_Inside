/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

/**
 * Lazy loading supplier.
 * @param <T> Type of the supplied value.
 */
public class CachingSupplier<T> implements Supplier<T> {
	
	private final Supplier<T> supplier;
	private final AtomicReference<T> value;
	
	/**
	 * Creates a new cached value.
	 * @param supplier Supplies the value when it is requested for the first time.
	 */
	public CachingSupplier(final Supplier<T> supplier) {
		this.supplier = supplier;
		this.value = new AtomicReference<>();
	}
	
	/**
	 * Creates the value upon first invocation and returns the same value again for all subsequent invocations. 
	 */
	@Override
	public T get() {
		if (value.get() == null) {
			synchronized (value) {
				if (value.get() == null) {
					value.set(supplier.get());
				}
			}
		}
		return Objects.requireNonNull(value.get());
	}
	
	/**
	 * Checks if the value for this supplier has been created.
	 * @return If the {@link #get()} method of the underlying supplier has ever returned a non-null result.
	 */
	public boolean isPresent() {
		return value.get() != null;
	}
	
}
