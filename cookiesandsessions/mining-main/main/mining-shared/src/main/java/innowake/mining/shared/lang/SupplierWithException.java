/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.function.Supplier;

/**
 * Interface for a {@link Supplier} with one checked exception.
 * 
 * @param <T> the argument passed to the supplier
 * @param <E> the type of exception thrown by this supplier
 */
@FunctionalInterface
public interface SupplierWithException<T, E extends Exception> {

    /**
     * Supplier signature with exception.
     *
     * @return the result of the supplier
     * @throws E the exception type
     */
	T get() throws E;
}
