/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.function.Function;

/**
 * Interface for a {@link Function} with one checked exception.
 * 
 * @param <T> the argument passed to the function
 * @param <R> the return type of the function
 * @param <E> the type of exception thrown by this function
 */
@FunctionalInterface
public interface FunctionWithException<T, R, E extends Exception> {

    /**
     * Function interface with additional exception.
     *
     * @param arg the function argument
     * @return the function result
     * @throws E the exception type
     */
    R apply(T arg) throws E;
}
