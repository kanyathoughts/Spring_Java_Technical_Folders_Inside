/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.function.Consumer;

/**
 * Interface for a {@link Consumer} with one checked exception.
 * 
 * @param <T> the argument passed to the consumer
 * @param <E> the type of exception thrown by this consumer
 */
@FunctionalInterface
public interface ConsumerWithException<T, E extends Exception> {

    /**
     * Consumer signature with exception.
     *
     * @param arg the consumer argument
     * @throws E the exception type
     */
    void accept(T arg) throws E;
}
