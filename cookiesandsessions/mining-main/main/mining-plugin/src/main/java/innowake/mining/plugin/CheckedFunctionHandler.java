/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin;

import java.util.function.Function;

/**
 * The functional interface to represent lambda which throws checked exception.
 * Also provides a method implementation for handling checked exception on lambda.
 *
 * @param <T> the type of the argument passed to the lambda expression
 * @param <R> the type of the value returned from the lambda expression
 * @param <E> the exception thrown from the lambda expression
 */
@FunctionalInterface
public interface CheckedFunctionHandler<T, R, E extends Exception> {

	/**
	 * This method acts as lambda body and is implemented by the lambda of form {@link CheckedFunctionHandler} functional interface.
	 *
	 * @param t the parameter passed to the method in lambda body
	 * @return the result of the method after execution
	 * @throws E the type parameter which denotes the actual exception thrown from the checked method
	 */
	R apply(T t) throws E;

	/**
	 * This method can be used as a wrapper around lambda which implements {@link CheckedFunctionHandler}
	 * so using methods which throw checked exception inside java streams is convenient.
	 *
	 * @param <T> the type of the argument passed to the lambda expression
	 * @param <R> the type of the value returned from the lambda expression
	 * @param <E> the exception thrown from the lambda expression
	 * @param checkedFunction the lambda expression which implements the {@link CheckedFunctionHandler}
	 * @return the function that can be readily used in stream with checked exceptions handled
	 */
	static <T, R, E extends Exception> Function<T, R> handleException(final CheckedFunctionHandler<T, R, E> checkedFunction) {
		return t -> {
			try {
				return checkedFunction.apply(t);
			} catch (final Exception e) {
				throw new IllegalStateException(e);
			}
		};
	}
}
