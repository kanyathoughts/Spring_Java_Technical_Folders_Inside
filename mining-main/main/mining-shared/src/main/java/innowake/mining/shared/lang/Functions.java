/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.lang;

import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

/**
 * Common methods related to {@linkplain Function functions}.
 */
public final class Functions {

	private Functions() {}
	
	/**
	 * Wraps a {@link FunctionWithException} into a {@link Function}. Throws the checked exception as {@code RuntimeException}.
	 *
	 * @param function the function to wrap
	 * @return the wrapped function
	 */
	public static <T, R, E extends Exception> Function<T, R> checkedFunction(final FunctionWithException<T, R, E> function) {
		return arg -> {
			try {
				return function.apply(arg);
			} catch (final Exception e) {
				throw new RuntimeException(e);
			}
		};
	}
	
	/**
	 * Wraps a {@link ConsumerWithException} into a {@link Consumer}. Throws the checked exception as {@code RuntimeException}.
	 *
	 * @param consumer the consumer to wrap
	 * @return the wrapped consumer
	 */
	public static <T, E extends Exception> Consumer<T> checkedConsumer(final ConsumerWithException<T, E> consumer) {
		return arg -> {
			try {
				consumer.accept(arg);
			} catch (final Exception e) {
				throw new RuntimeException(e);
			}
		};
	}
	
	/**
	 * Wraps a {@link SupplierWithException} into a {@link Supplier}. Throws the checked exception as {@code RuntimeException}.
	 *
	 * @param supplier the supplier to wrap
	 * @return the wrapped supplier
	 */
	public static <T, E extends Exception> Supplier<T> checkedSupplier(final SupplierWithException<T, E> supplier) {
		return () -> {
			try {
				return supplier.get();
			} catch (final Exception e) {
				throw new RuntimeException(e);
			}
		};
	}
	
	/**
	 * Filters the elements of a list, remove them if not assignable to the type of {@code clazz} and cast them to the {@code clazz}. 
	 * Return the result as a new list.
	 *
	 * @param list list of elements to filter and cast
	 * @param clazz type to filter and cast
	 * @return list of filtered and casted elements
	 */
	public static <T> List<T> filter(final Collection<?> list, final Class<T> clazz) {
		return list.stream()
			.filter(node -> clazz.isAssignableFrom(node.getClass()))
			.map(clazz::cast)
			.collect(Collectors.toList());
	}
	
	/**
	 * A functional for-loop: check condition, execute action, iterate variable.
	 *
	 * @param <T> Type of the loop variable.
	 * @param initial Initial value for the loop variable.
	 * @param condition Function that determines, based on the loop variable, if execution shall continue before each <i>action</i>. 
	 * @param next Function producing the next value for the loop variable after each <i>action</i>.
	 * @param action Function to be repeatedly executed, first with the <i>initial</i> and then every <i>next</i> value, as long as <i>condition</i> is <i>true</i>. 
	 */
	public static <T> void loop(final T initial, final Predicate<T> condition, final UnaryOperator<T> next, final Consumer<T> action) {
		T x = initial;
		while (condition.test(x)) {
			action.accept(x);
			x = next.apply(x);
		}
	}

}
