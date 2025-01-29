/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.Objects;
import java.util.function.Consumer;

/**
 * Consumer for receiving and returning a builder.
 * @param <T> Type of the argument.
 */
public interface BuildingConsumer<T> extends Consumer<T> {
	
	/**
	 * Performs operations on the argument and then returns it. 
	 * @param <U> Any sub-type of the argument.
	 * @param builder Object to operate on.
	 * @return The input object after operations have been performed.
	 */
	default <U extends T> U prepare(final U builder) {
		accept(builder);
		return builder;
	}
	
	/**
	 * Passes an object to a {@link Consumer} and returns it afterwards.
	 * @param <T> Type of the argument.
	 * @param builder Object to operate on.
	 * @param buildingConsumer Operations to perform.
	 * @return The input object after operations have been performed.
	 */
	static <T> T of(final T builder, final Consumer<? super T> buildingConsumer) {
		buildingConsumer.accept(builder);
		return builder;
	}

	/**
	 * Clone of {@link Consumer#andThen(Consumer)}.
	 * Returns a composed {@code BuildingConsumer} that performs, in sequence, this
	 * operation followed by the {@code after} operation. If performing either
	 * operation throws an exception, it is relayed to the caller of the
	 * composed operation.  If performing this operation throws an exception,
	 * the {@code after} operation will not be performed.
	 *
	 * @param after the operation to perform after this operation
	 * @return a composed {@code BuildingConsumer} that performs in sequence this
	 * operation followed by the {@code after} operation
	 * @throws NullPointerException if {@code after} is null
	 */
	default BuildingConsumer<T> andThen(final BuildingConsumer<? super T> after) {
		Objects.requireNonNull(after);
		return (T t) -> { accept(t); after.accept(t); };
	}
	
}
