/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.function.Consumer;

/**
 * A Consumer that may not be applicable, like a Collection of operations that may be empty.
 * @param <T> the type of the input to the operation
 */
public interface ConditionalConsumer<T> extends Consumer<T> {
	
	/**
	 * Tells a caller which needs to set some kind of stage before calling the Consumer whether it may be ignored.
	 * @return If this Consumer will do anything if called.
	 */
	default boolean isApplicable() {
		return true;
	}
	
}
