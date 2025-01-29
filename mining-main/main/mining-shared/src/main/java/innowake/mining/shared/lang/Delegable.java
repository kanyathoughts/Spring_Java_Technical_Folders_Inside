/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.function.Consumer;

/**
 * Provides a chainable method for performing a subset of operations on an Object.
 * @param <T> Must be the type of the implementer or one of its super-types.
 */
public interface Delegable<T> {
	
	/**
	 * Performs actions on this object and returns it.
	 * @param action Operations to perform.
	 * @return This object.
	 */
	@SuppressWarnings("unchecked")
	default T delegate(Consumer<T> action) {
		action.accept((T) this);
		return (T) this;
	}
	
}
