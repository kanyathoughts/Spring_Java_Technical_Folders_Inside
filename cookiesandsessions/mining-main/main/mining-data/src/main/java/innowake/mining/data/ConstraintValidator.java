/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data;

import java.util.function.Supplier;

import innowake.lib.core.lang.Assert;
import innowake.lib.core.lang.Assert.AssertionException;
import innowake.mining.data.error.ConstraintViolationException;

/**
 * Checks for entity constraints.
 */
public class ConstraintValidator {

	private ConstraintValidator() {}
	
	/**
	 * Catches an {@link AssertionException} if thrown by the {@code supplier} and throws a {@link ConstraintViolationException}. 
	 *
	 * @param supplier the supplier to check for an {@link AssertionException}
	 * @param entity the entity for the {@link ConstraintViolationException}
	 * @return the result from the supplier
	 */
	public static <E, R> R validate(final Supplier<R> supplier, final E entity) {
		try {
			return supplier.get();
		} catch (final AssertionException e) {
			throw new ConstraintViolationException(entity, e.getMessage(), e);
		}
	}
	
	/**
	 * Checks that the value of the supplier equals the expected value.
	 *
	 * @param supplier the supplier to check for an {@link AssertionException}
	 * @param expected the expected value
	 * @param message the message for the {@link ConstraintViolationException}
	 * @param entity the entity for the {@link ConstraintViolationException}
	 * @return the result from the supplier
	 */
	public static <E, R> R validate(final Supplier<R> supplier, final R expected, final String message, final E entity) {
		try {
			return Assert.assertEqual(expected, supplier.get(), message);
		} catch (final AssertionException e) {
			throw new ConstraintViolationException(entity, message, e);
		}
		
	}
}
