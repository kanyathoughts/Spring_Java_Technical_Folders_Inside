/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.error;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.UncategorizedSQLException;

import innowake.mining.shared.access.EntityId;

/**
 * Utility functions for {@linkplain Exception exceptions}.
 */
public class Exceptions {

	private Exceptions() {
		/* prevent instantiation */
	}

	/**
	 * Checks if the root cause of the given exception was an empty result.
	 *
	 * @param exception the exception to check
	 * @param clazz the class of the entity
	 * @param id the numerical id of the entity
	 * @throws MiningEntityNotFoundException if the root cause was an empty result
	 */
	public static void checkForEmptyResultDataAccessException(final UncategorizedSQLException exception, final Class<?> clazz, final Long id) {
		final Throwable rootCause = ExceptionUtils.getRootCause(exception);
		if (rootCause instanceof EmptyResultDataAccessException) {
			throw new MiningEntityNotFoundException(clazz, EntityId.of(id), rootCause);
		} 
	}

	/**
	 * Checks if the root cause of the given exception was an empty result.
	 *
	 * @param exception the exception to check
	 * @param clazz the class of the entity
	 * @param id the numerical id of the entity
	 * @throws MiningEntityNotFoundException if the root cause was an empty result
	 */
	public static void checkForEmptyResultDataAccessException(final UncategorizedSQLException exception, final Class<?> clazz, final EntityId id) {
		final Throwable rootCause = ExceptionUtils.getRootCause(exception);
		if (rootCause instanceof EmptyResultDataAccessException) {
			throw new MiningEntityNotFoundException(clazz, id, rootCause);
		} 
	}

	/**
	 * Checks if the root cause of the given exception was an empty result.
	 *
	 * @param exception the exception to check
	 * @param clazz the class of the entity
	 * @param id the string id of the entity
	 * @throws MiningEntityNotFoundException if the root cause was an empty result
	 */
	public static void checkForEmptyResultDataAccessException(final UncategorizedSQLException exception, final Class<?> clazz, final String id) {
		final Throwable rootCause = ExceptionUtils.getRootCause(exception);
		if (rootCause instanceof EmptyResultDataAccessException) {
			throw new MiningEntityNotFoundException(clazz, id, rootCause);
		} 
	}

}
