/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.error;

import innowake.mining.shared.access.EntityId;

import javax.persistence.EntityNotFoundException;

/**
 * This exception covers an error when an entity cannot be found by an id.
 */
public class MiningEntityNotFoundException extends EntityNotFoundException {

	/**
	 * Creates an instance.
	 *
	 * @param clazz the entity class which cannot be found
	 * @param id the entity id which cannot be found
	 */
	public MiningEntityNotFoundException(final Class<?> clazz, final EntityId id) {
		super(clazz.getSimpleName() + " id not found: " + id);
	}

	/**
	 * Creates an instance.
	 *
	 * @param clazz the entity class which cannot be found
	 * @param id the numeric id which cannot be found
	 */
	public MiningEntityNotFoundException(final Class<?> clazz, final Long id) {
		super(clazz.getSimpleName() + " id not found: " + id);
	}

	/**
	 * Creates an instance.
	 * 
	 * @param clazz the entity class which cannot be found
	 * @param id the entity id which cannot be found
	 * @param cause the original database exception
	 */
	public MiningEntityNotFoundException(final Class<?> clazz, final EntityId id, final Throwable cause) {
		super(clazz.getSimpleName() + " id not found: " + id);
		addSuppressed(cause);
	}

	/**
	 * Creates an instance.
	 * 
	 * @param clazz the entity class which cannot be found
	 * @param identifer the entity identifier which cannot be found
	 */
	public MiningEntityNotFoundException(final Class<?> clazz, final String identifer) {
		super(clazz.getSimpleName() + " identifier not found: " + identifer);
	}

	/**
	 * Creates an instance.
	 * 
	 * @param msg the detail message.
	 */
	public MiningEntityNotFoundException(final String msg) {
		super(msg);
	}

	/**
	 * Creates an instance.
	 * 
	 * @param clazz the entity class which cannot be found
	 * @param identifer the entity identifier which cannot be found
	 * @param cause the original database exception
	 */
	public MiningEntityNotFoundException(final Class<?> clazz, final String identifer, final Throwable cause) {
		super(clazz.getSimpleName() + " identifier not found: " + identifer);
		addSuppressed(cause);
	}
}
