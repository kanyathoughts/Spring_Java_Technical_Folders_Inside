/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.commons.exception;

/**
 * An Exception which is thrown when there is an error in mapping of the fields
 * of the entity to the proxy instance.
 */
public class EntityProxyMappingException extends RuntimeException {

	/**
	 * Initializes the exception which is thrown whenever there is an error in the mapping 
	 * of the fields of the entity to the proxy instance.
	 */
	public EntityProxyMappingException() {}

	/**
	 * Initializes the exception which is thrown whenever there is an error in the mapping 
	 * of the fields of the entity to the proxy instance.
	 *
	 * @param message the error message of the exception to be thrown
	 */
	public EntityProxyMappingException(final String message) {
		super(message);
	}

	/**
	 * Initializes the exception which is thrown whenever there is an error in the mapping 
	 * of the fields of the entity to the proxy instance.
	 *
	 * @param message the error message of the exception to be thrown
	 * @param throwable cause of the original exception which is then wrapped around the {@link EntityProxyMappingException}
	 */
	public EntityProxyMappingException(final String message, final Throwable throwable) {
		super(message, throwable);
	}

}
