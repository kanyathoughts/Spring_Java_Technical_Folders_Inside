/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.commons.exception;

/**
 * Exception thrown when the field is not found while mapping vertex to entity.
 */
public class FieldNotFoundException extends RuntimeException {
	
	/**
	 * Initializes the custom field not found exception.
	 * 
	 * @param message to be displayed when their is a field not found exception
	 * @param throwable the cause of the exception which is then wrapped around {@link FieldNotFoundException}
	 */
	public FieldNotFoundException(final String message, final Throwable throwable) {
		super(message, throwable);
	}
	
}
