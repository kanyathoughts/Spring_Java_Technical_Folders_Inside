/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.commons.exception;

/**
 * An Exception which is thrown when there a field's setter and getter methods are not present.
 */
public class AccessorMethodNotFoundException extends RuntimeException {
	
	/**
	 * Initializes the exception whenever a field's setter and getter methods 
	 * are not accessible from the entity class.
	 * 
	 * @param message  the error message of the exception to be thrown
	 */
	public AccessorMethodNotFoundException(final String message) {
		super(message);
	}
	
	/**
	 * Initializes the exception whenever a field's setter and getter methods
	 *  are not accessible from the entity class.
	 *  
	 * @param message the error message of the exception to be thrown
	 * @param throwable cause of the original exception which is then wrapped around the {@link AccessorMethodNotFoundException}
	 */
	public AccessorMethodNotFoundException(final String message, final Throwable throwable) {
		super(message, throwable);
	}

}
