/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.commons.exception;

/**
 * Exception thrown when a query return an empty result set. 
 */
public class NoRecordFoundException extends RuntimeException {

	/**
	 * Initializes the {@code NoRecordFoundException} exception.
	 * 
	 * @param message to be displayed when no records are found
	 */
	public NoRecordFoundException(final String message) {
		super(message);
	}
}
