/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.commons.exception;

/**
 * Exception is thrown when an unsupported data type is passed to build a query or 
 * is expected as query output type.
 */
public class UnsupportedQueryTypeException extends RuntimeException {

	/**
	 * Initializes the custom {@code UnsupportedQueryTypeException} exception.
	 * 
	 * @param message to be displayed
	 */
	public UnsupportedQueryTypeException(final String message) {
		super(message);
	}

	/**
	 * Initializes the custom {@code UnsupportedQueryTypeException} exception.
	 * 
	 * @param message to be displayed when their is an exception processing query
	 * @param throwable the cause of the exception which is then wrapped around {@link UnsupportedQueryTypeException}
	 */
	public UnsupportedQueryTypeException(final String message, final Throwable throwable) {
		super(message, throwable);
	}

}
