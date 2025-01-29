/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.error;

/**
 * Converter specific exception for no matching converters.
 */
public class NoMatchingConverterException extends RuntimeException{
	
	/**
	 * Constructs an {@code NoMatchingConverterException} with the specified message.
	 *
	 * @param message the detail message
	 */
	public NoMatchingConverterException(final String message) {
		super(message);
	}
}
