/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.error;

/**
 * Converter specific exception for multiple valid converters leading to ambiguity.
 */
public class AmbiguousConverterException extends RuntimeException{
	
	/**
	 * Constructs an {@code AmbiguousConverterException} with the specified message.
	 *
	 * @param message the detail message
	 */
	public AmbiguousConverterException(final String message) {
		super(message);
	}
}
