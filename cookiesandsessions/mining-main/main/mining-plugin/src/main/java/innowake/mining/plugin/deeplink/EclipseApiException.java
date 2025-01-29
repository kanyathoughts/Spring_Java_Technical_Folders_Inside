/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink;

/**
 * This exception is used, whenever an error happens while executing an Eclipse command.
 */
public class EclipseApiException extends Exception {

	/**
	 * Constructor for the exception
	 * 
	 * @param message The message which accompanies the error 
	 * @param cause The cause leading to this error
	 */
	public EclipseApiException(final String message, final Throwable cause) {
		super(message, cause);
	}	
}
