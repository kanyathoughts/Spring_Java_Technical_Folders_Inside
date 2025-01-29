/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.command;

/**
 * Exception used whenever a problem parsing a command arises.
 */
public class RequestParseException extends Exception {

	public RequestParseException() {
	}

	/**
	 * Constructor for the exception
	 * 
	 * @param message The message which accompanies the error 
	 * @param cause The cause leading to this error
	 * @param enableSuppression Flag to enable or disable suppression
	 * @param writableStackTrace Flag to set the stack trace writable or not
	 */
	public RequestParseException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

	/**
	 * Constructor for the exception
	 * 
	 * @param message The message which accompanies the error 
	 * @param cause The cause leading to this error
	 */
	public RequestParseException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Constructor for the exception
	 * 
	 * @param message The message which accompanies the error
	 */
	public RequestParseException(String message) {
		super(message);
	}

	/**
	 * Constructor for the exception
	 * 
	 * @param cause The cause leading to this error
	 */
	public RequestParseException(Throwable cause) {
		super(cause);
	}

	

}
