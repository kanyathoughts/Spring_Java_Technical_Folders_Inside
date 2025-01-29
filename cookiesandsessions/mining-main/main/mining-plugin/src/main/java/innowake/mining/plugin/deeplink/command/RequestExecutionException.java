/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.command;

/**
 * Exception used whenever a problem executing a command arises.
 * 
 */
public class RequestExecutionException extends Exception {

	public RequestExecutionException() {
	}

	/**
	 * Constructor for the exception
	 * 
	 * @param message The message which accompanies the error 
	 * @param cause The cause leading to this error
	 * @param enableSuppression Flag to enable or disable suppression
	 * @param writableStackTrace Flag to set the stack trace writable or not
	 */
	public RequestExecutionException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

	/**
	 * Constructor for the exception
	 * 
	 * @param message The message which accompanies the error 
	 * @param cause The cause leading to this error
	 */
	public RequestExecutionException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Constructor for the exception
	 * 
	 * @param message The message which accompanies the error
	 */
	public RequestExecutionException(String message) {
		super(message);
	}

	/**
	 * Constructor for the exception
	 * 
	 * @param cause The cause leading to this error
	 */
	public RequestExecutionException(Throwable cause) {
		super(cause);
	}
}
