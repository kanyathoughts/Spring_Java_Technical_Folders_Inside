/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.server;

/**
 * This exception is used, whenever an error happens while setting up the server or while handling requests
 */
public class DeepLinkServerException extends Exception {

	public DeepLinkServerException() {
	}

	/**
	 * Constructor for the exception
	 * 
	 * @param message The message which accompanies the error 
	 * @param cause The cause leading to this error
	 * @param enableSuppression Flag to enable or disable suppression
	 * @param writableStackTrace Flag to set the stack trace writable or not
	 */
	public DeepLinkServerException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

	/**
	 * Constructor for the exception
	 * 
	 * @param message The message which accompanies the error 
	 * @param cause The cause leading to this error
	 */
	public DeepLinkServerException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Constructor for the exception
	 * 
	 * @param message The message which accompanies the error 
	 */
	public DeepLinkServerException(String message) {
		super(message);
	}

	/**
	 * Constructor for the exception
	 * 
	 * @param cause The cause leading to this error
	 */
	public DeepLinkServerException(Throwable cause) {
		super(cause);
	}
}
