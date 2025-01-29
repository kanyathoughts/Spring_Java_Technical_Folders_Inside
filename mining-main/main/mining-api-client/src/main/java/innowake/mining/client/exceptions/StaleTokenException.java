/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.client.exceptions;

/**
 * Runtime Exception to throw if the refresh token has expired or has been revoked.
 */
public class StaleTokenException extends RuntimeException {

	/**
	 * Constructs a new StaleTokenException.
	 */
	public StaleTokenException() {
	}

	/**
	 * Constructs a new StaleTokenException with the specific detail message. A detail message is a String that describes this particular exception.
	 * 
	 * @param message the error message for the state token exception
	 */
	public StaleTokenException(final String message) {
		super(message);
	}

	/**
	 * Constructs a new exception with the specified detail message and cause.
	 *
	 * <p>Note that the detail message associated with <code>cause</code> is <i>not</i> automatically incorporated in this exception's detail message.
	 *
	 * @param  message the detail message (which is saved for later retrieval by the {@link Throwable#getMessage()} method).
	 * @param  cause the cause (which is saved for later retrieval by the {@link Throwable#getCause()} method).
	 * (A <tt>null</tt> value is permitted, and indicates that the cause is nonexistent or unknown.)
	 */
	public StaleTokenException(final String message, final Throwable cause) {
		super(message, cause);
	}

	/**
	 * Constructs a new exception with the specified cause and a detail message of <tt>(cause==null ? null : cause.toString())</tt>
	 * (which typically contains the class and detail message of <tt>cause</tt>).
	 * This constructor is useful for exceptions that are little more than wrappers for other throwables
	 * (for example, {@link java.security.PrivilegedActionException}).
	 *
	 * @param  cause the cause (which is saved for later retrieval by the {@link Throwable#getCause()} method).
	 * (A <tt>null</tt> value is permitted, and indicates that the cause is nonexistent or unknown.)
	 */
	public StaleTokenException(final Throwable cause) {
		super(cause);
	}
}
