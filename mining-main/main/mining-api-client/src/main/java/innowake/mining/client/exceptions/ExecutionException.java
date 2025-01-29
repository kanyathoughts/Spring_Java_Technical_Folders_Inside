/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.client.exceptions;

/**
 * Runtime Exception to throw if some error occurred while executing service call.
 */
public class ExecutionException extends RuntimeException {

	/**
	 * Constructs a new ExecutionException.
	 */
	public ExecutionException() {
	}

	/**
	 * Constructs a new ExecutionException with the specific detail message. A detail message is a String that describes this particular exception.
	 * 
	 * @param message the error message for the execution exception
	 */
	public ExecutionException(final String message) {
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
	public ExecutionException(final String message, final Throwable cause) {
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
	public ExecutionException(final Throwable cause) {
		super(cause);
	}

}
