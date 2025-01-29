/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.lib.job.api;

/**
 * Thrown when the {@link Job} is not in proper {@link Status} to perform the specified operation.
 */
public class IllegalJobStateException extends IllegalStateException {

	private static final long serialVersionUID = 1L;

	/**
	 * Constructs an IllegalJobStateException with default message.
	 * A detail message is a String that describes this particular exception.
	 */
	public IllegalJobStateException() {
		super("Job is not in proper status to perform the specified operation");
	}

	/**
	 * Constructs an IllegalStateException with the specified detail
	 * message.  A detail message is a String that describes this particular
	 * exception.
	 *
	 * @param message the String that contains a detailed message
	 */
	public IllegalJobStateException(final String message) {
		super(message);
	}
}
