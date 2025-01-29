/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

/**
 * Exception thrown when the capacity of the cluster has been exhausted and cannot accept any more jobs or tasks.
 */
public class CapacityExceededException extends RuntimeException {

	/**
	 * Constructor.
	 * 
	 * @param message the error message
	 */
	public CapacityExceededException(final String message) {
		super(message);
	}
}
