/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

import innowake.lib.job.api.task.Task;

/**
 * The severity of a {@link Job} or {@link Task} execution status provided with the {@link Result}.
 */
public enum Severity {
	
	/** The job or task finished with errors. */
	ERROR(4),
	
	/** The job has been canceled. */
	CANCELED(3),
	
	/** The job or task finished with warnings. */
	WARNING(2),
	
	/** The job or task finished successfully without any warnings or errors. */
	OK(1),
	
	/** The job or task finished with an undefined severity. */
	UNDEFINED(0);
	
	private final int weight;
	
	private Severity(final int weight) {
		this.weight = weight;
	}
	
	/**
	 * Returns the {@link Severity} with the highest weight going from {@link #ERROR} (highest) to {@link #UNDEFINED} (lowest).
	 * 
	 * @param a the first {@link Severity} to compare
	 * @param b the second {@link Severity} to compare
	 * @return the {@link Severity} with the highest weight
	 */
	public static Severity max(final Severity a, final Severity b) {
		return a.weight >= b.weight ? a : b;
	}

}
