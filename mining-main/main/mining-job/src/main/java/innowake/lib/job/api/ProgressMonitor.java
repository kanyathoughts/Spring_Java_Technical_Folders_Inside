/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

import java.io.Serializable;

import innowake.lib.job.api.task.Task;
import innowake.lib.job.internal.SubProgressMonitor;

/**
 * Tracks progress of a job and provides the ability to cancel it. Be aware of the fact that the monitor must also work in a massively parallel job.
 * <p>
 * The progress monitor must always be initialized with {@link #begin(int)} to define the maximum amount of work units for the job.
 */
public interface ProgressMonitor extends Serializable {
	
	/** Constant used in conjunction with {@link #begin(int)}, in order to switch the {@link ProgressMonitor} into an indeterministic mode. */
	public static final int INDETERMINISTIC = -1;
	
	/**
	 * Sets the description of the job.
	 *
	 * @param description the description of the job
	 */
	void setJobDescription(String description);
	
	/**
	 * Sets the description of the job step.
	 * 
	 * @param description the description of the job step
	 */
	void setStepDescription(String description);
	
	/**
	 * Initializes the progress monitor with the maximum available work units for this {@link ProgressMonitor}.
	 * <p>
	 * This value is shared with the work units requested by sub progress monitors created with {@link #subMonitor(int)}
	 * <p>
	 * To show indeterministic progress {@link #INDETERMINISTIC} should be passed. In this case any call to {@link #worked(int)} will have no effect.
	 * This method should be called again as soon as the work units are available to enable proper progress monitoring.
	 *
	 * @param workUnits the maximum available work units
	 */
	void begin(int workUnits);
	
	/**
	 * Increments the progress of the job with the provided amount of work units.
	 * <p>
	 * <b>Note: </b>this implicitly calls {@link #checkCanceled()}
	 *
	 * @param workUnits a positive number of work units that have been completed
	 */
	default void worked(final int workUnits) {
		checkCanceled();
		internalWork(workUnits);
	}
	
	/**
	 * Increments the total progress of the job. This method must not be directly called by clients. Clients should always use {@link #worked(int)}.
	 * 
	 * @param work the amount of work that has been completed
	 */
	void internalWork(double work);

	/**
	 * Requests to cancel the job. Job implementations must regularly check if a job has been canceled with {@link #checkCanceled()} to react on this.
	 * 
	 * @see {@link #checkCanceled()}
	 */
	void cancel();
	
	/**
	 * @return {@code true} if the job has been requested to cancel or is already canceled; {@code false} otherwise
	 * @see {@link #cancel()}
	 */
	boolean isCanceled();

	/**
	 * Job implementations must regularly check with this method if a job has been canceled.
	 * <p>
	 * <b>Note: </b>this is also implicitly called by {@link #subMonitor(int)} and {@link #worked(int)}.
	 * 
	 * @throws OperationCanceledException if the job had been canceled
	 */
	default void checkCanceled() {
		if (isCanceled()) {
			throw new OperationCanceledException();
		}
	}
	
	/**
	 * Creates a sub {@link ProgressMonitor} for example to be used by a {@link Task} that consumes the specified amount of work units from the parent.
	 * <p>
	 * <b>Note: </b>this implicitly calls {@link #checkCanceled()}
	 *
	 * @param workUnitsFromParent the amount of work units to be consumed from the parent {@link ProgressMonitor}
	 * @return the sub {@link ProgressMonitor}
	 */
	default ProgressMonitor subMonitor(final int workUnitsFromParent) {
		checkCanceled();
		return new SubProgressMonitor(this, workUnitsFromParent);
	}
	
}
