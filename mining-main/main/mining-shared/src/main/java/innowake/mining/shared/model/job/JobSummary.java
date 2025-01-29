/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.job;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import innowake.mining.shared.access.EntityId;

/**
 *  Summarizes the results of running a Job.
 */
public class JobSummary implements Serializable, ParsingSummary {
	
	private static final String JOB_SUMMARY_TEXT = 
			"%d module(s) were successful.%n" + 
			"%d module(s) were unsuccessful.%n" + 
			"%d module(s) were unsupported.%n";
	
	private final Set<EntityId> successfulModules = new HashSet<EntityId>();
	private final Set<EntityId> unsuccessfulModules = new HashSet<EntityId>();
	private final Set<EntityId> unsupportedModules = new HashSet<EntityId>();
	
	/**
	 * Returns the modules that were successfully processed.
	 * @return the modules that were successfully processed.
	 */
	@Override
	public Set<EntityId> getSuccessfulModules() {
		return Collections.unmodifiableSet(successfulModules);
	}
	
	/**
	 * Returns the modules that were unsuccessfully processed.
	 * @return the modules that were unsuccessfully processed.
	 */
	@Override
	public Set<EntityId> getUnsuccessfulModules() {
		return Collections.unmodifiableSet(unsuccessfulModules);
	}
	
	/**
	 * Returns the modules that were unsupported for processing.
	 * @return the modules that were unsupported for processing.
	 */
	@Override
	public Set<EntityId> getUnsupportedModules() {
		return Collections.unmodifiableSet(unsupportedModules);
	}
	
	/**
	 * Aggregates the {@link ParsingSummary} as they are completed.
	 * @param parsingSummary that has been completed and is ready to be added to the job summary.
	 */
	public void aggregate(final ParsingSummary parsingSummary) {
		this.successfulModules.addAll(parsingSummary.getSuccessfulModules());
		this.unsuccessfulModules.addAll(parsingSummary.getUnsuccessfulModules());
		this.unsupportedModules.addAll(parsingSummary.getUnsupportedModules());
	}
	
	/**
	 * Record unsupported modules that will not be provided to any tasks.
	 * @param unsupportedModule the id of the supported module.
	 */
	public void addUnsupportedModule(final EntityId unsupportedModule) {
		this.unsupportedModules.add(unsupportedModule);
	}
	
	@Override
	public String toString() {
		return String.format(JOB_SUMMARY_TEXT,
				Integer.valueOf(getSuccessfulModules().size()),
				Integer.valueOf(getUnsuccessfulModules().size()),
				Integer.valueOf(getUnsupportedModules().size()));
	}
}
