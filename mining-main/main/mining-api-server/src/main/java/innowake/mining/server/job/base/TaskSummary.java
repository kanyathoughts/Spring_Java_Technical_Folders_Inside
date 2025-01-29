/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.base;

import java.io.Serializable;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.task.Task;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.job.ParsingSummarizer;
import innowake.mining.shared.model.job.ParsingSummary;

/**
 *  Summarizes the results of running a {@link Task}.
 */
public class TaskSummary implements Serializable, ParsingSummarizer, ParsingSummary {
	
	final static int SUCCESSFUL_RESULT_CODE = 0;
	final static int PARSE_ERROR_RESULT_CODE = -1;
	final static int TASK_FAILED_RESULT_CODE = -2;
	
	final private Set<EntityId> successfulModules;
	final private Set<EntityId> unsuccessfulModules;
	final private Set<EntityId> unsupportedModules;
	
	private boolean hadUnhandledException = false;
	
	/**
	 * Constructs an instance of the {@link TaskSummary}.
	 * @param moduleId the Id of the module processed by this task.
	 */
	public TaskSummary(final EntityId moduleId) {
		this(Collections.singletonList(moduleId));
	}
	
	/**
	 * Constructs an instance of the {@link TaskSummary}.
	 * @param moduleIds The Id's of the modules processed by this task.
	 */
	public TaskSummary(final List<EntityId> moduleIds) {
		this.successfulModules = new HashSet<EntityId>(moduleIds);
		this.unsuccessfulModules = new HashSet<EntityId>();
		this.unsupportedModules = new HashSet<EntityId>();
	}
	
	/*
	 * Returns a boolean value indicating if the task had an unhandled exception.
	 */
	public boolean getHadUnhandledException() {
		return hadUnhandledException;
	}
	
	/*
	 * Sets a boolean value indicating if the task had an unhandled exception.
	 * @param value boolean value set to indicate if the task had an unhandled exception.
	 */
	public void setHadUnhandledException(final boolean value) {
		hadUnhandledException = value;
	}
	
	/**
	 * The result code of the operation.
	 * @return Zero if the result is successful, otherwise non-zero.
	 */
	public Integer getResultCode() {
		if (getHadUnhandledException()) {
			return Integer.valueOf(TASK_FAILED_RESULT_CODE);
		}
		if ( ! unsuccessfulModules.isEmpty()) {
			return Integer.valueOf(PARSE_ERROR_RESULT_CODE);
		}
		return Integer.valueOf(SUCCESSFUL_RESULT_CODE);
	}
	
	/**
	 * The {@link Status} of the operation.
	 * @return An instance of {@link Status}.
	 */
	public Status getStatus() {
		return new Status(getSeverity());
	}
	
	/**
	 * The {@link Severity} of the operation.
	 * @return OK if the operation was successful, 
	 * Error if there was an unhandled exception, or 
	 * Warning if there was a problem parsing.
	 */
	public Severity getSeverity() {
		final Integer resultCode = this.getResultCode();
		switch (resultCode.intValue()) {
			case TASK_FAILED_RESULT_CODE:
				return Severity.ERROR;
			case PARSE_ERROR_RESULT_CODE:
				return Severity.WARNING;
			default:
				return Severity.OK;
		}
	}
	
	@Override
	public Set<EntityId> getSuccessfulModules() {
		return Collections.unmodifiableSet(successfulModules);
	}
	
	@Override
	public Set<EntityId> getUnsuccessfulModules() {
		return Collections.unmodifiableSet(unsuccessfulModules);
	}
	
	@Override
	public Set<EntityId> getUnsupportedModules() {
		return Collections.unmodifiableSet(unsupportedModules);
	}

	@Override
	public void success(final EntityId moduleId) {
		this.successfulModules.add(moduleId);
	}
	
	@Override
	public void error(final EntityId moduleId) {
		if (successfulModules.contains(moduleId)) {
			successfulModules.remove(moduleId);
		}
		
		unsuccessfulModules.add(moduleId);
	}
	
	@Override
	public void unsupported(final EntityId moduleId) {
		if (successfulModules.contains(moduleId)) {
			successfulModules.remove(moduleId);
		}
		
		this.unsupportedModules.add(moduleId);
	}
}
