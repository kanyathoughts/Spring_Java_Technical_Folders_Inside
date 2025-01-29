/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.job.base;

import java.io.Serializable;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.task.Task;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.model.job.Message;

/**
 * Base implementation of a task working on a single Module via its path.
 */
public abstract class ModuleTask extends Task<Serializable> {

	private static final Logger LOG = LoggerFactory.getLogger(ModuleTask.class);

	protected final EntityId projectId;

	private final EntityId moduleId;
	
	private final TaskSummary taskSummary;
	
	@Autowired
	protected transient ModuleService moduleService;

	/**
	 * Creates a new instance of the task.
	 * 
	 * @param progressMonitor the progress monitor to use
	 * @param jobId the ID of the job this task belongs to
	 * @param projectId the Id of the project
	 * @param moduleId the id of the Module to work on
	 */
	protected ModuleTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		super(progressMonitor, jobId);
		this.taskSummary = new TaskSummary(moduleId);
		this.projectId = projectId;
		this.moduleId = moduleId;
	}

	@Override
	protected final Result<Serializable> run(final ProgressMonitor progressMonitor) {
		progressMonitor.checkCanceled();
		try {		
			run(moduleId);
		} catch (final Exception ex) {
			LOG.error("Module task " + getClass().getSimpleName() + " for module " + moduleId + " has failed", ex);
			getTaskSummary().error(moduleId);
			getTaskSummary().setHadUnhandledException(true);
		}
		
		return new Result<>(getTaskSummary().getStatus(), getTaskSummary());
	}

	/**
	 * Prints message in Job log if the module doesn't have sourceCode Available.
	 * 
	 * @param moduleId the Id of the module
	 * @return {@code true} if sourceCode is available. {{@code false}} otherwise
	 */
	protected boolean isSourceAvailable(final EntityId moduleId) {
		final var info = moduleService.findAnyModule(b -> b.byId(moduleId));
		if (info.isEmpty()) {
			throw new MiningEntityNotFoundException("Could not find module with id: " + moduleId);
		}

		final var modulePojo = info.get();
		if ( ! modulePojo.isSourceCodeAvailable()) {
			writeMessage(Message.Severity.ERROR,
					String.format("Source code missing: Can not identify Module with Id: '%s' with Name: '%s'", modulePojo.identity().toString(), modulePojo.getName()));
			return false;
		}
		return true;
	}

	/**
	 * Implementing classes should do their actual work here.
	 * <p>
	 * Monitoring for the single Module is already taken care of.
	 *
	 * @param moduleId the ID of the Module to work on
	 */
	protected abstract void run(final EntityId moduleId);

	protected EntityId getProjectId() {
		return projectId;
	}
	
	protected TaskSummary getTaskSummary() {
		return taskSummary;
	}
}