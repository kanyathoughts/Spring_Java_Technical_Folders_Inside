/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.job.base;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.task.ReportMessageExceptionHandler;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.ResultOrderTaskProcessor;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskProcessor;
import innowake.lib.job.api.task.TaskSource;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.util.ProgressMonitorThrottle;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.job.Message;
import innowake.mining.shared.model.job.JobSummary;
import innowake.mining.shared.model.job.ParsingSummary;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Base implementation of a Job working on Mining Modules.
 * Sub-classes are required to provide a {@link Task} that will do the actual work on such a Module.
 */
public abstract class ModulesJob extends MiningJob<Serializable> {

	private static final Logger LOG = LoggerFactory.getLogger(ModulesJob.class);

	protected final ModuleMatcher moduleMatcher;
	
	private final JobSummary jobSummary;

	@Autowired
	private transient ProjectService projectService;

	@Autowired
	protected transient ModuleService moduleService;

	/**
	 * Creates a new job instance for identifying Module descriptions.
	 *
	 * @param projectId the Id of the project
	 * @param moduleMatcher the ids of all modules to handle
	 */
	public ModulesJob(final EntityId projectId, final ModuleMatcher moduleMatcher) {
		/* We are supporting of storing only project id and a single module id for now, since module matcher can have multiple module ids,
		 * we will send module id only, when module matcher have single module otherwise we will send only project id to store.
		 */
		super(projectId, moduleMatcher.getIds().size() == 1 ? moduleMatcher.getIds().get(0) : null);
		this.moduleMatcher = moduleMatcher;
		this.jobSummary = new JobSummary();
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		final String projectName = projectService.find(projectId).map(ProjectPojo::getName).orElseGet(() -> String.format(" with the ID '%s'", projectId));

		progressMonitor.setJobDescription(String.format(getJobDescriptionPattern(), projectName));
		progressMonitor.setStepDescription("Initializing");

		final ModulesResultConsumer resultConsumer = forkModuleTasks(progressMonitor);
		writeMessage(Message.Severity.INFO, String.format(getIdentifiedMessagePattern(), Integer.valueOf(jobSummary.getSuccessfulModules().size())));

		/* Clear the step description after the job is done */
		progressMonitor.setStepDescription("");
		
		writeMessage(Message.Severity.INFO, jobSummary.toString());
		
		return new Result<>(new Status(resultConsumer.getHighestSeverity()), jobSummary);
	}

	/**
	 * Implementations should create a task doing the actual work on the Module identified by its path.
	 *
	 * @param subMonitor the progress monitor used for keeping track of the overall progress
	 * @param jobId the ID of the Job the task belongs to
	 * @param projectId the Id of the project
	 * @param moduleId the id of the module to work on
	 * @return a task handling the Module
	 */
	protected abstract Task<Serializable> createModuleTask(ProgressMonitor subMonitor, String jobId, EntityId projectId, EntityId moduleId);

	/**
	 * Returns the message pattern for the job description.
	 * <p>
	 * The pattern <b>must</b> have one string parameter for the project name.
	 *
	 * @return the message pattern used for the job description
	 */
	protected abstract String getJobDescriptionPattern();

	/**
	 * Returns the message pattern which is used to give user feedback on how many relevant Modules were identified.
	 * <p>
	 * The pattern <b>must</b> have one numerical parameter for the number of identified Modules.
	 *
	 * @return the message pattern shown as user feedback for the number of identified Modules
	 */
	protected abstract String getIdentifiedMessagePattern();

	private ModulesResultConsumer forkModuleTasks(final ProgressMonitor progressMonitor) {
		final Set<EntityId> moduleIds = new HashSet<>();
		moduleIds.addAll(filterExecutableModuleIds());
		moduleIds.addAll(resolveModulePaths());
		final EntityId[] moduleIdArray = moduleIds.stream().toArray(EntityId[]::new);
		final TaskSource<Serializable> taskSource = new TaskSource<Serializable>() {
			private int taskCount = 0;

			@Override
			public boolean hasNextTask() {
				return taskCount < moduleIds.size();
			}

			@Override
			public Task<Serializable> nextTask() {
				final EntityId entityId = moduleIdArray[taskCount++];
				return createModuleTask(progressMonitor.subMonitor(1), getJobId(), projectId, entityId);
			}
		};

		final ModulesResultConsumer resultConsumer = new ModulesResultConsumer(progressMonitor, moduleIds.size());
		forkTasks(createTaskProcessor(), taskSource, resultConsumer);
		return resultConsumer;
	}

	protected Set<EntityId> filterExecutableModuleIds() {
		final Set<EntityId> moduleIds = new HashSet<>();
		moduleMatcher.getIds().forEach(moduleId -> {
			final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).byId(moduleId))
											.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + moduleId + " in project: " + projectId));
			if (getSupportedModuleTypes().contains(Tuple2.of(module.getTechnology(), module.getType()))) {
				moduleIds.add(module.identity());
			} else {
				jobSummary.addUnsupportedModule(module.identity());
			}
		});
		return moduleIds;
	}
	protected Set<EntityId> resolveModulePaths() {
		final Set<EntityId> moduleIds = new HashSet<>();
		moduleMatcher.getPathPatterns().forEach(path -> {
			try {
				moduleIds.add(moduleService.findAnyModuleId(b -> b.ofProject(projectId)
																.withPath(path)
																.withTechnologiesAndTypes(getSupportedModuleTypes()))
						.orElseThrow(() -> new MiningEntityNotFoundException(
								String.format("Module in project: %s with technology and type in: %s and path %s must exist", 
										projectId, getSupportedModuleTypes(), path))));
			} catch (final MiningEntityNotFoundException e) {
				LOG.trace("Unable to find module for given path : {}", path);
			}
		});
		return moduleIds;
	}

	@Override
	protected <R extends Serializable> TaskProcessor<R> createTaskProcessor() {
		return new ResultOrderTaskProcessor<>(jobManager, assertNotNull(jobMonitor));
	}

	protected abstract List<Tuple2<Technology, Type>> getSupportedModuleTypes();


	private class ModulesResultConsumer extends ResultConsumer<Serializable> {

		private int resultCount = 1;
		private final ProgressMonitor progressMonitor;
		private final int totalTasks;
		
		public ModulesResultConsumer(final ProgressMonitor progressMonitor, final int totalTasks) {
			super(new ReportMessageExceptionHandler<>(assertNotNull(jobMonitor)));
			this.progressMonitor = progressMonitor;
			this.totalTasks = totalTasks;
		}
		

		@Override
		protected void handleResult(final String taskId, final Result<Serializable> result) {
			if (result.value instanceof ParsingSummary) {
				jobSummary.aggregate((ParsingSummary) result.value);
			}
			
			ProgressMonitorThrottle.throttleStepDescription(
					String.format("Executing Tasks (%d/%d)", Integer.valueOf(resultCount++), Integer.valueOf(totalTasks)), progressMonitor);
		}
	}
}
