/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import innowake.mining.shared.model.job.JobStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import innowake.lib.core.lang.Assert;
import innowake.mining.shared.model.job.Message;
import innowake.lib.job.api.OperationCanceledException;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.mining.server.error.PermissionException;
import innowake.mining.server.event.ModulesModifiedEvent;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.service.GenAIAvailabilityService;
import innowake.mining.server.service.GenerativeModuleDescriptionService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.server.error.UserFacingException;

/**
 * Job that utilizes GenAI to bulk-generate module descriptions for a list of modules.
 */
public class GenerateModuleDescriptionsJob extends MiningJob<Serializable> {

	@Autowired
	private transient GenAIAvailabilityService availabilityService;
	
	@Autowired
	private transient GenerativeModuleDescriptionService generativeModuleDescriptionService;
	
	@Autowired
	private transient ModuleService moduleService;
	
	@Autowired
	private transient ApplicationEventPublisher eventPublisher;

	@Autowired
	private transient MonitoredTaskManagerService monitoredTaskManagerService;
	
	private final Map<String, List<String>> parameters;
	
	private boolean nonNumericIdsExist = false;
	private boolean nonExistentIdsExist = false;
	private boolean modulesNoContentExist = false;
	private boolean modulesNoPermissionExist = false;
	private boolean genAIExceptionOccurred = false;
	private boolean genericErrorOccurred = false;

	private boolean overwriteAll = false;
	
	private static final String STEP_DESCRIPTION = "Generating module descriptions (%s/%s modules)";
	
	private static final String INVALID_ID_MESSAGE = "The following ID is invalid: \"%s\"";
	private static final String NON_EXISTENT_MODULE_MESSAGE = "There is no module corresponding to the following ID: \"%s\"";
	private static final String NO_CONTENT_MESSAGE = "Description could not be generated because content is empty for module with name: \"%s\" and ID: \"%s\"";


	/**
	 * Creates a new GenerateModuleDescriptionsJob.
	 *
	 * @param projectId the id of the project which contains the modules we want to generate descriptions for
	 * @param parameters parameters map - should contain a list of module IDs at key "ids" and (optionally) a flag "overwrite" for
	 * overwriting existing descriptions
	 */
	public GenerateModuleDescriptionsJob(final EntityId projectId, final Map<String, List<String>> parameters) {
		super(projectId);
		this.projectId = projectId;
		this.parameters = parameters;
	}

	/**
	 * Checks whether the genAI service is available. If yes, uses it to generate module descriptions for all modules submitted to the Job.
	 * If the overwrite flag in the parameters is set to true, existing module descriptions are always overwritten by the GenAI generated descriptions. 
	 * If it is set to false, only annotations for which {@linkplain ModuleDescriptionOverwriteValidatorUtil#shouldOverwriteModuleDescription} returns true
	 * are overwritten.
	 */
	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Generating GenAI descriptions for modules");
		
		final List<String> moduleIds = parameters.get("ids");
		if (moduleIds == null || moduleIds.isEmpty()) {
			return new Result<>(new Status(Severity.ERROR), "No module IDs were passed. Please provide a list of module IDs to the job via the parameter 'ids'.");
		}
		
		progressMonitor.setStepDescription("Checking if GenAI service is available ...");
		progressMonitor.checkCanceled();
		if ( ! availabilityService.isGenAIServiceAvailable()) {
			return new Result<>(new Status(Severity.ERROR), "Descriptions couldn't be created because the GenAI service is not available. Please check your configuration.");
		}
		
		final Optional<List<String>> oOverwrite = Optional.ofNullable(parameters.get("overwrite"));
		if (oOverwrite.isPresent() && ! oOverwrite.get().isEmpty()) {
			this.overwriteAll = Boolean.parseBoolean(oOverwrite.get().get(0));
		}
		
		generateModuleDescriptions(progressMonitor, moduleIds);
		return determineResult();
	}
	
	private Result<Serializable> determineResult() {
		if (nonNumericIdsExist || nonExistentIdsExist || genAIExceptionOccurred || genericErrorOccurred) {
			return new Result<>(new Status(Severity.ERROR), "Some descriptions could not be generated");
		} else if (modulesNoContentExist || modulesNoPermissionExist) {
			return new Result<>(new Status(Severity.WARNING), "Some descriptions could not be generated");
		} else {
			return new Result<>(Status.OK);
		}
	}
	
	private void generateModuleDescriptions(final ProgressMonitor progressMonitor, final List<String> moduleIds) {
		final MonitoredTaskManager monitoredTaskManager = monitoredTaskManagerService.newTaskManager(progressMonitor, moduleIds.size());
		progressMonitor.checkCanceled();
		for (final String moduleId : moduleIds) {
			monitoredTaskManager.run(this::handleModule,
					moduleId,
					monitoredTaskParameter -> String.format(STEP_DESCRIPTION, monitoredTaskManager.getNumberOfWorkedSteps(), monitoredTaskManager.getNumberOfProgressSteps()),
					exception -> {
						if (exception.getCause() instanceof OperationCanceledException) {
							Assert.assertNotNull(jobMonitor).setStatus(JobStatus.CANCELED);
						}
					});
		}
		monitoredTaskManager.join();
	}

	private void handleModule(final MonitoredTaskParameter monitoredTaskParameter, final String moduleId) {
		final ProgressMonitor progressMonitor = monitoredTaskParameter.getProgressMonitor();
		progressMonitor.checkCanceled();
		final EntityId moduleIdAsE;
		final var jobMonitor = Assert.assertNotNull(this.jobMonitor);
		try {
			moduleIdAsE = EntityId.of(moduleId);
		} catch (final NumberFormatException e) {
			nonNumericIdsExist = true;
			jobMonitor.addMessage(new Message(Message.Severity.ERROR, String.format(INVALID_ID_MESSAGE, moduleId)));
			return;
		}

		final var module = moduleService.findAnyModule(q -> q.ofProject(projectId).byId(moduleIdAsE).includeContent(true));
		if (module.isEmpty()) {
			nonExistentIdsExist = true;
			jobMonitor.addMessage(new Message(Message.Severity.ERROR, String.format(NON_EXISTENT_MODULE_MESSAGE, moduleId)));
			return;
		}
		
		final String content = module.get().getContent().orElse(null);
		if (content == null || content.isBlank()) {
			modulesNoContentExist = true;
			jobMonitor.addMessage(new Message(Message.Severity.WARNING, String.format(NO_CONTENT_MESSAGE, module.get().getName(), moduleId)));
		} else if (ModuleDescriptionOverwriteValidatorUtil.shouldOverwriteModuleDescription(module.get(), overwriteAll)) {
			try {
				final var description = generativeModuleDescriptionService.deduceDescription(module.get());
				moduleService.update(new ModulePojoPrototype()
											.withId(module.get().identity())
											.setDescription(description));
				eventPublisher.publishEvent(new ModulesModifiedEvent(projectId, Optional.of(module.get().identity())));
			} catch (final PermissionException e) {
				modulesNoPermissionExist = true;
				jobMonitor.addMessage(new Message(Message.Severity.WARNING, "Error processing Module: " + moduleId + ". Message: " + e.getMessage()));
			} catch (final UserFacingException e) {
				genAIExceptionOccurred = true;
				jobMonitor.addMessage(new Message(Message.Severity.ERROR,
						"An error occurred while generating description for a module using Generative AI for Module: "+ moduleId + " Cause: " + e.getMessage()));
			} catch (final Exception e) {
				genericErrorOccurred = true;
				jobMonitor.addMessage(new Message(Message.Severity.ERROR,
						"An error occurred while generating description for a module using Generative AI for Module: "+ moduleId + " Cause: " + e.getMessage()));
			}
		}
	}

}
