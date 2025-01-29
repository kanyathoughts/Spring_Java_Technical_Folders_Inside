/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;



import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.OperationCanceledException;
import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.genai.RequestMetadataUtil;
import innowake.mining.server.genai.ResponseFormat;
import innowake.mining.server.job.genai.GenerateReachabilityStepDescriptionHolder;
import innowake.mining.server.job.genai.MonitoredTaskManager;
import innowake.mining.server.job.genai.MonitoredTaskParameter;
import innowake.mining.server.job.genai.MonitoredTaskManagerService;
import innowake.mining.server.service.prompt.GenAiPromptService;
import innowake.mining.shared.model.job.JobStatus;
import org.javatuples.Triplet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.server.error.PermissionException;
import innowake.mining.server.genai.requestresponse.BaseResponseModel;
import innowake.mining.server.genai.requestresponse.CustomPromptRequest;
import innowake.mining.server.permission.GenAIModulePermissionChecker;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.model.job.Message;

/**
 * Service for generating descriptions for reachability blocks using generative AI.
 */
@Service
public class GenerativeReachabilityBlockDescriptionService extends CentralCommunicationWithGenAIService {


	@Autowired
	private transient FunctionalBlockService blockService;
	@Autowired
	private transient GenerativeModuleDescriptionService moduleDescriptionService;
	@Autowired
	private transient ModuleService moduleService;
	@Autowired
	private transient MonitoredTaskManagerService monitoredTaskManagerService;

	public GenerativeReachabilityBlockDescriptionService(final GenericConfigProperties configProperties, final ModuleService moduleService,
			final GenAIAvailabilityService availabilityService, final GenAIModulePermissionChecker permissionChecker, final GenAiPromptService promptService) {
		super(configProperties, moduleService, availabilityService, permissionChecker, promptService);
	}

	/**
	 * Uses generative AI to generate a description of a reachability block based on the descriptions of the contained modules.
	 *
	 * @param projectId ID of project containing the reachability block
	 * @param blockUid the UID of the block we want to generate a description for
	 * @param generateModuleDescriptions whether missing descriptions for modules that are part of the specified block should be auto-generated
	 * @return AI generated description of reachability block
	 */
	@Nullable
	public String generateDescription(final EntityId projectId, final UUID blockUid, final boolean generateModuleDescriptions) {
		final Optional<FunctionalBlockPojo> oBlock = blockService.find(blockUid);
		if (oBlock.isEmpty()) {
			throw new IllegalArgumentException("Block with the specified UID could not be found: " + blockUid);
		}
		final FunctionalBlockPojo block = oBlock.get();
		final ReachabilityDescriptionResult result = generateDescription(projectId, block, generateModuleDescriptions, Optional.empty(), null,  null);
		if (result.error.isPresent()) {
			throw new UserFacingException(result.error.get());
		}
		return result.getResult();
	}
	
	/**
	 * Uses generative AI to generate a description of a reachability block based on the descriptions of the contained modules.
	 *
	 * @param projectId ID of project containing the reachability block
	 * @param block the block we want to generate a description for
	 * @param generateModuleDescriptions whether missing descriptions for modules that are part of the specified block should be auto-generated
	 * @param jobMonitor Optional of JobMonitor which can be set if the service is called from a job (for error/warning reporting)
	 * @param descriptionHolder template to update the step description of the Job Monitor. Can be empty if this method is not called from a Job.
	 *  Example: "Generating reachability block descriptions (%s/%s modules of 1/2 blocks)"
	 * @return AI generated description of reachability block
	 */
	public ReachabilityDescriptionResult generateDescription(final EntityId projectId, final FunctionalBlockPojo block, final boolean generateModuleDescriptions,
			final Optional<JobMonitor> jobMonitor, @Nullable final GenerateReachabilityStepDescriptionHolder descriptionHolder, @Nullable final MonitoredTaskParameter monitoredTaskParameter) {
		if ( ! block.isOfType(FunctionalBlockType.REACHABILITY)) {
			throw new IllegalArgumentException("Specified block is not a reachability block: " + block);
		}

		final List<ModulePojo> modules;
		if (generateModuleDescriptions) {
			modules = generateDescriptionsForBlockModules(projectId, block, jobMonitor, descriptionHolder, monitoredTaskParameter);
		} else {
			modules = obtainModules(projectId, block);
		}
		final Map<Boolean, List<ModulePojo>> modulesMap = modules.stream().collect(Collectors.partitioningBy(m -> (m.getDescription().isEmpty() || m.getDescription().get().isBlank())));
		final List<ModulePojo> modulesWithDescription = modulesMap.get(false);
		final List<ModulePojo> modulesWithoutDescription = modulesMap.get(true);
		if (modules.isEmpty()) {
			publishMessage(jobMonitor, Message.Severity.ERROR,
					"Unable to generate a description for the reachability block because it has no modules.");
			return new ReachabilityDescriptionResult(Optional.of("Unable to generate a description for the reachability block because it has no modules."));

		} else if (modulesWithDescription.isEmpty()) {
			publishMessage(jobMonitor, Message.Severity.ERROR,
					"Unable to generate a description for the reachability block because none of the included modules have descriptions.");
			return new ReachabilityDescriptionResult(Optional.of("Unable to generate a description for the reachability block because none of the included modules have descriptions."));
		} else {
			final String prompt = promptService.buildReachabilityBlockPrompt(modulesWithDescription, modulesWithoutDescription);
			final var request = new CustomPromptRequest(getConfigProperties().getGenAiPlugin(), prompt, getConfigProperties().getGenAiMaxNewToken(),
					getConfigProperties().getGenAiTemperature(), getConfigProperties().getGenAiDoSample(), ResponseFormat.TEXT.name(),
					RequestMetadataUtil.getReachabilityBlockMetadata(modulesWithDescription, modulesWithoutDescription));
			final BaseResponseModel responseObject = callGenAi(request, BaseResponseModel.class);
			final String modelResponse = markContent(Assert.assertNotNull(responseObject.getModelResponse())).trim();
			return new ReachabilityDescriptionResult(modelResponse);
		}
	}

	private List<ModulePojo> obtainModules(final EntityId projectId, final FunctionalBlockPojo block) {
		final List<ResolvedModulePart> moduleParts = blockService.getResolvedModuleParts(block.getUid());
		return moduleService.findModules(q -> q.ofProject(projectId).byIds(moduleParts.stream().map(p -> p.getModuleId()).collect(Collectors.toList())));
	}

	private List<ModulePojo> generateDescriptionsForBlockModules(final EntityId projectId, final FunctionalBlockPojo block, final Optional<JobMonitor> jobMonitor,
			@Nullable GenerateReachabilityStepDescriptionHolder descriptionHolder, @Nullable final MonitoredTaskParameter monitoredTaskParameter) {
		final List<ResolvedModulePart> moduleParts = blockService.getResolvedModuleParts(block.getUid());
		final List<EntityId> moduleIds = moduleParts.stream().map(p -> p.getModuleId()).collect(Collectors.toList());
		final List<ModulePojo> modules = moduleService.findModules(q -> q.ofProject(projectId).byIds(moduleIds).includeContent(true));
		final List<ModulePojo> updatedModules = new ArrayList<>();
		final int modulesInBlock = modules.size();
		MonitoredTaskManager monitoredTaskManager;
		if (monitoredTaskParameter == null) {
			for (final ModulePojo module : modules) {
				if (module.getDescription().orElse("").isBlank()) {
					if (!module.getContent().orElse("").isBlank()) {
						try {
							final String description = moduleDescriptionService.deduceDescription(module);
							moduleService.update(new ModulePojoPrototype().withId(module.identity()).setDescription(description));
							updatedModules.add(moduleService.getModule(module.identity()));
						} catch (final PermissionException e) {
							publishMessage(jobMonitor, Message.Severity.WARNING, e.getMessage());
						} catch (final UserFacingException e) {
							publishMessage(jobMonitor, Message.Severity.ERROR,
									"An error occurred while generating description for a module using Generative AI for Module: " + module.getId() + " Cause: "
											+ e.getMessage());
						}
					}
				} else {
					updatedModules.add(module);
				}
			}

		} else {
			monitoredTaskParameter.getProgressMonitor().checkCanceled();
			monitoredTaskManager = monitoredTaskManagerService.newSubTaskManager(monitoredTaskParameter.getMonitoredTaskManagerId(), modulesInBlock);
			if (descriptionHolder == null) {
				descriptionHolder = new GenerateReachabilityStepDescriptionHolder(monitoredTaskManager);
			}
			for (final ModulePojo module : modules) {
				monitoredTaskManager.run(this::updateModule, new Triplet<>(jobMonitor, updatedModules, module), descriptionHolder::createStepDescription,
						exception -> {
							if (exception.getCause() instanceof OperationCanceledException) {
								jobMonitor.get().setStatus(JobStatus.CANCELED);
							}
						});
			}
			monitoredTaskManager.join();
		}
		return updatedModules;
	}

	private void updateModule(final MonitoredTaskParameter monitoredTaskParameter, final Triplet<Optional<JobMonitor>, List<ModulePojo>, ModulePojo> parameter) {
		monitoredTaskParameter.getProgressMonitor().checkCanceled();
		final Optional<JobMonitor> jobMonitor = parameter.getValue0();
		final List<ModulePojo> updatedModules = parameter.getValue1();
		final ModulePojo module = parameter.getValue2();

		if (module.getDescription().orElse("").isBlank()) {
			if (!module.getContent().orElse("").isBlank()) {
				try {
					final String description = moduleDescriptionService.deduceDescription(module);
					moduleService.update(new ModulePojoPrototype().withId(module.identity()).setDescription(description));
					updatedModules.add(moduleService.getModule(module.identity()));
				} catch (final PermissionException e) {
					publishMessage(jobMonitor, Message.Severity.WARNING, e.getMessage());
				} catch (final UserFacingException e) {
					publishMessage(jobMonitor, Message.Severity.ERROR,
							"An error occurred while generating description for a module using Generative AI for Module: " + module.getId() + " Cause: "
									+ e.getMessage());
				}
			}
		} else {
			updatedModules.add(module);
		}
	}


	private void publishMessage(final Optional<JobMonitor> jobMonitor, final Message.Severity severity, final String message) {
		if (jobMonitor.isPresent()) {
			jobMonitor.get().addMessage(new Message(severity, message));
		}
	}

	public static class ReachabilityDescriptionResult {
		@Nullable
		private String result;
		private Optional<String> error = Optional.empty();

		public ReachabilityDescriptionResult(final String result) {
			this.result = result;
		}

		public ReachabilityDescriptionResult(final Optional<String> error) {
			this.error = error;
		}

		@Nullable
		public String getResult() {
			return result;
		}

		public Optional<String> getError() {
			return error;
		}
	}

}
