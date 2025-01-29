/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import innowake.mining.server.error.UserFacingException;
import innowake.mining.shared.model.job.JobStatus;
import org.javatuples.Quartet;
import org.springframework.beans.factory.annotation.Autowired;


import innowake.lib.job.api.OperationCanceledException;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.service.GenAIAvailabilityService;
import innowake.mining.server.service.GenerativeReachabilityBlockDescriptionService;
import innowake.mining.server.service.GenerativeReachabilityBlockDescriptionService.ReachabilityDescriptionResult;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.model.job.Message;

import static innowake.lib.core.lang.Assert.assertNotNull;

/**
 * Job that utilizes GenAI to bulk-generate descriptions for reachability blocks.
 */
public class GenerateReachabilityBlockDescriptionsJob extends MiningJob<Serializable> {

	@Autowired
	private transient GenerativeReachabilityBlockDescriptionService generativeReachabilityBlockDescriptionService;
	
	@Autowired
	private transient GenAIAvailabilityService availabilityService;
	
	@Autowired
	private transient FunctionalBlockService blockService;

	@Autowired
	private transient MonitoredTaskManagerService monitoredTaskManagerService;

	protected final Map<String, List<String>> parameters;
	
	protected boolean errorOccurredDuringGeneration = false;

	protected String result;

	private boolean overwriteAll;
	private boolean generateModuleDescriptions;

	private static final String NON_UUID_MESSAGE = "The following uid does not match the UUID format: \"%s\"";
	private static final String NON_EXISTENT_BLOCK_MESSAGE = "There is no reachability block corresponding to the following uid: \"%s\"";
	private static final String NO_REACHABILITY_BLOCK_MESSAGE = "The functional block corresponding to the following uid is not a reachability block: \"%s\"";
	
	/**
	 * Creates a new GenerateReachabilityBlockDescriptionsJob.
	 *
	 * @param projectId the id of the project which contains the reachability blocks we want to generate descriptions for
	 * @param parameters the export parameters - they should contain a list of reachability block UIDs at key "uids" and a flag "overwrite" for 
	 * overwriting existing descriptions
	 */
	public GenerateReachabilityBlockDescriptionsJob(final EntityId projectId, final Map<String, List<String>> parameters) {
		super(projectId);
		this.parameters = parameters;
	}

	/**
	 * Checks whether the genAI service is available. If yes, uses it to generate descriptions for all reachability blocks submitted to the Job.
	 * If the overwrite flag in the parameters is set to true, existing reachability block descriptions are always overwritten by the GenAI generated descriptions. 
	 * If it is set to false, only empty existing description are overwritten.
	 */
	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Generating GenAI descriptions for reachability blocks");
		
		final List<String> uids = parameters.get("uids");
		if (uids == null || uids.isEmpty()) {
			return new Result<>(new Status(Severity.ERROR),
					"No uids were passed. Please provide a list of reachability block uids to the job via the parameter 'uids'.");
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
		
		final List<String> list = parameters.get("generateModuleDescriptions");
		if (list != null && ! list.isEmpty()) {
			this.generateModuleDescriptions = Boolean.parseBoolean(list.get(0));
		}

		generateReachabilityBlockDescriptions(progressMonitor, uids);
		return determineResult();
	}

	private void generateReachabilityBlockDescriptions(final ProgressMonitor progressMonitor, final List<String> uids) {
		final MonitoredTaskManager monitoredTaskManager = monitoredTaskManagerService.newTaskManager(progressMonitor, uids.size());
		final GenerateReachabilityStepDescriptionHolder descriptionHolder = new GenerateReachabilityStepDescriptionHolder(monitoredTaskManager);
		progressMonitor.checkCanceled();
		for (final String uid : uids) {
			monitoredTaskManager.run(this::generateReachabilityBlockDescription,
					new Quartet<>(uid, jobMonitor, uids.size(), descriptionHolder),
					descriptionHolder::createStepDescription,
					exception -> {
						if (exception.getCause() instanceof OperationCanceledException) {
							assertNotNull(jobMonitor).setStatus(JobStatus.CANCELED);
						}
					});
			monitoredTaskManager.join();
		}
	}

	private void generateReachabilityBlockDescription(final MonitoredTaskParameter monitoredTaskParameter,
			final Quartet<String, JobMonitor, Integer, GenerateReachabilityStepDescriptionHolder> parameter) {
		final String uid = parameter.getValue0();
		final JobMonitor jobMonitor = parameter.getValue1();
		jobMonitor.checkCanceled();
		final GenerateReachabilityStepDescriptionHolder descriptionHolder = parameter.getValue3();

		final UUID uuid;
		try {
			uuid = UUID.fromString(uid);
		} catch (final IllegalArgumentException e) {
			errorOccurredDuringGeneration = true;
			jobMonitor.addMessage(new Message(Message.Severity.ERROR, String.format(NON_UUID_MESSAGE, uid)));
			return;
		}
		final Optional<FunctionalBlockPojo> oBlock = blockService.find(uuid);
		if (oBlock.isEmpty()) {
			errorOccurredDuringGeneration = true;
			jobMonitor.addMessage(new Message(Message.Severity.ERROR, String.format(NON_EXISTENT_BLOCK_MESSAGE, uid)));
			return;
		}
		final FunctionalBlockPojo block = oBlock.get();
		if ( ! block.isOfType(FunctionalBlockType.REACHABILITY)) {
			errorOccurredDuringGeneration = true;
			jobMonitor.addMessage(new Message(Message.Severity.ERROR, String.format(NO_REACHABILITY_BLOCK_MESSAGE, uid)));
			return;
		}

		if (shouldGenerateDescription(block)) {
			try {
				final ReachabilityDescriptionResult reachabilityDescriptionResult = generativeReachabilityBlockDescriptionService.generateDescription(
						projectId, block, this.generateModuleDescriptions, Optional.of(jobMonitor), descriptionHolder, monitoredTaskParameter);
				final String description = reachabilityDescriptionResult.getResult();
				final Optional<String> error = reachabilityDescriptionResult.getError();
				if (error.isPresent()) {
					final String errorMessage = error.get();
					throw new UserFacingException(errorMessage);
				} else {
					handleDescription(description, uid);
				}
			} catch (final Exception e) {
				errorOccurredDuringGeneration = true;
				jobMonitor.addMessage(new Message(Message.Severity.ERROR,
						"Error occurred while generating description using Gen AI for the reachability block with the following UID: " + uid + " Cause: " + e.getMessage()));
			}
		}
	}

	protected boolean shouldGenerateDescription(final FunctionalBlockPojo block) {
		return BlockDescriptionOverwriteValidatorUtil.shouldOverwriteDescription(block, overwriteAll);
	}

	protected void handleDescription(final String description, final String uid) {
		blockService.update(new FunctionalBlockPojoPrototype().setDescription(description).setUid(UUID.fromString(uid)));
	}

	private Result<Serializable> determineResult() {
		if (errorOccurredDuringGeneration) {
			return new Result<>(new Status(Severity.ERROR), "Some descriptions could not be generated");
		}
		return new Result<>(Status.OK, result);
	}

}

