/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

import static innowake.lib.core.lang.Assert.assertNotNull;
import innowake.lib.job.api.OperationCanceledException;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.service.GenAIAvailabilityService;
import innowake.mining.server.service.GenerativeFunctionalBlocksDescriptionService;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.MiningPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;
import org.javatuples.Quartet;
import org.springframework.beans.factory.annotation.Autowired;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Job that utilizes GenAI to bulk-generate descriptions for functional blocks.
 */
public class GenerateFunctionalBlockDescriptionsJob extends MiningJob<Serializable> {

	@Autowired
	private transient GenerativeFunctionalBlocksDescriptionService generativeFunctionalBlocksDescriptionService;

	@Autowired
	private transient GenAIAvailabilityService availabilityService;

	@Autowired
	private transient FunctionalBlockService functionalBlockService;

	@Autowired
	private transient GenericConfigProperties configProperties;

    @Autowired
	private transient MonitoredTaskManagerService monitoredTaskManagerService;

	@Autowired
	private transient AnnotationService annotationService;

	private boolean errorOccurredDuringGeneration = false;

	private boolean overwriteAll;

	private final Map<String, List<String>> parameters;

	/**
	 * Creates a new GenerateFunctionalBlockDescriptionsJob.
	 *
	 * @param projectId the id of the project which contains the functional blocks we want to generate descriptions for
	 * @param parameters the export parameters - they should contain a list of functional block UIDs at key "uids" and a flag "overwrite" for
	 * overwriting existing descriptions
	 */
	public GenerateFunctionalBlockDescriptionsJob(final EntityId projectId, final Map<String, List<String>> parameters) {
		super(projectId);
		this.parameters = parameters;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
			if ( ! availabilityService.isGenAIServiceAvailable()) {
				return new Result<>(new Status(Severity.ERROR), "Descriptions couldn't be created because the GenAI service is not available. Please check your configuration.");
			}
		   progressMonitor.setJobDescription("Generating GenAI descriptions for functional blocks");
		   progressMonitor.checkCanceled();
		   final List<String> blocks = parameters.get("functionalBlockIds");
		   if (blocks == null || blocks.isEmpty()) {
			   return new Result<>(new Status(Severity.ERROR),
					   "No uids were passed. Please provide a list of functional block uids to the job via the parameter 'functionalBlockIds'.");
		   }
		   final Optional<List<String>> oOverwrite = Optional.ofNullable(parameters.get("overwrite"));
		   if (oOverwrite.isPresent() && ! oOverwrite.get().isEmpty()) {
			   this.overwriteAll = Boolean.valueOf(oOverwrite.get().get(0));
		   }
		   progressMonitor.setStepDescription("Checking if GenAI service is available ...");
		   progressMonitor.checkCanceled();
		   if ( ! availabilityService.isGenAIServiceAvailable()) {
			   return new Result<>(new Status(Severity.ERROR), "Descriptions couldn't be created because the GenAI service is not available. Please check your configuration.");
		   }

		   generateFunctionalBlocksDescriptions(progressMonitor, blocks);
		   return determineResult();
	}

	private void generateFunctionalBlocksDescriptions(final ProgressMonitor progressMonitor , final List<String> blocks){
		final JobMonitor monitor = assertNotNull(jobManager.getJobMonitor(jobId));
		final MonitoredTaskManager monitoredTaskManager = monitoredTaskManagerService.newTaskManager(progressMonitor, blocks.size());
		final GenerateFunctionalBlockStepDescriptionHolder descriptionHolder = new GenerateFunctionalBlockStepDescriptionHolder(monitoredTaskManager);

		final List<FunctionalBlockPojo> rootBlocks = blocks.stream().map(UUID::fromString).map(fb -> functionalBlockService.find(fb).get()).toList();

		final Map<UUID, List<FunctionalBlockPojo>> tree = functionalBlockService.findChildrenDeep(
				rootBlocks.stream().map(FunctionalBlockPojo::getUid).toList(), -1,
				q -> q.withTypes(List.of(FunctionalBlockType.FUNCTIONAL_GROUP, FunctionalBlockType.FUNCTIONAL_UNIT)));

		for (final Map.Entry<UUID, List<FunctionalBlockPojo>> map : tree.entrySet()) {
			progressMonitor.checkCanceled();
			final List<UUID> functionalUnitUUIDs = map.getValue().stream().map(MiningPojo::getUid).toList();
			final Map<UUID, GeneratedFrom> generatedFrom = functionalBlockService.getGeneratedFrom(functionalUnitUUIDs);
			final Map<Long, UUID> annotationIdMap = generatedFrom.entrySet().stream().filter(entry -> !entry.getValue().getAnnotationId().get().isEmpty()).collect(Collectors.toMap(entry -> entry.getValue().getAnnotationId().get().getNid(), Map.Entry::getKey));
			final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).byNids(new ArrayList<>(annotationIdMap.keySet())));
			final Optional<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(map.getKey());
			if (shouldGenerateDescription(functionalBlockPojo.get())) {
				functionalBlockPojo.ifPresent(blockPojo -> monitoredTaskManager.run(this::generateFunctionalBlockDescriptions,
						new Quartet<>(monitor, annotations, blockPojo, descriptionHolder), descriptionHolder::createStepDescription, exception -> {
							if (exception.getCause() instanceof OperationCanceledException) {
								assertNotNull(jobMonitor).setStatus(JobStatus.CANCELED);
							}
						}));
				monitoredTaskManager.join();
			}
		}

	}

	private void generateFunctionalBlockDescriptions(final MonitoredTaskParameter monitoredTaskParameter,
			final Quartet<JobMonitor, List<AnnotationPojo>, FunctionalBlockPojo, GenerateFunctionalBlockStepDescriptionHolder> parameter) {
		final JobMonitor jobMonitor = parameter.getValue0();
		final List<AnnotationPojo> annotations = parameter.getValue1();
		final FunctionalBlockPojo functionalBlock = parameter.getValue2();
		jobMonitor.checkCanceled();

			try {
				final String description = generativeFunctionalBlocksDescriptionService.generateAnnotationsDescription(monitoredTaskParameter, jobMonitor,
						annotations, projectId, true);
				final FunctionalBlockPojoPrototype prototype = new FunctionalBlockPojoPrototype();
				prototype.setUid(functionalBlock.getUid());
				prototype.setDescription(description);
				prototype.setChildren(functionalBlock.getChildren());
				prototype.setName(functionalBlock.getName());
				prototype.setModuleParts(functionalBlock.getModuleParts());
				functionalBlockService.update(prototype);
			} catch (final UserFacingException e) {
				errorOccurredDuringGeneration = true;
				jobMonitor.addMessage(new Message(Message.Severity.ERROR,
						"Error occurred while generating description using Gen AI for the functional block with the following UID: " + functionalBlock.getUid() + " Cause: " + e.getMessage()));
			}
	}

	private Result<Serializable> determineResult() {
		if (errorOccurredDuringGeneration) {
			return new Result<>(new Status(Severity.ERROR), "Some descriptions could not be generated");
		}
		return new Result<>(Status.OK);
	}
	protected boolean shouldGenerateDescription(final FunctionalBlockPojo block) {
		return BlockDescriptionOverwriteValidatorUtil.shouldOverwriteDescription(block, overwriteAll);
	}

}

