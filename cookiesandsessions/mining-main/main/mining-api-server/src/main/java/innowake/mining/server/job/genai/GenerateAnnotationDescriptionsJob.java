/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

import static innowake.lib.core.lang.Assert.assertNotNull;
import innowake.lib.job.api.OperationCanceledException;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import innowake.mining.shared.model.job.JobStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.mining.data.core.SchemaConstants;
import innowake.mining.server.error.PermissionException;
import innowake.mining.server.event.AnnotationUpdatedEvent;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.job.identification.AnnotationOverwriteValidator;
import innowake.mining.server.service.GenAIAvailabilityService;
import innowake.mining.server.service.GenerativeAnnotationTranslationService;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.job.Message;
import innowake.mining.shared.service.UserRoleService;

/**
 * Job that utilizes GenAI to bulk-generate annotation descriptions for all annotations referenced by a list of modules.
 */
public class GenerateAnnotationDescriptionsJob extends MiningJob<Serializable> {

	@Autowired
	private transient GenerativeAnnotationTranslationService generativeAnnotationTranslationService;

	@Autowired
	private transient GenAIAvailabilityService availabilityService;

	@Autowired
	private transient AnnotationService annotationService;

	@Autowired
	protected transient UserRoleService user;

	@Autowired
	private transient ApplicationEventPublisher eventPublisher;

	@Autowired
	private transient MonitoredTaskManagerService monitoredTaskManagerService;

	private final Map<String, List<String>> parameters;

	private boolean overwriteAll = false;

	private boolean modulesNoPermissionExist = false;
	private boolean genericErrorOccurred = false;


	/**
	 * Creates a new ExplainAnnotationsJob.
	 *
	 * @param projectId the id of the project which contains the annotations we want to generate descriptions for
	 * @param parameters the export parameters - they should contain a list of moduleIds at key "moduleId" and a flag "overwrite" for overwriting existing descriptions
	 */
	public GenerateAnnotationDescriptionsJob(final EntityId projectId, final Map<String, List<String>> parameters) {
		super(projectId);
		this.parameters = parameters;
	}
	
	/**
	 * Checks whether the genAI service is available. If yes, uses it to generate annotation descriptions for all annotations referenced by the modules submitted to the Job.
	 * If the overwrite flag in the parameters is set to true, existing annotation descriptions are always overwritten by the GenAI generated descriptions. 
	 * If it is set to false, only annotations for which {@linkplain AnnotationOverwriteValidator#shouldOverwriteAnnotationName} returns true are overwritten.
	 */
	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Generating GenAI descriptions for annotations");
		progressMonitor.setStepDescription("Checking if GenAI service is available ...");
		progressMonitor.checkCanceled();
		if ( ! availabilityService.isGenAIServiceAvailable()) {
			return new Result<>(new Status(Severity.ERROR), "Descriptions couldn't be created because the GenAI service is not available. Please check your configuration.");
		}
		
		final Optional<List<String>> oOverwrite = Optional.ofNullable(parameters.get("overwrite"));
		if (oOverwrite.isPresent() && ! oOverwrite.get().isEmpty()) {
			this.overwriteAll = Boolean.parseBoolean(oOverwrite.get().get(0));
		}
		
		generateAnnotationDescriptions(progressMonitor);
		
		eventPublisher.publishEvent(new AnnotationUpdatedEvent(projectId));
		return determineResult();
	}
	
	private Result<Serializable> determineResult() {
		if (modulesNoPermissionExist) {
			return new Result<>(new Status(Severity.WARNING), "Some descriptions could not be generated");
		} else if (genericErrorOccurred) {
			return new Result<>(new Status(Severity.ERROR), "Some descriptions could not be generated");
		}
		return new Result<>(Status.OK);
	}
	
	private void generateAnnotationDescriptions(final ProgressMonitor progressMonitor) {
		final List<String> annotationIds = Optional.ofNullable(parameters.get("ids")).orElse(Collections.emptyList());
		final List<Long> annotations = annotationIds.stream()
				.map(Long::valueOf)
				.toList();
		progressMonitor.checkCanceled();
		final MonitoredTaskManager monitoredTaskManager = monitoredTaskManagerService.newTaskManager(progressMonitor, annotationIds.size());
		final GenerateAnnotationsStepDescriptionHolder descriptionHolder = new GenerateAnnotationsStepDescriptionHolder(monitoredTaskManager);
		final List<AnnotationPojo> annotationPojos = annotationService.find(q -> q.ofProject(projectId).byNids(annotations));

		for (final AnnotationPojo annotation : annotationPojos) {
			final AnnotationOverwriteValidator validator = new AnnotationOverwriteValidator();
			if (!overwriteAll && !validator.shouldOverwriteAnnotationName(annotation)) {
				continue;
			}
			progressMonitor.checkCanceled();
			monitoredTaskManager.run(this::getAnnotationDescription,
					annotation,
					descriptionHolder::createStepDescription,
					exception -> {
						if (exception.getCause() instanceof OperationCanceledException) {
							assertNotNull(jobMonitor).setStatus(JobStatus.CANCELED);
						} else if (exception.getCause() instanceof PermissionException) {
							modulesNoPermissionExist = true;
							assertNotNull(jobMonitor).addMessage(new Message(Message.Severity.WARNING, "Error processing Module: " + annotation.getModuleName() + " Message: " + exception.getMessage()));
						} else {
							genericErrorOccurred = true;
							assertNotNull(jobMonitor).addMessage(new Message(Message.Severity.ERROR, "Error processing Module: " + annotation.getModuleName() + " Message: " + exception.getMessage()));
						}
					});
			}
		monitoredTaskManager.join();
	}

	@SuppressWarnings("unused")
	private void getAnnotationDescription(final MonitoredTaskParameter monitoredTaskParameter,  final AnnotationPojo annotation) {
		monitoredTaskParameter.getProgressMonitor().checkCanceled();
		final String genAiDescription = generativeAnnotationTranslationService.translateAnnotationUsingGenAI(annotation);
		annotationService.update(new AnnotationPojoPrototype()
									.withId(annotation.identity())
									.setName(genAiDescription)
									.setUpdatedByUserId(SchemaConstants.SYSTEM_USER));
	}

}
