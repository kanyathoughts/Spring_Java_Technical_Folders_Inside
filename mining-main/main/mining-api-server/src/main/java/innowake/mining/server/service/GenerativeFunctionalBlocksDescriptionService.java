/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.service;

import innowake.lib.core.lang.Assert;
import static innowake.lib.core.lang.Assert.assertNotNull;
import innowake.lib.job.api.OperationCanceledException;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.data.core.SchemaConstants;
import innowake.mining.server.error.PermissionException;
import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.genai.RequestMetadataUtil;
import innowake.mining.server.genai.ResponseFormat;
import innowake.mining.server.genai.requestresponse.BaseResponseModel;
import innowake.mining.server.genai.requestresponse.CustomPromptRequest;
import innowake.mining.server.job.genai.GenerateAnnotationsStepDescriptionHolder;
import innowake.mining.server.job.genai.MonitoredTaskManager;
import innowake.mining.server.job.genai.MonitoredTaskManagerService;
import innowake.mining.server.job.genai.MonitoredTaskParameter;
import innowake.mining.server.job.identification.AnnotationOverwriteValidator;
import innowake.mining.server.permission.GenAIModulePermissionChecker;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.service.prompt.GenAiPromptService;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;
import org.javatuples.Quintet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * Service for generating descriptions for functional blocks using generative AI.
 */
@Service
public class GenerativeFunctionalBlocksDescriptionService extends CentralCommunicationWithGenAIService {


	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private GenerativeAnnotationTranslationService annotationTranslationService;
	@Autowired
	private transient GenAiPromptService promptService;

	@Autowired
	private MonitoredTaskManagerService monitoredTaskManagerService;


	public GenerativeFunctionalBlocksDescriptionService(final GenericConfigProperties configProperties, final ModuleService moduleService,
			final GenAIAvailabilityService availabilityService, final GenAIModulePermissionChecker permissionChecker,  final GenAiPromptService promptService) {
		super(configProperties, moduleService, availabilityService, permissionChecker, promptService);
	}

	public String generateAnnotationsDescription(final MonitoredTaskParameter monitoredTaskParameter, final JobMonitor jobMonitor, List<AnnotationPojo> annotations, final EntityId projectId, final boolean generateDescriptions) {
		final Map<Long, Boolean> cachedModulePermission = new HashMap<>();

		if (generateDescriptions) {
			annotations = generateAnnotationDescriptions(monitoredTaskParameter, jobMonitor, annotations, projectId, cachedModulePermission);
			if (annotations.isEmpty()) {
				throw new UserFacingException("Describing annotations failed due to missing permissions or missing source attachment.");
			}
		} else if (!hasAtLeastOneDescription(annotations)) {
			throw new UserFacingException("Since none of the annotations has a description, a group description cannot be generated.");
		}
		return getDescription(annotations);

	}

	private boolean hasAtLeastOneDescription(final List<AnnotationPojo> annotations) {
		for (final AnnotationPojo annotation : annotations) {
			if (annotation.getName().isBlank() || (annotation.getCreatedByUserId().equals(SchemaConstants.SYSTEM_USER) && annotation.getUpdatedByUserId().isEmpty())) {
				continue;
			}
			return true;
		}
		return false;
	}

	private String getDescription(final List<AnnotationPojo> annotations) {
		final var annotationGroupDescriptionRequest = new CustomPromptRequest(getConfigProperties().getGenAiPlugin(),
				promptService.buildAnnotationGroupPrompt(annotations), getConfigProperties().getGenAiMaxNewToken(), getConfigProperties().getGenAiTemperature(),
				getConfigProperties().getGenAiDoSample(), ResponseFormat.TEXT.name(), RequestMetadataUtil.getFunctionalBlockMetadata(annotations));
		final BaseResponseModel responseObject = callGenAi(annotationGroupDescriptionRequest, BaseResponseModel.class);
		return markContent(Assert.assertNotNull(responseObject.getModelResponse())).trim();

	}

	public List<AnnotationPojo> generateAnnotationDescriptions(final MonitoredTaskParameter monitoredTaskParameter, final JobMonitor jobMonitor, final List<AnnotationPojo> annotations, final EntityId projectId,  final Map<Long, Boolean> cachedModulePermission) {
		final AnnotationOverwriteValidator validator = new AnnotationOverwriteValidator();
		final List<AnnotationPojo> updatedAnnotations = new ArrayList<>();
		final MonitoredTaskManager monitoredTaskManager = monitoredTaskManagerService.newSubTaskManager(monitoredTaskParameter.getMonitoredTaskManagerId(),
				annotations.size());
		final GenerateAnnotationsStepDescriptionHolder descriptionHolder = new GenerateAnnotationsStepDescriptionHolder(monitoredTaskManager);
		jobMonitor.checkCanceled();
		for (final AnnotationPojo annotation : annotations) {
			if (validator.shouldOverwriteAnnotationName(annotation)) {
				monitoredTaskManager.run(this::generateDescription,
						new Quintet<>(projectId, jobMonitor, updatedAnnotations, annotation, cachedModulePermission), descriptionHolder::createStepDescription,
						exception -> {
							if (exception.getCause() instanceof OperationCanceledException) {
								assertNotNull(jobMonitor).setStatus(JobStatus.CANCELED);
							}
						});
			} else {
				updatedAnnotations.add(annotation);
			}
		}
		monitoredTaskManager.join();
		return updatedAnnotations;
	}

	private void generateDescription(final MonitoredTaskParameter monitoredTaskParameter,
			final Quintet<EntityId, JobMonitor, List<AnnotationPojo>, AnnotationPojo, Map<Long, Boolean> > parameter){
		final EntityId projectId = parameter.getValue0();
		final JobMonitor jobMonitor = parameter.getValue1();
		final List<AnnotationPojo> updatedAnnotations = parameter.getValue2();
		final AnnotationPojo annotation = parameter.getValue3();
		final Map<Long, Boolean> cachedModulePermission = parameter.getValue4();
		final EntityId moduleId = annotation.getModule();
		final Boolean cacheValue = cachedModulePermission.get(moduleId.getNid());
		jobMonitor.checkCanceled();
		if (cacheValue == null || cacheValue) {
			try {
				final String genAiDescription = annotationTranslationService.translateUsingGenAI(annotation, projectId, moduleId, cacheValue != null);
				cachedModulePermission.put(moduleId.getNid(), true);
				final AnnotationPojoPrototype updatedAnnotation = new AnnotationPojoPrototype()
						.withId(annotation.identity())
						.setName(genAiDescription)
						.setUpdatedByUserId(SchemaConstants.SYSTEM_USER);
				annotationService.update(updatedAnnotation);
				updatedAnnotations.add(annotationService.get(annotation.identity()));
			} catch (final PermissionException e) {
				cachedModulePermission.put(moduleId.getNid(), false);
				publishMessage(jobMonitor, Message.Severity.WARNING, e.getMessage());
			} catch (final IllegalArgumentException | UserFacingException e) {
				cachedModulePermission.put(moduleId.getNid(), true);
				publishMessage(jobMonitor, Message.Severity.WARNING, e.getMessage());
			}
		}
	}

	private void publishMessage(final JobMonitor jobMonitor, final Message.Severity severity, final String message) {
		if (jobMonitor!= null) {
			jobMonitor.addMessage(new Message(severity, message));
		}
	}
}
