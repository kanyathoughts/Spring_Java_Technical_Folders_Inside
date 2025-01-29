/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import innowake.mining.server.genai.RequestMetadataUtil;
import innowake.mining.server.genai.ResponseFormat;
import innowake.mining.server.service.prompt.GenAiPromptService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.data.core.SchemaConstants;
import innowake.mining.server.error.PermissionException;
import innowake.mining.server.genai.requestresponse.BaseResponseModel;
import innowake.mining.server.genai.requestresponse.CustomPromptRequest;
import innowake.mining.server.job.identification.AnnotationOverwriteValidator;
import innowake.mining.server.permission.GenAIModulePermissionChecker;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.job.Message;
import innowake.mining.server.error.UserFacingException;

/**
 * Service for generating descriptions for annotation groups using generative AI.
 */
@Service
public class GenerativeAnnotationGroupDescriptionService extends CentralCommunicationWithGenAIService {


	@Autowired
	private transient AnnotationService annotationService;
	@Autowired
	private transient GenerativeAnnotationTranslationService annotationTranslationService;
	@Autowired
	private transient GenAiPromptService promptService;
	@Nullable
	protected transient JobMonitor jobMonitor;

	public GenerativeAnnotationGroupDescriptionService(final GenericConfigProperties configProperties, final ModuleService moduleService,
			final GenAIAvailabilityService availabilityService, final GenAIModulePermissionChecker permissionChecker, final GenAiPromptService promptService) {
		super(configProperties, moduleService, availabilityService, permissionChecker, promptService);
	}

	/**
	 * Generates a meta-description based on the descriptions
	 *
	 * @param annotationIds annotations in the group
	 * @param projectId ID of project the Annotations belongs to
	 * @param generateDescriptions if missing  descriptions should be generated or not
	 * @return the description of group annotations
	 */
	public String generateAnnotationGroupDescription(final List<EntityId> annotationIds, final EntityId projectId, final boolean generateDescriptions) {
		final Map<Long, Boolean> cachedModulePermission = new HashMap<>();
		List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).byIds(annotationIds));
		if (annotations.isEmpty()) {
			throw new UserFacingException("No annotations could be found that match the given annotation ids. Please check your annotations.");
		}

		if (generateDescriptions) {
			annotations = generateAnnotationDescriptions(annotations, projectId, Optional.ofNullable(jobMonitor), cachedModulePermission);
			if (annotations.isEmpty()) {
				throw new UserFacingException("Describing annotations failed due to missing permissions or missing source attachment. Please check your source modules.");
			}
		} else if ( ! hasAtLeastOneDescription(annotations)) {
			throw new UserFacingException("Since none of the annotations has a description, a group description cannot be generated. Please generate descriptions for the annotations first.");
		}
		return getDescription(annotations);

	}

	private boolean hasAtLeastOneDescription(final List<AnnotationPojo> annotations) {
		for (final AnnotationPojo annotation : annotations) {
			if (annotation.getName().isBlank() || (annotation.getCreatedByUserId().equals(SchemaConstants.SYSTEM_USER) && annotation.getUpdatedByUserId()
					.isEmpty())) {
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

	private List<AnnotationPojo> generateAnnotationDescriptions(final List<AnnotationPojo> annotations, final EntityId projectId, final Optional<JobMonitor> jobMonitor, final Map<Long, Boolean> cachedModulePermission) {
		final AnnotationOverwriteValidator validator = new AnnotationOverwriteValidator();
		final List<AnnotationPojo> updatedAnnotations = new ArrayList<>();
		for (final AnnotationPojo annotation : annotations) {
			if (validator.shouldOverwriteAnnotationName(annotation)) {
				generateDescription(projectId, jobMonitor, updatedAnnotations, annotation, cachedModulePermission);
			} else {
				updatedAnnotations.add(annotation);
			}
		}
		return updatedAnnotations;
	}

	private void generateDescription(final EntityId projectId, final Optional<JobMonitor> jobMonitor, final List<AnnotationPojo> updatedAnnotations, final AnnotationPojo annotation, final Map<Long, Boolean> cachedModulePermission) {
		final EntityId moduleId = annotation.getModule();
		final Boolean cacheValue = cachedModulePermission.get(moduleId.getNid());
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

	private void publishMessage(final Optional<JobMonitor> jobMonitor, final Message.Severity severity, final String message) {
		if (jobMonitor.isPresent()) {
			jobMonitor.get().addMessage(new Message(severity, message));
		}
	}

}
