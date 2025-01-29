/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.service;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.genai.GenAiAnnotationContextRetriever;
import innowake.mining.server.genai.AiGenerationContext;
import innowake.mining.server.genai.AnnotationContext;
import innowake.mining.server.genai.RequestMetadataUtil;
import innowake.mining.server.genai.requestresponse.CustomPromptRequest;
import innowake.mining.server.genai.requestresponse.GenAiRequestOptions;
import innowake.mining.server.service.prompt.GenAiPromptService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.Technology;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Service;

import innowake.lib.core.lang.Assert;
import innowake.mining.server.genai.requestresponse.BaseResponseModel;
import innowake.mining.server.permission.GenAIModulePermissionChecker;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;

/**
 * Generative AI translation for Annotation Source Code related methods.
 */
@Service
public class GenerativeAnnotationTranslationService extends CentralCommunicationWithGenAIService {

	private static final Logger LOG = LoggerFactory.getLogger(GenerativeAnnotationTranslationService.class);

	private final List<GenAiAnnotationContextRetriever> contextRetrievers;

	public GenerativeAnnotationTranslationService(final GenericConfigProperties configProperties, final ModuleService moduleService, final GenAIAvailabilityService availabilityService,
			final GenAIModulePermissionChecker permissionChecker, final List<GenAiAnnotationContextRetriever> contextRetrievers, final GenAiPromptService promptService) {
		super(configProperties, moduleService, availabilityService, permissionChecker, promptService);
		this.contextRetrievers = contextRetrievers;
		LOG.info(() -> "Context retrievers registered:");
		contextRetrievers.forEach(contextRetriever -> LOG.info(() -> contextRetriever.getClass().getName()));
	}

	/**
	 * Accepts a Source code and tries to provide a translation of the Source Code using Generative AI.
	 *
	 * @param annotation annotation we want to explain
	 * @param projectId ID of project the Annotation belongs to
	 * @param moduleId ID of module the annotation belongs to
	 * @return the translation of the source code.
	 */
	public String translateUsingGenAI(final AnnotationPojo annotation, final EntityId projectId, final EntityId moduleId) {
		checkPermission(projectId, moduleId);
		return translateAnnotationUsingGenAI(annotation);
	}

	/**
	 * Accepts a Source code and tries to provide a translation of the Source Code using Generative AI.
	 *
	 * @param annotation- annotation we want to explain
	 * @param projectId- ID of project the Annotation belongs to
	 * @param moduleId- ID of module the annotation belongs to
	 * @param skipPermissionCheck - cachedModulePermission
	 * @return the translation of the source code.
	 */
	public String translateUsingGenAI(final AnnotationPojo annotation, final EntityId projectId, final EntityId moduleId, boolean skipPermissionCheck) {
		if ( ! skipPermissionCheck) {
			checkPermission(projectId, moduleId);
		}
		return translateAnnotationUsingGenAI(annotation);
	}

	/**
	 * Accepts a Source code and tries to provide a translation of the Source Code using Generative AI.
	 *
	 * @param annotation annotation we want to explain
	 * @return the translation of the source code.
	 */
	public String translateAnnotationUsingGenAI(final AnnotationPojo annotation) {
		final UUID requestUUID = UUID.randomUUID();
		final ModulePojo module = getModuleService().getModule(annotation.getModule());
		checkPermission(module.getProject(), module.identity());
		final String sourceCode = checkSourceCode(annotation);
		final String context = retrieveContext(annotation.convertToPrototype(), requestUUID);
		return translateUsingGenAiCustomPrompt(sourceCode, context, module.getTechnology(), requestUUID);
	}

	/**
	 * Accepts a Source code and tries to provide a translation of the Source Code using Generative AI.
	 *
	 * @param annotation annotation we want to explain
	 * @param projectId  ID of project the Annotation belongs to
	 * @param moduleId   ID of module the annotation belongs to
	 * @param options   options for the GenAI request
	 * @return the translation of the source code.
	 */
	public String translateUsingGenAI(final AnnotationPojoPrototype annotation, final EntityId projectId, final EntityId moduleId, final GenAiRequestOptions options) {
		final UUID requestUUID = UUID.randomUUID();
		checkPermission(projectId, moduleId);
		final String sourceCode = annotation.sourceAttachment.orElseNonNull(BinaryString.EMPTY).toString();
		if ( ! sourceCode.isBlank()) {
			final String context = retrieveContext(annotation, requestUUID);
			return translateUsingGenAiCustomPrompt(sourceCode, context + "\n" + options.getContext(),
					getModuleService().getModule(moduleId).getTechnology(), requestUUID);
		} else {
			throw new UserFacingException("Source attachment is missing from Annotation.");
		}
	}

	private String retrieveContext(final AnnotationPojoPrototype annotation, final UUID uuid) {
		final AnnotationContext ctx = new AnnotationContext(annotation, AiGenerationContext.EMPTY);
		for (final GenAiAnnotationContextRetriever contextRetriever : contextRetrievers) {
			LOG.debug(() -> "Retrieving context using: " + contextRetriever.getClass().getName());
			try {
				ctx.setGenerationContext(contextRetriever.retrieve(ctx, uuid));
			} catch (final Exception e) {
				LOG.error(() -> "Error while retrieving context for " + annotation + " using '" + contextRetriever.getClass().getName() + "'. Skipping retriever.", e);
			}
		}
		return ctx.getGenerationContext().getAdditionalPromptContext();
	}

	/**
	 * Accepts a source code, builds a custom prompt and sends it to the custom prompt endpoint for translation.
	 *
	 * @param sourceCode the source code.
	 * @param context the context.
	 * @param technology the technology of the module the annotation refers to
	 * @param requestUUID the uuid associated with the annotation description generation, the knowledge service request will have the same uuid
	 * @return the translation of the source code.
	 */
	private String translateUsingGenAiCustomPrompt(final String sourceCode, final String context, final Technology technology, final UUID requestUUID) {
		final String prompt = promptService.buildAnnotationPrompt(sourceCode, context, 5, 80);
		if ( ! prompt.isBlank()) {
			try {
				final var request = new CustomPromptRequest(getConfigProperties().getGenAiPlugin(), prompt, getConfigProperties().getGenAiMaxNewToken(),
						getConfigProperties().getGenAiTemperature(), getConfigProperties().getGenAiDoSample(), getConfigProperties().getResponseFormat().name(),
						RequestMetadataUtil.getAnnotationMetadata(sourceCode, technology, requestUUID));
				final BaseResponseModel responseObject = callGenAi(request, BaseResponseModel.class);
				final String resultString = Assert.assertNotNull(responseObject).getModelResponse().trim();
				LOG.debug(() -> "Raw response from GenAI: " + resultString);
				final Optional<JsonObject> jsonObject = parseAsJson(resultString);

				return markContent(jsonObject.map(
						object -> object.get("title").getAsString() + "\n\n" + object.get("highLevelBusiness").getAsString() + "\n\n" + object.get(
								"detailedSummary").getAsString()).orElse(resultString)).trim();
			} catch (final Exception e) {
				final String rootCauseMessage = ExceptionUtils.getRootCauseMessage(e);
				LOG.error(() -> "Error while calling Gen AI: " + rootCauseMessage, e);
				throw e;
			}
		} else {
			throw new UserFacingException("Could not load prompt. Please contact the administrator.");
		}
	}

	/**
	 * Tries to parse the passed String as valid JSON Object and checks if it contains the expected keys.
	 *
	 * @param resultString the LLM result to be parsed as JSON
	 * @return Optional<JsonObject> depending on the parse result
	 */
	private Optional<JsonObject> parseAsJson(final String resultString) {
		final Optional<JsonObject> jsonObject;
		try {
			jsonObject = Optional.ofNullable(JsonParser.parseString(resultString).getAsJsonObject());
			if (jsonObject.isPresent()) {
				if (jsonObject.get().get("title") != null && jsonObject.get().get("highLevelBusiness") != null && jsonObject.get().get("detailedSummary") != null) {
					return jsonObject;
				} else {
					LOG.debug(() -> "The LLM result does not contain the expected JSON keys.");
					return Optional.empty();
				}
			} else {
				LOG.debug(() -> "The LLM result could not be parsed as JSON.");
				return Optional.empty();
			}
		} catch (final JsonSyntaxException e) {
			LOG.debug(() -> "An error occurred while parsing the LLM result as JSON.", e);
			return Optional.empty();
		}
	}
}
