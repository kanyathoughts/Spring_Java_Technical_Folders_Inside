/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service.prompt;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.cache.MiningCacheConfig;
import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ModulePojo;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Service for generating prompts. This service attempts to query the prompt service at
 * {@linkplain GenericConfigProperties#getResolvedPromptServiceURL}/{@linkplain GenAiPromptService#ENDPOINT} for a prompt.
 * If the prompt service is not available, locally defined prompts in src/main/resources/genai/prompts will be used.
 * <br><br>
 * The used prompts (no matter if they come from the service or from a local file) get cached to avoid querying the same prompt an unnecessary amount of times.
 * The prompt cache can be configured by changing the configuration.prompt-cache-size and configuration.prompt-cache-duration parameters.
 */
@Service
public class GenAiPromptService {

	private static final Logger LOG = LoggerFactory.getLogger(GenAiPromptService.class);

	private static final String ENDPOINT = "prompts/%s/%s";

	private RestTemplate restTemplate = new RestTemplate();

	private final GenericConfigProperties configProperties;

	private final CacheManager cacheManager;

	public GenAiPromptService(final GenericConfigProperties configProperties, final CacheManager cacheManager) {
		this.configProperties = configProperties;
		this.cacheManager = cacheManager;
	}

	public void setRestTemplate(final RestTemplate restTemplate) {
		this.restTemplate = restTemplate;
	}

	/**
	 * Builds a prompt for generating an annotation group description.
	 *
	 * @param annotations the annotations in the annotation group
	 * @return the prompt
	 */
	public String buildAnnotationGroupPrompt(final List<AnnotationPojo> annotations) {
		final String annotationsList = GenAiPromptUtil.buildAnnotationList(annotations);
		final Map<String, String> placeholdersToData = Map.of("annotation_list", annotationsList);
		return buildPrompt(PromptType.ANNOTATION_GROUP, placeholdersToData);
	}

	/**
	 * Builds a prompt for generating a reachability block description.
	 *
	 * @param modulesWithDescription    the modules in the reachability block which have a description
	 * @param modulesWithoutDescription the modules in the reachability block which do not have a description
	 * @return the prompt
	 */
	public String buildReachabilityBlockPrompt(final List<ModulePojo> modulesWithDescription, final List<ModulePojo> modulesWithoutDescription) {
		final Map<String, String> placeHoldersToData = Map.of(
				"module_with_description_list", GenAiPromptUtil.buildModulesListWithDescription(modulesWithDescription),
				"module_without_description_list", GenAiPromptUtil.buildModulesListWithoutDescription(modulesWithoutDescription));
		return buildPrompt(PromptType.REACHABILITY_BLOCK, placeHoldersToData);
	}

	/**
	 * Builds a prompt for generating an annotation description.
	 *
	 * @param sourceCode    the source code the annotation refers to
	 * @param context       additional context information to add to the prompt
	 * @param titleLength   the maximum length of the title
	 * @param summaryLength the maximum length of the summary
	 * @return the prompt
	 */
	public String buildAnnotationPrompt(final String sourceCode, final String context, final Integer titleLength, final Integer summaryLength) {
		final Map<String, String> placeholdersToData = Map.of(
				"code", sourceCode,
				"context", context,
				"title_length", titleLength.toString(),
				"summary_length", summaryLength.toString());
		return buildPrompt(PromptType.ANNOTATION, placeholdersToData);
	}

	/**
	 * Builds a prompt for classifying a search query into categories.
	 *
	 * @param query      the search query to be classified
	 * @param categories the saved search categories the text should be classified into
	 * @return the prompt
	 */
	public String buildSimilaritySearchPrompt(final String query, final String categories) {
		final Map<String, String> placeholdersToData = Map.of(
				"query", query,
				"categories", categories);
		return buildPrompt(PromptType.SIMILARITY_SEARCH, placeholdersToData);
	}

	private String buildPrompt(final PromptType promptType, final Map<String, String> placeholdersToData) {
		final Optional<Prompt> oCachedPrompt = getPromptFromCache(promptType);
		Prompt prompt;
		if (oCachedPrompt.isPresent()) {
			prompt = oCachedPrompt.get();
			LOG.info("Prompt from cache is being used.");
			return replacePlaceholders(prompt, placeholdersToData);
		}
		final Optional<Prompt> oPrompt = queryPrompt(promptType.getUseCase(), configProperties.getGenAiPlugin());
		if (oPrompt.isPresent()) {
			prompt = oPrompt.get();
			LOG.info("Prompt queried successfully from prompt service.");
		} else {
			try {
				prompt = GenAiPromptUtil.loadPrompt(promptType.getFileName());
				LOG.info("Prompt from local file is being used.");
			} catch (final IOException e) {
				LOG.error("Could not load prompt file: {}", promptType.getFileName());
				throw new UserFacingException("Could not load prompt. Please contact the administrator.");
			}
		}
		putPromptInCache(promptType, prompt);
		return replacePlaceholders(prompt, placeholdersToData);
	}

	private Optional<Prompt> getPromptFromCache(final PromptType promptType) {
		final Cache promptCache = cacheManager.getCache(MiningCacheConfig.PROMPT_CACHE);
		if (promptCache == null) {
			return Optional.empty();
		}
		final Prompt prompt = promptCache.get(promptType, Prompt.class);
		return Optional.ofNullable(prompt);
	}

	private void putPromptInCache(final PromptType promptType, final Prompt prompt) {
		final Cache promptCache = cacheManager.getCache(MiningCacheConfig.PROMPT_CACHE);
		if (promptCache != null) {
			promptCache.put(promptType, prompt);
		}
	}

	private Optional<Prompt> queryPrompt(final String useCase, final String llmType) {
		try {
			final var headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			final HttpEntity<Prompt> request = new HttpEntity<>(headers);
			final ResponseEntity<Prompt> responseEntity = restTemplate.exchange(configProperties.getResolvedPromptServiceURL() + String.format(ENDPOINT,
					useCase, llmType), HttpMethod.GET, request, Prompt.class);
			final Prompt prompt = responseEntity.getBody();
			if (prompt == null || prompt.getTemplate().isBlank()) {
				LOG.warn("Prompt obtained from prompt service is null or blank. Local prompt will be used instead.");
				return Optional.empty();
			}
			return Optional.of(prompt);
		} catch (final Exception e) {
			LOG.error("Error while querying prompt: {}", e.getMessage());
			return Optional.empty();
		}
	}

	private String replacePlaceholders(final Prompt prompt, final Map<String, String> placeholdersToData) {
		String promptText = prompt.getTemplate();
		for (final String placeholder: prompt.getPlaceholders()) {
			final String value = placeholdersToData.get(placeholder);
			if (value != null) {
				promptText = promptText.replace("{" + placeholder + "}", value);
			}
		}
		return promptText;
	}

}
