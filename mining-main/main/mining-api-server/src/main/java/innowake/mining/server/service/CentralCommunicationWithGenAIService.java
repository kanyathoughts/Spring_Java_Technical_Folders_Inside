package innowake.mining.server.service;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import innowake.lib.core.lang.Nullable;
import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.service.prompt.GenAiPromptService;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.error.PermissionException;
import innowake.mining.server.genai.requestresponse.GenAiRequest;
import innowake.mining.server.permission.GenAIModulePermissionChecker;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Central Service for communication with Gen AI.
 */
@Service
public class CentralCommunicationWithGenAIService{

	private static final Logger LOG = LoggerFactory.getLogger(CentralCommunicationWithGenAIService.class);
	public static final int RATE_LIMIT = 50000;
	public static final String AI_GENERATED_PREFIX = "[AI generated] ";
	private static final String X_API_KEY = "x-api-key";
	private final GenericConfigProperties configProperties;
	private final ModuleService moduleService;
	private final GenAIAvailabilityService availabilityService;
	private final GenAIModulePermissionChecker permissionChecker;
	protected final GenAiPromptService promptService;
	protected static final ConcurrentMap<Long, Integer> timestampToTokenCountMap = new ConcurrentHashMap<>();
	@Nullable
	private RestTemplate sharedRestTemplate = null;

	public GenericConfigProperties getConfigProperties() {
		return configProperties;
	}

	public ModuleService getModuleService() {
		return moduleService;
	}

	public GenAIAvailabilityService getAvailabilityService() {
		return availabilityService;
	}

	public void setRestTemplate(final RestTemplate restTemplate) {
		this.sharedRestTemplate = restTemplate;
	}

	public CentralCommunicationWithGenAIService(final GenericConfigProperties configProperties, final ModuleService moduleService,
			final GenAIAvailabilityService availabilityService, final GenAIModulePermissionChecker permissionChecker, final GenAiPromptService promptService) {
		this.configProperties = configProperties;
		this.moduleService = moduleService;
		this.availabilityService = availabilityService;
		this.permissionChecker = permissionChecker;
		this.promptService = promptService;
	}

	private List<HttpMessageConverter<?>> getJsonMessageConverters() {
		final List<HttpMessageConverter<?>> converters = new ArrayList<>();
		converters.add(new MappingJackson2HttpMessageConverter());
		return converters;
	}

	public  <T extends GenAiRequest, R> R  callGenAi(final T request, final Class<R> responseType) {
		RestTemplate restTemplate = this.sharedRestTemplate;
		if (restTemplate == null) { restTemplate = new RestTemplate(); }
		return callGenAi(request, responseType, restTemplate);
	}

	public  <T extends GenAiRequest, R> R  callGenAi(final T request, final Class<R> responseType, final RestTemplate restTemplate) {
		LOG.debug("Request Payload : {}", request);
		if (StringUtils.isBlank(configProperties.getGenAiPlugin())) {
			throw new UserFacingException("GenAI plugin is not set in the server configuration. Please contact the administrator.");
		}
		if (!availabilityService.isGenAIServiceAvailable()) {
			LOG.error("Could not reach the designated GenAI service with url: {}. Please try again later.", configProperties.getResolvedGenAiURL());
			throw new UserFacingException("Could not reach the designated GenAI service. Please try again later.");
		}
		final String url = configProperties.getGenAiURL();
		if (url == null || StringUtils.isBlank(url)) {
			throw new UserFacingException("GenAI URL is not set in the server configuration. Please contact the administrator.");
		}

		final var headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		if (StringUtils.isNotBlank(configProperties.getGenAiApiKey())) {
			headers.set(X_API_KEY, configProperties.getGenAiApiKey());
		}

		final String resolvedGenAiURL = combineUrlAndPath(url, request.getUrlPath(configProperties));

		try {
			waitForOpenTokenWindow();
			return getGenAiResponse(request, responseType, headers, resolvedGenAiURL, restTemplate);
		} catch (final Exception ex) {
			final String rootCauseMessage = ExceptionUtils.getRootCauseMessage(ex);
			LOG.error(() -> "Error while calling GenAI: " + rootCauseMessage, ex);
			throw ex;
		}
	 }

	private <T extends GenAiRequest, R> R getGenAiResponse(final T request, final Class<R> responseType, final HttpHeaders headers,
			final String resolvedGenAiURL, final RestTemplate restTemplate) {
		restTemplate.setMessageConverters(getJsonMessageConverters());
		final ResponseEntity<R> exchange;
		try {
			exchange = restTemplate.exchange(resolvedGenAiURL, HttpMethod.POST, new HttpEntity<>(request, headers), responseType);
		} catch (final HttpClientErrorException.Unauthorized e) {
			LOG.error("Unauthorized access to GenAI. Please check the API key in the server configuration.", e);
			throw new UserFacingException("Unauthorized access to GenAI. Please check the API key in the server configuration.");
		}
		final R responseBody = Optional.ofNullable(exchange.getBody()).orElseThrow(() -> new IllegalStateException("The response body is null"));
		if (!exchange.getStatusCode().is2xxSuccessful()) {
			LOG.error("Response from GenAI - Status: {} ({}), Response JSON: {}", exchange.getStatusCode(), exchange.getStatusCodeValue(), responseBody);
		} else {
			LOG.debug("Response from GenAI - Status: {} ({}), Response JSON: {}", exchange.getStatusCode(), exchange.getStatusCodeValue(), responseBody);
		}
		return responseBody;
	}

	private String combineUrlAndPath(final String url, final String path) {
		String resolvedUrl = url.trim();
		if ( ! resolvedUrl.endsWith("/")) {
			resolvedUrl += "/";
		}
		resolvedUrl += path;
		return resolvedUrl;
	}

	public void checkPermission(final EntityId projectId, final EntityId moduleId) {
		final var module = moduleService.findAnyModule(q -> q.ofProject(projectId).byId(moduleId).includeContent(true));
		if (module.isEmpty()) {
			throw new IllegalArgumentException("Could not check permission, due to missing module with project ID " + projectId + " and module ID " + moduleId);
		}
		checkPermission(module.get());
	}

	public void checkPermission(final ModulePojo module) {
		if ( ! permissionChecker.allowedToBeProcessed(module)) {
			final var e = new PermissionException(module.getId(), module.getName(), permissionChecker.getReason(module));
			LOG.warn(e.getMessage());
			throw e;
		}
	}

	public String checkSourceCode(final AnnotationPojo annotation) {
		return annotation.getSourceAttachment()
				.map(Object::toString)
				.filter(src -> !src.isBlank())
				.orElseThrow(() -> new IllegalArgumentException("Source attachment is missing from Annotation."));
	}

	public void checkModuleContent(final ModulePojo module) {
		if (StringUtils.isBlank(module.getContent().orElse(null))) {
			throw new UserFacingException("The following module does not contain any content: " + module.getName());
		}
	}

	private void waitForOpenTokenWindow() {
		boolean isWaiting;

		do {
			int rateLimit;
			final long oneMinuteBefore = System.currentTimeMillis() - 60000;
			long tokenCount = 0;

			// remove all timestamps that are older than one minute
			tokenCount = getTokenCount(oneMinuteBefore, tokenCount);
			// check the rate limit
			rateLimit = getConfigProperties().getMiningGenAiTokenRateLimit().orElse(RATE_LIMIT);
			isWaiting = tokenCount > rateLimit;

			// if waiting, sleep for a short time to avoid busy-waiting
			if (isWaiting) {
				try {
					Thread.sleep(1000);  // sleep for 1 second
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();  // restore the interrupt status
					break;
				}
			}
		} while (isWaiting);
	}

	private synchronized long getTokenCount(final long oneMinuteBefore, long tokenCount) {
		//This synchronized method avoid getting ConcurrentModificationException
		final Iterator<Map.Entry<Long, Integer>> iterator = timestampToTokenCountMap.entrySet().iterator();
		while (iterator.hasNext()) {
			final Map.Entry<Long, Integer> entry = iterator.next();
			if (entry.getKey() < oneMinuteBefore) {
				iterator.remove();
			} else {
				tokenCount += entry.getValue();
			}
		}
		return tokenCount;
	}

	public String markContent(final String content) {
		return AI_GENERATED_PREFIX + content;
	}
}
