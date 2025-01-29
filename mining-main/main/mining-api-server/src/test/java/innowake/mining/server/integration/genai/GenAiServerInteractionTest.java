/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.genai;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.metrics.SourceExportService;
import innowake.mining.server.genai.requestresponse.BaseResponseModel;
import innowake.mining.server.genai.requestresponse.CustomPromptRequest;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.service.CentralCommunicationWithGenAIService;
import innowake.mining.server.service.semanticsearch.SemanticSearchResultPojo;
import innowake.mining.server.service.semanticsearch.SemanticSearchService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.server.genai.requestresponse.DeduceRequestModel;
import innowake.mining.server.genai.requestresponse.DeduceResponseModel;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.testcontainers.shaded.org.apache.commons.lang3.StringUtils;

import java.util.Collections;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests the interaction between Mining and a configured GenAI server and validates the request & response objects.
 * <p>
 * By default, tests run against the GenAI server configured in the application.yaml and if empty, system properties are used.
 * <p>
 * These tests can be run separately from other integration tests via dedicated Maven Profile: genai-server-integration-tests-only
 */
// @GenAiServerIntegrationTest
// @SpringBootTest
// @TestInstance(Lifecycle.PER_CLASS)
// @EnabledIf("#{ 'true'.equals( systemProperties[ 'innowake.integration.genai.tests.run' ] ) }")
// @Disabled
class GenAiServerInteractionTest {

	@Autowired
	private CentralCommunicationWithGenAIService centralCommunicationWithGenAIService;

	@Autowired
	private SemanticSearchService semanticSearchService;

	@Autowired
	private GenericConfigProperties genericConfigProperties;

	@Nullable
	@MockBean
	private SourceExportService sourceExportService;

	// @BeforeAll
	void setUp() {
		if (StringUtils.isBlank(genericConfigProperties.getGenAiApiKey())) {
			fail("Ensure the mining.genAI.apiKey property is set");
		}
	}

	// @Test
	void testCustomPromptJson() {
		final String prompt = "Return the first letter of the latin alphabet. Your response should only contain one letter, nothing else.";
		final CustomPromptRequest request = getCustomPromptRequest("JSON", prompt);

		final BaseResponseModel responseObject = centralCommunicationWithGenAIService.callGenAi(request, BaseResponseModel.class);

		if (responseObject == null) {
			fail("Response object is null.");
		}

		assertEquals(prompt, responseObject.getPrompt(), "Initial prompt is not as expected.");
		assertFalse(responseObject.getModelResponse().isBlank(), "Model response is blank");
		assertNotNull(responseObject.getResponseMetadata(), "Response metadata is null");
		Optional.ofNullable(responseObject.getResponseMetadata())
				.map(metadata -> metadata.get("token_count"))
				.map(Integer::parseInt)
				.ifPresentOrElse(tokenCount -> {
					assertTrue(tokenCount > 0, "Token count is not > 0");
				}, () -> {
					fail("Token count is missing in response");
				});
	}

	// @Test
	void testCustomPromptText() {
		final String prompt = "Return the first letter of the latin alphabet. Your response should only contain one letter, nothing else.";
		final CustomPromptRequest request = getCustomPromptRequest("TEXT", prompt);

		final BaseResponseModel responseObject = centralCommunicationWithGenAIService.callGenAi(request, BaseResponseModel.class);

		if (responseObject == null) {
			fail("Response object is null.");
		}

		assertEquals(prompt, responseObject.getPrompt(), "Initial prompt is not as expected.");
		assertFalse(responseObject.getModelResponse().isBlank(), "Model response is blank");
		assertNotNull(responseObject.getResponseMetadata(), "Response metadata is null");
		Optional.ofNullable(responseObject.getResponseMetadata())
				.map(metadata -> metadata.get("token_count"))
				.map(Integer::parseInt)
				.ifPresentOrElse(tokenCount -> {
					assertTrue(tokenCount > 0, "Token count is not > 0");
				}, () -> {
					fail("Token count is missing in response");
				});
	}

	// @Test
	void testSemanticSearch() {
		final String query = "Where is the interest calculated?";
		final Long projectId = 1L;
		final SemanticSearchResultPojo resultPojo = semanticSearchService.doSemanticSearch(EntityId.of(projectId), query);

		if (resultPojo == null) {
			fail("Result Pojo is null.");
		}

		assertFalse(resultPojo.getAnswer().isBlank(), "Answer of result pojo is blank");
	}

	// @Test
	void testDeduce() {
		final DeduceRequestModel request = getDeduceRequest();

		final DeduceResponseModel responseObject = centralCommunicationWithGenAIService.callGenAi(request, DeduceResponseModel.class);

		if (responseObject == null) {
			fail("Response object is null.");
		}

		assertFalse(responseObject.getPurpose().isBlank(), "Model response is blank");
		assertNotNull(responseObject.getResponseMetadata(), "Response metadata is null");
		Optional.ofNullable(responseObject.getResponseMetadata())
				.map(metadata -> metadata.get("token_count"))
				.map(Integer::parseInt)
				.ifPresentOrElse(tokenCount -> {
					assertTrue(tokenCount > 0, "Token count is not > 0");
				}, () -> {
					fail("Token count is missing in response");
				});
	}

	/**
	 * Returns a {@link CustomPromptRequest} object for testing the GenAI communication.
	 *
	 * @param responseFormat
	 * 		the response format to be used (either "JSON" or "TEXT")
	 * @return a {@link CustomPromptRequest} object for testing the GenAI communication.
	 */
	private CustomPromptRequest getCustomPromptRequest(final String responseFormat, final String prompt) {
		return new CustomPromptRequest(
				genericConfigProperties.getGenAiPlugin(), 
				prompt, 
				genericConfigProperties.getGenAiMaxNewToken(), 
				genericConfigProperties.getGenAiTemperature(), 
				genericConfigProperties.getGenAiDoSample(), 
				responseFormat, Collections.emptyMap());

	}

	/**
	 * Returns a {@link DeduceRequestModel} object for testing the GenAI communication.
	 *
	 * @return a {@link DeduceRequestModel} object for testing the GenAI communication.
	 */
	private DeduceRequestModel getDeduceRequest() {
		return new DeduceRequestModel(
				genericConfigProperties.getGenAiPlugin(),
				"System.out.println(\"Hello, World!\");",
				genericConfigProperties.getGenAiMaxNewToken(),
				genericConfigProperties.getGenAiTemperature(),
				genericConfigProperties.getGenAiDoSample(),
				"Java", Collections.emptyMap());
	}

}
