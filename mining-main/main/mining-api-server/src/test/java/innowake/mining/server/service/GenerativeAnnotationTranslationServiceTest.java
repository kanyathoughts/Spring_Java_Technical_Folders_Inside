/*
 * Copyright (c) Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.genai.AiGenerationContext;
import innowake.mining.server.genai.GenAiAnnotationContextRetriever;
import innowake.mining.server.genai.ResponseFormat;
import innowake.mining.server.genai.requestresponse.CustomPromptRequest;
import innowake.mining.server.service.prompt.GenAiPromptService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.server.MockedBaseTest;
import innowake.mining.server.error.PermissionException;
import innowake.mining.server.genai.requestresponse.BaseResponseModel;
import innowake.mining.server.permission.GenAIModulePermissionChecker;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.model.Technology;

/**
 * Tests the {@linkplain GenerativeAnnotationTranslationService}.
 */
@Import({ GenerativeAnnotationTranslationService.class })
@WithMockUser
class GenerativeAnnotationTranslationServiceTest extends MockedBaseTest {
	
	private static final EntityId PROJECT_ID = EntityId.of(1l);
	private static final EntityId MODULE_ID = EntityId.of(1l);
	private static final EntityId ANNOTATION_ID = EntityId.of(1l);
	private static final String AI_GENERATED_DESCRIPTION_JSON = "{\n" + " \"title\": \"some title\",\n"
			+ " \"highLevelBusiness\": \"some high level business description\",\n"
			+ " \"detailedSummary\": \"some detailed summary.\""
			+ "}";
	private static final String AI_GENERATED_DESCRIPTION_PARSED = "[AI generated] some title\n\nsome high level business description\n\nsome detailed summary.";

	@Nullable
	@Mock
	private RestTemplate restTemplate;

	@Nullable
	@MockBean
	private GenAIAvailabilityService availabilityService;

	@Nullable
	@MockBean
	private GenericConfigProperties configProperties;

	@Nullable
	@MockBean
	private GenAIModulePermissionChecker permissionChecker;

	@Nullable
	@MockBean
	private GenAiPromptService promptService;

	@Nullable
	@MockBean
	private GenAiAnnotationContextRetriever contextRetriever;

	/* The contextRetriever MockBean will be injected into this list. */
	@Mock
	private List<GenAiAnnotationContextRetriever> contextRetrievers = new ArrayList<>();

	@InjectMocks
	@Autowired
	private GenerativeAnnotationTranslationService generativeAnnotationTranslationService;

	@Nullable
	private ModulePojo module;
	@Nullable
	private AnnotationPojo annotation;
	
	@Nullable
	private AutoCloseable mocks;
	
	@BeforeEach
	void prepareTestDataAndMocks() {
		mocks = MockitoAnnotations.openMocks(this);
		
		module = ModulePojoDummy.build(new ModulePojoPrototype()
											.withId(MODULE_ID)
											.setProject(PROJECT_ID)
											.setName("Some module")
											.setContent("some content")
											.setTechnology(Technology.COBOL));
		
		annotation = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.withId(ANNOTATION_ID)
				.setSourceAttachment(new BinaryString("some content"))
				.setModule(MODULE_ID));

		var configProperties = Assert.assertNotNull(this.configProperties);
		var restTemplate = Assert.assertNotNull(this.restTemplate);
		var availabilityService = Assert.assertNotNull(this.availabilityService);

		when(configProperties.getResolvedGenAiURL()).thenReturn("http://localhost");
		when(configProperties.getGenAiPlugin()).thenReturn("echo");
		when(configProperties.getGenAiURL()).thenReturn("http://localhost");
		when(configProperties.getDefaultResponseFormatCustomPrompt()).thenReturn(ResponseFormat.JSON);
		when(configProperties.getResponseFormat()).thenReturn(ResponseFormat.JSON);

		assert promptService != null;
		when(promptService.buildAnnotationPrompt(any(), any(), any(), any())).thenReturn("Dummy Prompt");
		when(availabilityService.isGenAIServiceAvailable()).thenReturn(true);
		when(Assert.assertNotNull(moduleService).findAnyModule(any())).thenReturn(Optional.of(module));
		when(Assert.assertNotNull(moduleService).getModule((EntityId) any())).thenReturn(module);
		final Map<String, String> responseMetadata = new HashMap<>();
		responseMetadata.put("token_count", "1");
		final BaseResponseModel baseGenAIResponseModel = new BaseResponseModel(AI_GENERATED_DESCRIPTION_JSON, "prompt", responseMetadata);
		final ResponseEntity<Object> responseEntity = new ResponseEntity<>(baseGenAIResponseModel, HttpStatus.OK);
		when(restTemplate.exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class))).thenReturn(responseEntity);
		when(Assert.assertNotNull(permissionChecker).allowedToBeProcessed(any())).thenReturn(true);
		when(Assert.assertNotNull(contextRetriever).retrieve(any(), any())).thenReturn(new AiGenerationContext());
	}
	
	@AfterEach
	void closeMocks() throws Exception {
		Assert.assertNotNull(mocks).close();
	}
	
	@Test
	void testServiceUnavailable() {
		Mockito.doReturn(false).when(availabilityService).isGenAIServiceAvailable();
		assertThrows(UserFacingException.class, () -> generativeAnnotationTranslationService.translateUsingGenAI(Objects.requireNonNull(annotation), PROJECT_ID, MODULE_ID));
	}

	@Test
	void testGenerateAnnotationDescription() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final String translation = generativeAnnotationTranslationService.translateUsingGenAI(Objects.requireNonNull(annotation), PROJECT_ID, MODULE_ID);
		assertEquals(AI_GENERATED_DESCRIPTION_PARSED, translation);
		verify(this.restTemplate, times(1)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
	}

	@Test
	void testNoPermission() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		when(Assert.assertNotNull(permissionChecker).allowedToBeProcessed(any())).thenReturn(false);
		assertThrows(PermissionException.class, () -> generativeAnnotationTranslationService.translateUsingGenAI(Objects.requireNonNull(annotation), PROJECT_ID, MODULE_ID));
		verify(this.restTemplate, times(0)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
	}

	/**
	 * Ensure that exceptions thrown by context retrievers are not breaking the Annotation description generation
	 */
	@Test
	void exceptionHandlingInContextRetrievers() {
		// Create a list of context retrievers that throw a runtime exception
		final List<GenAiAnnotationContextRetriever> exceptionThrowingRetrievers = List.of((context, uuid) -> {
			throw new RuntimeException("runtime exception");
		});

		// Mock the availability service to return true
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();

		// Create an instance of the service under test (SUT) with the exception-throwing retrievers
		final GenerativeAnnotationTranslationService SUT = new GenerativeAnnotationTranslationService(
				configProperties,
				moduleService,
				availabilityService,
				permissionChecker,
				exceptionThrowingRetrievers, /* relevant part of the setup */
				promptService);

		// Set the RestTemplate mock so that the call to the GenAI backend does not fail
		SUT.setRestTemplate(restTemplate);

		// Call the method under test
		SUT.translateUsingGenAI(Objects.requireNonNull(annotation), PROJECT_ID, MODULE_ID);

		// Ensure that the RestTemplate was called even though the context retriever threw an exception
		verify(this.restTemplate, times(1)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
	}

	/**
	 * When generating an annotation description we make a call to the knowledge service and another call to the GenAI service.
	 * We want to make it possible to associate the tokens used for both of these calls to the generation of one description, so we add a UUID to these requests.
	 * This test case checks that the same UUID gets added to these two requests.
	 */
	@Test
	void sameUuidUsedForBothServiceCalls() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();

		final AtomicReference<UUID> uuidKnowledgeService = new AtomicReference<>();
		final AtomicReference<UUID> uuidGenAIService = new AtomicReference<>();

		when(contextRetriever.retrieve(any(), any())).thenAnswer(invocation -> {
			uuidKnowledgeService.set(invocation.getArgument(1));
			return new AiGenerationContext();
		});

		when(restTemplate.exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class)))
				.thenAnswer(invocation -> {
					final HttpEntity<CustomPromptRequest> entity = invocation.getArgument(2);
					final CustomPromptRequest request = entity.getBody();

					assert request != null;
					uuidGenAIService.set(UUID.fromString(request.getRequestMetadata().get("uuid")));
					return new ResponseEntity<>(new BaseResponseModel(AI_GENERATED_DESCRIPTION_JSON, "prompt", new HashMap<>()), HttpStatus.OK);
				});

		generativeAnnotationTranslationService.translateUsingGenAI(Objects.requireNonNull(annotation), PROJECT_ID, MODULE_ID);

		assertEquals(uuidKnowledgeService.get(), uuidGenAIService.get());
	}
}
