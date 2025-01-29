/*
 * Copyright (c) Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import innowake.mining.server.genai.GenAiAnnotationContextRetriever;
import innowake.mining.server.genai.ResponseFormat;
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
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.server.MockedBaseTest;
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
class GenerativeAnnotationTranslationServiceInvalidJsonTest extends MockedBaseTest {

	private static final EntityId PROJECT_ID = EntityId.of(1l);
	private static final EntityId MODULE_ID = EntityId.of(1l);
	private static final EntityId ANNOTATION_ID = EntityId.of(1l);
	private static final String AI_GENERATED_DESCRIPTION_INVALID_JSON = "title: some title, highLevelBusiness: some high level business description, detailedSummary: some detailed summary.";
	private static final String AI_GENERATED_DESCRIPTION_INVALID_JSON_PREFIXED = "[AI generated] title: some title, highLevelBusiness: some high level business description, detailedSummary: some detailed summary.";

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

	@Mock
	private List<GenAiAnnotationContextRetriever> contextRetrievers = Collections.emptyList();

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
		when(configProperties.getResponseFormat()).thenReturn(ResponseFormat.JSON);
		when(availabilityService.isGenAIServiceAvailable()).thenReturn(true);
		when(promptService.buildAnnotationPrompt(any(), any(), any(), any())).thenReturn("Dummy Prompt");
		when(configProperties.getDefaultResponseFormatCustomPrompt()).thenReturn(ResponseFormat.JSON);
		when(Assert.assertNotNull(moduleService).findAnyModule(any())).thenReturn(Optional.of(module));
		when(Assert.assertNotNull(moduleService).getModule((EntityId) any())).thenReturn(module);
		final BaseResponseModel baseGenAIResponseModel = new BaseResponseModel(AI_GENERATED_DESCRIPTION_INVALID_JSON, "prompt", null);
		final ResponseEntity<Object> responseEntity = new ResponseEntity<>(baseGenAIResponseModel, HttpStatus.OK);
		when(restTemplate.exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class))).thenReturn(responseEntity);
		when(Assert.assertNotNull(permissionChecker).allowedToBeProcessed(any())).thenReturn(true);
	}

	@AfterEach
	void closeMocks() throws Exception {
		Assert.assertNotNull(mocks).close();
	}

	@Test
	void testInvalidJson() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final String translation = generativeAnnotationTranslationService.translateUsingGenAI(Objects.requireNonNull(annotation), PROJECT_ID, MODULE_ID);
		assertEquals(AI_GENERATED_DESCRIPTION_INVALID_JSON_PREFIXED, translation);
	}
}
