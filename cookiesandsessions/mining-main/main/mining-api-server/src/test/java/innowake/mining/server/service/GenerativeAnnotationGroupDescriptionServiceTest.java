package innowake.mining.server.service;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.server.service.CentralCommunicationWithGenAIService.AI_GENERATED_PREFIX;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.genai.ResponseFormat;
import innowake.mining.server.service.prompt.GenAiPromptService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
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
import innowake.mining.data.core.SchemaConstants;
import innowake.mining.server.MockedBaseTest;
import innowake.mining.server.error.PermissionException;
import innowake.mining.server.genai.requestresponse.BaseResponseModel;
import innowake.mining.server.permission.GenAIModulePermissionChecker;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Tests the {@linkplain GenerativeAnnotationGroupDescriptionService}.
 */
@Import({ GenerativeAnnotationGroupDescriptionService.class })
@WithMockUser
class GenerativeAnnotationGroupDescriptionServiceTest extends MockedBaseTest {

	private static final EntityId PROJECT_ID = EntityId.of(1L);
	private static final EntityId MODULE_1_ID = EntityId.of(1L);
	private static final EntityId ANNOTATION_1_ID = EntityId.of(1L);
	private static final EntityId ANNOTATION_2_ID = EntityId.of(2L);
	private static final String AI_GENERATED_DESCRIPTION = "AI generated description";

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
	private GenerativeAnnotationTranslationService generativeAnnotationTranslationService;

	@Nullable
	@MockBean
	private GenAiPromptService promptService;

	@InjectMocks
	@Autowired
	private GenerativeAnnotationGroupDescriptionService generativeAnnotationGroupDescriptionService;

	@Nullable
	@MockBean
	private GenAIModulePermissionChecker permissionChecker;

	private final ModulePojo module1 = ModulePojoDummy.build(new ModulePojoPrototype()
			.setName("module 1")
			.setId(MODULE_1_ID.getNid().toString())
			.setContent("content 1")
			.setProject(PROJECT_ID));
	private final AnnotationPojo annotation1 = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
			.setId(ANNOTATION_1_ID.getNid().toString())
			.setSourceAttachment("content 1")
			.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
			.setModule(MODULE_1_ID)
			.setName("description of annotation 1"));
	private final AnnotationPojo annotation2 = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
			.setId(ANNOTATION_2_ID.getNid().toString())
			.setSourceAttachment("content 2")
			.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
			.setModule(MODULE_1_ID)
			.setName("description of annotation 2"));
	@Nullable
	private AutoCloseable mocks;

	@BeforeEach
	void prepareTestDataAndMocks() {
		super.setup();
		mocks = MockitoAnnotations.openMocks(this);

		var configProperties = assertNotNull(this.configProperties);
		var restTemplate = assertNotNull(this.restTemplate);
		var availabilityService = assertNotNull(this.availabilityService);
		var moduleService = assertNotNull(this.moduleService);
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		when(configProperties.getResolvedGenAiURL()).thenReturn("http://localhost");
		when(configProperties.getGenAiPlugin()).thenReturn("echo");
		when(configProperties.getGenAiURL()).thenReturn("http://localhost");
		when(configProperties.getDefaultResponseFormatCustomPrompt()).thenReturn(ResponseFormat.JSON);
		when(configProperties.getResponseFormat()).thenReturn(ResponseFormat.JSON);
		when(availabilityService.isGenAIServiceAvailable()).thenReturn(true);
		when(promptService.buildAnnotationGroupPrompt(any())).thenReturn("Dummy Prompt");

		when(moduleService.findAnyModule(any())).then(i -> {
			final BuildingConsumer<ModuleService.ModuleInquiryBuilder> builderConsumer = i.getArgument(0);
			final ModuleService.ModuleInquiryBuilder builder = Mockito.mock(ModuleService.ModuleInquiryBuilder.class);
			final ArgumentCaptor<EntityId> idCaptor = ArgumentCaptor.forClass(EntityId.class);

			/* Avoid NPEs when chaining is used when calling filter methods */
			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.byId(any())).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder).byId(idCaptor.capture());

			switch (idCaptor.getValue().getNid().intValue()) {
				case 1:
					return Optional.of(module1);
				default:
					throw new IllegalStateException(String.valueOf(idCaptor));
			}
		});
		final Map<String, String>  responseMetadata = new HashMap<>();
		responseMetadata.put("token_count", "1");

		final BaseResponseModel baseGenAIResponseModel = new BaseResponseModel(AI_GENERATED_DESCRIPTION, "prompt", responseMetadata);
		final ResponseEntity<Object> responseEntity = new ResponseEntity<>(baseGenAIResponseModel, HttpStatus.OK);
		when(restTemplate.exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class))).thenReturn(responseEntity);
	}

	@AfterEach
	void closeMocks() throws Exception {
		assertNotNull(mocks).close();
	}

	@Test
	void testAutoGenerateTrueAndGenerateTranslationForBothAnnotations() {
		var annotationService = Assert.assertNotNull(this.annotationService);
		var generativeAnnotationTranslationService = Assert.assertNotNull(this.generativeAnnotationTranslationService);
		when(annotationService.find(any())).thenReturn(List.of(annotation1, annotation2));
		when(generativeAnnotationTranslationService.translateUsingGenAI(any(AnnotationPojo.class), any(), any(), anyBoolean())).thenReturn("description1");
		when(annotationService.update(any())).then(i -> {
			AnnotationPojoPrototype proto = i.getArgument(0);
			assertEquals(SchemaConstants.SYSTEM_USER, proto.updatedByUserId.get());
			return null;
		});
		when(annotationService.get((EntityId)any())).then(i -> {
			EntityId eid = i.getArgument(0);
			switch (eid.getNid().intValue()) {
				case 1:
					return annotation1;
				case 2:
					return annotation2;
				default:
					throw new IllegalStateException(String.valueOf(eid));
			}
		});
		when(Assert.assertNotNull(permissionChecker).allowedToBeProcessed(any())).thenReturn(true);
		when(promptService.buildAnnotationPrompt(any(), any(), any(), any())).thenReturn("prompt");
		final String result = generativeAnnotationGroupDescriptionService.generateAnnotationGroupDescription(List.of(ANNOTATION_1_ID, ANNOTATION_2_ID), PROJECT_ID,
				true);
		verify(this.generativeAnnotationTranslationService, times(2)).translateUsingGenAI(any(AnnotationPojo.class), any(), any(), anyBoolean());
		verify(this.restTemplate, times(1)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
		assertEquals(AI_GENERATED_PREFIX + AI_GENERATED_DESCRIPTION, result);
	}

	@Test
	void testAutoGenerateTrueButNotGenerated() {
		var annotationService = Assert.assertNotNull(this.annotationService);
		var generativeAnnotationTranslationService = Assert.assertNotNull(this.generativeAnnotationTranslationService);

		var annotation1b = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setId(ANNOTATION_1_ID.getNid().toString())
				.setSourceAttachment("content 1")
				.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
				.setUpdatedByUserId(SchemaConstants.SYSTEM_USER)
				.setModule(MODULE_1_ID)
				.setName("description of annotation 1")
		);
		when(Assert.assertNotNull(permissionChecker).allowedToBeProcessed(any())).thenReturn(true);
		when(annotationService.find(any())).thenReturn(List.of(annotation1b));
		when(generativeAnnotationTranslationService.translateUsingGenAI(any(AnnotationPojo.class), any(), any())).thenReturn("description1");
		final String result = generativeAnnotationGroupDescriptionService.generateAnnotationGroupDescription(List.of(ANNOTATION_1_ID), PROJECT_ID,
				true);
		verify(this.restTemplate, times(1)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
		assertEquals(AI_GENERATED_PREFIX + AI_GENERATED_DESCRIPTION, result);
	}

	@Test
	void testAutoGenerateFalse() {
		var annotationService = Assert.assertNotNull(this.annotationService);
		var generativeAnnotationTranslationService = Assert.assertNotNull(this.generativeAnnotationTranslationService);
		var annotation1b = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setId(ANNOTATION_1_ID.getNid().toString())
				.setSourceAttachment("content 1")
				.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
				.setUpdatedByUserId(SchemaConstants.SYSTEM_USER)
				.setModule(MODULE_1_ID)
				.setName("description of annotation 1")
		);
		var annotation2b = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setId(ANNOTATION_1_ID.getNid().toString())
				.setSourceAttachment("content 2")
				.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
				.setUpdatedByUserId(SchemaConstants.SYSTEM_USER)
				.setModule(MODULE_1_ID)
				.setName("description of annotation 2")
		);
		when(annotationService.find(any())).thenReturn(List.of(annotation1b, annotation2b));
		when(generativeAnnotationTranslationService.translateUsingGenAI(any(AnnotationPojo.class), any(), any())).thenReturn("description1")
				.thenReturn("description2");
		final String result = generativeAnnotationGroupDescriptionService.generateAnnotationGroupDescription(List.of(ANNOTATION_1_ID, ANNOTATION_2_ID), PROJECT_ID,
				false);
		verify(this.restTemplate, times(1)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
		assertEquals(AI_GENERATED_PREFIX +AI_GENERATED_DESCRIPTION, result);
	}

	@Test
	void testAutoGenerateFalseWhenPermissionCheckIsFailed() {
		var annotationService = Assert.assertNotNull(this.annotationService);
		when(annotationService.find(any())).thenReturn(List.of(annotation1));
		when(annotationService.update(any())).thenReturn(annotation1.identity());
		when(Assert.assertNotNull(permissionChecker).allowedToBeProcessed(any())).thenReturn(true);
		final List<EntityId> annotationIds = List.of(ANNOTATION_1_ID);
		assertThrows(UserFacingException.class,
				() -> generativeAnnotationGroupDescriptionService.generateAnnotationGroupDescription(annotationIds, PROJECT_ID, false),
				"Since none of the annotations has a description, a group description cannot be generated. Please generate descriptions for the annotations first.");
	}

	@Test
	void testAutoGenerateTrueWhenPermissionCheckIsFailedAutoGenerate() {
		when(Assert.assertNotNull(permissionChecker).allowedToBeProcessed(any())).thenReturn(false);
		var annotationService = Assert.assertNotNull(this.annotationService);
		when(generativeAnnotationTranslationService.translateUsingGenAI(any(AnnotationPojo.class), any(), any(), anyBoolean())).thenReturn("description1");
		var generativeAnnotationTranslationService = Assert.assertNotNull(this.generativeAnnotationTranslationService);
		when(annotationService.find(any())).thenReturn(List.of(annotation1));
		when(generativeAnnotationTranslationService.translateUsingGenAI(annotation1, PROJECT_ID, MODULE_1_ID, false)).thenThrow(
				new PermissionException(module1.getId(), module1.getName(), ""));
		final List<EntityId> annotationIds = List.of(ANNOTATION_1_ID);
		assertThrows(UserFacingException.class,
				() -> generativeAnnotationGroupDescriptionService.generateAnnotationGroupDescription(annotationIds, PROJECT_ID, true),
				"Describing annotations failed due to missing permissions or missing source attachment. Please check your source modules.");
	}

}
