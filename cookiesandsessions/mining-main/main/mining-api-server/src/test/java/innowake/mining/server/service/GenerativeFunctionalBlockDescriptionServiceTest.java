package innowake.mining.server.service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import static innowake.lib.core.lang.Assert.assertNotNull;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.data.core.SchemaConstants;
import innowake.mining.server.MockedBaseTest;
import innowake.mining.server.genai.ResponseFormat;
import innowake.mining.server.genai.requestresponse.BaseResponseModel;
import innowake.mining.server.job.genai.MonitoredTaskManager;
import innowake.mining.server.job.genai.MonitoredTaskManagerService;
import innowake.mining.server.job.genai.MonitoredTaskParameter;
import innowake.mining.server.permission.GenAIModulePermissionChecker;
import innowake.mining.server.properties.GenericConfigProperties;
import static innowake.mining.server.service.CentralCommunicationWithGenAIService.AI_GENERATED_PREFIX;
import innowake.mining.server.service.prompt.GenAiPromptService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.lang.BuildingConsumer;
import org.junit.jupiter.api.AfterEach;
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import static org.mockito.ArgumentMatchers.any;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import static org.mockito.Mockito.*;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.web.client.RestTemplate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Tests the {@linkplain GenerativeFunctionalBlocksDescriptionService}.
 */
@Import({ GenerativeFunctionalBlocksDescriptionService.class })
@WithMockUser
class GenerativeFunctionalBlockDescriptionServiceTest extends MockedBaseTest {

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

	@MockBean
	private MonitoredTaskManagerService monitoredTaskManagerService;

	@MockBean
	private MonitoredTaskParameter monitoredTaskParameter;

	@Nullable
	@MockBean
	private GenerativeAnnotationTranslationService generativeAnnotationTranslationService;

	@Nullable
	@MockBean
	JobMonitor progressMonitor;

	@Nullable
	@MockBean
	private GenAiPromptService promptService;

	@InjectMocks
	@Autowired
	private GenerativeFunctionalBlocksDescriptionService generativeFunctionalBlocksDescriptionService;

	@Nullable
	@MockBean
	private GenAIModulePermissionChecker permissionChecker;

	@Nullable
	private AutoCloseable mocks;

	private final ModulePojo module1 = ModulePojoDummy.build(new ModulePojoPrototype()
			.setName("module 1")
			.setId(MODULE_1_ID.getNid().toString())
			.setContent("content 1")
			.setProject(PROJECT_ID));

	@BeforeEach
	void prepareTestDataAndMocks() {
		super.setup();
		mocks = MockitoAnnotations.openMocks(this);
		var configProperties = assertNotNull(this.configProperties);
		var restTemplate = assertNotNull(this.restTemplate);
		var availabilityService = assertNotNull(this.availabilityService);
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		when(configProperties.getResolvedGenAiURL()).thenReturn("http://localhost");
		when(configProperties.getGenAiPlugin()).thenReturn("echo");
		when(configProperties.getGenAiURL()).thenReturn("http://localhost");
		when(configProperties.getDefaultResponseFormatCustomPrompt()).thenReturn(ResponseFormat.JSON);
		when(configProperties.getResponseFormat()).thenReturn(ResponseFormat.JSON);
		when(promptService.buildAnnotationPrompt(any(), any(), any(), any())).thenReturn("prompt");
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
		final ExecutorService executorService =  Executors.newFixedThreadPool(5);
		MonitoredTaskManager subMonitorTask = new MonitoredTaskManager(progressMonitor, executorService, 1);
		Mockito.when(monitoredTaskManagerService.newSubTaskManager(any(), anyInt())).thenReturn(subMonitorTask);
	}

	@AfterEach
	void closeMocks() throws Exception {
		assertNotNull(mocks).close();
	}

	/**
	 * Test if the boolean value of generateDescription is true, generate description for annotations and at the end for functional block
	 */
	@Test
	void testAutoGenerateTrueAndGenerateTranslationForAllAnnotations() {
		var annotation1 = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setId(ANNOTATION_1_ID.getNid().toString())
				.setSourceAttachment("content 1")
				.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
				.setModule(MODULE_1_ID)
				.setName("description of annotation 1"));
		var annotation2 = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setId(ANNOTATION_2_ID.getNid().toString())
				.setSourceAttachment("content 2")
				.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
				.setModule(MODULE_1_ID)
				.setName("description of annotation 2"));
		when(Assert.assertNotNull(permissionChecker).allowedToBeProcessed(any())).thenReturn(true);
		when(generativeAnnotationTranslationService.translateUsingGenAI(any(AnnotationPojo.class), any(), any(), anyBoolean())).thenReturn("description1");
		when(annotationService.get((EntityId) any())).thenReturn(annotation1).thenReturn(annotation2);
		final String result = generativeFunctionalBlocksDescriptionService.generateAnnotationsDescription(monitoredTaskParameter , progressMonitor, List.of(annotation1, annotation2), PROJECT_ID,
				true);
		//Two times for generate annotation descriptions and one time for functional block description
		verify(this.generativeAnnotationTranslationService, times(2)).translateUsingGenAI(any(AnnotationPojo.class), any(), any(), anyBoolean());
		verify(this.restTemplate, times(1)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
		assertEquals(AI_GENERATED_PREFIX + AI_GENERATED_DESCRIPTION, result);
	}
	/**
	 * Test if the boolean value of generateDescription is false, generate description for functional block
	 */
	@Test
	void testAutoGenerateFalseWhichDoesNotGenerateDescriptionForEachAnnotation() {
		var annotation1b = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setId(ANNOTATION_1_ID.getNid().toString())
				.setSourceAttachment("content 1")
				.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
				.setUpdatedByUserId("Customer")
				.setModule(MODULE_1_ID)
				.setName("description of annotation 1")
		);
		var annotation2b = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setId(ANNOTATION_1_ID.getNid().toString())
				.setSourceAttachment("content 2")
				.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
				.setUpdatedByUserId("Client")
				.setModule(MODULE_1_ID)
				.setName("description of annotation 2")
		);

		final String result = generativeFunctionalBlocksDescriptionService.generateAnnotationsDescription(monitoredTaskParameter , progressMonitor, List.of(annotation1b ,annotation2b), PROJECT_ID,
				false);
		//One time for functional block since it does not generate the description for annotations
		verify(this.restTemplate, times(1)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
		assertEquals(AI_GENERATED_PREFIX + AI_GENERATED_DESCRIPTION, result);
	}

}
