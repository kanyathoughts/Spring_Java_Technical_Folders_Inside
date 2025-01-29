/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static innowake.mining.server.service.CentralCommunicationWithGenAIService.AI_GENERATED_PREFIX;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.service.prompt.GenAiPromptService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import static org.mockito.Mockito.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.genai.requestresponse.BaseResponseModel;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.job.genai.MonitoredTaskManager;
import innowake.mining.server.job.genai.MonitoredTaskManagerService;
import innowake.mining.server.job.genai.MonitoredTaskParameter;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.model.Technology;

/**
 * Tests the {@linkplain GenerativeReachabilityBlockDescriptionService}.
 */
@WithMockUser
class GenerativeReachabilityBlockDescriptionServiceTest extends DatabaseRelatedTest {
	
	@Nullable
	@Mock
	RestTemplate restTemplate;

	@Nullable
	@MockBean
	ProgressMonitor progressMonitor;

	@Nullable
	@MockBean
	private GenAIAvailabilityService availabilityService;
	
	@Nullable
	@MockBean
	private FunctionalBlockService blockService;
	
	@Nullable
	@MockBean
	private GenerativeModuleDescriptionService moduleDescriptionService;
	
	@Nullable
	@MockBean
	private ModuleService moduleService;

	@Nullable
	@MockBean
	private MonitoredTaskManagerService monitoredTaskManagerService;

	@Nullable
	@MockBean
	private MonitoredTaskParameter monitoredTaskParameter;

	@Nullable
	@MockBean
	private GenAiPromptService promptService;

	@InjectMocks
	@Autowired
	private GenerativeReachabilityBlockDescriptionService blockDescriptionService;
	
	@Nullable
	private ModulePojo moduleNoDescription;
	@Nullable
	private ModulePojo moduleEmptyDescription;
	@Nullable
	private ModulePojo moduleExistingDescription;

	private static final String BLOCK_UUID = "b44ca2ce-6b77-458f-b871-d2110bcc029e";
	private static final String AI_GENERATED_DESCRIPTION = "AI generated description";
	
	@BeforeEach
	void prepareTestDataAndMocks() {
		final List<ModulePart> moduleParts = Arrays.asList(new ModulePart("abc"), new ModulePart("def"));
		final Map<String, Object> flags = new HashMap<>();
		flags.put("TYPE", Collections.singletonList(FunctionalBlockType.REACHABILITY));
		Mockito.when(Assert.assertNotNull(blockService).find(any(UUID.class)))
				.thenReturn(Optional.of(new FunctionalBlockPojo(UUID.fromString(BLOCK_UUID), null, null, moduleParts, null, null,
						null, null, flags, null)));

		moduleNoDescription = ModulePojoDummy.build(new ModulePojoPrototype()
				.setNid(1l)
				.setUid(UUID.randomUUID())
				.setName("module1")
				.setContent("some content")
				.setTechnology(Technology.COBOL));

		moduleEmptyDescription = ModulePojoDummy.build(new ModulePojoPrototype()
				.setNid(2l)
				.setUid(UUID.randomUUID())
				.setName("module2")
				.setContent("some content")
				.setTechnology(Technology.COBOL)
				.setDescription(" "));

		moduleExistingDescription = ModulePojoDummy.build(new ModulePojoPrototype()
				.setNid(3l)
				.setUid(UUID.randomUUID())
				.setName("module3")
				.setDescription("some existing description")
				.setContent("some content")
				.setTechnology(Technology.COBOL));
		
		when(Objects.requireNonNull(moduleService).findModules(any())).thenReturn(Arrays.asList(moduleNoDescription, moduleEmptyDescription, moduleExistingDescription));
		when(Objects.requireNonNull(moduleService).getModule(any(EntityId.class))).then(i -> {
			final EntityId id = i.getArgument(0);
			switch (id.getNid().intValue()) {
				case 1:
					return moduleNoDescription;
				case 2:
					return moduleEmptyDescription;
				case 3:
					return moduleExistingDescription;
				default:
					throw new IllegalArgumentException("Module with id is unknown: " + id);
			}
		});
		when(Assert.assertNotNull(moduleDescriptionService).deduceDescription(Objects.requireNonNull(moduleNoDescription))).thenReturn("AI generated description");
		when(Assert.assertNotNull(moduleDescriptionService).deduceDescription(Objects.requireNonNull(moduleEmptyDescription))).thenReturn("AI generated description");
		when(promptService.buildReachabilityBlockPrompt(any(), any())).thenReturn("Dummy Prompt");
		final Map<String, String> responseMetadata = new HashMap<>();
		responseMetadata.put("token_count", "1");
		final BaseResponseModel baseGenAIResponseModel = new BaseResponseModel(AI_GENERATED_DESCRIPTION, "prompt", responseMetadata);
		final ResponseEntity<Object> responseEntity = new ResponseEntity<>(baseGenAIResponseModel, HttpStatus.OK);
		when(Objects.requireNonNull(restTemplate).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class))).thenReturn(responseEntity);
		final ExecutorService executorService =  Executors.newFixedThreadPool(5);
		MonitoredTaskManager monitoredTaskManager = new MonitoredTaskManager(progressMonitor, executorService, 1);
		MonitoredTaskManager subMonitorTask = new MonitoredTaskManager(progressMonitor, executorService, 1);
		Mockito.when(Assert.assertNotNull(monitoredTaskManagerService).newTaskManager(any(), anyInt())).thenReturn(monitoredTaskManager);
		Mockito.when(Assert.assertNotNull(monitoredTaskManagerService).newSubTaskManager(any(), anyInt())).thenReturn(subMonitorTask);
	}
	
	@Test
	void testServiceUnavailable() {
		Mockito.doReturn(false).when(availabilityService).isGenAIServiceAvailable();
		final UUID uuid = UUID.fromString(BLOCK_UUID);
		assertThrows(UserFacingException.class, () -> blockDescriptionService.generateDescription(EntityId.of(1l), uuid, true));
	}
	
	@Test
	void testGenerateModuleDescriptionsFromController() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		blockDescriptionService.setRestTemplate(Assert.assertNotNull(restTemplate));
		blockDescriptionService.generateDescription(EntityId.of(1l), UUID.fromString(BLOCK_UUID), true);
		verify(moduleDescriptionService, times(1)).deduceDescription(Objects.requireNonNull(moduleNoDescription));
		verify(moduleDescriptionService, times(1)).deduceDescription(Objects.requireNonNull(moduleEmptyDescription));
		verify(moduleDescriptionService, times(0)).deduceDescription(Objects.requireNonNull(moduleExistingDescription));
	}

	@Test
	void testGenerateModuleDescriptionsFromJob() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		blockDescriptionService.setRestTemplate(Assert.assertNotNull(restTemplate));
		final var functionalBlock = new FunctionalBlockPojo(UUID.randomUUID(), null,  EntityId.of(BLOCK_UUID, 1l), null, null, null, "Block 1", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY.name())), null);
	    when(monitoredTaskParameter.getProgressMonitor()).thenReturn(progressMonitor);
		blockDescriptionService.generateDescription(EntityId.of(1l), functionalBlock, true, Optional.empty(), null,   monitoredTaskParameter);
		verify(moduleDescriptionService, times(1)).deduceDescription(Objects.requireNonNull(moduleNoDescription));
		verify(moduleDescriptionService, times(1)).deduceDescription(Objects.requireNonNull(moduleEmptyDescription));
		verify(moduleDescriptionService, times(0)).deduceDescription(Objects.requireNonNull(moduleExistingDescription));
		verify(monitoredTaskManagerService, times(1)).newSubTaskManager(any(), anyInt());
	}

	@Test
	void testDoNotGenerateModuleDescriptions() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		blockDescriptionService.generateDescription(EntityId.of(1l), UUID.fromString(BLOCK_UUID), false);
		verify(moduleDescriptionService, times(0)).deduceDescription(any(), any());
		verify(moduleDescriptionService, times(0)).deduceDescription(any());
		verify(moduleService, times(0)).update(any());
	}

	@Test
	void testDoNotGenerateModuleDescriptionsWithNoModule() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		when(Assert.assertNotNull(moduleService).findModules(any())).thenReturn(List.of());
		assertThrows(
				UserFacingException.class, () -> blockDescriptionService.generateDescription(EntityId.of(1l), UUID.fromString(BLOCK_UUID), false));
		verify(moduleDescriptionService, times(0)).deduceDescription(any(), any());
		verify(moduleDescriptionService, times(0)).deduceDescription(any());
		verify(moduleService, times(0)).update(any());
		verify(monitoredTaskManagerService, times(0)).newTaskManager(any(), anyInt());
	}

	@Test
	void testDoNotGenerateOnlyModulesWithoutDescriptions() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		when(Assert.assertNotNull(moduleService).findModules(any())).thenReturn(List.of(moduleNoDescription, moduleEmptyDescription));
		assertThrows(
				UserFacingException.class, () -> blockDescriptionService.generateDescription(EntityId.of(1l), UUID.fromString(BLOCK_UUID), false));
		verify(moduleDescriptionService, times(0)).deduceDescription(any(), any());
		verify(moduleDescriptionService, times(0)).deduceDescription(any());
		verify(moduleService, times(0)).update(any());
		verify(monitoredTaskManagerService, times(0)).newTaskManager(any(), anyInt());
	}

	@Test
	void testDoNotGenerateWhenOnlyOneModuleWithoutDescription() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		when(Assert.assertNotNull(moduleService).findModules( any())).thenReturn(List.of(moduleNoDescription));
		assertThrows(
				UserFacingException.class, () -> blockDescriptionService.generateDescription(EntityId.of(1l), UUID.fromString(BLOCK_UUID), false));
		verify(moduleDescriptionService, times(0)).deduceDescription(any(), any());
		verify(moduleDescriptionService, times(0)).deduceDescription(any());
		verify(moduleService, times(0)).update(any());
		verify(monitoredTaskManagerService, times(0)).newTaskManager(any(), anyInt());
	}

	@Test
	void testGenerateWhenOneModuleWithDescriptionAndOneWithout() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		blockDescriptionService.setRestTemplate(Objects.requireNonNull(restTemplate));
		when(Assert.assertNotNull(moduleService).findModules(any())).thenReturn(List.of(moduleNoDescription, moduleExistingDescription));
		String description = blockDescriptionService.generateDescription(EntityId.of(1l), UUID.fromString(BLOCK_UUID), false);
		verify(moduleDescriptionService, times(0)).deduceDescription(any(), any());
		verify(moduleDescriptionService, times(0)).deduceDescription(any());
		verify(moduleService, times(0)).update(any());
		verify(monitoredTaskManagerService, times(0)).newTaskManager(any(), anyInt());
		assertEquals( AI_GENERATED_PREFIX + "AI generated description", description);
	}

}
