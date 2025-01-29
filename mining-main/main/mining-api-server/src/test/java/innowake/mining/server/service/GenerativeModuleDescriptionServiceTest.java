/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;
import static innowake.mining.server.service.CentralCommunicationWithGenAIService.AI_GENERATED_PREFIX;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.service.prompt.GenAiPromptService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
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
import innowake.mining.server.error.PermissionException;
import innowake.mining.server.genai.requestresponse.DeduceResponseModel;
import innowake.mining.server.genai.requestresponse.ExplainedPartitionModel;
import innowake.mining.server.permission.GenAIModulePermissionChecker;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.codeviewer.AssembledContent;

@Import({ GenerativeModuleDescriptionService.class })
@WithMockUser
class GenerativeModuleDescriptionServiceTest extends MockedBaseTest {

	private static final String GEN_AI_DESCRIPTION = "[AI generated] content";

	@Nullable
	@MockBean
	RestTemplate restTemplate;

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
	private ContentAssemblingService contentAssemblingService;

	@Nullable
	@MockBean
	private GenAiPromptService promptService;

	@InjectMocks
	@Autowired
	private GenerativeModuleDescriptionService generativeModuleDescriptionService;

	private ModulePojo moduleWithCobolLanguage = ModulePojoDummy.VOID;
	private ModulePojo moduleWithNaturalLanguage = ModulePojoDummy.VOID;
	private ModulePojo moduleJCLWithContentNull = ModulePojoDummy.VOID;

	@BeforeEach
	void prepareTestDataAndMocks() {

		moduleWithCobolLanguage = ModulePojoDummy.build(new ModulePojoPrototype()
				.setNid(1l)
				.setUid(UUID.randomUUID())
				.setName("module1")
				.setContent("some content")
				.setTechnology(Technology.COBOL));

		moduleWithNaturalLanguage = ModulePojoDummy.build(new ModulePojoPrototype()
				.setNid(3l)
				.setUid(UUID.randomUUID())
				.setName("module2")
				.setContent("some content")
				.setTechnology(Technology.NATURAL)
				.setDescription("any"));
		moduleJCLWithContentNull = ModulePojoDummy.build(new ModulePojoPrototype()
				.setNid(2l)
				.setUid(UUID.randomUUID())
				.setName("module2")
				//setContent("some content")
				.setTechnology(Technology.JCL)
				.setDescription("any"));

		var configProperties = Assert.assertNotNull(this.configProperties);
		var restTemplate = Assert.assertNotNull(this.restTemplate);

		when(configProperties.getResolvedGenAiURL()).thenReturn("http://localhost");
		when(configProperties.getGenAiURL()).thenReturn("http://localhost");
		when(configProperties.getGenAiPlugin()).thenReturn("Azure");
		final Map<String, String> responseMetadata = new HashMap<>();
		responseMetadata.put("token_count", "1");
		DeduceResponseModel body = new DeduceResponseModel(GEN_AI_DESCRIPTION, List.of(new ExplainedPartitionModel("any", "first", 1, 2, "", Collections.emptyList())), responseMetadata);
		final ResponseEntity<Object> responseEntity = new ResponseEntity<>(body, HttpStatus.OK);
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		generativeModuleDescriptionService.setRestTemplate(restTemplate);
		when(restTemplate.exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class))).thenReturn(responseEntity);
		when(Assert.assertNotNull(permissionChecker).allowedToBeProcessed(any())).thenReturn(true);
	}

	@Test
	void testServiceWithCobolLanguage() {
		final AssembledContent assembledContent = new AssembledContent(true, moduleWithCobolLanguage.getContent().get(), null);
		when(Assert.assertNotNull(contentAssemblingService).getModuleAssembledContent(moduleWithCobolLanguage)).thenReturn(assembledContent);
		final String description = generativeModuleDescriptionService.deduceDescription(Assert.assertNotNull(moduleWithCobolLanguage));
		assertNotNull(description);
		assertEquals(AI_GENERATED_PREFIX + GEN_AI_DESCRIPTION , description);
		verify(this.restTemplate, times(1)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
	}

	@Test
	void testServiceWithNATURALLanguage() {
		final AssembledContent assembledContent = new AssembledContent(true, moduleWithNaturalLanguage.getContent().get(), null);
		when(Assert.assertNotNull(contentAssemblingService).getModuleAssembledContent(moduleWithNaturalLanguage)).thenReturn(assembledContent);
		final String description = generativeModuleDescriptionService.deduceDescription(Assert.assertNotNull(moduleWithNaturalLanguage));
		assertNotNull(description);
		assertEquals(AI_GENERATED_PREFIX + GEN_AI_DESCRIPTION , description);
	}

	@Test
	void testNoPermission() {
		final AssembledContent assembledContent = new AssembledContent(true, moduleWithCobolLanguage.getContent().get(), null);
		when(Assert.assertNotNull(contentAssemblingService).getModuleAssembledContent(moduleWithCobolLanguage)).thenReturn(assembledContent);
		when(Assert.assertNotNull(permissionChecker).allowedToBeProcessed(any())).thenReturn(false);
		assertThrows(PermissionException.class, () -> generativeModuleDescriptionService.deduceDescription(Assert.assertNotNull(moduleWithCobolLanguage)));
		verify(this.restTemplate, times(0)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
	}
	@Test
	void testServiceNullResultFromGenAi() {
		final Map<String, String> responseMetadata = new HashMap<>();
		responseMetadata.put("token_count", "200");
		DeduceResponseModel body = new DeduceResponseModel(null, List.of(new ExplainedPartitionModel("any", "first", 1, 2, "", Collections.emptyList())), responseMetadata);
		final ResponseEntity<Object> responseEntity = new ResponseEntity<>(body, HttpStatus.OK);
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		generativeModuleDescriptionService.setRestTemplate(Assert.assertNotNull(restTemplate));
		when(Assert.assertNotNull(restTemplate).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class))).thenReturn(responseEntity);
		assertThrows(UserFacingException.class, () -> generativeModuleDescriptionService.deduceDescription(Assert.assertNotNull(moduleWithNaturalLanguage)));
		verify(this.restTemplate, times(1)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));

	}

	@Test
	void testContentNull() {
		final AssembledContent assembledContent = new AssembledContent(true, moduleJCLWithContentNull.getContent().toString(), null);
		when(Assert.assertNotNull(contentAssemblingService).getModuleAssembledContent(moduleJCLWithContentNull)).thenReturn(assembledContent);
		assertThrows(UserFacingException.class, () -> generativeModuleDescriptionService.deduceDescription(Assert.assertNotNull(moduleJCLWithContentNull)));
		verify(this.restTemplate, times(0)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
	}

	@Test
	void testContentAssembledIsNullGetModuleContent() {
		final AssembledContent assembledContent = new AssembledContent(true, null, null);
		when(Assert.assertNotNull(contentAssemblingService).getModuleAssembledContent(moduleWithCobolLanguage)).thenReturn(assembledContent);
		final String description = generativeModuleDescriptionService.deduceDescription(Assert.assertNotNull(moduleWithCobolLanguage));
		assertNotNull(description);
		assertEquals(AI_GENERATED_PREFIX + GEN_AI_DESCRIPTION , description);
		verify(this.restTemplate, times(1)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
	}

	@Test
	void testContentAssembledIsBlankGetModuleContent() {
		final AssembledContent assembledContent = new AssembledContent(false, "", null);
		when(Assert.assertNotNull(contentAssemblingService).getModuleAssembledContent(moduleWithCobolLanguage)).thenReturn(assembledContent);
		final String assembledModuleContent = generativeModuleDescriptionService.getAssembledContent(Objects.requireNonNull(moduleWithCobolLanguage));
		assertEquals("some content", assembledModuleContent);
	}

	@Test
	void testContentAssembledIsWithExceptionGetModuleContent() {
		when(Assert.assertNotNull(contentAssemblingService).getModuleAssembledContent(any())).thenThrow(RuntimeException.class);
		final String description = generativeModuleDescriptionService.deduceDescription(Assert.assertNotNull(moduleWithCobolLanguage));
		assertNotNull(description);
		assertEquals(AI_GENERATED_PREFIX + GEN_AI_DESCRIPTION , description);
		verify(this.restTemplate, times(1)).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class));
	}

}
