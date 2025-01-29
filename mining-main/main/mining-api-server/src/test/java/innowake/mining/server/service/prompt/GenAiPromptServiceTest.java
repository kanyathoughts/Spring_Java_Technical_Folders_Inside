/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service.prompt;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.server.service.semanticsearch.SemanticSearchService.SAVED_SEARCHES;
import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import com.google.common.cache.CacheBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.MockedBaseTest;
import innowake.mining.server.cache.GuavaCache;
import innowake.mining.server.cache.MiningCacheConfig;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.TestResourceUtil;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;
import innowake.mining.shared.entities.testing.ModulePojoDummy;

@Import({ GenAiPromptService.class })
@WithMockUser
class GenAiPromptServiceTest extends MockedBaseTest {

	@Nullable
	@Mock
	private RestTemplate restTemplate;

	@Nullable
	@MockBean
	private GenericConfigProperties configProperties;

	@Nullable
	@MockBean
	private CacheManager cacheManager;

	@InjectMocks
	@Autowired
	private GenAiPromptService genAiPromptService;

	@Nullable
	private AutoCloseable mocks;

	@BeforeEach
	public void setup() {
		super.setup();
		mocks = MockitoAnnotations.openMocks(this);
	}

	@AfterEach
	void closeMocks() throws Exception {
		assertNotNull(mocks).close();
	}

	private void setServiceUnavailable() {
		when(restTemplate.exchange(any(String.class), eq(HttpMethod.GET), any(HttpEntity.class), eq(Prompt.class)))
				.thenThrow(new RestClientException("Service unavailable"));
	}

	private void setServiceReturnValue(final String prompt, final List<String> placeholders) {
		when(restTemplate.exchange(any(String.class), eq(HttpMethod.GET), any(HttpEntity.class), eq(Prompt.class)))
				.thenReturn(new ResponseEntity<>(new Prompt(prompt, placeholders, "", ""), HttpStatus.OK));
	}

	private void setCache(final String prompt, final List<String> placeholders, final PromptType key) {
		final Cache cache = new GuavaCache(MiningCacheConfig.PROMPT_CACHE, CacheBuilder.newBuilder());
		cache.put(key, new Prompt(prompt, placeholders, "", ""));
		when(cacheManager.getCache(any(String.class))).thenReturn(cache);
	}

	@Test
	void testBuildAnnotationGroupPromptServiceUnavailable() {
		setServiceUnavailable();
		final AnnotationPojo annotation1 = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setName("This code checks if A is greater than B"));
		final AnnotationPojo annotation2 = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setName("This code checks if B is greater than A"));

		final String prompt = genAiPromptService.buildAnnotationGroupPrompt(Arrays.asList(annotation1, annotation2));

		assertEquals("""
				# Goal

				Provide a single, comprehensive description for a given group of code, rather than individual descriptions for each Annotation within the group. \
				Synthesize the main point of all annotations together into one cohesive description. \
				Your description combines all the annotations into one paragraph and focuses on the meaning of the entire text, rather than on describing each annotation separately. \
				Additionally, provide a detailed explanation of variable conditions and flags. \
				Don't add any headers to your response.

				## Annotation 1

				This code checks if A is greater than B

				## Annotation 2

				This code checks if B is greater than A

				""", prompt);
	}

	@Test
	void testBuildAnnotationGroupPromptServiceAvailable() {
		setServiceReturnValue("Dummy Prompt\n\n{annotation_list}", List.of("annotation_list"));

		final AnnotationPojo annotation = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setName("Dummy Description"));

		final String prompt = genAiPromptService.buildAnnotationGroupPrompt(List.of(annotation));

		assertEquals("""
				Dummy Prompt

				## Annotation 1

				Dummy Description

				""", prompt);
	}

	@Test
	void testBuildAnnotationGroupPromptCached() {
		setCache("Cached Prompt\n\n{annotation_list}", List.of("annotation_list"), PromptType.ANNOTATION_GROUP);

		final AnnotationPojo annotation = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setName("Dummy Description"));

		final String prompt = genAiPromptService.buildAnnotationGroupPrompt(List.of(annotation));

		assertEquals("""
				Cached Prompt

				## Annotation 1

				Dummy Description

				""", prompt);
	}

	@Test
	void testBuildReachabilityBlockPromptServiceUnavailable() {
		setServiceUnavailable();
		final ModulePojo moduleA = ModulePojoDummy.build(new ModulePojoPrototype()
				.setName("Module A")
				.setDescription("This module does stuff")
		);
		final ModulePojo moduleB = ModulePojoDummy.build(new ModulePojoPrototype()
				.setName("Module B")
				.setDescription("This module does different stuff")
		);
		final ModulePojo moduleC = ModulePojoDummy.build(new ModulePojoPrototype()
				.setName("Module C")
		);

		final String prompt = genAiPromptService.buildReachabilityBlockPrompt(Arrays.asList(moduleA, moduleB), List.of(moduleC));

		assertEquals("""
				# Context

				Reachability blocks are groups of modules obtained by following the call chains between upper and lower bound modules. \
				A module could f.e. be a program, a resource file or a database table.
				# Goal

				Provide a description for a given reachability block based on the following modules. \
				Don't list the existing modules or descriptions.

				## Module A

				This module does stuff

				## Module B

				This module does different stuff

				## Modules without description
				- Module C
				""", prompt);
	}

	@Test
	void testBuildReachabilityBlockPromptServiceAvailable() {
		setServiceReturnValue("Dummy Prompt\n\n{module_with_description_list} {module_without_description_list}",
				List.of("annotation_list", "module_with_description_list", "module_without_description_list"));

		final ModulePojo moduleA = ModulePojoDummy.build(new ModulePojoPrototype()
				.setName("Module A")
				.setDescription("This module does stuff")
		);
		final ModulePojo moduleB = ModulePojoDummy.build(new ModulePojoPrototype()
				.setName("Module B")
		);

		final String prompt = genAiPromptService.buildReachabilityBlockPrompt(List.of(moduleA), List.of(moduleB));

		assertEquals("""
				Dummy Prompt

				## Module A

				This module does stuff

				 - Module B
				""", prompt);
	}

	@Test
	void testBuildAnnotationPromptServiceUnavailable() throws IOException, URISyntaxException {
		setServiceUnavailable();
		final String code = TestResourceUtil.getContent("innowake/mining/server/genai/code/evaluateWsIdx.cbl", UTF_8);
		final String context = TestResourceUtil.getContent("innowake/mining/server/genai/context/evaluateWsIdx.txt", UTF_8);
		final String prompt = genAiPromptService.buildAnnotationPrompt(code, context, 5, 80);

		final String expectedPrompt = TestResourceUtil.getContent("innowake/mining/server/genai/expected/expectedGenerateAnnotationDescriptionPrompt.txt", UTF_8);
		assertEquals(expectedPrompt.trim().replaceAll("\\R", ""), prompt.trim().replaceAll("\\R", ""));
	}

	@Test
	void testBuildAnnotationPromptServiceAvailable() {
		setServiceReturnValue("Dummy Prompt\n\n{code} {context} {title_length} {summary_length}",
				List.of("code", "context", "title_length", "summary_length"));

		final String code = "This is some code";
		final String context = "This is some context";
		final String prompt = genAiPromptService.buildAnnotationPrompt(code, context, 5, 80);

		assertEquals("Dummy Prompt\n\nThis is some code This is some context 5 80", prompt);

	}

	@Test
	void testBuildSimilaritySearchPromptServiceUnavailable() throws IOException, URISyntaxException {
		setServiceUnavailable();
		final String query = "Modules without taxonomies";
		final String categories = SAVED_SEARCHES.toString();

		final String prompt = genAiPromptService.buildSimilaritySearchPrompt(query, categories);

		final String expectedPrompt = TestResourceUtil.getContent("innowake/mining/server/genai/expected/expectedSearchSimilarityPrompt.txt", UTF_8);
		assertEquals(expectedPrompt.trim().replaceAll("\\R", ""), prompt.trim().replaceAll("\\R", ""));
	}

	@Test
	void testBuildSimilaritySearchPromptServiceAvailable() {
		setServiceReturnValue("Dummy Prompt\n\n{query} {categories}", List.of("query", "categories"));

		final String query = "Modules without taxonomies";
		final String categories =  SAVED_SEARCHES.toString();

		final String prompt = genAiPromptService.buildSimilaritySearchPrompt(query, categories);

		assertEquals("Dummy Prompt\n\nModules without taxonomies " + SAVED_SEARCHES, prompt);
	}

	@Test
	void testServiceReturnsBlankPrompt() {
		setServiceReturnValue(" ", List.of());
		final String prompt = genAiPromptService.buildAnnotationGroupPrompt(List.of());
		assertFalse(prompt.isBlank(), "Local prompt should be used which should not be blank.");
	}
}
