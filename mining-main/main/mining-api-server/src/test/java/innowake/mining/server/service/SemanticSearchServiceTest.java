package innowake.mining.server.service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.server.MockedBaseTest;
import innowake.mining.server.genai.requestresponse.BaseResponseModel;
import innowake.mining.server.permission.GenAIModulePermissionChecker;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.service.prompt.GenAiPromptService;
import innowake.mining.shared.access.EntityId;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import static org.mockito.ArgumentMatchers.any;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import static org.mockito.Mockito.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.security.test.context.support.WithMockUser;
import innowake.mining.server.service.semanticsearch.*;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import java.util.HashMap;
import java.util.Map;


/**
 * Tests the {@linkplain SemanticSearchService}.
 */
@Import({ SemanticSearchService.class })
@WithMockUser
class SemanticSearchServiceTest extends MockedBaseTest{

	private static final String AI_GENERATED_DESCRIPTION = "AI generated description";
	private static final String MODULE_WITHOUT_TAXONOMY = "No Taxonomies Assigned";
	private static final String DATA_DICTIONARY_BUSINESS_RELATED = "Business Related DD Entries";


	private static Long projectId = Long.valueOf(0);

	@Nullable
	@Mock RestTemplate restTemplate;

	@Nullable
	@MockBean
	private GenericConfigProperties configProperties;

	@Nullable
	@MockBean
	private ParameterizedTypeReference<SemanticSearchResultPojo> responseType;

	@Nullable
	@MockBean
	private GenAIAvailabilityService availabilityService;

	@Nullable
	@MockBean
	private GenAIModulePermissionChecker genAIModulePermissionChecker;

	@Nullable
	@MockBean
	private GenAiPromptService promptService;

	@InjectMocks
	@Autowired
	private SemanticSearchService semanticSearchService;

	@BeforeEach
	void prepareTestDataAndMocks() {
		var configProperties = Assert.assertNotNull(this.configProperties);
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		when(configProperties.getResolvedGenAiURL()).thenReturn("http://localhost");
		when(configProperties.getResolvedSemanticSearchURL()).thenReturn("/semantic-search-rag");
		when(configProperties.getGenAiURL()).thenReturn("http://localhost");
		when(configProperties.getGenAiPlugin()).thenReturn("Azure");
		when(configProperties.getGenAiApiKey()).thenReturn("MINING_KEY");
		MockHttpServletRequest request = new MockHttpServletRequest();
		RequestContextHolder.setRequestAttributes(new ServletRequestAttributes(request));
	}

	@Test
	void testCallSemanticSearchEndpoint() {
		final Map<String, String> meta = new HashMap<>();
		meta.put("name", "any");
		SemanticSearchDocumentPojo semanticSearchDocumentPojo = new SemanticSearchDocumentPojo(1d, "any", meta);
		final SemanticSearchResultPojo semanticSearchResultPojo = new SemanticSearchResultPojo(AI_GENERATED_DESCRIPTION,
				new SemanticSearchDocumentPojo[] { semanticSearchDocumentPojo });
		final ResponseEntity<Object> semanticSearchResponse = new ResponseEntity<>(semanticSearchResultPojo, HttpStatus.OK);
		final BaseResponseModel baseGenAIResponseModel = new BaseResponseModel("Any response", "prompt", null);
		final ResponseEntity<Object> responseEntity = new ResponseEntity<>(baseGenAIResponseModel, HttpStatus.OK);
		semanticSearchService.setRestTemplate(Assert.assertNotNull(restTemplate));
		when(Assert.assertNotNull(restTemplate).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class))).thenReturn(responseEntity).thenReturn(semanticSearchResponse);
		SemanticSearchResultPojo resultPojo = semanticSearchService.doSemanticSearch(EntityId.of(projectId), "what is rate calculation");
		assertEquals(AI_GENERATED_DESCRIPTION , resultPojo.getAnswer());
	}

	@Test
	void testSavedSearchSimilarityModuleNoTaxonomy() {
		final BaseResponseModel baseGenAIResponseModel = new BaseResponseModel(MODULE_WITHOUT_TAXONOMY, "prompt", null);
		final ResponseEntity<Object> responseEntity = new ResponseEntity<>(baseGenAIResponseModel, HttpStatus.OK);
		semanticSearchService.setRestTemplate(Assert.assertNotNull(restTemplate));
		when(Assert.assertNotNull(restTemplate).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class))).thenReturn(responseEntity);
		SemanticSearchResultPojo resultPojo = semanticSearchService.doSemanticSearch(EntityId.of(projectId), "all modules without taxonomies ");
		assertTrue(resultPojo.getAnswer().contains("/modules?savedSearch=No%20Taxonomies%20Assigned"));
	}

	@Test
	void testSavedSearchSimilarityBusinessRelatedDataDictionary() {
		final BaseResponseModel baseGenAIResponseModel = new BaseResponseModel(DATA_DICTIONARY_BUSINESS_RELATED, "prompt", null);
		final ResponseEntity<Object> responseEntity = new ResponseEntity<>(baseGenAIResponseModel, HttpStatus.OK);
		semanticSearchService.setRestTemplate(Assert.assertNotNull(restTemplate));
		when(Assert.assertNotNull(restTemplate).exchange(any(), any(), any(), ArgumentMatchers.<Class<Object>>any(), any(Object.class))).thenReturn(responseEntity);
		SemanticSearchResultPojo resultPojo = semanticSearchService.doSemanticSearch(EntityId.of(projectId), "all entries related business ");
		assertTrue(resultPojo.getAnswer().contains("/data-dictionary?savedSearch=Business%20Related%20DD%20Entries") );
	}
}
