/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.genai;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.server.MockedBaseTest;
import innowake.mining.server.genai.AiGenerationContext;
import innowake.mining.server.genai.AnnotationContext;
import innowake.mining.server.genai.knowledgeservice.KnowledgeQuery;
import innowake.mining.server.genai.knowledgeservice.KnowledgeServiceResultObject;
import innowake.mining.server.genai.requestresponse.KnowledgeServiceResponseModel;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.util.BranchStatementUtility;
import innowake.mining.shared.access.*;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.ModuleLocation;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

/**
 * Tests the {@linkplain KnowledgeServiceContextRetriever}.
 */
@Import({ KnowledgeServiceContextRetriever.class })
@WithMockUser
class GenAiKnowledgeQueryContextRetrieverTest extends MockedBaseTest {

	private static final EntityId MODULE_ID = EntityId.of(1L);
	private static final EntityId ANNOTATION_ID = EntityId.of(1L);
	private static final AiGenerationContext AI_GENERATION_CONTEXT_EXPECTED = new AiGenerationContext("already existing context\n... Document Service Content 1...\n... Document Service Content 2...\n\n");
	private static final UUID uuid = UUID.fromString("00000000-0000-0000-0000-000000000000");

	@MockBean
	@Autowired
	private KnowledgeQuery knowledgeQuery;

	@MockBean
	@Autowired
	private GenericConfigProperties genericConfigProperties;

	@InjectMocks
	private KnowledgeServiceContextRetriever knowledgeServiceContextRetriever;

	@Nullable
	private AnnotationPojoPrototype annotation;

	@Nullable
	private AutoCloseable mocks;

	@Nullable
	private AnnotationContext annotationContext;

	@Nullable
	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Nullable
	@Autowired
	private BranchStatementUtility branchStatementUtility;

	@BeforeEach
	void prepareTestDataAndMocks() {
		mocks = MockitoAnnotations.openMocks(this);

		annotation = new AnnotationPojoPrototype()
				.withId(ANNOTATION_ID)
				.setSourceAttachment(new BinaryString("some content"))
				.setModule(MODULE_ID)
				.setLocation(new ModuleLocation(500, 2000));

		annotationContext = new AnnotationContext(annotation, new AiGenerationContext("already existing context"));

		var knowledgeService = Assert.assertNotNull(this.knowledgeQuery);
		final KnowledgeServiceResponseModel mockedKnowledgeQueryResult = getKnowledgeServiceResponseModel();
		when(knowledgeService.queryDocuments(any(), any(), any(), anyInt(), eq(uuid))).thenReturn(mockedKnowledgeQueryResult);
	}

	@AfterEach
	void closeMocks() throws Exception {
		Assert.assertNotNull(mocks).close();
	}

	@Test
	void testRetrieve() {
		assert annotationContext != null;
		final AiGenerationContext ctx = knowledgeServiceContextRetriever.retrieve(annotationContext, uuid);

		assertEquals(AI_GENERATION_CONTEXT_EXPECTED.getAdditionalPromptContext(), ctx.getAdditionalPromptContext());
	}

	@Test
	void retrieveShouldReturnPreviousContextWhenDataDictionaryServiceThrowsException() {
		var dataDictionaryService = Assert.assertNotNull(this.dataDictionaryService);

		when(dataDictionaryService.find(any())).thenThrow(new RuntimeException("Some exception"));

		assert annotationContext != null;
		final AiGenerationContext ctx = knowledgeServiceContextRetriever.retrieve(annotationContext, uuid);

		assertEquals("already existing context", ctx.getAdditionalPromptContext());
	}

	@Test
	void retrieveShouldReturnPreviousContextWhenBranchStatementUtilityThrowsException() {
		var branchStatementUtility = Assert.assertNotNull(this.branchStatementUtility);

		when(branchStatementUtility.getReferencedDDEntries(any(), any())).thenThrow(new RuntimeException("Some exception"));

		assert annotationContext != null;
		final AiGenerationContext ctx = knowledgeServiceContextRetriever.retrieve(annotationContext, uuid);

		assertEquals("already existing context", ctx.getAdditionalPromptContext());
	}

	private static KnowledgeServiceResponseModel getKnowledgeServiceResponseModel() {
		final KnowledgeServiceResultObject interestDocumentResult = new KnowledgeServiceResultObject("d95cef71-7e55-40ac-b440-97bc2ff464ab", 0.9, "... Document Service Content 1...", 1, "interest.pdf");
		final KnowledgeServiceResultObject salariesDocumentResult = new KnowledgeServiceResultObject("a54c7709-da21-78bb-980a-a2232078bc12", 0.8, "... Document Service Content 2...", 2, "salaries.xlsx");
		return new KnowledgeServiceResponseModel(List.of(interestDocumentResult, salariesDocumentResult));
	}

}
