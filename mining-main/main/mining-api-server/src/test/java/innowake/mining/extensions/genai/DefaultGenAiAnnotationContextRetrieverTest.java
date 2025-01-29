/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.genai;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.server.MockedBaseTest;
import innowake.mining.server.genai.AiGenerationContext;
import innowake.mining.server.genai.AnnotationContext;
import innowake.mining.server.util.BranchStatementUtility;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.*;
import innowake.mining.shared.model.ModuleLocation;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Tests the {@linkplain DefaultGenAiAnnotationContextRetriever}.
 */
@Import({ DefaultGenAiAnnotationContextRetriever.class })
@WithMockUser
class DefaultGenAiAnnotationContextRetrieverTest extends MockedBaseTest {

	private static final EntityId MODULE_ID = EntityId.of(1L);
	private static final EntityId ANNOTATION_ID = EntityId.of(1L);
	private static final AiGenerationContext AI_GENERATION_CONTEXT_EXPECTED = new AiGenerationContext("already existing context\nACCTBAL = balance of the bank deposit account. Account Balance\nIO-STATUS = indicates the operational status of the credit card transaction device. Input/Output Status\n");
	private static final UUID uuid = UUID.fromString("00000000-0000-0000-0000-000000000000");

	@Nullable
	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Nullable
	@Autowired
	private BranchStatementUtility branchStatementUtility;

	@InjectMocks
	@Autowired
	private DefaultGenAiAnnotationContextRetriever defaultGenAiAnnotationContextRetriever;

	@Nullable
	private AnnotationPojoPrototype annotation;

	@Nullable
	private AutoCloseable mocks;

	@Nullable
	private List<DataDictionaryPojo> dataDictionaryPojos;

	@Nullable
	private AnnotationContext annotationContext;

	@BeforeEach
	void prepareTestDataAndMocks() {
		mocks = MockitoAnnotations.openMocks(this);

		annotation = new AnnotationPojoPrototype()
				.withId(ANNOTATION_ID)
				.setSourceAttachment(new BinaryString("some content"))
				.setModule(MODULE_ID)
				.setLocation(new ModuleLocation(500, 2000));

		annotationContext = new AnnotationContext(annotation, new AiGenerationContext("already existing context"));

		final List<DataDictionaryPojo> dataDictionaryPojos = new ArrayList<>();
		dataDictionaryPojos.add(createDataDictionary("ACCTBAL", "balance of the bank deposit account", "Account Balance", new ModuleLocation(1000, 50)));
		dataDictionaryPojos.add(createDataDictionary("IO-STATUS", "indicates the operational status of the credit card transaction device", "Input/Output Status", new ModuleLocation(2000, 20)));
		this.dataDictionaryPojos = dataDictionaryPojos;
		
		var dataDictionaryService = Assert.assertNotNull(this.dataDictionaryService);
		var branchStatementUtility = Assert.assertNotNull(this.branchStatementUtility);

		when(dataDictionaryService.find(any())).thenReturn(dataDictionaryPojos);
		when(branchStatementUtility.getReferencedDDEntries(any(), any())).thenReturn(dataDictionaryPojos);
	}

	@AfterEach
	void closeMocks() throws Exception {
		Assert.assertNotNull(mocks).close();
	}

	@Test
	void testRetrieve() {
		assert annotationContext != null;
		final AiGenerationContext ctx = defaultGenAiAnnotationContextRetriever.retrieve(annotationContext, uuid);

		assertEquals(AI_GENERATION_CONTEXT_EXPECTED.getAdditionalPromptContext(), ctx.getAdditionalPromptContext());
	}

	@Test
	void retrieveShouldReturnPreviousContextWhenDataDictionaryServiceThrowsException() {
		var dataDictionaryService = Assert.assertNotNull(this.dataDictionaryService);

		when(dataDictionaryService.find(any())).thenThrow(new RuntimeException("Some exception"));

		assert annotationContext != null;
		final AiGenerationContext ctx = defaultGenAiAnnotationContextRetriever.retrieve(annotationContext, uuid);

		assertEquals("already existing context", ctx.getAdditionalPromptContext());
	}

	@Test
	void retrieveShouldReturnPreviousContextWhenBranchStatementUtilityThrowsException() {
		var branchStatementUtility = Assert.assertNotNull(this.branchStatementUtility);

		when(branchStatementUtility.getReferencedDDEntries(any(), any())).thenThrow(new RuntimeException("Some exception"));

		assert annotationContext != null;
		final AiGenerationContext ctx = defaultGenAiAnnotationContextRetriever.retrieve(annotationContext, uuid);

		assertEquals("already existing context", ctx.getAdditionalPromptContext());
	}

	/**
	 * Creates a {@linkplain DataDictionaryPojo} with the given name, description and location.
	 *
	 * @param name        the name
	 * @param description the description
	 * @param location    the location
	 *
	 * @return the created {@linkplain DataDictionaryPojo}
	 */
	private DataDictionaryPojo createDataDictionary(final String name, final String  description, final String translation, final ModuleLocation location) {
		return new DataDictionaryPojo(UUID.randomUUID(), 1L, MODULE_ID, null, null,
				location, name, description,
				null, Map.of(), null,
				"system_user", null, null,
				null, null, null,
				null, null, null,
				null, null, false,
				null, null, null,
				null, null, translation, Collections.emptyList(), CustomPropertiesMap.empty());
	}
}
