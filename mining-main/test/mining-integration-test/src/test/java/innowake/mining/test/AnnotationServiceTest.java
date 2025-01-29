/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.test;

import static innowake.mining.test.CustomProperties.getCustomPropertyByName;
import static innowake.mining.test.CustomProperties.verifyNumberOfCustomProperties;
import static innowake.mining.test.util.RestTemplateUtil.getHttpHeaders;
import static org.apache.http.HttpStatus.SC_CREATED;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.client.service.annotation.AnnotationServiceProvider;
import innowake.mining.client.service.annotation.FindAllAnnotations;
import innowake.mining.client.service.annotation.SearchAnnotations;
import innowake.mining.client.service.module.ModuleServiceProvider;
import innowake.mining.data.access.postgres.AnnotationPgDao;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.AnnotationSearch;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Integration tests for the {@code Annotation} service.
 */
@TestInstance(Lifecycle.PER_CLASS)
class AnnotationServiceTest extends IntegrationTest {

	private static final Logger LOG = LoggerFactory.getLogger(AnnotationServiceTest.class);

	private static final int NUMBER_OF_CUSTOM_PROPERTIES_BEING_CREATED_ON_ANNOTATION_ENTITY = 1;
	private static final String LINKED_BUSINESS_VARIABLES_URL = 
			RouteConfiguration.API_BASE + "/v1/projects/{projectId}/annotations/{annotationId}/linked-business-variables";
	private static final String ANNOTATION_DATADICTIONARY_BULK_DELETE_URL = RouteConfiguration.API_BASE + "/v1/projects/{projectId}/bulk-delete/{entityType}";
	private static final int NUMBER_OF_CUSTOM_PROPERTIES_ON_ANNOTATIONELEMENT_ENTITY = 0;
	private static final AnnotationType TEST_ANNOTATION_TYPE = AnnotationType.RULE;
	private static final AnnotationPojoPrototype TEST_ANNOTATION_1 = new AnnotationPojoPrototype();
	private static final AnnotationPojoPrototype TEST_ANNOTATION_2 = new AnnotationPojoPrototype();
	private static final AnnotationPojoPrototype TEST_ANNOTATION_3 = new AnnotationPojoPrototype();
	private static final AnnotationPojoPrototype TEST_ANNOTATION_4 = new AnnotationPojoPrototype();
	private static final EntityId NON_EXISTING_ID = EntityId.of(Long.MAX_VALUE);
	private static final EntityId ENTITY_ONE = EntityId.of(1L);
	private static final EntityId ENTITY_TWO = EntityId.of(2L);
	private static final Long CATEGORY_B_ID = Long.valueOf(1002);
	private static final Long CATEGORY_C_ID = Long.valueOf(1003);
	private static final Long FIVE = Long.valueOf(5);
	private static final EntityId CUSTOM_MODULE_ID_PROJECT_1 = EntityId.of(2000L);
	private static final EntityId CUSTOM_MODULE_ID_PROJECT_2 = EntityId.of(2003L);
	private static final String ANNOTATION5 = "Annotation 5";
	private static final String ANNOTATION6 = "Annotation 6";
	private static final String INITIAL_SOURCE_ATTACHMENT = "This is initial source attachment content";
	private static final String ADMIN_USER_ID = "admin";

	private final RestTemplate restTemplate = new RestTemplate();
	private final ConnectionInfo info = getConnectionInfo();
	private final AnnotationServiceProvider annotationServiceProvider = MiningApiClient.annotationService(getConnectionInfo());
	private final ModuleServiceProvider moduleServiceProvider = MiningApiClient.moduleService(getConnectionInfo());
	private final ParameterizedTypeReference<DataDictionaryPojo[]> responseType = new ParameterizedTypeReference<DataDictionaryPojo[]>() { };
	
	@Nullable
	private AnnotationPgDao annotationDao;
	
	@BeforeAll
	void init() {
		annotationDao = new AnnotationPgDao(getDataSource());
		
		TEST_ANNOTATION_1.setName("TEST ANNOTATION 1");
		TEST_ANNOTATION_1.setState(WorkingState.CANDIDATE);
		TEST_ANNOTATION_1.setType(TEST_ANNOTATION_TYPE);
		TEST_ANNOTATION_1.setSourceAttachment(INITIAL_SOURCE_ATTACHMENT);
		TEST_ANNOTATION_1.setEnglishTranslation("English translation 1");

		TEST_ANNOTATION_2.setName("TEST ANNOTATION 2");
		TEST_ANNOTATION_2.setState(WorkingState.FOR_REVIEW);
		TEST_ANNOTATION_2.setType(TEST_ANNOTATION_TYPE);
		TEST_ANNOTATION_2.setSourceAttachment(INITIAL_SOURCE_ATTACHMENT);
		TEST_ANNOTATION_2.setEnglishTranslation("English translation 2");

		TEST_ANNOTATION_3.setName("LONGANNOTATIONNAME");
		TEST_ANNOTATION_3.setState(WorkingState.APPROVED);
		TEST_ANNOTATION_3.setType(TEST_ANNOTATION_TYPE);
		TEST_ANNOTATION_3.setSourceAttachment(INITIAL_SOURCE_ATTACHMENT);
		TEST_ANNOTATION_3.setEnglishTranslation("English translation 3");

		TEST_ANNOTATION_4.setName("LONGERANNOTATIONNAME");
		TEST_ANNOTATION_4.setState(WorkingState.IN_ANALYSIS);
		TEST_ANNOTATION_4.setType(TEST_ANNOTATION_TYPE);
		TEST_ANNOTATION_4.setSourceAttachment(INITIAL_SOURCE_ATTACHMENT);
		TEST_ANNOTATION_4.setEnglishTranslation("English translation 4");
	}

	@Test
	void testFindAllAnnotationsOfCategory() throws IOException {
		final Result<AnnotationPojo[]> resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setCategoryIds(new Long[] {
				CATEGORY_C_ID, CATEGORY_B_ID
		}).execute();
		final AnnotationPojo[] allAnnotations = resultFindAll.getValue().get();
		assertEquals(HttpStatus.SC_OK, resultFindAll.getStatusCode());
		Assertions.assertTrue(resultContains(allAnnotations, ANNOTATION5));
		Assertions.assertTrue(resultContains(allAnnotations, ANNOTATION6));
		final Result<AnnotationPojo[]> resultFindAllWithLessCategories = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE)
				.setCategoryIds(new Long[] {
						CATEGORY_B_ID
				}).execute();
		final AnnotationPojo[] lessAnnotations = resultFindAllWithLessCategories.getValue().get();
		assertEquals(HttpStatus.SC_OK, resultFindAll.getStatusCode());
		Assertions.assertTrue(resultContains(lessAnnotations, ANNOTATION5));
		Assertions.assertFalse(resultContains(lessAnnotations, ANNOTATION6));
		for (final AnnotationPojo annotation : allAnnotations) {
			verifyNumberOfCustomProperties(annotation, NUMBER_OF_CUSTOM_PROPERTIES_ON_ANNOTATIONELEMENT_ENTITY);
		}

		final Result<AnnotationSearch> resultSearch = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setCategoryIds(new Long[] {
				CATEGORY_C_ID, CATEGORY_B_ID
		}).execute();
		final List<AnnotationPojo> searchedAnnotations = resultSearch.getValue().get().getElements();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		Assertions.assertTrue(resultContains(searchedAnnotations, ANNOTATION5));
		Assertions.assertTrue(resultContains(searchedAnnotations, ANNOTATION6));
		final Result<AnnotationSearch> resultSearchWithLessCategories = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE)
				.setCategoryIds(new Long[] {
						CATEGORY_B_ID
				}).execute();
		final List<AnnotationPojo> lessSearchedAnnotations = resultSearchWithLessCategories.getValue().get().getElements();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		Assertions.assertTrue(resultContains(lessSearchedAnnotations, ANNOTATION5));
		Assertions.assertFalse(resultContains(lessSearchedAnnotations, ANNOTATION6));
		for (final AnnotationPojo annotation : searchedAnnotations) {
			assertEquals(NUMBER_OF_CUSTOM_PROPERTIES_ON_ANNOTATIONELEMENT_ENTITY, annotation.getCustomProperties().size());
		}
	}

	@Test
	void testFindAllAnnotationsOfState() throws IOException {
		final Result<AnnotationPojo[]> resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setStates(new WorkingState[] {
				WorkingState.IN_ANALYSIS, WorkingState.REJECTED
		}).execute();
		final AnnotationPojo[] allAnnotations = resultFindAll.getValue().get();
		assertEquals(HttpStatus.SC_OK, resultFindAll.getStatusCode());
		Assertions.assertTrue(resultContains(allAnnotations, ANNOTATION5));
		Assertions.assertTrue(resultContains(allAnnotations, ANNOTATION6));
		final Result<AnnotationPojo[]> resultFindAllWithLessStates = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setStates(new WorkingState[] {
						WorkingState.IN_ANALYSIS
				}).execute();
		final AnnotationPojo[] lessAnnotations = resultFindAllWithLessStates.getValue().get();
		assertEquals(HttpStatus.SC_OK, resultFindAll.getStatusCode());
		Assertions.assertTrue(resultContains(lessAnnotations, ANNOTATION5));
		Assertions.assertFalse(resultContains(lessAnnotations, ANNOTATION6));
		
		final Result<AnnotationSearch> resultSearch = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setStates(new WorkingState[] {
				WorkingState.IN_ANALYSIS, WorkingState.REJECTED
		}).execute();
		final List<AnnotationPojo> allSearchedAnnotations = resultSearch.getValue().get().getElements();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		Assertions.assertTrue(resultContains(allSearchedAnnotations, ANNOTATION5));
		Assertions.assertTrue(resultContains(allSearchedAnnotations, ANNOTATION6));
		final Result<AnnotationSearch> resultSearchWithLessStates = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE)
				.setStates(new WorkingState[] {
						WorkingState.IN_ANALYSIS
				}).execute();
		final List<AnnotationPojo> lessSearchedAnnotations = resultSearchWithLessStates.getValue().get().getElements();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		Assertions.assertTrue(resultContains(lessSearchedAnnotations, ANNOTATION5));
		Assertions.assertFalse(resultContains(lessSearchedAnnotations, ANNOTATION6));
	}

	@Test
	void testFindAllAnnotationsOfModulePattern() throws IOException {
		/*
		 * the module for ANNOTATION5 is named: QBGPSLP1MMRS710A.STEP01.MMRS7102
		 * the module for ANNOTATION6 is named: MMRS7101
		 */
		Result<AnnotationPojo[]> resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("*").execute();
		AnnotationPojo[] foundAnnotations = resultFindAll.getValue().get();
		assertEquals(HttpStatus.SC_OK, resultFindAll.getStatusCode());
		Assertions.assertTrue(resultContains(foundAnnotations, ANNOTATION5));
		Assertions.assertTrue(resultContains(foundAnnotations, ANNOTATION6));
		
		resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("*2").execute();
		assertEquals(HttpStatus.SC_OK, resultFindAll.getStatusCode());
		foundAnnotations = resultFindAll.getValue().get();
		Assertions.assertTrue(resultContains(foundAnnotations, ANNOTATION5));
		Assertions.assertFalse(resultContains(foundAnnotations, ANNOTATION6));
		
		resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("QBGPSLP1MMRS710A.STEP01.MMRS7102").execute();
		assertEquals(HttpStatus.SC_OK, resultFindAll.getStatusCode());
		foundAnnotations = resultFindAll.getValue().get();
		Assertions.assertTrue(resultContains(foundAnnotations, ANNOTATION5));
		Assertions.assertFalse(resultContains(foundAnnotations, ANNOTATION6));
		
		resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("*MMRS710?").execute();
		assertEquals(HttpStatus.SC_OK, resultFindAll.getStatusCode());
		foundAnnotations = resultFindAll.getValue().get();
		Assertions.assertTrue(resultContains(foundAnnotations, ANNOTATION5));
		Assertions.assertTrue(resultContains(foundAnnotations, ANNOTATION6));
		
		resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("mmrs710?").execute();
		assertEquals(HttpStatus.SC_OK, resultFindAll.getStatusCode());
		foundAnnotations = resultFindAll.getValue().get();
		Assertions.assertFalse(resultContains(foundAnnotations, ANNOTATION5));
		Assertions.assertTrue(resultContains(foundAnnotations, ANNOTATION6));
		
		resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("????????").execute();
		assertEquals(HttpStatus.SC_OK, resultFindAll.getStatusCode());
		foundAnnotations = resultFindAll.getValue().get();
		Assertions.assertFalse(resultContains(foundAnnotations, ANNOTATION5));
		Assertions.assertTrue(resultContains(foundAnnotations, ANNOTATION6));

		Result<AnnotationSearch> resultSearch = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("*").execute();
		List<AnnotationPojo> foundSearchedAnnotations = resultSearch.getValue().get().getElements();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		Assertions.assertTrue(resultContains(foundSearchedAnnotations, ANNOTATION5));
		Assertions.assertTrue(resultContains(foundSearchedAnnotations, ANNOTATION6));
		
		resultSearch = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("*2").execute();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		foundSearchedAnnotations = resultSearch.getValue().get().getElements();
		Assertions.assertTrue(resultContains(foundSearchedAnnotations, ANNOTATION5));
		Assertions.assertFalse(resultContains(foundSearchedAnnotations, ANNOTATION6));
		
		resultSearch = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("QBGPSLP1MMRS710A.STEP01.MMRS7102").execute();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		foundSearchedAnnotations = resultSearch.getValue().get().getElements();
		Assertions.assertTrue(resultContains(foundSearchedAnnotations, ANNOTATION5));
		Assertions.assertFalse(resultContains(foundSearchedAnnotations, ANNOTATION6));
		
		resultSearch = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("*MMRS710?").execute();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		foundSearchedAnnotations = resultSearch.getValue().get().getElements();
		Assertions.assertTrue(resultContains(foundSearchedAnnotations, ANNOTATION5));
		Assertions.assertTrue(resultContains(foundSearchedAnnotations, ANNOTATION6));
		
		resultSearch = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("mmrs710?").execute();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		foundSearchedAnnotations = resultSearch.getValue().get().getElements();
		Assertions.assertFalse(resultContains(foundSearchedAnnotations, ANNOTATION5));
		Assertions.assertTrue(resultContains(foundSearchedAnnotations, ANNOTATION6));
		
		resultSearch = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("????????").execute();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		foundSearchedAnnotations = resultSearch.getValue().get().getElements();
		Assertions.assertFalse(resultContains(foundSearchedAnnotations, ANNOTATION5));
		Assertions.assertTrue(resultContains(foundSearchedAnnotations, ANNOTATION6));
	}
	
	@Test()
	void testFindAllAnnotationsWithPathAndPatternThrowsAnException() {
		final FindAllAnnotations findAllAnnotations = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("*").setModulePath("PATH");
		Assertions.assertThrows(IllegalStateException.class, findAllAnnotations::execute);
		final SearchAnnotations searchAnnotations = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setModuleNamePattern("*").setModulePath("PATH");
		Assertions.assertThrows(IllegalStateException.class, searchAnnotations::execute);
	}

	@Test
	void testFindAllAnnotationsWithValidPathReturnsAnnotations() throws IOException {
		final Result<AnnotationPojo[]> result = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setModulePath("src/cobol/programs/MMRS7101.cbl")
				.execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		Assertions.assertEquals(1, result.getValue().get().length);
		Assertions.assertTrue(resultContains(result.getValue().get(), ANNOTATION6));
		
		final Result<AnnotationSearch> searchResult = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE)
				.setModulePath("src/cobol/programs/MMRS7101.cbl").execute();
		assertEquals(HttpStatus.SC_OK, searchResult.getStatusCode());
		Assertions.assertEquals(1, searchResult.getValue().get().getCurrentSize());
		Assertions.assertTrue(resultContains(searchResult.getValue().get().getElements(), ANNOTATION6));
	}

	@Test
	void testFindAllAnnotationsWithValidPathButWithoutAnnotationsAssociatedReturnsEmptyList() throws IOException {
		final Result<AnnotationPojo[]> result = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setModulePath("src/cobol/programs/MMRS7102.cbl")
				.execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		Assertions.assertEquals(0, result.getValue().get().length);
		
		final Result<AnnotationSearch> searchResult = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE)
				.setModulePath("src/cobol/programs/MMRS7102.cbl").execute();
		assertEquals(HttpStatus.SC_OK, searchResult.getStatusCode());
		Assertions.assertEquals(0, searchResult.getValue().get().getCurrentSize());
	}

	@Test
	void testFindAllAnnotationsWithNonExistingPathReturnsEmptyList() throws IOException {
		final Result<AnnotationPojo[]> result = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).setModulePath("NON/EXISTING/PATH").execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		Assertions.assertEquals(0, result.getValue().get().length);
		
		final Result<AnnotationSearch> searchResult = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setModulePath("NON/EXISTING/PATH")
				.execute();
		assertEquals(HttpStatus.SC_OK, searchResult.getStatusCode());
		Assertions.assertEquals(0, searchResult.getValue().get().getCurrentSize());
	}

	@Test
	void testFindAllAnnotationsWithDatabase() throws IOException {
		final Result<AnnotationPojo[]> resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).execute();
		verifyFindAll(resultFindAll.getValue().get(), Objects.requireNonNull(annotationDao).find(q -> q.ofProject(ENTITY_ONE)));
	}

	@Test
	void testFindAllAnnotationsWithDatabaseAndNewAnnotation() throws IOException {
		final Result<AnnotationPojo[]> resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).execute();
		createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo[]> resultFindAll2 = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).execute();
		final AnnotationPojo[] allAnnotations = resultFindAll2.getValue().get();
		Assertions.assertEquals(allAnnotations.length, resultFindAll.getValue().get().length + 1);
		Assertions.assertTrue(resultContains(allAnnotations, TEST_ANNOTATION_1.name.getNonNull()));
		verifyFindAll(allAnnotations, Objects.requireNonNull(annotationDao).find(q -> q.ofProject(ENTITY_ONE)));

		final Result<AnnotationSearch> searchResult = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).execute();
		createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1, null);
		final Result<AnnotationSearch> searchResult2 = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).execute();
		final List<AnnotationPojo> allSearchedAnnotations = searchResult2.getValue().get().getElements();
		Assertions.assertEquals(allSearchedAnnotations.size(), searchResult.getValue().get().getCurrentSize() + 1);
		Assertions.assertTrue(resultContains(allSearchedAnnotations, TEST_ANNOTATION_1.name.getNonNull()));
	}

	@Test
	void testFindById() throws IOException {
		final AnnotationPojo annotationExpected = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo> findResult = annotationServiceProvider.findAnnotationById().setProjectId(ENTITY_ONE).setAnnotationId(annotationExpected.identity())
				.execute();
		assertEquals(HttpStatus.SC_OK, findResult.getStatusCode());
		final AnnotationPojo annotationActual = findResult.getValue().get();
		verifyAnnotationAllFields(annotationExpected, annotationActual, annotationExpected.getSourceAttachment().map(BinaryString::toString).orElse(null));
		verifyNumberOfCustomProperties(annotationActual, NUMBER_OF_CUSTOM_PROPERTIES_BEING_CREATED_ON_ANNOTATION_ENTITY);
	}

	@Test
	void testLinkedBusinessVariables() {
		final ResponseEntity<DataDictionaryPojo[]> validResult = restTemplate.exchange(
				info.getUrl() + LINKED_BUSINESS_VARIABLES_URL, HttpMethod.GET,
				new HttpEntity<>(getHttpHeaders(info)),
				responseType,
				ENTITY_ONE.getNid(), Long.valueOf(6));
		assertEquals(200, validResult.getStatusCodeValue());
		final DataDictionaryPojo[] foundLinkedBusinessVariables =  validResult.getBody();
		assertNotNull(foundLinkedBusinessVariables);
		assertEquals(3, foundLinkedBusinessVariables.length);
		assertTrue("Should contain Data Dictionary will element name MY-PROGRAM-NAME, MY-BIN-FIELDS, MY-HEX-ORIGIN-LEN",
				Arrays.asList(foundLinkedBusinessVariables).stream().map(DataDictionaryPojo::getName).collect(Collectors.toList())
						.containsAll(Arrays.asList("MY-PROGRAM-NAME", "MY-BIN-FIELDS", "MY-HEX-ORIGIN-LEN")));
	}

	@Test
	void testFindByIdNonExisting() throws IOException {
		final Result<AnnotationPojo> findResult = annotationServiceProvider.findAnnotationById().setProjectId(ENTITY_ONE).setAnnotationId(NON_EXISTING_ID).execute();
		assertEquals(404, findResult.getStatusCode());
	}

	@Test
	void testFindDistinctTypesNonExisting() throws IOException {
		final Result<AnnotationType[]> findResult = annotationServiceProvider.findDistinctAnnotationTypes().setProjectId(FIVE).execute();
		assertEquals(404, findResult.getStatusCode());
	}

	@Test
	void testFindDistinctTypesOfExisting() throws IOException {
		final Result<AnnotationType[]> findResult = annotationServiceProvider.findDistinctAnnotationTypes().setProjectId(ENTITY_ONE).execute();
		assertNotNull(findResult.getValue().get());
		final AnnotationType[] annotationTypes = findResult.getValue().get();
		assertEquals(3, annotationTypes.length);
		assertTrue(Arrays.asList(annotationTypes).contains(AnnotationType.RULE));
		assertTrue(Arrays.asList(annotationTypes).contains(AnnotationType.DATABASE));
		assertTrue(Arrays.asList(annotationTypes).contains(AnnotationType.FUNCTIONAL));
	}

	@Test
	void testFindByIdNonExistingInProject() throws IOException {
		final Result<AnnotationPojo> findResult = annotationServiceProvider.findAnnotationById().setProjectId(ENTITY_ONE)
				.setAnnotationId(createAnnotation(ENTITY_TWO, CUSTOM_MODULE_ID_PROJECT_2, TEST_ANNOTATION_2).identity()).execute();
		assertEquals(404, findResult.getStatusCode());
	}

	@Test
	void testFindModule() throws IOException {
		final Result<ModulePojo> findResult = annotationServiceProvider.findModule().setProjectId(ENTITY_ONE).setAnnotationId(ENTITY_ONE).execute();
		assertEquals(HttpStatus.SC_OK, findResult.getStatusCode());
		final ModulePojo module = findResult.getValue().get();
		assertEquals("PRG1", module.getName());
		verifyNumberOfCustomProperties(module, 2);
	}

	@Test
	void testFindModuleNonExistingAnnotation() throws IOException {
		final Result<ModulePojo> findResult = annotationServiceProvider.findModule().setProjectId(ENTITY_ONE).setAnnotationId(NON_EXISTING_ID).execute();
		assertEquals(404, findResult.getStatusCode());
	}

	// It Should Be Enabled After the Postgres Migration of Annotation and While fixing WMIN-10260
	@Disabled("Should be enable while fixing WMIN-10260")
	@Test
	void testFindByName() throws IOException {

		final AnnotationPojo t1 = createTestAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final AnnotationPojo t2 = createTestAnnotation(ENTITY_TWO, CUSTOM_MODULE_ID_PROJECT_2, TEST_ANNOTATION_2);
		final AnnotationPojo t3 = createTestAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_3);
		final AnnotationPojo t4 = createTestAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_4);

		String searchName;
		Result<AnnotationPojo[]> findAnnotationResult;
		AnnotationPojo[] result;

		/* exact match */
		searchName = "TEST ANNOTATION 1";
		findAnnotationResult = annotationServiceProvider.findAnnotationByName().setProjectId(ENTITY_ONE).setName(searchName).execute();
		assertEquals(HttpStatus.SC_OK, findAnnotationResult.getStatusCode());
		result = findAnnotationResult.getValue().get();
		assertNotEquals(0, result.length);
		assertEquals(t1.getId(), result[0].getId());

		/* hyphenation */
		searchName = "TEST-ANNOTATION 1";
		findAnnotationResult = annotationServiceProvider.findAnnotationByName().setProjectId(ENTITY_ONE).setName(searchName).execute();
		assertEquals(HttpStatus.SC_OK, findAnnotationResult.getStatusCode());
		result = findAnnotationResult.getValue().get();
		assertNotEquals(0, result.length);
		assertEquals(t1.getId(), result[0].getId());

		/* no match */
		searchName = "gibberish";
		findAnnotationResult = annotationServiceProvider.findAnnotationByName().setProjectId(ENTITY_ONE).setName(searchName).execute();
		assertEquals(HttpStatus.SC_OK, findAnnotationResult.getStatusCode());
		result = findAnnotationResult.getValue().get();
		assertEquals(0, result.length);

		/* fuzzy search */
		searchName = "longannotationname";
		findAnnotationResult = annotationServiceProvider.findAnnotationByName().setProjectId(ENTITY_ONE).setName(searchName).execute();
		assertEquals(HttpStatus.SC_OK, findAnnotationResult.getStatusCode());
		result = findAnnotationResult.getValue().get();
		resultContains(result, t4.getName());
		resultContains(result, t3.getName());

		/* case insensitive */
		searchName = "AnnOTatiOn";
		findAnnotationResult = annotationServiceProvider.findAnnotationByName().setProjectId(ENTITY_TWO).setName(searchName).execute();
		assertEquals(HttpStatus.SC_OK, findAnnotationResult.getStatusCode());
		result = findAnnotationResult.getValue().get();
		resultContains(result, t2.getName());
	}

	@Test
	void testCreateAnnotation() throws IOException {
		verifyAnnotationWithoutIdAndRecord(TEST_ANNOTATION_1, createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1, null));
	}

	@Test
	void testCreateAnnotationWithSource() throws IOException {
		final AnnotationPojoPrototype newAnnotation = createMinimalAnnotation();
		newAnnotation.setSourceAttachment("SOURCE");
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_2).setProjectId(ENTITY_TWO).execute();
		assertEquals(SC_CREATED, annotationResult.getStatusCode());
		verifyAnnotationWithoutIdAndRecord(newAnnotation, annotationResult.getValue().get());
	}

	@Test
	void testCreateAnnotationWithSourceAndCategory() throws IOException {
		final AnnotationPojoPrototype newAnnotation = createMinimalAnnotation();
		newAnnotation.setSourceAttachment("SOURCE");
		newAnnotation.setCategoryId(1001L);
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_2).setProjectId(ENTITY_TWO).execute();
		assertEquals(SC_CREATED, annotationResult.getStatusCode());
		verifyAnnotationWithoutIdAndRecord(newAnnotation, annotationResult.getValue().get());
	}

	@Test
	void testCreateEmptyAnnotation() throws IOException {
		final AnnotationPojoPrototype newAnnotation = new AnnotationPojoPrototype();
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_2).setProjectId(ENTITY_TWO).execute();
		assertEquals(400, annotationResult.getStatusCode());
		assertFalse(annotationResult.getValue().isPresent());
	}

	@Test
	void testCreateAnnotationWithNameThatExists() throws IOException {
		final AnnotationPojoPrototype newAnnotation = createMinimalAnnotation();
		annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation).setModuleId(CUSTOM_MODULE_ID_PROJECT_1).setProjectId(ENTITY_ONE).execute();
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_1).setProjectId(ENTITY_ONE).execute();
		assertEquals(SC_CREATED, annotationResult.getStatusCode());
	}

	@Test
	void testCreateAnnotationWithNameThatExistsInDifferentProject() throws IOException {
		final AnnotationPojoPrototype newAnnotation = createMinimalAnnotation();
		annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation).setModuleId(CUSTOM_MODULE_ID_PROJECT_1).setProjectId(ENTITY_ONE).execute();
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_2).setProjectId(ENTITY_TWO).execute();
		assertEquals(SC_CREATED, annotationResult.getStatusCode());
	}

	@Test
	void testCreateAnnotationWithoutName() throws IOException {
		final AnnotationPojoPrototype newAnnotation = new AnnotationPojoPrototype();
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_2).setProjectId(ENTITY_TWO).execute();
		assertEquals(400, annotationResult.getStatusCode());
		assertFalse(annotationResult.getValue().isPresent());
	}

	@Test
	void testCreateAnnotationWithoutProject() throws IOException {
		final AnnotationPojoPrototype newAnnotation = new AnnotationPojoPrototype();
		newAnnotation.setName("I HAVE A NAME");
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_1).setProjectId(ENTITY_ONE).execute();
		assertEquals(400, annotationResult.getStatusCode());
		assertFalse(annotationResult.getValue().isPresent());
	}

	@Test
	void testCreateAnnotationWithoutStateAndType() throws IOException {
		final AnnotationPojoPrototype newAnnotation = new AnnotationPojoPrototype();
		newAnnotation.setName("I HAVE A NAME");
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_1).setProjectId(ENTITY_ONE).execute();
		assertEquals(400, annotationResult.getStatusCode());
		assertFalse(annotationResult.getValue().isPresent());
	}

	@Test
	void testCreateAnnotationWithoutState() throws IOException {
		final AnnotationPojoPrototype newAnnotation = new AnnotationPojoPrototype();
		newAnnotation.setName("I HAVE A NAME");
		newAnnotation.setType(TEST_ANNOTATION_TYPE);
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_1).setProjectId(ENTITY_ONE).execute();
		assertEquals(400, annotationResult.getStatusCode());
		assertFalse(annotationResult.getValue().isPresent());
	}

	@Test
	void testCreateAnnotationWithoutType() throws IOException {
		final AnnotationPojoPrototype newAnnotation = new AnnotationPojoPrototype();
		newAnnotation.setName("I HAVE A NAME");
		newAnnotation.setState(WorkingState.INVALID);
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_1).setProjectId(ENTITY_ONE).execute();
		assertEquals(400, annotationResult.getStatusCode());
		assertFalse(annotationResult.getValue().isPresent());
	}

	@Test
	void testCreateAnnotationWithWrongProjectId() throws IOException {
		final AnnotationPojoPrototype newAnnotation = new AnnotationPojoPrototype();
		newAnnotation.setName("I HAVE A NAME");
		newAnnotation.setState(WorkingState.INVALID);
		newAnnotation.setType(TEST_ANNOTATION_TYPE);
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_1).setProjectId(ENTITY_ONE).execute();
		assertEquals(400, annotationResult.getStatusCode());
		assertFalse(annotationResult.getValue().isPresent());
	}

	@Test
	void testUpdateAnnotation() throws IOException {
		final AnnotationPojo annotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final String updateSourceAttachment = "I HAVE SOURCE";
		final Result<AnnotationPojo> annotationUpdateResult = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_ONE)
				.setAnnotation(new AnnotationPojoPrototype()
					.withId(annotation.identity())
					.setCategoryId(1001L)
					.setSourceAttachment(updateSourceAttachment)
					.setState(WorkingState.REJECTED)
					.setEnglishTranslation("englishTranslation")
					.setCustomProperties(new NestedMap()
							.set(CustomPropertyClass.AnnotationCustomProperties.name(),"customAnnotationProperty", "Updated value"))
				).execute();
		assertEquals(HttpStatus.SC_OK, annotationUpdateResult.getStatusCode());
		final AnnotationPojo updatedAnnotation = annotationUpdateResult.getValue().get();
		verifyAnnotationAllFields(Objects.requireNonNull(annotationDao).findAny(q -> q.byId(annotation.identity())).orElseThrow(),
				updatedAnnotation, updateSourceAttachment);
		final Object updatedCustomProperty = getCustomPropertyByName("customAnnotationProperty", updatedAnnotation);
		assertEquals("Updated value", updatedCustomProperty);
	}

	@Test
	void testUpdateAnnotation2() throws IOException {
		final AnnotationPojo annotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo> annotationUpdateResult = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_ONE)
				.setAnnotation(new AnnotationPojoPrototype().withId(annotation.identity())).execute();
		assertEquals(HttpStatus.SC_OK, annotationUpdateResult.getStatusCode());
		verifyAnnotationAllFields(annotation, annotationUpdateResult.getValue().get(), annotation.getSourceAttachment().map(BinaryString::toString).orElse(null));
	}

	@Test
	void testUpdateAnnotationInWrongProject() throws IOException {
		final AnnotationPojo annotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo> annotationUpdateResult = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_TWO)
				.setAnnotation(new AnnotationPojoPrototype().withId(annotation.identity())).execute();
		assertEquals(400, annotationUpdateResult.getStatusCode());
		assertFalse(annotationUpdateResult.getValue().isPresent());
	}

	@Test
	void testUpdateAnnotationNonExisting() throws IOException {
		final AnnotationPojoPrototype annotation = createMinimalAnnotation();
		annotation.setNid(NON_EXISTING_ID.getNid());
		final Result<AnnotationPojo> annotationUpdateResult = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_ONE)
				.setAnnotation(annotation).execute();
		assertEquals(404, annotationUpdateResult.getStatusCode());
		assertFalse(annotationUpdateResult.getValue().isPresent());
	}

	@Test
	void testUpdateAnnotationWithoutContent() throws IOException {
		final AnnotationPojo newAnnotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo> annotationUpdateResult = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_ONE)
				.setAnnotation(new AnnotationPojoPrototype().withId(newAnnotation.identity())
				).execute();
		assertEquals(200, annotationUpdateResult.getStatusCode());
		assertTrue(annotationUpdateResult.getValue().isPresent());
	}

	@Test
	void testUpdateAnnotationWithOnlyName() throws IOException {
		final AnnotationPojo annotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo> annotationUpdateResult = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_ONE)
				.setAnnotation(new AnnotationPojoPrototype().withId(annotation.identity())
					.setName("I HAVE A NAME")
				).execute();
		assertEquals(200, annotationUpdateResult.getStatusCode());
		assertTrue(annotationUpdateResult.getValue().isPresent());
	}

	@Test
	void testUpdateAnnotationWithOnlyState() throws IOException {
		final AnnotationPojo annotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo> annotationUpdateResult = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_ONE)
				.setAnnotation(new AnnotationPojoPrototype().withId(annotation.identity())
					.setState(WorkingState.APPROVED)
				).execute();
		assertEquals(200, annotationUpdateResult.getStatusCode());
		assertTrue(annotationUpdateResult.getValue().isPresent());
	}

	@Test
	void testUpdateAnnotationWithOnlyType() throws IOException {
		final AnnotationPojo annotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo> annotationUpdateResult = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_ONE)
				.setAnnotation(new AnnotationPojoPrototype().withId(annotation.identity())
					.setType(TEST_ANNOTATION_TYPE)
				).execute();
		assertEquals(200, annotationUpdateResult.getStatusCode());
		assertTrue(annotationUpdateResult.getValue().isPresent());
	}

	@Test
	void testUpdateAnnotationWithoutName() throws IOException {
		final AnnotationPojo annotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo> annotationUpdateResult = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_ONE)
				.setAnnotation(new AnnotationPojoPrototype().withId(annotation.identity())
					.setState(WorkingState.APPROVED)
					.setType(TEST_ANNOTATION_TYPE)
				).execute();
		assertEquals(200, annotationUpdateResult.getStatusCode());
		assertTrue(annotationUpdateResult.getValue().isPresent());
	}

	@Test
	void testUpdateAnnotationWithoutState() throws IOException {
		final AnnotationPojo annotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo> annotationUpdateResult = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_ONE)
				.setAnnotation(new AnnotationPojoPrototype().withId(annotation.identity())
					.setName("I HAVE A NAME")
					.setType(TEST_ANNOTATION_TYPE)
				).execute();
		assertEquals(200, annotationUpdateResult.getStatusCode());
		assertTrue(annotationUpdateResult.getValue().isPresent());
	}

	@Test
	void testUpdateAnnotationWithoutType() throws IOException {
		final AnnotationPojo newAnnotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo> annotationUpdateResult = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_ONE)
			.setAnnotation(new AnnotationPojoPrototype().withId(newAnnotation.identity())
				.setName("I HAVE A NAME")
				.setState(WorkingState.APPROVED)
			).execute();
		assertEquals(200, annotationUpdateResult.getStatusCode());
		assertTrue(annotationUpdateResult.getValue().isPresent());
	}

	@Test
	void testDeleteAnnotation() throws IOException {
		final Result<AnnotationPojo[]> resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).execute();
		final AnnotationPojo[] findAllBeginning = resultFindAll.getValue().get();
		final AnnotationPojo newCreatedAnnotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo[]> resultFindAll2 = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).execute();
		final AnnotationPojo[] allAnnotations = resultFindAll2.getValue().get();
		Assertions.assertEquals(allAnnotations.length, findAllBeginning.length + 1);
		Assertions.assertTrue(resultContains(allAnnotations, TEST_ANNOTATION_1.name.getNonNull()));
		final Result<Void> annotationDeleteResult = annotationServiceProvider.deleteAnnotation().setAnnotationId(newCreatedAnnotation.identity()).setProjectId(ENTITY_ONE)
				.execute();
		assertEquals(204, annotationDeleteResult.getStatusCode());
		final Result<AnnotationPojo[]> resultFindAll3 = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).execute();
		final AnnotationPojo[] allAnnotationsEnd = resultFindAll3.getValue().get();
		Assertions.assertEquals(allAnnotationsEnd.length, findAllBeginning.length);
		Assertions.assertFalse(resultContains(allAnnotationsEnd, TEST_ANNOTATION_1.name.getNonNull()));
		verifyFindAll(findAllBeginning, Objects.requireNonNull(annotationDao).find(q -> q.ofProject(ENTITY_ONE)));
	}

	@Test
	void testDeleteAnnotationWithFromAndToReference() throws IOException {
		final Result<AnnotationPojo[]> annotationResult = annotationServiceProvider.findAnnotationByName().setProjectId(ENTITY_ONE).setName("DeleteAnnotation").execute();
		assertEquals(HttpStatus.SC_OK, annotationResult.getStatusCode());
		final AnnotationPojo[] annotations = annotationResult.getValue().get();
		assertEquals(1, annotations.length);

		assertLocation(annotations[0].getLocation().orElse(null), 677, 7);

		final Result<Void> annotationDeleteResult = annotationServiceProvider.deleteAnnotation().setAnnotationId(annotations[0].identity()).setProjectId(ENTITY_ONE)
				.execute();
		assertEquals(204, annotationDeleteResult.getStatusCode());
	}

	@Test
	void testDeleteAnnotationWithCustomProperties() throws IOException {
		final String propertyValue = "A value for the custom Annotation property";
		final Result<AnnotationPojo[]> findAnnotationResult = annotationServiceProvider.findAnnotationByName().setProjectId(ENTITY_ONE).setName("Annotation 1").execute();
		assertEquals("Annotation find by name should return Success code", HttpStatus.SC_OK, findAnnotationResult.getStatusCode());
		final AnnotationPojo[] foundAnnotation = findAnnotationResult.getValue().get();
		final AnnotationPojo annotation = foundAnnotation[0];
		final Object customProperty = getCustomPropertyByName("customAnnotationProperty", annotation);
		assertEquals("Custom property value should match", propertyValue, customProperty);
		final Result<Void> annotationDeleteResult = annotationServiceProvider.deleteAnnotation().setAnnotationId(annotation.identity()).setProjectId(ENTITY_ONE)
				.execute();
		assertEquals("Annotation delete should return Success code", HttpStatus.SC_NO_CONTENT, annotationDeleteResult.getStatusCode());
	}

	@Test
	void testDeleteAnnotationNonExisting() throws IOException {
		final Result<Void> annotationDeleteResult = annotationServiceProvider.deleteAnnotation().setAnnotationId(NON_EXISTING_ID).setProjectId(ENTITY_ONE).execute();
		assertEquals(404, annotationDeleteResult.getStatusCode());
	}

	@Test
	void testDeleteAnnotationWrongProject() throws IOException {
		final AnnotationPojo createdAnnotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final EntityId id = createdAnnotation.identity();
		final Result<Void> annotationDeleteResult = annotationServiceProvider.deleteAnnotation().setAnnotationId(id).setProjectId(ENTITY_TWO).execute();
		assertEquals(404, annotationDeleteResult.getStatusCode());
		final Result<AnnotationPojo> findResult = annotationServiceProvider.findAnnotationById().setProjectId(ENTITY_ONE).setAnnotationId(id).execute();
		assertEquals(HttpStatus.SC_OK, findResult.getStatusCode());
		verifyAnnotationAllFields(createdAnnotation, findResult.getValue().get(), createdAnnotation.getSourceAttachment().map(BinaryString::toString).orElse(null));
	}

	@Test
	void testCreatedByUserIdAvailableOnExisting() throws IOException {
		final Result<AnnotationPojo[]> annotations = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).execute();
		for (final AnnotationPojo annotation : annotations.getValue().orElseThrow(IllegalStateException::new)) {
			assertTrue(StringUtils.isNotBlank(annotation.getCreatedByUserId()));
		}
	}

	@Test
	void testCreatedByUserIdAvailable() throws IOException {
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(TEST_ANNOTATION_1)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_1).setProjectId(ENTITY_ONE).execute();
		final AnnotationPojo newAnnotation = annotationResult.getValue().orElseThrow(IllegalStateException::new);
		assertEquals(ADMIN_USER_ID, newAnnotation.getCreatedByUserId());

		final Result<AnnotationPojo> findAnnotationResult = annotationServiceProvider.findAnnotationById()
				.setProjectId(ENTITY_ONE).setAnnotationId(newAnnotation.identity()).execute();
		assertEquals(ADMIN_USER_ID, findAnnotationResult.getValue().orElseThrow(IllegalStateException::new).getCreatedByUserId());
	}

	@Test
	void testUpdatedUserInitNull() throws IOException {
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(TEST_ANNOTATION_1)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_1).setProjectId(ENTITY_ONE).execute();
		final AnnotationPojo newAnnotation = annotationResult.getValue().orElseThrow(IllegalStateException::new);
		assertTrue(newAnnotation.getUpdatedByUserId().isEmpty());
	}

	@Test
	void testUpdatedUserSetAfterUpdate() throws IOException {
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(TEST_ANNOTATION_1)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_1).setProjectId(ENTITY_ONE).execute();
		final AnnotationPojo newAnnotation = annotationResult.getValue().orElseThrow(IllegalStateException::new);

		final Result<AnnotationPojo> annotationResultUpdate = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_ONE)
			.setAnnotation(new AnnotationPojoPrototype().withId(newAnnotation.identity())
				.setName("Changed content")
			).execute();
		assertTrue(annotationResult.getStatusMessage(), annotationResultUpdate.isValid());

		final Result<AnnotationPojo> annotationResultRead = annotationServiceProvider.findAnnotationById()
				.setProjectId(ENTITY_ONE).setAnnotationId(newAnnotation.identity()).execute();
		final AnnotationPojo updatedAnnotation = annotationResultRead.getValue().orElseThrow(IllegalStateException::new);
		assertEquals(ADMIN_USER_ID, updatedAnnotation.getUpdatedByUserId().get());
	}

	@Test
	void testCreatedByUserIdNotRequiredOnUpdate() throws IOException {
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation()
				.setAnnotation(TEST_ANNOTATION_1.setLocation(new ModuleLocation(10, 20)))
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_1).setProjectId(ENTITY_ONE).execute();
		final EntityId annotationId = annotationResult.getValue().orElseThrow(IllegalStateException::new).identity();

		final Result<AnnotationPojo> findAnnotationResult = annotationServiceProvider.findAnnotationById()
				.setProjectId(ENTITY_ONE).setAnnotationId(annotationId).execute();
		final AnnotationPojo findAnnotation = findAnnotationResult.getValue().orElseThrow(IllegalStateException::new);
		assertEquals(ADMIN_USER_ID, findAnnotation.getCreatedByUserId());

		final Result<AnnotationPojo> update = annotationServiceProvider.updateAnnotation().setProjectId(ENTITY_ONE)
			.setAnnotation(new AnnotationPojoPrototype().withId(findAnnotation.identity())
				.setCreatedByUserId("")
			).execute();
		assertTrue(update.isValid());

		final Result<AnnotationPojo> findAnnotationResultAfterUpdate = annotationServiceProvider.findAnnotationById().setProjectId(ENTITY_ONE)
				.setAnnotationId(annotationId).execute();
		assertEquals(ADMIN_USER_ID, findAnnotationResultAfterUpdate.getValue().orElseThrow(IllegalStateException::new).getCreatedByUserId());
	}

	@Test
	void testFindAnnotationIncludesCustomProperties() throws IOException {
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.findAnnotationById().setProjectId(ENTITY_ONE).setAnnotationId(ENTITY_ONE).execute();
		final AnnotationPojo annotation = annotationResult.getValue().orElseThrow(IllegalStateException::new);

		verifyNumberOfCustomProperties(annotation, 3);
	}

	@Test
	void annotationSearchResultCanBeLimitedInSizeWithMoreRecords() throws IOException {
		final Result<AnnotationSearch> resultSearch = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setCategoryIds(new Long[] {
				CATEGORY_C_ID, CATEGORY_B_ID
		}).setSize(2L).execute();
		final AnnotationSearch search = resultSearch.getValue().get();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		assertEquals(2, search.getCurrentSize());
		assertTrue(search.hasMore());
		assertEquals(5, search.getOverallSize().intValue());
	}

	@Test
	void annotationSearchResultCanBeLimitedInSizeWithoutMoreRecords() throws IOException {
		final Result<AnnotationSearch> resultSearch = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setCategoryIds(new Long[] {
				CATEGORY_C_ID, CATEGORY_B_ID
		}).setSize(FIVE).execute();
		final AnnotationSearch search = resultSearch.getValue().get();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		assertEquals(FIVE.intValue(), search.getCurrentSize());
		assertFalse(search.hasMore());
		assertEquals(FIVE.intValue(), search.getOverallSize().intValue());
	}

	@Test
	void annotationSearchResultFetchAllRecords() throws IOException {
		final Result<AnnotationSearch> resultSearch = annotationServiceProvider.searchAnnotations().setProjectId(ENTITY_ONE).setCategoryIds(new Long[] {
				CATEGORY_C_ID, CATEGORY_B_ID
		}).execute();
		final AnnotationSearch search = resultSearch.getValue().get();
		assertEquals(HttpStatus.SC_OK, resultSearch.getStatusCode());
		assertEquals(FIVE.intValue(), search.getCurrentSize());
		assertFalse(search.hasMore());
		assertEquals(FIVE.intValue(), search.getOverallSize().intValue());
	}

	@Test
	void annotationStatePartialUpdate() throws IOException {
		final AnnotationPojo annotationExpected = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);
		final Result<AnnotationPojo> findResult = annotationServiceProvider.findAnnotationById()
				.setProjectId(ENTITY_ONE).setAnnotationId(annotationExpected.identity()).execute();
		final AnnotationPojo annotation = findResult.getValue().get();

		final WorkingState initialState = annotation.getState();
		final Long initialCategoryId = annotation.getCategoryId().orElse(null);
		final String initialName = annotation.getName();
		final AnnotationType initialType = annotation.getType();
		assertEquals(WorkingState.CANDIDATE, initialState);
		assertEquals(AnnotationType.RULE, initialType);
		
		final Result<Void> partialUpdateResult = annotationServiceProvider.updateAnnotationState().setProjectId(ENTITY_ONE)
			.setAnnotation(new AnnotationPojoPrototype().withId(annotation.identity())
				/* change the state */
				.setState(WorkingState.IN_ANALYSIS)
				/* also change other properties which should not be changed by the partial update call */
				.setCategoryId(99L)
				.setName("BOGUS NAME")
				.setType(AnnotationType.DATABASE)
			).execute();
		assertEquals(HttpStatus.SC_NO_CONTENT, partialUpdateResult.getStatusCode());
		assertFalse(partialUpdateResult.getValue().isPresent());

		/* Make sure the state was change and the other properties still have their initial value */
		final Result<AnnotationPojo> findResultAfterPartialUpdate = annotationServiceProvider.findAnnotationById()
				.setProjectId(ENTITY_ONE).setAnnotationId(annotationExpected.identity()).execute();
		final AnnotationPojo annotationAfterPartialStateUpdate = findResultAfterPartialUpdate.getValue().get();
		assertEquals(WorkingState.IN_ANALYSIS, annotationAfterPartialStateUpdate.getState());
		assertEquals(initialType, annotationAfterPartialStateUpdate.getType());
		assertEquals(initialName, annotationAfterPartialStateUpdate.getName());
		assertEquals(initialCategoryId, annotationAfterPartialStateUpdate.getCategoryId().orElse(null));
	}

	@Test
	void testCreateAnnotationWithReference() throws IOException {
		final AnnotationPojo annotation = createAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1);

		final Result<ModulePojo> moduleResult = moduleServiceProvider.findModuleById().setProjectId(ENTITY_ONE)
				.setModuleId(CUSTOM_MODULE_ID_PROJECT_1).execute();
		assertEquals(HttpStatus.SC_OK, moduleResult.getStatusCode());
		assertTrue(moduleResult.getValue().isPresent());

		assertEquals(moduleResult.getValue().get().identity(), annotation.getModule());
		assertTrue(annotation.getLocation().isPresent());
		assertLocation(annotation.getLocation().orElse(null), 10, 20);
	}

	@Test
	void createAnnotationWithoutSourceAttachmentHasSourceWhenModuleHasSource() throws IOException {
		final AnnotationPojo annotation = createAnnotationWithoutSourceAttachment("/innowake/mining/test/annotation/source-extraction/MMRS7120.cbl", true);
		assertEquals("NTIFICATION DIVISION", annotation.getSourceAttachment().map(BinaryString::toString).orElse(null));
	}

	@Test
	void createAnnotationWithoutSourceAttachmentHasNoSourceWhenModuleHasNoSource() throws IOException {
		final AnnotationPojo annotation = createAnnotationWithoutSourceAttachment("/innowake/mining/test/annotation/source-extraction/MMRS7120.cbl", false);
		assertTrue(annotation.getSourceAttachment().isEmpty());
	}

	@Test
	void createAnnotationWithoutSourceAttachmentHasNoSourceWhenModuleHasEmptySource() throws IOException {
		final AnnotationPojo annotation = createAnnotationWithoutSourceAttachment("/innowake/mining/test/annotation/source-extraction/EMPTY.cbl", true);
		assertTrue(annotation.getSourceAttachment().get().isEmpty());
	}

	@Test
	void createAnnotationWithoutSourceAttachmentAndInvalidOffsetsUsesValidSubrange() throws IOException {
		final EntityId moduleId = createModule("/innowake/mining/test/annotation/source-extraction/MMRS7120.cbl");
		final AnnotationPojoPrototype newAnnotation = createMinimalAnnotation();
		/* The file only has 243 characters, so 237 + 20 will exceed the total length, which should still work. */
		final ModuleLocation location = new ModuleLocation(237, 20);
		newAnnotation.setLocation(location);
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation).setModuleId(moduleId)
				.setProjectId(ENTITY_ONE).execute();
		assertEquals(SC_CREATED, annotationResult.getStatusCode());
		assertTrue("Created Annotation must be present", annotationResult.getValue().isPresent());
		final AnnotationPojo createdAnnotation = annotationResult.getValue().get();
		/* We expect the valid subrange will be extracted from the file. */
		assertEquals("-NAME.", createdAnnotation.getSourceAttachment().get().toString());
	}

	@Test
	void testDeleteAnnotations() throws IOException, TimeoutException {
		Result<AnnotationPojo[]> resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).execute();
		final int intialAnnotationsLength = resultFindAll.getValue().get().length;
		final EntityId annotation1 = createTestAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_1).identity();
		final EntityId annotation2 = createTestAnnotation(ENTITY_ONE, CUSTOM_MODULE_ID_PROJECT_1, TEST_ANNOTATION_2).identity();
		
		/* Check total count before deletion */
		resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).execute();
		AnnotationPojo[] allAnnotations = resultFindAll.getValue().get();
		assertEquals(intialAnnotationsLength + 2, allAnnotations.length);
		
		final List<EntityId> idsToBeDeleted = Arrays.asList(annotation1, annotation2);
		final ResponseEntity<char[]> response = restTemplate.exchange(info.getUrl() + ANNOTATION_DATADICTIONARY_BULK_DELETE_URL, HttpMethod.DELETE,
				new HttpEntity<>(idsToBeDeleted, getHttpHeaders(info)), char[].class, ENTITY_ONE.getNid(), "Annotation");

		assertNotNull(response.getBody());
		waitForJobCompletion(new String(response.getBody()), 2);

		resultFindAll = annotationServiceProvider.findAllAnnotations().setProjectId(ENTITY_ONE).execute();
		allAnnotations = resultFindAll.getValue().get();
		assertEquals(intialAnnotationsLength, allAnnotations.length);
	}

	private AnnotationPojo createAnnotationWithoutSourceAttachment(final String path, final boolean includeModuleSource) throws IOException {
		final EntityId moduleId = includeModuleSource ? createModule(path) : createModuleWithoutSource(path);
		final AnnotationPojoPrototype newAnnotation = createMinimalAnnotation();
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setAnnotation(newAnnotation).setModuleId(moduleId)
				.setProjectId(ENTITY_ONE).execute();
		assertEquals(SC_CREATED, annotationResult.getStatusCode());
		assertTrue("Created Annotation must be present", annotationResult.getValue().isPresent());
		return annotationResult.getValue().get();
	}

	/*
	private void verifyCustomPropertyDeleted(final String propertyValue) {
		try {
			if (connectionOrient != null) {
				try (final Statement stmt = connectionOrient.createStatement()) {
					stmt.execute("select count(*) from AnnotationCustomProperties where customAnnotationProperty = \"" + propertyValue + "\"");
					assertEquals("Annotation delete should delete custom property as well", 0, stmt.getResultSet().getLong(1));
				}
			} else {
				throw new IllegalStateException("Database connection was lost.");
			}
		} catch (final SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	private static Map<Long, AnnotationPOjo> findAllByJDBC() {
		final Map<Long, Annotation> result = new HashMap<>();
		try {
			if (connectionOrient != null) {
				try (final Statement stmt = connectionOrient.createStatement()) {
					stmt.execute("SELECT id, name, projectId, stateLink.name, typeLink.name, typeLink.@rid, categoryLink.id, sourceAttachmentLink.content,"
							+ " @rid, inE('HasAnnotation').id, englishTranslation FROM Annotation WHERE projectId = 1");

					final ResultSet resultSet = stmt.getResultSet();
					while (resultSet.next()) {
						final Long id = (Long) resultSet.getObject("id");

						final Annotation annotation = new Annotation();
						annotation.setId(id);
						annotation.setName(resultSet.getString("name"));
						annotation.setProjectId((Long) resultSet.getObject("projectId"));
						annotation.setState(resultSet.getString("stateLink.name"));
						annotation.setType(AnnotationType.valueOf(resultSet.getString("typeLink.name")));
						annotation.setCategoryId((Long) resultSet.getObject("categoryLink.id"));
						annotation.setSourceAttachment(resultSet.getString("sourceAttachmentLink.content"));
						annotation.setRecordId(resultSet.getString("@rid"));
						annotation.setEnglishTranslation(resultSet.getString("englishTranslation"));

						@SuppressWarnings("unchecked")
						final List<Long> hasAnnotationIds = (List<Long>) resultSet.getObject("inE('HasAnnotation').id");
						assertEquals("There are more than 1 incoming edges for annotation.", 1, hasAnnotationIds.size());

						stmt.execute("SELECT *, out.@rid, in.@rid, " + "fromModuleLocation.offset, fromModuleLocation.length, "
								+ "toModuleLocation.offset, toModuleLocation.length " + "FROM Reference WHERE id=" + hasAnnotationIds.get(0));
						final ResultSet resultSetReference = stmt.getResultSet();
						assertTrue(resultSetReference.next());
						final Reference reference = new Reference();
						reference.setId((Long) resultSetReference.getObject("id"));
						reference.setRecordId(resultSetReference.getString("@rid"));
						reference.setRelationship(resultSetReference.getString("@class"));
						reference.setFromId(resultSetReference.getString("out.@rid"));
						reference.setToId(resultSetReference.getString("in.@rid"));
						final ModuleLocation fromModuleLocation = new ModuleLocation(resultSetReference.getInt("fromModuleLocation.offset"),
								resultSetReference.getInt("fromModuleLocation.length"));
						reference.setFromModuleLocation(fromModuleLocation);
						final ModuleLocation toModuleLocation = new ModuleLocation(resultSetReference.getInt("toModuleLocation.offset"),
								resultSetReference.getInt("toModuleLocation.length"));
						reference.setToModuleLocation(toModuleLocation);
						annotation.setReference(reference);

						result.put(id, annotation);
					}
				}
			} else {
				throw new IllegalStateException("Database connection was lost.");
			}
		} catch (final SQLException e) {
			throw new IllegalStateException(e);
		}
		return result;
	}
	*/

	private static boolean resultContains(final AnnotationPojo[] result, final String annotationName) {
		for (int i = 0; i < result.length; i++) {
			if (annotationName.equals(result[i].getName())) {
				return true;
			}
		}
		return false;
	}

	private boolean resultContains(final List<AnnotationPojo> result, final String annotationName) {
		for (int i = 0; i < result.size(); i++) {
			if (annotationName.equals(result.get(i).getName())) {
				return true;
			}
		}
		return false;
	}

	static void verifyFindAll(final AnnotationPojo[] annotations, final Collection<AnnotationPojo> result) {
		var databaseResult = result.stream().collect(Collectors.toMap(a -> a.getId(), a -> a));
		for (final AnnotationPojo annotation : annotations) {
			final Long id = annotation.getId();
			final AnnotationPojo expected = databaseResult.get(id);
			assertNotNull(expected);
			verifyAnnotationAllFields(expected, annotation, annotation.getSourceAttachment().map(BinaryString::toString).orElse(null));
			databaseResult.remove(id);
		}
		assertTrue(databaseResult.isEmpty());
	}

	private static void verifyAnnotationWithoutIdAndRecord(final AnnotationPojoPrototype input, final AnnotationPojo result) {
		assertEquals(input.name.get(), result.getName());
		assertEquals(input.categoryId.orElse(null), result.getCategoryId().orElse(null));
		assertEquals(input.sourceAttachment.orElse(null), result.getSourceAttachment().orElse(null));
		assertEquals(input.state.get(), result.getState());
		assertEquals(input.type.get(), result.getType());
	}

	private static void verifyAnnotationAllFields(final AnnotationPojo input, final AnnotationPojo result, @Nullable final String sourceAttachment) {
		assertEquals(input.getId(), result.getId());
		assertEquals(input.getProjectNid(), result.getProjectNid());
		assertEquals(input.getName(), result.getName());
		assertEquals(sourceAttachment, result.getSourceAttachment().map(BinaryString::toString).orElse(null));
		assertEquals(input.getState(), result.getState());
		assertEquals(input.getType(), result.getType());
		assertEquals(input.getModule(), result.getModule());
		assertEquals(input.getEnglishTranslation(), result.getEnglishTranslation());
	}
	
	private AnnotationPojo createAnnotation(final EntityId projectId, final EntityId moduleId, final AnnotationPojoPrototype annotation) throws IOException {
		return createAnnotation(projectId, moduleId, annotation, null);
	}

	private AnnotationPojo createAnnotation(final EntityId projectId, final EntityId moduleId, final AnnotationPojoPrototype annotation,
			@Nullable final ModuleLocation moduleLocation) throws IOException {
		final String propertyName = "customMetaInfo";
		final String expectedCustomValue = "Property value for annotations";
		annotation.setCustomProperties(new NestedMap().set(CustomPropertyClass.AnnotationCustomProperties.name(), propertyName, expectedCustomValue));
		annotation.setLocation(moduleLocation != null ? moduleLocation : new ModuleLocation(10, 20));
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setProjectId(projectId).setModuleId(moduleId)
				.setAnnotation(annotation).execute();
		
		assertEquals(SC_CREATED, annotationResult.getStatusCode());
		assertTrue(annotationResult.getValue().isPresent());
		final AnnotationPojo createdAnnotation = annotationResult.getValue().get();
		verifyNumberOfCustomProperties(createdAnnotation, NUMBER_OF_CUSTOM_PROPERTIES_BEING_CREATED_ON_ANNOTATION_ENTITY);
		final Object customPropertyByName = getCustomPropertyByName(propertyName, createdAnnotation);
		assertEquals(expectedCustomValue, customPropertyByName);

		return createdAnnotation;
	}

	private AnnotationPojoPrototype createMinimalAnnotation() {
		return new AnnotationPojoPrototype()
			.setName("MINIMAL ANNOTATION")
			.setState(WorkingState.IN_ANALYSIS)
			.setType(TEST_ANNOTATION_TYPE)
			.setEnglishTranslation("MINIMAL English translation")
			.setLocation(new ModuleLocation(10, 20));
	}
	
	private AnnotationPojo createTestAnnotation(final EntityId projectId, final EntityId moduleId, final AnnotationPojoPrototype annotation) throws IOException {
		annotation.setLocation(new ModuleLocation(12, 24));
		final Result<AnnotationPojo> annotationResult = annotationServiceProvider.createAnnotation().setProjectId(projectId).setModuleId(moduleId)
				.setAnnotation(annotation).execute();
		if (annotationResult.getCustomErrorResponse() != null) {
			LOG.info("StatusBody: "+ annotationResult.getStatusBody().toString());
			LOG.info("StatusMessage: " + annotationResult.getStatusMessage().toString());
			LOG.info("Value: "+ annotationResult.getValue().toString());
		}
		assertEquals(SC_CREATED, annotationResult.getStatusCode());
		assertTrue(annotationResult.getValue().isPresent());
		final AnnotationPojo createdAnnotation =  annotationResult.getValue().get();
		return createdAnnotation;
	}

	private static void assertLocation(@Nullable final ModuleLocation location, final int expectedOffset, final int expectedLength) {
		assertNotNull("Location must exists", location);
		assertEquals("Offset must match", expectedOffset, location.getOffset().intValue());
		assertEquals("Length must match", expectedLength, location.getLength().intValue());
	}

	private EntityId createModule(final String path) {
		final String content;
		try (final InputStream inputStream = Files.newInputStream(Paths.get("./src/test/resources/" + path))) {
			content = IOUtils.toString(inputStream, StandardCharsets.UTF_8);
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			return EntityId.VOID;
		}

		final String name = FilenameUtils.getBaseName(path);

		LOG.trace(() -> String.format("Name: %s\nContent:\n%s", name, content));

		final var module = new ModulePojoPrototype()
				.setName(name)
				.setPath(path)
				.setProject(EntityId.of(1L))
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API)
				.setContent(content);
		final Result<ModulePojo> resultCreate;
		try {
			resultCreate = moduleServiceProvider.createModule().setModule(module).setProjectId(Long.valueOf(1)).execute();
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			return EntityId.VOID;
		}
		assertEquals(SC_CREATED, resultCreate.getStatusCode());
		assertTrue(resultCreate.getValue().isPresent());
		final var createdModule = resultCreate.getValue().get();
		return createdModule.identity();
	}

	private EntityId createModuleWithoutSource(final String path) {
		final String name = FilenameUtils.getBaseName(path);
		final var module = new ModulePojoPrototype()
				.setName(name)
				.setPath(path)
				.setProject(EntityId.of(1L))
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API);
		final Result<ModulePojo> resultCreate;
		try {
			resultCreate = moduleServiceProvider.createModule().setModule(module).setProjectId(Long.valueOf(1)).execute();
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			return EntityId.VOID;
		}
		assertEquals(SC_CREATED, resultCreate.getStatusCode());
		assertTrue(resultCreate.getValue().isPresent());
		final var createdModule = resultCreate.getValue().get();
		return createdModule.identity();
	}

}
