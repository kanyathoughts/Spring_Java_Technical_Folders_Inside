/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static innowake.mining.test.CustomProperties.getCustomPropertyByName;
import static innowake.mining.test.util.RestTemplateUtil.getHttpHeaders;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.InputStream;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import innowake.mining.data.access.postgres.AstPgDao;
import innowake.mining.data.access.postgres.DataFlowPgDao;
import innowake.mining.shared.entities.ast.AstNodePojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojoPrototype;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.datalineage.DataFlowId;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.client.service.datadictionary.CreateDataDictionaryEntry;
import innowake.mining.client.service.datadictionary.DataDictionaryServiceProvider;
import innowake.mining.client.service.datadictionary.DeleteDataDictionaryEntry;
import innowake.mining.client.service.datadictionary.FindAllDataDictionaryEntries;
import innowake.mining.client.service.datadictionary.SearchDataDictionaryEntry;
import innowake.mining.client.service.datadictionary.UpdateDataDictionaryEntry;
import innowake.mining.client.service.module.ModuleServiceProvider;
import innowake.mining.client.service.reference.ReferenceServiceProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.DataDictionaryVariableAttribute;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DataFieldFormat;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Integration tests for the data dictionary services.
 * 
 * @see DataDictionaryServiceProvider
 */
class DataDictionaryServiceTest extends IntegrationTest {
	
	private static final EntityId FIRST_CUSTOM_PROJECT_ID = EntityId.of(Long.valueOf(1));
	private static final EntityId SECOND_CUSTOM_PROJECT_ID = EntityId.of(Long.valueOf(2));
	private static final EntityId MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES = EntityId.of(Long.valueOf(2002));
	private static final EntityId NONE_EXISTING_ID = EntityId.of(Long.valueOf(Long.MAX_VALUE));
	private static final int NUMBER_OF_DEFAULT_DATA_DICTIONARY_ENTRIES = 3;
	private static final int NUMBER_OF_DEFAULT_DATA_DICTIONARY_OTHER_SCOPES = 2;
	private static final int MODULE_LOCATION_OFFSET = 1;
	private static final int MODULE_LOCATION_LENGTH = 10;
	private static final String RESOURCE_PATH = "/innowake/mining/test/data-dictionary/source-extraction/";
	private static final String VALIDATE_BY_OFFSET_URL = "/api/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/offset/{offset}";
	private static final String LINKED_BUSINESS_RULES_URL = RouteConfiguration.API_BASE + 
			"/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/{ddeId}/linked-annotations";
	private static final String DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET = "/api/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/byOffset";
	private static final String ANNOTATION_DATADICTIONARY_BULK_DELETE_URL = RouteConfiguration.API_BASE + "/v1/projects/{projectId}/bulk-delete/{entityType}";

	private static final String DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS = "/api/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/byDataFlowIds";
	
	private final DataDictionaryServiceProvider dataDictionaryServiceProvider = MiningApiClient.dataDictionaryService(getConnectionInfo());
	private final ModuleServiceProvider moduleServiceProvider = MiningApiClient.moduleService(getConnectionInfo());
	private final ReferenceServiceProvider referenceServiceProvider = MiningApiClient.referenceService(getConnectionInfo());
	private final RestTemplate restTemplate = new RestTemplate();
	private final ConnectionInfo info = getConnectionInfo();
	private final ParameterizedTypeReference<DataFieldFormat> responseType = new ParameterizedTypeReference<DataFieldFormat>() { };
	private final ParameterizedTypeReference<AnnotationPojo[]> responseTypeOfLinkedBusinessRule = new ParameterizedTypeReference<AnnotationPojo[]>() { };
	private final ParameterizedTypeReference<DataDictionaryPojo[]> responseTypeOfDataDictionaryBasedOnOffset =
			new ParameterizedTypeReference<>() { };

	@Test
	void testFindAllDataDictionaryEntriesOnModuleReturnsCorrectNumberOfEntries() throws Exception {
		final FindAllDataDictionaryEntries findAllDataDictionaryEntries = dataDictionaryServiceProvider.findAllDataDictionaryEntries();
		final Result<DataDictionaryPojo[]> findResult = findAllDataDictionaryEntries
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.execute();
		
		assertEquals(HttpStatus.SC_OK, findResult.getStatusCode());
		assertTrue("Serialized data should be available", findResult.getValue().isPresent());
		
		final DataDictionaryPojo[] dataDictionaryEntries = findResult.getValue().get();
		assertEquals(NUMBER_OF_DEFAULT_DATA_DICTIONARY_ENTRIES, dataDictionaryEntries.length);
		
		for (final DataDictionaryPojo entry : dataDictionaryEntries) {
			if ("MY-PROGRAM-NAME".equals(entry.getName())) {
				CustomProperties.verifyNumberOfCustomProperties(entry, 1);
			} else {
				CustomProperties.verifyNumberOfCustomProperties(entry, 0);
			}
		}
	}
	
	@Test
	void testFindAllReturns404WhenProjectDoesNotExist() throws Exception {
		final FindAllDataDictionaryEntries findAllDataDictionaryEntries = dataDictionaryServiceProvider.findAllDataDictionaryEntries();
		final Result<DataDictionaryPojo[]> findResult = findAllDataDictionaryEntries
			.setProjectId(NONE_EXISTING_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.execute();
		
		assertEquals(HttpStatus.SC_NOT_FOUND, findResult.getStatusCode());
		assertFalse("Find result should not be present", findResult.getValue().isPresent());
	}
	
	
	@Test
	void testFindAllReturns404WhenModuleDoesNotExist() throws Exception {
		final FindAllDataDictionaryEntries findAllDataDictionaryEntries = dataDictionaryServiceProvider.findAllDataDictionaryEntries();
		final Result<DataDictionaryPojo[]> findResult = findAllDataDictionaryEntries
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(NONE_EXISTING_ID)
			.execute();
		
		assertEquals(HttpStatus.SC_NOT_FOUND, findResult.getStatusCode());
		assertFalse("Find result should not be present", findResult.getValue().isPresent());
	}

	@Test
	void testFindById() throws IOException {
		final DataDictionaryPojo createdDataDictionaryEntry = createFullDataDictionaryEntry("New DataDictionaryElement", false, null);
		final EntityId id = createdDataDictionaryEntry.identity();
		assertNotNull(id);
		final EntityId idNonNull = id;
		
		final Result<DataDictionaryPojo> foundEntryResult = dataDictionaryServiceProvider.findDataDictionaryEntryById()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setId(idNonNull).execute();
		assertEquals(200, foundEntryResult.getStatusCode());
		final DataDictionaryPojo foundEntry = foundEntryResult.getValue().get();
		foundEntry.equals(createdDataDictionaryEntry);
		assertEquals(1, foundEntry.getCustomProperties().size());
	}

	@Test
	void testLinkedBusinessRule() throws IOException {
		final FindAllDataDictionaryEntries findAllDataDictionaryEntries = dataDictionaryServiceProvider.findAllDataDictionaryEntries();
		final Result<DataDictionaryPojo[]> findResult = findAllDataDictionaryEntries.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES).execute();

		assertEquals(HttpStatus.SC_OK, findResult.getStatusCode());
		assertTrue("Serialized data should be available", findResult.getValue().isPresent());

		final DataDictionaryPojo[] dataDictionaryEntries = findResult.getValue().get();
		assertEquals(NUMBER_OF_DEFAULT_DATA_DICTIONARY_ENTRIES, dataDictionaryEntries.length);

		for (final DataDictionaryPojo entry : dataDictionaryEntries) {
			if ("MY-BIN-FIELDS".equals(entry.getName())) {
				final ResponseEntity<AnnotationPojo[]> validResult = restTemplate.exchange(info.getUrl() + LINKED_BUSINESS_RULES_URL, HttpMethod.GET,
						new HttpEntity<>(getHttpHeaders(info)), responseTypeOfLinkedBusinessRule, FIRST_CUSTOM_PROJECT_ID.getNid(),
						MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES.getNid(), Optional.ofNullable(entry.identity()).get().getNid());
				assertEquals(200, validResult.getStatusCodeValue());
				final AnnotationPojo[] foundLinkedBusinessRules = validResult.getBody();
				assertNotNull(foundLinkedBusinessRules);
				assertEquals(2, foundLinkedBusinessRules.length);
				// found annotation with name Annotation 5 and assert the location of the annotation.
				final AnnotationPojo foundAnnotation1 = Arrays.stream(foundLinkedBusinessRules)
						.filter(annotation -> "Annotation 5".equals(annotation.getName())).findFirst().orElse(null);
				assertNotNull(foundAnnotation1);
				assertEquals(Integer.valueOf(500), foundAnnotation1.getLocation().orElseThrow().getOffset());
				assertEquals(Integer.valueOf(5), foundAnnotation1.getLocation().orElseThrow().getLength());
				// found annotation with name Annotation 6 and assert the location of the annotation.
				final AnnotationPojo foundAnnotation2 = Arrays.stream(foundLinkedBusinessRules)
						.filter(annotation -> "Annotation 6".equals(annotation.getName())).findFirst().orElse(null);
				assertNotNull(foundAnnotation2);
				assertEquals(Integer.valueOf(600), foundAnnotation2.getLocation().orElseThrow().getOffset());
				assertEquals(Integer.valueOf(6), foundAnnotation2.getLocation().orElseThrow().getLength());
			}
		}
	}
	
	@Test
	void testFindByRecordIdNonExisting() throws IOException {
		final Result<DataDictionaryPojo> foundDataDictionaryEntry = dataDictionaryServiceProvider.findDataDictionaryEntryById()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setId(EntityId.of(Long.MAX_VALUE)).execute();
		assertEquals(404, foundDataDictionaryEntry.getStatusCode());
	}
	
	@Test
	void testFindByRecordIdWrongType() throws IOException {
		final Result<DataDictionaryPojo> foundDataDictionaryEntry = dataDictionaryServiceProvider.findDataDictionaryEntryById()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setId(EntityId.of(Long.MAX_VALUE)).execute();
		assertEquals(404, foundDataDictionaryEntry.getStatusCode());
	}

	@Test
	void testFindByRecordIdWithWrongProject() throws IOException {
		final DataDictionaryPojo createdDataDictionaryEntry = createFullDataDictionaryEntry("New DataDictionaryElement", false, null);
		final EntityId recordId = createdDataDictionaryEntry.identity();
		assertNotNull(recordId);
		final EntityId recordIdNonNull = recordId;
	
		final Result<DataDictionaryPojo> foundDataDictionaryEntry = dataDictionaryServiceProvider.findDataDictionaryEntryById()
				.setProjectId(SECOND_CUSTOM_PROJECT_ID)
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setId(recordIdNonNull).execute();
		assertEquals(404, foundDataDictionaryEntry.getStatusCode());
	}

	@Test
	void testFindByRecordIdWithWrongModule() throws IOException {
		final DataDictionaryPojo createdDataDictionaryEntry = createFullDataDictionaryEntry("New DataDictionaryElement", false, null);
		final EntityId recordId = createdDataDictionaryEntry.identity();
		assertNotNull(recordId);
		final EntityId recordIdNonNull = recordId;
		
		final Result<DataDictionaryPojo> foundDataDictionaryEntry = dataDictionaryServiceProvider.findDataDictionaryEntryById()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(NONE_EXISTING_ID)
				.setId(recordIdNonNull).execute();
		assertEquals(404, foundDataDictionaryEntry.getStatusCode());
	}
	
	@Test
	void testCreateDataDictionaryEntryThrowsWhenModuleIdIsNotSet() {
		final CreateDataDictionaryEntry createDataDictionaryEntry = dataDictionaryServiceProvider.createDataDictionaryEntry()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID);
		Assertions.assertEquals("Module ID must be set.", Assertions.assertThrows(IllegalStateException.class,
				createDataDictionaryEntry::execute).getMessage());
	}
	
	@Test
	void testCreateDataDictionaryEntryThrowsWhenProjectIdIsNotSet() {
		final CreateDataDictionaryEntry createDataDictionaryEntry = dataDictionaryServiceProvider.createDataDictionaryEntry()
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES);
		Assertions.assertEquals("Project id must be set.", Assertions.assertThrows(IllegalStateException.class,
				createDataDictionaryEntry::execute).getMessage());
	}
	
	@Test
	void testCreateDataDictionaryEntryThrowsWhenDataDictionaryEntryIsNotSet() {
		final CreateDataDictionaryEntry createDataDictionaryEntry = dataDictionaryServiceProvider.createDataDictionaryEntry()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES);
		Assertions.assertEquals("Data dictionary entry must be set.", Assertions.assertThrows(IllegalStateException.class,
				createDataDictionaryEntry::execute).getMessage());
	}
	
	@Test
	void testCreateDataDictionaryEntry() throws Exception {
		final DataDictionaryPojo createdDataDictionaryEntry = createFullDataDictionaryEntry("MyElmement", true, null);
		assertNotNull(createdDataDictionaryEntry.getId());
		assertNotNull(createdDataDictionaryEntry.identity());
		assertNotNull(createdDataDictionaryEntry.getCreatedByUserId());
		
		try {
			final DataDictionaryPojo findDataDictionaryEntryById = findDataDictionaryEntryById(createdDataDictionaryEntry.getId());
			assertNotNull(findDataDictionaryEntryById.getCreatedByUserId());
			assertEquals(1, findDataDictionaryEntryById.getCustomProperties().size());
			final var scopes = findDataDictionaryEntryById.getScopes();
			assertEquals(Map.of(DataDictionaryVariableAttribute.SQL_DATABASE_TABLES.getKey(), "TEST TABLES"), scopes.get(DataDictionaryVariableScope.SQL_DATABASE));
			assertEquals(Map.of(
						DataDictionaryVariableAttribute.CICS_UI_MAPSET.getKey(), "TEST MAPSET",
						DataDictionaryVariableAttribute.CICS_UI_MAPNAME.getKey(), "TEST MAPNAME"),
					scopes.get(DataDictionaryVariableScope.CICS_UI));
			assertEquals("SCOPE_1", findDataDictionaryEntryById.getScopes().get(DataDictionaryVariableScope.OTHER).get("scope"));
			assertEquals("TEST SOURCE", findDataDictionaryEntryById.getScopes().get(DataDictionaryVariableScope.OTHER).get("source"));
			assertTrue("isCandidate must be true", findDataDictionaryEntryById.getIsCandidate());
			assertEquals("TEST PIC CLAUSE", findDataDictionaryEntryById.getPicClause().orElseThrow());
			assertEquals(DefinedLocation.PROGRAM, findDataDictionaryEntryById.getDefinedLocation().orElseThrow());
			assertEquals(WorkingState.CANDIDATE, findDataDictionaryEntryById.getState().orElseThrow());
			assertFalse("isReferenced should be false", findDataDictionaryEntryById.getIsReferenced().orElseThrow());
			assertFalse("isBusiness should be false", findDataDictionaryEntryById.getIsBusiness().orElseThrow()	);
			assertEquals("TEST TRANSFORMATION", findDataDictionaryEntryById.getFieldTransformation().orElseThrow());
			assertEquals("TEST INPUT", findDataDictionaryEntryById.getSourceInput().orElseThrow());
			assertEquals("TEST OUTPUT", findDataDictionaryEntryById.getTargetOutput().orElseThrow());
		} catch (final IllegalStateException e) {
			fail("The newly created data dictionary entry should be findable", e);
		}
	}

	@Test
	void createMinimalDataDictionaryEntry() throws IOException {
		final CreateDataDictionaryEntry createDataDictionaryEntryService = dataDictionaryServiceProvider.createDataDictionaryEntry();
	
		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype()
				.setName("MyElmement")
				.setDescription("MY description")
				.setLocation(new ModuleLocation(MODULE_LOCATION_OFFSET, MODULE_LOCATION_LENGTH))
				.setModule(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setCreatedByUserId("admin")
				.setIsCandidate(true);
	
		final Result<DataDictionaryPojo> createdResult = createDataDictionaryEntryService
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.setDataDictionaryEntry(dataDictionaryEntry)
			.execute();
		
		assertEquals(HttpStatus.SC_CREATED, createdResult.getStatusCode());
		assertTrue("Created dde should be present", createdResult.getValue().isPresent());
		
		final DataDictionaryPojo createdDataDictionaryEntry = createdResult.getValue().get();
		
		assertNotNull(createdDataDictionaryEntry.getId());
		assertNotNull(createdDataDictionaryEntry.identity());
		assertNotNull(createdDataDictionaryEntry.getCreatedByUserId());
		assertTrue("isCandidate must be true", createdDataDictionaryEntry.getIsCandidate());
		CustomProperties.verifyNumberOfCustomProperties(createdDataDictionaryEntry, 0);
		
		try {
			final DataDictionaryPojo findDataDictionaryEntryById = findDataDictionaryEntryById(createdDataDictionaryEntry.getId());
			assertNotNull(findDataDictionaryEntryById.getCreatedByUserId());
		} catch (final IllegalStateException e) {
			fail("The newly created data dictionary entry should be findable", e);
		}
	
	}
	
	@Test
	void createEntryWithUndefinedCustomProperty() throws IOException {
		final CreateDataDictionaryEntry createDataDictionaryEntryService = dataDictionaryServiceProvider.createDataDictionaryEntry();

		final Map<String, Object> props = Map.of(CustomPropertyClass.DataDictionaryEntryCustomProperties.name(), Map.of("NON_EXISTING_PROPERTY", "a created value for the custom property"));
	
		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype()
				.setName("MyElmement")
				.setDescription("MY description")
				.setLocation(new ModuleLocation(MODULE_LOCATION_OFFSET, MODULE_LOCATION_LENGTH))
				.setModule(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setCreatedByUserId("admin")
				.setCustomProperties(props);
	
		final Result<DataDictionaryPojo> createdResult = createDataDictionaryEntryService
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.setDataDictionaryEntry(dataDictionaryEntry)
			.execute();
		
		/* Before creating or updating DataDictionary, It is checked if CustomProperty exist, If it doesn't exist, it throws EntityNotFoundException(404) */
		assertEquals(HttpStatus.SC_NOT_FOUND, createdResult.getStatusCode());
		assertTrue("Create should throw CustomProperty not found error.", createdResult.getExtendedStatusMessage().contains(String.format(
				"MiningEntityNotFoundException thrown from controller while trying to access /api/v1/projects/%s/modules/%s/data-dictionary",
				FIRST_CUSTOM_PROJECT_ID.getNid(), MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES.getNid())));
		assertFalse("Created result should not be present", createdResult.getValue().isPresent());
	}

	@Test
	void testCreateDataDictionaryEntryWithoutDataElementName() throws Exception {
		final CreateDataDictionaryEntry createDataDictionaryEntryService = dataDictionaryServiceProvider.createDataDictionaryEntry();
	
		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype()
				.setDescription("MY description")
				.setLocation(new ModuleLocation(MODULE_LOCATION_OFFSET, MODULE_LOCATION_LENGTH))
				.setModule(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setCreatedByUserId("admin");

		final Result<DataDictionaryPojo> createdResult = createDataDictionaryEntryService
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.setDataDictionaryEntry(dataDictionaryEntry)
			.execute();
		
		assertEquals(HttpStatus.SC_BAD_REQUEST, createdResult.getStatusCode());
		assertFalse("Created result should not be present", createdResult.getValue().isPresent());
	}
	
	@Test
	void testCreateDataDictionaryEntryWithoutDescription() throws Exception {
		final CreateDataDictionaryEntry createDataDictionaryEntryService = dataDictionaryServiceProvider.createDataDictionaryEntry();
	
		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype()
				.setName("MyElmement")
				.setLocation(new ModuleLocation(MODULE_LOCATION_OFFSET, MODULE_LOCATION_LENGTH))
				.setModule(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setCreatedByUserId("admin");

		final Result<DataDictionaryPojo> createdResult = createDataDictionaryEntryService
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.setDataDictionaryEntry(dataDictionaryEntry)
			.execute();
		
		assertEquals(HttpStatus.SC_BAD_REQUEST, createdResult.getStatusCode());
		assertFalse("Created result should not be present", createdResult.getValue().isPresent());
	}

	@Test
	void testCreateDataDictionaryEntryWithoutFormat() throws Exception {
		final CreateDataDictionaryEntry createDataDictionaryEntryService = dataDictionaryServiceProvider.createDataDictionaryEntry();
	
		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype()
				.setName("MyElmement")
				.setDescription("MY description")
				.setLocation(new ModuleLocation(MODULE_LOCATION_OFFSET, MODULE_LOCATION_LENGTH))
				.setModule(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setCreatedByUserId("admin");

		final Result<DataDictionaryPojo> createdResult = createDataDictionaryEntryService
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.setDataDictionaryEntry(dataDictionaryEntry)
			.execute();
		
		assertEquals(HttpStatus.SC_CREATED, createdResult.getStatusCode());
		assertTrue("Created dde should be present", createdResult.getValue().isPresent());
	}

	@Test
	void testCreateDataDictionaryEntryWithoutModuleAndLocation() throws Exception {
		final CreateDataDictionaryEntry createDataDictionaryEntryService = dataDictionaryServiceProvider.createDataDictionaryEntry();

		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype()
				.setName("MyElmement")
				.setDescription("MY description")
				.setFormat("PICX");

		final Result<DataDictionaryPojo> createdResult = createDataDictionaryEntryService
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.setDataDictionaryEntry(dataDictionaryEntry)
			.execute();
		
		assertEquals(HttpStatus.SC_BAD_REQUEST, createdResult.getStatusCode());
		assertFalse("Created result should not be present", createdResult.getValue().isPresent());
	}
	
	@Test
	void testCreateEntryWithDuplicateSourceRange() throws Exception {
		final CreateDataDictionaryEntry createDataDictionaryEntryService = dataDictionaryServiceProvider.createDataDictionaryEntry();
	
		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype()
				.setName("MyElmement")
				.setDescription("MY description")
				.setLocation(new ModuleLocation(MODULE_LOCATION_OFFSET, MODULE_LOCATION_LENGTH))
				.setModule(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setCreatedByUserId("admin");
	
		final Result<DataDictionaryPojo> createdResult = createDataDictionaryEntryService
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.setDataDictionaryEntry(dataDictionaryEntry)
			.execute();
		
		assertEquals(HttpStatus.SC_CREATED, createdResult.getStatusCode());
		assertTrue("Created dde should be present", createdResult.getValue().isPresent());

		final Result<DataDictionaryPojo> duplicateCreatedResult = createDataDictionaryEntryService
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setDataDictionaryEntry(dataDictionaryEntry)
				.execute();

		assertEquals(HttpStatus.SC_BAD_REQUEST, duplicateCreatedResult.getStatusCode());
		assertFalse("Duplicate dde should not be present", duplicateCreatedResult.getValue().isPresent());
	}
	
	@Test
	void testUpdateDataDictionaryEntryThrowsWhenProjectIdIsNotSet() {
		final UpdateDataDictionaryEntry updateDataDictionaryEntry = dataDictionaryServiceProvider.updateDataDictionaryEntry()
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES);
		Assertions.assertEquals("Project id must be set.", Assertions.assertThrows(IllegalStateException.class,
				updateDataDictionaryEntry::execute).getMessage());
	}

	@Test
	void testUpdateDataDictionaryEntryThrowsWhenModuleIdIsNotSet() {
		final UpdateDataDictionaryEntry updateDataDictionaryEntry = dataDictionaryServiceProvider.updateDataDictionaryEntry()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID);
		Assertions.assertEquals("Module ID must be set.", Assertions.assertThrows(IllegalStateException.class,
			updateDataDictionaryEntry::execute).getMessage());
	}
	
	@Test
	void testUpdateDataDictionaryEntryThrowsWhenDataDictionaryEntryIsNotSet() {
		final UpdateDataDictionaryEntry updateDataDictionaryEntry = dataDictionaryServiceProvider.updateDataDictionaryEntry()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES);
		Assertions.assertEquals("Data dictionary entry must be set.", Assertions.assertThrows(IllegalStateException.class,
			updateDataDictionaryEntry::execute).getMessage());
	}

	@Test
	void testUpdateDataDictionaryEntry() throws Exception {
		final DataDictionaryPojo createdEntry = createFullDataDictionaryEntry("MyElmement", false, null);
		final Long id = createdEntry.getId();
		final EntityId recordId = createdEntry.identity();
		final String initialCreatedByUserId = createdEntry.getCreatedByUserId();
		final ModuleLocation initialModuleLocation = createdEntry.getLocation().orElseThrow();

		assertNotNull(id);
		assertNotNull(recordId);
		assertNotNull(initialCreatedByUserId);
		assertNotNull(initialModuleLocation);
		assertFalse("Has to be false before Updating", createdEntry.getIsCandidate());
		assertFalse("isBusiness should be false before updating", createdEntry.getIsBusiness().orElseThrow());
		assertFalse("isReferenced should be false before updating", createdEntry.getIsReferenced().orElseThrow());
		
		final String updatedDataElementName = "UPDATED DATA ELEMENT NAME";
		final String updatedDataset = "UPDATED DATASET NAME";
		final String updatedDescription = "UPDATED DESCRIPTION";
		final String updatedTables = "UPDATED TABLES";
		final String updatedMapset = "UPDATED MAPSET";
		final String updatedMapName = "UPDATED MAPNAME";
		final String updatedPicClause = "UPDATED PIC CLAUSE";
		final DefinedLocation updatedLocation = DefinedLocation.PROGRAM;
		final String updatedTransformation = "UPDATED TRANSFORMATION";
		final String updatedInput = "UPDATED INPUT";
		final String updatedOutput = "UPDATED OUTPUT";
		final long updatedLength = 9999;
		final var updatedModuleLocation = new ModuleLocation(111, 222);
		
		DataDictionaryPojoPrototype pojoPrototype = new DataDictionaryPojoPrototype().withId(recordId)
				.setCreatedByUserId("NOT RELEVANT USER ID BECAUSE VALUE IS IMMUTABLE")
				.setName(updatedDataElementName)
				.setLocation(updatedModuleLocation)
				.setScopes(Map.of(DataDictionaryVariableScope.FILE, Map.of(DataDictionaryVariableAttribute.FILE_DATASET.getKey(), updatedDataset),
						DataDictionaryVariableScope.SQL_DATABASE, Map.of(DataDictionaryVariableAttribute.SQL_DATABASE_TABLES.getKey(), updatedTables),
						DataDictionaryVariableScope.CICS_UI, Map.of(
								DataDictionaryVariableAttribute.CICS_UI_MAPSET.getKey(), updatedMapset,
								DataDictionaryVariableAttribute.CICS_UI_MAPNAME.getKey(), updatedMapName),
						DataDictionaryVariableScope.OTHER, Map.of()))
				.setDescription(updatedDescription)
				.setIsCandidate(true)
				.setLength(updatedLength)
				.setFormat("PIC9")
				.setPicClause(updatedPicClause)
				.setDefinedLocation(updatedLocation)
				.setState(WorkingState.IN_ANALYSIS)
				.setIsBusiness(true)
				.setFieldTransformation(updatedTransformation)
				.setSourceInput(updatedInput)
				.setTargetOutput(updatedOutput)
				.setIsReferenced(true)
				.setCustomProperties(new NestedMap()
						.set(CustomPropertyClass.DataDictionaryEntryCustomProperties.name(), "customDataDictionaryEntryProperty", "An updated value"));

		final UpdateDataDictionaryEntry updateDataDictionaryEntry = dataDictionaryServiceProvider.updateDataDictionaryEntry();
		final Result<DataDictionaryPojo> updateResult = updateDataDictionaryEntry
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.setDataDictionaryEntry(pojoPrototype)
			.execute();
		
		assertEquals(HttpStatus.SC_OK, updateResult.getStatusCode());
		assertTrue("Updated dde should be present", updateResult.getValue().isPresent());
		final DataDictionaryPojo updatedEntry = updateResult.getValue().get();
		assertNotNull(updatedEntry);
		
		assertEquals(id, updatedEntry.getId());
		assertTrue("isCandidate must be true after the update", updatedEntry.getIsCandidate());
		assertEquals(recordId, updatedEntry.identity());
		assertEquals(initialCreatedByUserId, updatedEntry.getCreatedByUserId());
		assertEquals(updatedDataElementName, updatedEntry.getName());
		assertEquals(Map.of(DataDictionaryVariableScope.FILE, Map.of(DataDictionaryVariableAttribute.FILE_DATASET.getKey(), updatedDataset),
				DataDictionaryVariableScope.SQL_DATABASE, Map.of(DataDictionaryVariableAttribute.SQL_DATABASE_TABLES.getKey(), updatedTables),
				DataDictionaryVariableScope.CICS_UI, Map.of(
						DataDictionaryVariableAttribute.CICS_UI_MAPSET.getKey(), updatedMapset,
						DataDictionaryVariableAttribute.CICS_UI_MAPNAME.getKey(), updatedMapName),
				DataDictionaryVariableScope.OTHER, Map.of()), updatedEntry.getScopes());
		assertEquals(updatedDescription, updatedEntry.getDescription());
		assertEquals(updatedLength, updatedEntry.getLength().orElseThrow().longValue());
		assertEquals("PIC9", updatedEntry.getFormat().orElseThrow());
		assertEquals(updatedModuleLocation, updatedEntry.getLocation().orElseThrow());
		assertEquals(updatedPicClause, updatedEntry.getPicClause().orElseThrow());
		assertEquals(updatedLocation, updatedEntry.getDefinedLocation().orElseThrow());
		assertEquals(WorkingState.IN_ANALYSIS, updatedEntry.getState().orElseThrow());
		assertTrue("isBusiness should be true after update", updatedEntry.getIsBusiness().orElseThrow());
		assertEquals(updatedTransformation, updatedEntry.getFieldTransformation().orElseThrow());
		assertEquals(updatedInput, updatedEntry.getSourceInput().orElseThrow());
		assertEquals(updatedOutput, updatedEntry.getTargetOutput().orElseThrow());
		assertTrue("isReferenced should be true after update", updatedEntry.getIsReferenced().orElseThrow());
		
		final Object customPropertyAfterUpdate = getCustomPropertyByName("customDataDictionaryEntryProperty", updatedEntry);
		assertEquals("An updated value", customPropertyAfterUpdate);

		try {
			final DataDictionaryPojo foundUpdatedEntry = findDataDictionaryEntryById(id);
			assertEquals(id, foundUpdatedEntry.getId());
			assertTrue("isCandidate must be true", foundUpdatedEntry.getIsCandidate());
			assertEquals(recordId, foundUpdatedEntry.identity());
			assertEquals(initialCreatedByUserId, foundUpdatedEntry.getCreatedByUserId());
			assertEquals(updatedDataElementName, foundUpdatedEntry.getName());
			assertEquals(Map.of(DataDictionaryVariableScope.FILE, Map.of(DataDictionaryVariableAttribute.FILE_DATASET.getKey(), updatedDataset),
					DataDictionaryVariableScope.SQL_DATABASE, Map.of(DataDictionaryVariableAttribute.SQL_DATABASE_TABLES.getKey(), updatedTables),
					DataDictionaryVariableScope.CICS_UI, Map.of(
							DataDictionaryVariableAttribute.CICS_UI_MAPSET.getKey(), updatedMapset,
							DataDictionaryVariableAttribute.CICS_UI_MAPNAME.getKey(), updatedMapName),
					DataDictionaryVariableScope.OTHER, Map.of()), foundUpdatedEntry.getScopes());
			assertEquals(updatedDescription, foundUpdatedEntry.getDescription());
			assertEquals(updatedLength, foundUpdatedEntry.getLength().orElseThrow().longValue());
			assertEquals("PIC9", foundUpdatedEntry.getFormat().orElseThrow());
			assertEquals(updatedModuleLocation, foundUpdatedEntry.getLocation().orElseThrow());
			
			final Object customPropertyFoundAfterUpdate = getCustomPropertyByName("customDataDictionaryEntryProperty", foundUpdatedEntry);
			assertEquals("An updated value", customPropertyFoundAfterUpdate);
			
			assertEquals(updatedPicClause, updatedEntry.getPicClause().orElseThrow());
			assertEquals(updatedLocation, updatedEntry.getDefinedLocation().orElseThrow());
			assertTrue("isBusiness should be true after update", updatedEntry.getIsBusiness().orElseThrow());
			assertEquals(updatedTransformation, updatedEntry.getFieldTransformation().orElseThrow());
			assertEquals(updatedInput, updatedEntry.getSourceInput().orElseThrow());
			assertEquals(updatedOutput, updatedEntry.getTargetOutput().orElseThrow());
			assertTrue("isReferenced should be true after update", updatedEntry.getIsReferenced().orElseThrow());
		} catch (final IllegalStateException e) {
			fail("The newly created data dictionary entry should be findable", e);
		}
	}
	
	@Test
	void testUpdateDataDictionaryEntryWithNullValues() throws Exception {
		final DataDictionaryPojo createdEntry = createFullDataDictionaryEntry("MyElmement", false, null);
		final Long id = createdEntry.getId();

		assertNotNull(id);
		assertFalse("isCandidate must be false here", createdEntry.getIsCandidate());

		final UpdateDataDictionaryEntry updateDataDictionaryEntry = dataDictionaryServiceProvider.updateDataDictionaryEntry();
		final Result<DataDictionaryPojo> updateResult = updateDataDictionaryEntry
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.setDataDictionaryEntry(new DataDictionaryPojoPrototype()
					.setUid(createdEntry.getUid())
					.setLength(null)
					.setFormat(null)
					.setScopes(Map.of())
					.setIsCandidate(true))
			.execute();
		
		assertEquals(HttpStatus.SC_OK, updateResult.getStatusCode());
		assertTrue("Updated result should be present", updateResult.getValue().isPresent());
		final DataDictionaryPojo updatedEntry = updateResult.getValue().get();
		assertNotNull(updatedEntry);
		
		assertTrue(updatedEntry.getLength().isEmpty());
		assertTrue(updatedEntry.getFormat().isEmpty());
		assert(updatedEntry.getScopes().isEmpty());
		assertNotNull(updatedEntry.getScopes());
		assertTrue("Scopes should be empty", updatedEntry.getScopes().isEmpty());
		assertTrue("isCandidate must be true", updatedEntry.getIsCandidate());

		try {
			final DataDictionaryPojo foundUpdatedEntry = findDataDictionaryEntryById(id);
			assertNotNull(foundUpdatedEntry.getScopes());
			assert(foundUpdatedEntry.getScopes().isEmpty());
			assertFalse(foundUpdatedEntry.getLength().isPresent());
			assertTrue("isCandidate must be true", foundUpdatedEntry.getIsCandidate());
			assertFalse(foundUpdatedEntry.getFormat().isPresent());
			assertTrue("Scopes should be empty", foundUpdatedEntry.getScopes().isEmpty());
		} catch (final IllegalStateException e) {
			fail("The newly created data dictionary entry should be findable", e);
		}
	}

	@Test
	void testUpdatedUserInitNull() throws IOException {
		final DataDictionaryPojo newEntry = createFullDataDictionaryEntry("AN-ELEMENT", true, null);
		assertTrue(newEntry.getUpdatedByUserId().isEmpty());
		assertTrue("isCandidate must be true", newEntry.getIsCandidate());
	}
	
	@Test
	void testUpdatedUserSetAfterUpdate() throws IOException {
		final DataDictionaryPojo newEntry = createFullDataDictionaryEntry("AN-ELEMENT", false, null);
		assertFalse("isCandidate should be false here", newEntry.getIsCandidate());
		final Result<DataDictionaryPojo> entryResultUpdate = dataDictionaryServiceProvider.updateDataDictionaryEntry()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setDataDictionaryEntry(new DataDictionaryPojoPrototype().setUid(newEntry.getUid()).setName("UPDATED-ELEMENT").setIsCandidate(true))
				.execute();
		assertTrue(entryResultUpdate.getStatusMessage(), entryResultUpdate.isValid());
		
		final DataDictionaryPojo foundUpdatedEntry = findDataDictionaryEntryById(newEntry.getId());
		assertEquals("admin", foundUpdatedEntry.getUpdatedByUserId().orElseThrow());
		assertTrue("isCandidate must be true", foundUpdatedEntry.getIsCandidate());
	}
	
	@Test
	void testDeleteDataDictionaryEntry() throws Exception {
		final DataDictionaryPojo createdEntry = createFullDataDictionaryEntry("MyElmement", false, null);
		final DataDictionaryPojo foundEntry = findDataDictionaryEntryById(createdEntry.getId());

		assertFalse("isCandidate should be false for delete", createdEntry.getIsCandidate());
		final DeleteDataDictionaryEntry deleteDataDictionaryEntryService = dataDictionaryServiceProvider.deleteDataDictionaryEntry();
		final Result<Void> deletedResult = deleteDataDictionaryEntryService
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setDataDictionaryEntryId(foundEntry.identity())
				.execute();
		
		assertEquals(HttpStatus.SC_NO_CONTENT, deletedResult.getStatusCode());
		assertFalse("Deleted dde should not be present", deletedResult.getValue().isPresent());
	}

	@Test
	void testDeleteDataDictionaryEntryThrowsWithoutProject() throws Exception {
		final DataDictionaryPojo createdEntry = createFullDataDictionaryEntry("MyElmement", false, null);
		assertFalse("isCandidate should be false for delete", createdEntry.getIsCandidate());
		
		final DeleteDataDictionaryEntry deleteDataDictionaryEntryService = dataDictionaryServiceProvider.deleteDataDictionaryEntry()
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setDataDictionaryEntryId(createdEntry.identity());
		
		Assertions.assertEquals("Project id must be set.", Assertions.assertThrows(IllegalStateException.class,
				deleteDataDictionaryEntryService::execute).getMessage());
	}
	
	@Test
	void testDeleteDataDictionaryEntryThrowsWithoutModule() throws Exception {
		final DataDictionaryPojo createdEntry = createFullDataDictionaryEntry("MyElmement", false, null);
		assertFalse("isCandidate should be false for delete", createdEntry.getIsCandidate());

		final DeleteDataDictionaryEntry deleteDataDictionaryEntryService = dataDictionaryServiceProvider.deleteDataDictionaryEntry()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setDataDictionaryEntryId(createdEntry.identity());
		
		Assertions.assertEquals("Module ID must be set.", Assertions.assertThrows(IllegalStateException.class,
				deleteDataDictionaryEntryService::execute).getMessage());
	}
	
	@Test
	void testDeleteDataDictionaryEntryThrowsWithoutDataDictionaryEntry() {
		final DeleteDataDictionaryEntry deleteDataDictionaryEntryService = dataDictionaryServiceProvider.deleteDataDictionaryEntry()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES);
		
		Assertions.assertEquals("Data dictionary entry ID must be set.", Assertions.assertThrows(IllegalStateException.class,
				deleteDataDictionaryEntryService::execute).getMessage());
	}
	
	@Test
	void testDeleteDataDictionaries() throws IOException, TimeoutException {
		Result<DataDictionaryPojo[]> resultFindAll = dataDictionaryServiceProvider.findAllDataDictionaryEntries()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID).setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES).execute();
		final int intialDataDictionariesLength = resultFindAll.getValue().get().length;
		final EntityId createdEntry_1 = createFullDataDictionaryEntry("MyElmement 1", false, new ModuleLocation(6000, 100)).identity();
		final EntityId createdEntry_2 = createFullDataDictionaryEntry("MyElmement 2", false, new ModuleLocation(700, 200)).identity();
		
		/* Check total count before deletion */
		resultFindAll = dataDictionaryServiceProvider.findAllDataDictionaryEntries().setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES).execute();
		DataDictionaryPojo[] allDataDictionaries = resultFindAll.getValue().get();
		assertEquals(intialDataDictionariesLength + 2, allDataDictionaries.length);
		
		final List<EntityId> idsToBeDeleted = Arrays.asList(createdEntry_1, createdEntry_2);
		final ResponseEntity<char[]> response = restTemplate.exchange(info.getUrl() + ANNOTATION_DATADICTIONARY_BULK_DELETE_URL, HttpMethod.DELETE, 
				new HttpEntity<>(idsToBeDeleted, getHttpHeaders(info)), char[].class, FIRST_CUSTOM_PROJECT_ID.getNid(), "Data Dictionary");
		assertNotNull(response.getBody());
		waitForJobCompletion(new String(response.getBody()), 2);
		
		resultFindAll = dataDictionaryServiceProvider.findAllDataDictionaryEntries().setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES).execute();
		allDataDictionaries = resultFindAll.getValue().get();
		assertEquals(intialDataDictionariesLength, allDataDictionaries.length);
	}
	
	@Test
	void testSearchForExistingDescriptionWithOneResult() throws Exception {
		final SearchDataDictionaryEntry searchDataDictionaryEntry = dataDictionaryServiceProvider.searchDataDictionaryEntry();
		final Result<DataDictionaryPojo[]> searchResult = searchDataDictionaryEntry
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setDescription("program")
			.execute();
		
		assertEquals(HttpStatus.SC_OK, searchResult.getStatusCode());
		assertTrue("Search result should be present", searchResult.getValue().isPresent());
		final DataDictionaryPojo[] dataDictionaryEntries = searchResult.getValue().get();
		assertEquals(1, dataDictionaryEntries.length);
		for (final DataDictionaryPojo entry : dataDictionaryEntries) {
			CustomProperties.verifyNumberOfCustomProperties(entry, 1);
		}
	}
	
	@Test
	void testSearchForExistingDescriptionWithMultipleResults() throws Exception {
		final SearchDataDictionaryEntry searchDataDictionaryEntry = dataDictionaryServiceProvider.searchDataDictionaryEntry();
		final Result<DataDictionaryPojo[]> searchResult = searchDataDictionaryEntry
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setDescription("element")
			.execute();
		
		assertEquals(HttpStatus.SC_OK, searchResult.getStatusCode());
		assertTrue("Search result should be present", searchResult.getValue().isPresent());
		final DataDictionaryPojo[] dataDictionaryEntries = searchResult.getValue().get();
		assertEquals(3, dataDictionaryEntries.length);
	}

	@Test
	void testSearchForNonExistingDescription() throws Exception {
		final SearchDataDictionaryEntry searchDataDictionaryEntry = dataDictionaryServiceProvider.searchDataDictionaryEntry();
		final Result<DataDictionaryPojo[]> searchResult = searchDataDictionaryEntry
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setDescription("not existing")
				.execute();

		assertEquals(HttpStatus.SC_OK, searchResult.getStatusCode());
		assertTrue("Search result should be present", searchResult.getValue().isPresent());
		final DataDictionaryPojo[] dataDictionaryEntries = searchResult.getValue().get();
		assertEquals(0, dataDictionaryEntries.length);
	}
	
	@Test
	void testSearchForExistingDataElementNameWithOneResult() throws Exception {
		final SearchDataDictionaryEntry searchDataDictionaryEntry = dataDictionaryServiceProvider.searchDataDictionaryEntry();
		final Result<DataDictionaryPojo[]> searchResult = searchDataDictionaryEntry
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setDataElementName("program")
				.execute();

		assertEquals(HttpStatus.SC_OK, searchResult.getStatusCode());
		assertTrue("Search result should be present", searchResult.getValue().isPresent());
		final DataDictionaryPojo[] dataDictionaryEntries = searchResult.getValue().get();
		assertEquals(1, dataDictionaryEntries.length);
	}
	
	@Test
	void testSearchForExistingDataElementNameWithMultipleResults() throws Exception {
		createFullDataDictionaryEntry("BARFOOBAZ", true, null);
		final SearchDataDictionaryEntry searchDataDictionaryEntry = dataDictionaryServiceProvider.searchDataDictionaryEntry();
		final Result<DataDictionaryPojo[]> searchResult = searchDataDictionaryEntry
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setDataElementName("my")
			.execute();
		
		assertEquals(HttpStatus.SC_OK, searchResult.getStatusCode());
		assertTrue("Search result should be present", searchResult.getValue().isPresent());
		final DataDictionaryPojo[] dataDictionaryEntries = searchResult.getValue().get();
		assertEquals(3, dataDictionaryEntries.length);
	}
	
	@Test
	void testSearchForNonExistingDataElementName() throws Exception {
		final SearchDataDictionaryEntry searchDataDictionaryEntry = dataDictionaryServiceProvider.searchDataDictionaryEntry();
		final Result<DataDictionaryPojo[]> searchResult = searchDataDictionaryEntry
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setDataElementName("non existing")
				.execute();

		assertEquals(HttpStatus.SC_OK, searchResult.getStatusCode());
		assertTrue("Search result should be present", searchResult.getValue().isPresent());
		final DataDictionaryPojo[] dataDictionaryEntries = searchResult.getValue().get();
		assertEquals(0, dataDictionaryEntries.length);
	}
	
	@Test
	void testSearchForDataElementNameAndDescription() throws Exception {
		final SearchDataDictionaryEntry searchDataDictionaryEntry = dataDictionaryServiceProvider.searchDataDictionaryEntry();
		final Result<DataDictionaryPojo[]> searchResult = searchDataDictionaryEntry
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setDataElementName("program")
				.setDescription("elemen")
				.execute();

		assertEquals(HttpStatus.SC_OK, searchResult.getStatusCode());
		assertTrue("Search result should be present", searchResult.getValue().isPresent());
		final DataDictionaryPojo[] dataDictionaryEntries = searchResult.getValue().get();
		assertEquals(1, dataDictionaryEntries.length);
	}

	@Test
	void testSearchForNonExistingDataElementNameAndDescriptionCombination() throws Exception {
		final SearchDataDictionaryEntry nonExistingDescriptionSearchDataDictionaryEntry = dataDictionaryServiceProvider.searchDataDictionaryEntry();
		final Result<DataDictionaryPojo[]> nonExistingDescriptionSearchResult = nonExistingDescriptionSearchDataDictionaryEntry
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setDataElementName("foo")
				.setDescription("non existing")
				.execute();

		assertEquals(HttpStatus.SC_OK, nonExistingDescriptionSearchResult.getStatusCode());
		assertTrue("Search result should be present", nonExistingDescriptionSearchResult.getValue().isPresent());
		final DataDictionaryPojo[] nonExistingDescriptionDataDictionaryEntries = nonExistingDescriptionSearchResult.getValue().get();
		assertEquals(0, nonExistingDescriptionDataDictionaryEntries.length);
		
		final SearchDataDictionaryEntry nonExistingDataElementNameSearchDataDictionaryEntry = dataDictionaryServiceProvider.searchDataDictionaryEntry();
		final Result<DataDictionaryPojo[]> nonExistingDataElementNameSearchResult = nonExistingDataElementNameSearchDataDictionaryEntry
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setDataElementName("non existing")
				.setDescription("foo")
				.execute();

		assertEquals(HttpStatus.SC_OK, nonExistingDataElementNameSearchResult.getStatusCode());
		assertTrue("Search result should be present", nonExistingDataElementNameSearchResult.getValue().isPresent());
		final DataDictionaryPojo[] nonExistingDataElementNameDataDictionaryEntries = nonExistingDataElementNameSearchResult.getValue().get();
		assertEquals(0, nonExistingDataElementNameDataDictionaryEntries.length);
	}
	
	@Test
	void testSearchThrowsWhenProjectIdIsNotSet() {
		final SearchDataDictionaryEntry searchDataDictionaryEntry = dataDictionaryServiceProvider.searchDataDictionaryEntry();
		Assertions.assertEquals("Project id must be set.", Assertions.assertThrows(IllegalStateException.class,
				searchDataDictionaryEntry::execute).getMessage());
	}
	
	@Test
	void testSearchThrowsWhenDescriptionAndDataElementNameIsNotSet() {
		final SearchDataDictionaryEntry searchDataDictionaryEntry = dataDictionaryServiceProvider.searchDataDictionaryEntry()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID);
		Assertions.assertEquals("Description AND/OR data element name must be set", Assertions.assertThrows(IllegalStateException.class,
				searchDataDictionaryEntry::execute).getMessage());
	}
	
	@Test
	void testFindAllDataDictionaryOtherScopes() throws Exception {
		final Result<String[]> findResult = dataDictionaryServiceProvider.findAllDataDictionaryOtherScopes()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.execute();
		
		assertEquals(HttpStatus.SC_OK, findResult.getStatusCode());
		assertTrue("Serialized data should be available", findResult.getValue().isPresent());
		
		final String[] dataDictionaryOtherScopes = findResult.getValue().get();
		assertEquals(NUMBER_OF_DEFAULT_DATA_DICTIONARY_OTHER_SCOPES, dataDictionaryOtherScopes.length);
	}
	
	@Test
	void testInvalidTextSelectionByOffset() throws IOException {
		final EntityId moduleAId = createModule(RESOURCE_PATH + "MMRS7101.cbl", Type.PROGRAM);
		final Result<Boolean> storeAstResult = moduleServiceProvider
				.storeAstNodes()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(moduleAId)
				.execute();
		
		assertEquals(202, storeAstResult.getStatusCode());
		try {
			restTemplate.exchange(
					info.getUrl() + VALIDATE_BY_OFFSET_URL, HttpMethod.GET,
					new HttpEntity<>(getHttpHeaders(info)),
					responseType,
					FIRST_CUSTOM_PROJECT_ID.getNid(), moduleAId.getNid(), Long.valueOf(240));
			fail("Expected an Exception due to invalid text selection");
		} catch(final HttpClientErrorException e) {
			assertEquals(404, e.getRawStatusCode());
			assertTrue("Message should contain module and offset information",
					Assert.assertNotNull(e.getMessage(), "Exception message should not be null").contains(String.format(
							"MiningEntityNotFoundException thrown from controller while trying to access /api/v1/projects/%s/modules/%s/data-dictionary/offset/240",
									FIRST_CUSTOM_PROJECT_ID.getNid(), moduleAId.getNid())));
		}
		
		/* Test that a field reference without a field definition is considered invalid selection. These are invalid since the CPYs have not been linked. */
		try {
			restTemplate.exchange(
					info.getUrl() + VALIDATE_BY_OFFSET_URL, HttpMethod.GET,
					new HttpEntity<>(getHttpHeaders(info)),
					responseType,
					FIRST_CUSTOM_PROJECT_ID.getNid(), moduleAId.getNid(), Long.valueOf(993));
			fail("Expected an Exception due to invalid text selection");
		} catch(final HttpClientErrorException e) {
			assertEquals(404, e.getRawStatusCode());
			assertTrue("Message should contail module and offset information",
					Assert.assertNotNull(e.getMessage(), "Exception message should not be null").contains(String.format(
							"MiningEntityNotFoundException thrown from controller while trying to access /api/v1/projects/%s/modules/%s/data-dictionary/offset/993",
							FIRST_CUSTOM_PROJECT_ID.getNid(), moduleAId.getNid())));
		}
	}

	@Test
	void testValidFieldDefinitionTextSelectionByOffset() throws IOException {
		final EntityId moduleAId = createModule(RESOURCE_PATH + "MMRS7101.cbl", Type.PROGRAM);
		final EntityId moduleBId = createModule(RESOURCE_PATH + "MMRS710A.cpy", Type.COPYBOOK);
		final EntityId moduleCId = createModule(RESOURCE_PATH + "MMRS710B.cpy", Type.COPYBOOK);
		final ModuleRelationshipPojoPrototype referenceBC = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(moduleBId)
				.setDstModule(moduleCId);
		final ModuleRelationshipPojoPrototype referenceAB = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(moduleAId)
				.setDstModule(moduleBId);
		referenceServiceProvider.createReference().setProjectId(FIRST_CUSTOM_PROJECT_ID).setModuleId(moduleAId).setReference(referenceAB).execute();
		referenceServiceProvider.createReference().setProjectId(FIRST_CUSTOM_PROJECT_ID).setModuleId(moduleAId).setReference(referenceBC).execute();

		final Result<Boolean> storeAstResult = moduleServiceProvider
				.storeAstNodes()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(moduleAId)
				.execute();
		
		assertEquals(202, storeAstResult.getStatusCode());
		
		final ResponseEntity<DataFieldFormat> validResult = restTemplate.exchange(
				info.getUrl() + VALIDATE_BY_OFFSET_URL, HttpMethod.GET,
				new HttpEntity<>(getHttpHeaders(info)),
				responseType,
				FIRST_CUSTOM_PROJECT_ID.getNid(), moduleAId.getNid(), Long.valueOf(341));
		
		assertEquals(200, validResult.getStatusCodeValue());
		final DataFieldFormat validSelection = Assert.assertNotNull(validResult.getBody());
		assertEquals(moduleAId, validSelection.getModuleId());
		assertEquals("PICX", validSelection.getLanguageType());
		assertEquals(10, validSelection.getByteLength());
		assertEquals("MY-PROGRAM-NAME", validSelection.getFieldName());
	}
	
	@Test
	void testValidFieldDefinitionTextSelectionByOffsetForFieldReferenceToCopybook() throws IOException {
		final EntityId moduleAId = createModule(RESOURCE_PATH + "MMRS7101.cbl", Type.PROGRAM);
		final EntityId moduleBId = createModule(RESOURCE_PATH + "MMRS710A.cpy", Type.COPYBOOK);
		final EntityId moduleCId = createModule(RESOURCE_PATH + "MMRS710B.cpy", Type.COPYBOOK);
		final ModuleRelationshipPojoPrototype referenceBC = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(moduleBId)
				.setDstModule(moduleCId);
		final ModuleRelationshipPojoPrototype referenceAB = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(moduleAId)
				.setDstModule(moduleBId);
		referenceServiceProvider.createReference().setProjectId(FIRST_CUSTOM_PROJECT_ID).setModuleId(moduleAId).setReference(referenceAB).execute();
		referenceServiceProvider.createReference().setProjectId(FIRST_CUSTOM_PROJECT_ID).setModuleId(moduleAId).setReference(referenceBC).execute();

		final Result<Boolean> storeAstResult = moduleServiceProvider
				.storeAstNodes()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(moduleAId)
				.execute();
		
		assertEquals(202, storeAstResult.getStatusCode());
		
		final ResponseEntity<DataFieldFormat> validResult = restTemplate.exchange(
				info.getUrl() + VALIDATE_BY_OFFSET_URL, HttpMethod.GET,
				new HttpEntity<>(getHttpHeaders(info)),
				responseType,
				FIRST_CUSTOM_PROJECT_ID.getNid(), moduleAId.getNid(), Long.valueOf(935));
		
		assertEquals(200, validResult.getStatusCodeValue());
		final DataFieldFormat validSelection = Assert.assertNotNull(validResult.getBody());
		assertEquals(moduleBId, validSelection.getModuleId());
		assertEquals("PICX", validSelection.getLanguageType());
		assertEquals(10, validSelection.getByteLength());
		assertEquals("MMRS-M01-GLOBAL-NAME", validSelection.getFieldName());
		assertFalse("This field should not be group", validSelection.isGroup());
	}
	
	@Test
	void testValidFieldDefinitionTextSelectionByOffsetForGroupParameter() throws IOException {
		final EntityId moduleAId = createModule(RESOURCE_PATH + "MMRS7112.cbl", Type.PROGRAM);
		final Result<Boolean> storeAstResult = moduleServiceProvider
				.storeAstNodes()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(moduleAId)
				.execute();

		assertEquals(202, storeAstResult.getStatusCode());
		final ResponseEntity<DataFieldFormat> validResult = restTemplate.exchange(info.getUrl() + VALIDATE_BY_OFFSET_URL,
				HttpMethod.GET, 
				new HttpEntity<>(getHttpHeaders(info)),
				responseType,
				FIRST_CUSTOM_PROJECT_ID.getNid(),
				moduleAId.getNid(),
				Long.valueOf(649));

		assertEquals(200, validResult.getStatusCodeValue());
		final DataFieldFormat validSelection = Assert.assertNotNull(validResult.getBody());
		assertEquals("MY-COUNTER", validSelection.getFieldName());
		assertTrue("This field should be group", validSelection.isGroup());
	}
	
	@Test
	void testValidFieldDefinitionTextSelectionByOffsetForFieldReferenceToCopybookToAnotherCopybook() throws IOException {
		final EntityId moduleAId = createModule(RESOURCE_PATH + "MMRS7101.cbl", Type.PROGRAM);
		final EntityId moduleBId = createModule(RESOURCE_PATH + "MMRS710A.cpy", Type.COPYBOOK);
		final EntityId moduleCId = createModule(RESOURCE_PATH + "MMRS710B.cpy", Type.COPYBOOK);
		final ModuleRelationshipPojoPrototype referenceBC = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(moduleBId)
				.setDstModule(moduleCId);
		final ModuleRelationshipPojoPrototype referenceAB = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(moduleAId)
				.setDstModule(moduleBId);
		referenceServiceProvider.createReference().setProjectId(FIRST_CUSTOM_PROJECT_ID).setModuleId(moduleAId).setReference(referenceAB).execute();
		referenceServiceProvider.createReference().setProjectId(FIRST_CUSTOM_PROJECT_ID).setModuleId(moduleAId).setReference(referenceBC).execute();

		final Result<Boolean> storeAstResult = moduleServiceProvider
				.storeAstNodes()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(moduleAId)
				.execute();
		
		assertEquals(202, storeAstResult.getStatusCode());
		
		final ResponseEntity<DataFieldFormat> validResult = restTemplate.exchange(
				info.getUrl() + VALIDATE_BY_OFFSET_URL, HttpMethod.GET,
				new HttpEntity<>(getHttpHeaders(info)),
				responseType,
				FIRST_CUSTOM_PROJECT_ID.getNid(), moduleAId.getNid(), Long.valueOf(991));
		
		assertEquals(200, validResult.getStatusCodeValue());
		final DataFieldFormat validSelection = Assert.assertNotNull(validResult.getBody());
		assertEquals(moduleCId, validSelection.getModuleId());
		assertEquals("PICX", validSelection.getLanguageType());
		assertEquals(10, validSelection.getByteLength());
		assertEquals("MMRS-M02-GLOBAL-NAME", validSelection.getFieldName());
	}
	
	@Test
	void testfindDataDictionaryBasedOnOffset() throws IOException {
		createFullDataDictionaryEntry("DataDictionaryEntry1", false, new ModuleLocation(3000, 100));
		createFullDataDictionaryEntry("DataDictionaryEntry2", false, new ModuleLocation(3200, 50));
		createFullDataDictionaryEntry("DataDictionaryEntry3", false, new ModuleLocation(3300, 100));
		createFullDataDictionaryEntry("DataDictionaryEntry4", false, new ModuleLocation(3500, 150));
		createFullDataDictionaryEntry("DataDictionaryEntry5", false, new ModuleLocation(3700, 100));
		createFullDataDictionaryEntry("DataDictionaryEntry6", false, new ModuleLocation(4000, 100));
		createFullDataDictionaryEntry("DataDictionaryEntry7", false, new ModuleLocation(4300, 200));
		createFullDataDictionaryEntry("DataDictionaryEntry8", false, new ModuleLocation(4600, 100));
		createFullDataDictionaryEntry("DataDictionaryEntry9", false, new ModuleLocation(5000, 200));
		createFullDataDictionaryEntry("DataDictionaryEntry10", false, new ModuleLocation(5300, 100));
		createFullDataDictionaryEntry("DataDictionaryEntry11", false, new ModuleLocation(5600, 250));
		createFullDataDictionaryEntry("DataDictionaryEntry12", false, new ModuleLocation(5950, 500));
		
		final Map<String, Object> urlParams = new HashMap<>();
		urlParams.put("projectId", FIRST_CUSTOM_PROJECT_ID.getNid());
		urlParams.put("moduleId", MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES.getNid());
		
		/* test Having Both EndOffset and StartOffset as Parameter*/
		final UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromHttpUrl(info.getUrl() + DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET)
				.queryParam("startOffset", "3300")
				.queryParam("endOffset", "4500");
		
		final ResponseEntity<DataDictionaryPojo[]> responseEntity = restTemplate.exchange(
			uriBuilder.buildAndExpand(urlParams).toUri(),
			HttpMethod.GET,
			new HttpEntity<>(getHttpHeaders(info)),
			responseTypeOfDataDictionaryBasedOnOffset);
		
		assertNotNull(responseEntity);
		assertEquals(HttpStatus.SC_OK, responseEntity.getStatusCodeValue());
		
		final DataDictionaryPojo[] ddeList  = responseEntity.getBody();
		assertNotNull(ddeList);
		assertEquals(5, ddeList.length);
		assertEquals(Set.of("DataDictionaryEntry3", "DataDictionaryEntry4", "DataDictionaryEntry5", "DataDictionaryEntry6", "DataDictionaryEntry7"),
				Arrays.asList(ddeList).stream().map(dde -> dde.getName()).collect(Collectors.toSet()));
		
		/* test Having Only StartOffset as Parameter*/
		final UriComponentsBuilder uriBuilder1 = UriComponentsBuilder.fromHttpUrl(info.getUrl() + DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET)
				.queryParam("startOffset", "4000");
		
		final ResponseEntity<DataDictionaryPojo[]> responseEntity1 = restTemplate.exchange(
			uriBuilder1.buildAndExpand(urlParams).toUri(),
			HttpMethod.GET,
			new HttpEntity<>(getHttpHeaders(info)),
			responseTypeOfDataDictionaryBasedOnOffset);
		
		assertNotNull(responseEntity1);
		assertEquals(HttpStatus.SC_OK, responseEntity1.getStatusCodeValue());
		
		final DataDictionaryPojo[] ddeList1  = responseEntity1.getBody();
		assertNotNull(ddeList1);
		assertEquals(7, ddeList1.length);
		assertEquals(Set.of("DataDictionaryEntry6", "DataDictionaryEntry7", "DataDictionaryEntry8", "DataDictionaryEntry9", "DataDictionaryEntry10",
				"DataDictionaryEntry11", "DataDictionaryEntry12"), Arrays.asList(ddeList1).stream().map(dde -> dde.getName())
				.collect(Collectors.toSet()));
	}

	@Test
	void testFindDataDictionariesByDataFlowId() throws IOException {
		final String fieldName1 = "DataDictionaryEntry1";
		final String fieldName2 = "DataDictionaryEntry2";
		final DataDictionaryPojo dde1 = createFullDataDictionaryEntry(fieldName1, false, new ModuleLocation(104, 12));
		final DataDictionaryPojo dde2 = createFullDataDictionaryEntry(fieldName2, false, new ModuleLocation(204, 30));

		final String dataFlowId1 = "data-flow-id-1";
		final String dataFlowId2 = "data-flow-id-2";
		createDataFlowNode(dataFlowId1, MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES, 100, 16, fieldName1, null);
		createDataFlowNode(dataFlowId2, MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES, 200, 34, fieldName2, null);

		final UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(info.getUrl() + DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS)
				.queryParam("dataFlowIds",String.join(",", Arrays.asList(dataFlowId1, dataFlowId2)));
		final ResponseEntity<List<EntityId>> dataDictionaryIds = restTemplate.exchange(
				URLDecoder.decode(builder.toUriString(), StandardCharsets.UTF_8), HttpMethod.GET,
				new HttpEntity<>(getHttpHeaders(info)),
				new ParameterizedTypeReference<List<EntityId>>() { },
				FIRST_CUSTOM_PROJECT_ID.getNid(), MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES.getNid());

		assertEquals(200, dataDictionaryIds.getStatusCodeValue());
		final Set<Long> ddeIds = dataDictionaryIds.getBody().stream().map(EntityId::getNid).collect(Collectors.toSet());
		assertEquals(2, ddeIds.size());
		assertTrue(ddeIds.contains(dde1.getId()));
		assertTrue(ddeIds.contains(dde2.getId()));
	}

	@Test
	void testFindDataDictionariesIncludedInCopyBookByDataFlowId() throws IOException {
		final String fieldName1 = "DataDictionaryEntry_1";
		final EntityId moduleA = createModuleWithContent("CBACT04C", Type.PROGRAM, "");
		final EntityId moduleB = createModuleWithContent("CVACT01Y", Type.COPYBOOK, "");
		final ModuleRelationshipPojoPrototype relationshipPojo = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(moduleA)
				.setDstModule(moduleB);
		referenceServiceProvider.createReference().setModuleId(moduleA).setReference(relationshipPojo).setProjectId(FIRST_CUSTOM_PROJECT_ID).execute();
		final DataDictionaryPojoPrototype dde1Prototype = new DataDictionaryPojoPrototype()
				.setName(fieldName1)
				.setLocation(new ModuleLocation(104, 12))
				.setCreatedByUserId("admin")
				.setModule(moduleB)
				.setDescription("Test DDE")
				.setDefinedLocation(DefinedLocation.COPYBOOK);

		final Result<DataDictionaryPojo> dde1 = dataDictionaryServiceProvider.createDataDictionaryEntry()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(moduleB)
				.setDataDictionaryEntry(dde1Prototype)
				.execute();

		assertEquals(201, dde1.getStatusCode());
		assertTrue("Created Module should be present", dde1.getValue().isPresent());

		final String dataFlowId1 = "data-flow-id-1";
		createDataFlowNode(dataFlowId1, moduleA, 100, 16, fieldName1, moduleB);

		final UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(info.getUrl() + DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS)
				.queryParam("dataFlowIds",String.join(",", List.of(dataFlowId1)));
		final ResponseEntity<List<EntityId>> dataDictionaryIds = restTemplate.exchange(
				URLDecoder.decode(builder.toUriString(), StandardCharsets.UTF_8), HttpMethod.GET,
				new HttpEntity<>(getHttpHeaders(info)),
				new ParameterizedTypeReference<List<EntityId>>() { },
				FIRST_CUSTOM_PROJECT_ID.getNid(), MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES.getNid());

		assertEquals(200, dataDictionaryIds.getStatusCodeValue());
		final Set<Long> ddeIds = dataDictionaryIds.getBody().stream().map(EntityId::getNid).collect(Collectors.toSet());
		assertEquals(1, ddeIds.size());
		assertTrue(ddeIds.contains(dde1.getValue().get().getId()));
	}

	private DataDictionaryPojo createFullDataDictionaryEntry(final String dataElementName, final boolean isCandidate,
			@Nullable final ModuleLocation moduleLocation) throws IOException {
		final CreateDataDictionaryEntry createDataDictionaryEntryService = dataDictionaryServiceProvider.createDataDictionaryEntry();

		final Map<String, Object> props = Map.of(CustomPropertyClass.DataDictionaryEntryCustomProperties.name(),
				Map.of("customDataDictionaryEntryProperty", "a created value for the custom property"));
		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype()
				.setName(dataElementName)
				.setLocation(new ModuleLocation(MODULE_LOCATION_OFFSET, MODULE_LOCATION_LENGTH))
				.setCreatedByUserId("admin")
				.setModule(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setScopes(Map.of(
						DataDictionaryVariableScope.FILE, Map.of(DataDictionaryVariableAttribute.FILE_DATASET.getKey(), "My data set name"),
						DataDictionaryVariableScope.SQL_DATABASE, Map.of(DataDictionaryVariableAttribute.SQL_DATABASE_TABLES.getKey(), "TEST TABLES"),
						DataDictionaryVariableScope.CICS_UI, Map.of(
								DataDictionaryVariableAttribute.CICS_UI_MAPSET.getKey(), "TEST MAPSET",
								DataDictionaryVariableAttribute.CICS_UI_MAPNAME.getKey(), "TEST MAPNAME"),
						DataDictionaryVariableScope.PARAMETER, Map.of(),
						DataDictionaryVariableScope.OTHER, Map.of(
								"scope", "SCOPE_1",
								"source", "TEST SOURCE")))
				.setDescription("MY description")
				.setFormat("PICX")
				.setIsCandidate(isCandidate)
				.setLength(999L)
				.setPicClause("TEST PIC CLAUSE")
				.setLocation(moduleLocation != null ? moduleLocation : new ModuleLocation(MODULE_LOCATION_OFFSET, MODULE_LOCATION_LENGTH))
				.setDefinedLocation(DefinedLocation.PROGRAM)
				.setState(WorkingState.CANDIDATE)
				.setFieldTransformation("TEST TRANSFORMATION")
				.setSourceInput("TEST INPUT")
				.setTargetOutput("TEST OUTPUT")
				.setCustomProperties(props);
	
		final Result<DataDictionaryPojo> createdResult = createDataDictionaryEntryService
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.setDataDictionaryEntry(dataDictionaryEntry)
			.execute();
		
		assertEquals(createdResult.getExtendedStatusMessage(), HttpStatus.SC_CREATED, createdResult.getStatusCode());
		assertTrue("Created result should be present", createdResult.getValue().isPresent());
		
		final DataDictionaryPojo createdDataDictionaryEntry = createdResult.getValue().get();
		CustomProperties.verifyNumberOfCustomProperties(createdDataDictionaryEntry, 1);
		final Object foundCustomProperty = getCustomPropertyByName("customDataDictionaryEntryProperty", createdDataDictionaryEntry);
		assertEquals("a created value for the custom property", foundCustomProperty);
		return createdDataDictionaryEntry;
	}

	private DataDictionaryPojo findDataDictionaryEntryById(final Long id) throws IOException {
		final FindAllDataDictionaryEntries findAllDataDictionaryEntries = dataDictionaryServiceProvider.findAllDataDictionaryEntries();
		final Result<DataDictionaryPojo[]> findResult = findAllDataDictionaryEntries
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
			.execute();
		assertEquals(HttpStatus.SC_OK, findResult.getStatusCode());
		assertTrue("Serialized data should be available", findResult.getValue().isPresent());
		
		final DataDictionaryPojo[] dataDictionaryEntries = findResult.getValue().get();
	
		for (final DataDictionaryPojo dataDictionaryEntry : dataDictionaryEntries) {
			if (dataDictionaryEntry.getId().equals(id)) {
				return dataDictionaryEntry;
			}
		}
		
		throw new IllegalStateException(String.format("Data dictionary entry with ID %d not found", id));
	}
	
	private EntityId createModule(final String path, final Type type) {
		final String content;
		try (final InputStream inputStream = Files.newInputStream(Paths.get("./src/test/resources/" + path))) {
			content = IOUtils.toString(inputStream, StandardCharsets.UTF_8);
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			return EntityId.VOID;
		}
		
		final String name = FilenameUtils.getBaseName(path);
		
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(name);
		module.setProject(FIRST_CUSTOM_PROJECT_ID);
		module.setTechnology(Technology.COBOL);
		module.setType(type);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setPath(path);
		module.setContent(content);
		module.setCreator(Creator.DISCOVERY);
		final Result<ModulePojo> resultCreate;
		try {
			resultCreate = moduleServiceProvider.createModule().setModule(module).setProjectId(FIRST_CUSTOM_PROJECT_ID).execute();
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			return EntityId.VOID;
		}
		assertEquals(201, resultCreate.getStatusCode());
		assertTrue("Created Module should be present", resultCreate.getValue().isPresent());
		return resultCreate.getValue().get().identity();
	}

	private EntityId createModuleWithContent(final String fileName, final Type type, final String content) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(fileName);
		module.setProject(FIRST_CUSTOM_PROJECT_ID);
		module.setTechnology(Technology.COBOL);
		module.setType(type);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setContent(content);
		module.setCreator(Creator.DISCOVERY);
		final Result<ModulePojo> resultCreate;
		try {
			resultCreate = moduleServiceProvider.createModule().setModule(module).setProjectId(FIRST_CUSTOM_PROJECT_ID).execute();
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			return EntityId.VOID;
		}
		assertEquals(201, resultCreate.getStatusCode());
		assertTrue("Created Module should be present", resultCreate.getValue().isPresent());
		return resultCreate.getValue().get().identity();
	}

	private UUID createAstNode(final EntityId moduleId, final Integer offset, final Integer length, @Nullable final EntityId includedModuleId) {
		return new AstPgDao(getDataSource()).put(new AstNodePojoPrototype()
				.setLabel("DummyLabel")
				.setType("DummyType")
				.setModule(moduleId)
				.setLocation(new AstNodeLocation(offset, length))
				.setIncludedModule(includedModuleId),
				true
		);
	}

	private UUID createDataFlowNode(final String dataFlowId, final EntityId moduleId, final Integer offset, final Integer length, final String name,
			final EntityId includedModuleId) {
		return new DataFlowPgDao(getDataSource()).put(new DataFlowNodePojoPrototype()
				.setModuleId(MODULE_ID_WHICH_HAS_DATA_DICTIONARY_ENTRIES)
				.setAstNode(createAstNode(moduleId, offset, length, includedModuleId))
				.setDataFlowId(DataFlowId.fromDb(dataFlowId))
				.setTraced(false)
				.setType(DataFlowNodePojo.Type.FIELD)
				.setName(name),
				true);
	}
}
