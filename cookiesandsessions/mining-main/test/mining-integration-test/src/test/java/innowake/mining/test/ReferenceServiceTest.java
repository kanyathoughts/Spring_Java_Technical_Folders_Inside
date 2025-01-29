/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsMapWithSize.anEmptyMap;
import static org.hamcrest.core.IsNot.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;
import org.apache.http.HttpStatus;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.module.ModuleServiceProvider;
import innowake.mining.client.service.reference.FindAllReferencesForModule;
import innowake.mining.client.service.reference.ReferenceServiceProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.ReferenceAttributes;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Integration tests for the Reference service.
 */
class ReferenceServiceTest extends IntegrationTest {

	private static final EntityId FIRST_CUSTOM_MODULE_ID = EntityId.of(2000l);
	private static final EntityId SECOND_CUSTOM_MODULE_ID = EntityId.of(2001l);
	private static final EntityId FIRST_CUSTOM_PROJECT_ID = EntityId.of(1l);
	private static final EntityId NON_EXISTING_ID = EntityId.of(Long.MAX_VALUE);

	private static final String KEY1 = "a";
	private static final String KEY2 = "c";
	private static final String VAL1 = "b";
	private static final String VAL2 = "d";
	private static final Map<String, Object> PROPERTIES = new HashMap<>();
		{
			PROPERTIES.put(KEY1, VAL1);
			PROPERTIES.put(KEY2, VAL2);
		}
	
	private ReferenceServiceProvider referenceServiceProvider = MiningApiClient.referenceService(getConnectionInfo());
	private ModuleServiceProvider moduleServiceProvider = MiningApiClient.moduleService(getConnectionInfo());
	
	@Test
	void testFindAllReferences() throws IOException {
		/* create a new module that has no references*/
		final ModuleServiceProvider moduleService = MiningApiClient.moduleService(getConnectionInfo());
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName("TEST MODULE FOR REFERECNE TEST");
		module.setProject(FIRST_CUSTOM_PROJECT_ID);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		final var moduleId = moduleService.createModule().setProjectId(FIRST_CUSTOM_PROJECT_ID).setModule(module).execute().getValue().get().identity();
		
		/* create two references for this new module*/
		final Result<ModuleRelationshipPojo> result = createReference(RelationshipType.CALLS, moduleId);
		assertEquals(201, result.getStatusCode());
		final Result<ModuleRelationshipPojo> result2 = createReference(RelationshipType.ACCESSES, moduleId);
		assertEquals(201, result2.getStatusCode());
		final var referenceCalls = result.getValue().get();
		final var referenceReadWrites = result2.getValue().get();
		
		/* find all references for the module */
		final Result<ModuleRelationshipPojo[]> resultAllReferences = referenceServiceProvider.findAllReferencesForModule().setProjectId(FIRST_CUSTOM_PROJECT_ID).setModuleId(moduleId).execute();
		assertEquals(200, resultAllReferences.getStatusCode());
		final ModuleRelationshipPojo[] allReferences = resultAllReferences.getValue().get();
		assertEquals(2, allReferences.length);
		
		/* check that found references are the two newly created references */
		if (referenceCalls.equals(allReferences[0])) {
			assertEquals(referenceReadWrites, allReferences[1]);
		} else {
			assertEquals(referenceReadWrites, allReferences[0]);
			assertEquals(referenceCalls, allReferences[1]);
		}
	}
	
	@Test
	void testReferencesWhenDistinctTrue() throws IOException {
		testFindAllReferencesWithDistinct(true);
	}
	
	@Test
	void testReferencesWhenDistinctFalse() throws IOException {
		testFindAllReferencesWithDistinct(false);
	}

	public void testFindAllReferencesWithDistinct(final boolean distinct) throws IOException {
		/* create a new module that has no references */
		final ModuleServiceProvider moduleService = MiningApiClient.moduleService(getConnectionInfo());
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName("TEST MODULE FOR REFERECNE TEST");
		module.setProject(FIRST_CUSTOM_PROJECT_ID);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		final var moduleId = moduleService.createModule().setProjectId(FIRST_CUSTOM_PROJECT_ID).setModule(module)
				.execute().getValue().get().identity();

		/* create two references for this new module */
		final Result<ModuleRelationshipPojo> result = createReference(RelationshipType.CALLS, moduleId);
		assertEquals(201, result.getStatusCode());
		final Result<ModuleRelationshipPojo> result2 = createReference(RelationshipType.CALLS, moduleId);
		assertEquals(201, result2.getStatusCode());

		/* find all references for the module */
		final FindAllReferencesForModule resultReferences = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID).setModuleId(moduleId).setDistinct(distinct);
		final Result<ModuleRelationshipPojo[]> resultAllReferences = resultReferences.execute();
		assertEquals(200, resultAllReferences.getStatusCode());
		final ModuleRelationshipPojo[] allReferences = resultAllReferences.getValue().get();
		if (Boolean.TRUE.equals(distinct)) {
			assertEquals(1, allReferences.length);
		} else {
			assertEquals(2, allReferences.length);
		}
	}
	
	/**
	 * Tests the location information from the Module and the Reference after
	 * hitting their respective end points.
	 *
	 * @throws IOException if error occurs during IO operation
	 */
	@Test
	void testLocationInformation() throws IOException {
		/* create a new module that has no references*/
		final ModuleServiceProvider moduleService = MiningApiClient.moduleService(getConnectionInfo());
		final int moduleOffset = 2;
		final int moduleLength = 32;
		final var moduleId = createModuleWithLocationInformation(moduleService, moduleOffset, moduleLength);
		final Result<ModulePojo> moduleResult = moduleService.findModuleById()
		                                                 .setModuleId(moduleId)
		                                                 .setIncludeContent(true)
		                                                 .setProjectId(FIRST_CUSTOM_PROJECT_ID)
		                                                 .execute();
		final var resultModule = moduleResult.getValue().get();
		final ModuleLocation location = resultModule.getLocation().orElseThrow();
		assertNotNull(location);
		assertEquals(Integer.valueOf(moduleOffset), location.getOffset());
		assertEquals(Integer.valueOf(moduleLength), location.getLength());
		
		final int referenceOffset = 2;
		final int referenceLength = 10;
		createReferenceWithLocationInformation(RelationshipType.CALLS, moduleId, referenceOffset, referenceLength);
		/* find all references for the module */
		final Result<ModuleRelationshipPojo[]> resultAllReferences = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(moduleId)
				.execute();
		assertEquals(HttpStatus.SC_OK, resultAllReferences.getStatusCode());
		final ModuleRelationshipPojo[] allReferences = resultAllReferences.getValue().get();
		assertEquals(1, allReferences.length);
		final ModuleLocation moduleLocation = allReferences[0].getSrcLocation().orElse(null);
		assertNotNull(moduleLocation);
		assertEquals(Integer.valueOf(moduleOffset), moduleLocation.getOffset());
		assertEquals(Integer.valueOf(referenceLength), moduleLocation.getLength());
	}
	
	@Test
	void testFindAllReferencesWithDirection() throws IOException {
		/* create a new module that has no references*/
		final ModuleServiceProvider moduleService = MiningApiClient.moduleService(getConnectionInfo());
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName("TEST MODULE FOR REFERECNE TEST");
		module.setProject(FIRST_CUSTOM_PROJECT_ID);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		final var moduleId = moduleService.createModule().setProjectId(FIRST_CUSTOM_PROJECT_ID).setModule(module).execute().getValue().get().identity();
		
		/* create two references for this new module*/
		final Result<ModuleRelationshipPojo> result = createReference(RelationshipType.CALLS, moduleId);
		assertEquals(201, result.getStatusCode());
		final Result<ModuleRelationshipPojo> result2 = createReference(RelationshipType.ACCESSES, moduleId);
		assertEquals(201, result2.getStatusCode());
		final var referenceCalls = result.getValue().get();
		final var referenceReadWrites = result2.getValue().get();
		
		/* find all incoming references for the module 1 */
		final Result<ModuleRelationshipPojo[]> resultAllReferencesInModule1 = referenceServiceProvider.findAllReferencesForModule().setProjectId(FIRST_CUSTOM_PROJECT_ID).setModuleId(moduleId).setDirection(RelationshipDirection.IN).execute();
		assertEquals(200, resultAllReferencesInModule1.getStatusCode());
		final ModuleRelationshipPojo[] allReferencesIn1 = resultAllReferencesInModule1.getValue().get();
		assertEquals(0, allReferencesIn1.length);

		/* find all outgoing references for the module 1 */
		final Result<ModuleRelationshipPojo[]> resultAllReferencesOutModule1 = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(moduleId)
				.setDirection(RelationshipDirection.OUT)
				.execute();
		assertEquals(200, resultAllReferencesOutModule1.getStatusCode());
		final ModuleRelationshipPojo[] allReferencesOut1 = resultAllReferencesOutModule1.getValue().get();
		assertEquals(2, allReferencesOut1.length);
		
		/* check that found references are the two newly created references */
		if (referenceCalls.equals(allReferencesOut1[0])) {
			assertEquals(referenceReadWrites, allReferencesOut1[1]);
		} else {
			assertEquals(referenceReadWrites, allReferencesOut1[0]);
			assertEquals(referenceCalls, allReferencesOut1[1]);
		}
		
		/* find all incoming references for the module 2 */
		final Result<ModuleRelationshipPojo[]> resultAllReferencesInModule2 = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(SECOND_CUSTOM_MODULE_ID)
				.setDirection(RelationshipDirection.IN)
				.execute();
		assertEquals(200, resultAllReferencesInModule2.getStatusCode());
		final ModuleRelationshipPojo[] allReferencesIn2 = resultAllReferencesInModule2.getValue().get();
		assertEquals(3, allReferencesIn2.length);

		/* find all outgoing references for the module 2 */
		final Result<ModuleRelationshipPojo[]> resultAllReferencesOutModule2 = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(SECOND_CUSTOM_MODULE_ID)
				.setDirection(RelationshipDirection.OUT)
				.execute();
		assertEquals(200, resultAllReferencesOutModule2.getStatusCode());
		final ModuleRelationshipPojo[] allReferencesOut2 = resultAllReferencesOutModule2.getValue().get();
		assertEquals("Must be empty, annotations have their own service, and are not count towards normal references.", 0, allReferencesOut2.length);
	}
	
	@Test
	void testReadWriteReferenceDbAccessTypesString() throws IOException {
		/* the access types used for this test */
		final List<String> dbAccessTypes = new ArrayList<>();
		dbAccessTypes.add(DatabaseAccessType.OTHER.name());
		dbAccessTypes.add(DatabaseAccessType.DELETE.name());
		
		final List<String> dbStmts = new ArrayList<>();
		dbStmts.add("DECLARE_CURSOR");
		
		/* create the reference with the access types */
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(FIRST_CUSTOM_MODULE_ID)
				.setDstModule(SECOND_CUSTOM_MODULE_ID);
		final Map<String, Object> properties = new HashMap<>();
		properties.put(ReferenceAttributes.DB_ACCESS_TYPES.getReferenceAttributeExcelName(), StringUtils.join(dbAccessTypes, ","));
		properties.put(ReferenceAttributes.DB_ACCESS_OPERATIONS.getReferenceAttributeExcelName(), StringUtils.join(dbStmts, ","));
		reference.setProperties(properties);
		final Result<ModuleRelationshipPojo> result = referenceServiceProvider.createReference()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(FIRST_CUSTOM_MODULE_ID)
			.setReference(reference)
			.execute();
		final var resultReference = result.getValue().get();
		
		/* check that the returned reference actually contains the access types */
		final Map<String, Object> resultProperties = resultReference.getProperties().orElse(Collections.emptyMap());
		MatcherAssert.assertThat(resultProperties, not(anEmptyMap()));
		assertNotNull(resultProperties);
		final List<String> resultAccessTypes = Arrays.asList(resultProperties.get(ReferenceAttributes.DB_ACCESS_TYPES.getReferenceAttributeExcelName()).toString().split(","));
		for (final String accessType : resultAccessTypes) {
			assertTrue(dbAccessTypes.remove(accessType));
		}
		assertTrue(dbAccessTypes.isEmpty());
		
		final List<String> resultDbStmts = Arrays.asList(resultProperties.get(ReferenceAttributes.DB_ACCESS_OPERATIONS.getReferenceAttributeExcelName()).toString().split(","));
		for (final String stmt : resultDbStmts) {
			assertTrue(dbStmts.remove(stmt));
		}
		assertTrue(dbStmts.isEmpty());
		
		final Result<ModuleRelationshipPojo[]> resultFindAll = referenceServiceProvider.findAllReferencesForModule()
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setRelationship(RelationshipType.ACCESSES)
				.execute();
		final ModuleRelationshipPojo[] allReadWrites = resultFindAll.getValue().get();
		assertTrue(containsReference(allReadWrites, resultReference));
	}
	
	@Test
	void testReadWriteReferenceDbAccessTypesEnum() throws IOException {
		/* the access types used for this test */
		final List<DatabaseAccessType> dbAccessTypes = new ArrayList<>();
		dbAccessTypes.add(DatabaseAccessType.READ);
		dbAccessTypes.add(DatabaseAccessType.OTHER);
		
		final List<String> dbStmts = new ArrayList<>();
		dbStmts.add("FIND");
		dbStmts.add("ABC");
		
		/* create the reference with the access types */
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(FIRST_CUSTOM_MODULE_ID)
				.setDstModule(SECOND_CUSTOM_MODULE_ID);

		final Map<String, Object> properties = new HashMap<>();
		properties.put(ReferenceAttributes.DB_ACCESS_TYPES.getReferenceAttributeExcelName(), StringUtils.join(dbAccessTypes, ","));
		properties.put(ReferenceAttributes.DB_ACCESS_OPERATIONS.getReferenceAttributeExcelName(), StringUtils.join(dbStmts, ","));
		reference.setProperties(properties);
		final Result<ModuleRelationshipPojo> result = referenceServiceProvider.createReference()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(FIRST_CUSTOM_MODULE_ID)
			.setReference(reference)
			.execute();
		final var resultReference = result.getValue().get();
		
		/* check that the returned reference actually contains the access types */
		final Map<String, Object> resultProperties = resultReference.getProperties().orElse(Collections.emptyMap());
		assertThat(resultProperties, not(anEmptyMap()));
		assertNotNull(resultProperties);
		final List<String> resultAccessTypes = Arrays.asList(resultProperties.get(ReferenceAttributes.DB_ACCESS_TYPES.getReferenceAttributeExcelName()).toString().split(","));
		for (final String accessType : resultAccessTypes) {
			assertTrue(dbAccessTypes.remove(DatabaseAccessType.valueOf(accessType)));
		}
		assertTrue(dbAccessTypes.isEmpty());
		
		final List<String> resultDbStmts = Arrays.asList(resultProperties.get(ReferenceAttributes.DB_ACCESS_OPERATIONS.getReferenceAttributeExcelName()).toString().split(","));
		for (final String stmt : resultDbStmts) {
			assertTrue(dbStmts.remove(stmt));
		}
		assertTrue(dbStmts.isEmpty());
		
		final Result<ModuleRelationshipPojo[]> resultFindAll = referenceServiceProvider.findAllReferencesForModule()
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setRelationship(RelationshipType.ACCESSES)
				.execute();
		final ModuleRelationshipPojo[] allReadWrites = resultFindAll.getValue().get();
		assertTrue(containsReference(allReadWrites, resultReference));
	}
	
	/*
	 * Test to check changes made to remove duplicate references WRT WMIN-828 ticket.
	 */
	@Test
	void testFindAllReferencesRemoveDuplicateResults() throws IOException {
		final Result<ModuleRelationshipPojo[]> resultAllReferences = referenceServiceProvider.findAllReferences().setProjectId(FIRST_CUSTOM_PROJECT_ID).execute();
		assertEquals(200, resultAllReferences.getStatusCode());
		final ModuleRelationshipPojo[] references = resultAllReferences.getValue().get();
		final Set<ModuleRelationshipPojo> uniqueReferences = new HashSet<>(Arrays.asList(references));
		assertEquals(uniqueReferences.size(), references.length);
	}
	
	@Test
	void testFindAllCallsReferencesDirection() throws IOException {
		final Result<ModuleRelationshipPojo> resultCreate = createReference(RelationshipType.CALLS, FIRST_CUSTOM_MODULE_ID);
		assertEquals(201, resultCreate.getStatusCode());
		Result<ModuleRelationshipPojo[]> result = referenceServiceProvider.findAllReferences()
				.setProjectId(EntityId.of(1l))
				.setRelationship(RelationshipType.CALLS)
				.setDirection(RelationshipDirection.OUT)
				.execute();
		
		assertEquals(200, result.getStatusCode());
		final int lengthOut = result.getValue().get().length;
		assertNotEquals(0, lengthOut);
		
		result = referenceServiceProvider.findAllReferences()
				.setProjectId(EntityId.of(1l))
				.setRelationship(RelationshipType.CALLS)
				.setDirection(RelationshipDirection.IN)
				.execute();
		assertEquals(200, result.getStatusCode());
		final int lengthIn = result.getValue().get().length;
		assertNotEquals(0, lengthIn);
		/*
		 * There cannot be more incoming than outgoing references.
		 */
		assertTrue(lengthOut >= lengthIn);
	}
	
	@Test
	void testFindAllHasAnnotationReferencesForModule() throws IOException {
		final Result<ModuleRelationshipPojo[]> result = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(EntityId.of(1l))
				.setModuleId(EntityId.of(2000l))
				.execute();

		assertEquals(200, result.getStatusCode());
		ModuleRelationshipPojo[] values = result.getValue().get();
		List<RelationshipType> resultNames = Stream.of(values)
				.map(ModuleRelationshipPojo::getRelationship)
				.collect(Collectors.toList());
		MatcherAssert.assertThat(resultNames, Matchers.equalTo(Arrays.asList(RelationshipType.CALLS, RelationshipType.CALLS, RelationshipType.CALLS)));

		Map<String, String> resultModuleNames = Stream.of(values)
				.collect(Collectors.toMap(
						p -> getModuleName(p.getDstModule()),
						p -> getModuleName(p.getSrcModule())
				));
		Map<String, String> expectedModuleNames = Map.of("MMRS7101","PRG1",
				"QBGPSLP1MMRS710A.STEP01.MMRS7102","PRG1",
				"DPGM1","PRG1"
		);

		MatcherAssert.assertThat(resultModuleNames, Matchers.equalTo(expectedModuleNames));
	}

	private String getModuleName(UUID moduleUid) {
		try {
			return moduleServiceProvider.findModuleById()
					.setProjectId(FIRST_CUSTOM_PROJECT_ID)
					.setModuleId(EntityId.of(moduleUid))
					.execute().getValue()
					.map(ModulePojo::getName).get();
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}

	@Test
	void testCreateCallsReference() throws IOException {
		final Result<ModuleRelationshipPojo> result = createReference(RelationshipType.CALLS);
		assertNotNull(result);
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
	}

	@Test
	void testCreateIncludesReference() throws IOException {
		final Result<ModuleRelationshipPojo> result = createReference(RelationshipType.INCLUDES);
		assertNotNull(result);
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
	}

	@Test
	void testCreateReferencesReference() throws IOException {
		final Result<ModuleRelationshipPojo> result = createReference(RelationshipType.REFERENCES);
		assertNotNull(result);
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
	}

	@Test
	void testCreateReadsWritesReference() throws IOException {
		final Result<ModuleRelationshipPojo> result = createReference(RelationshipType.ACCESSES);
		assertNotNull(result);
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
	}
	
	@Test
	void testCreateNoneReference() throws IOException {
		final Result<ModuleRelationshipPojo> result = createReference(RelationshipType.NONE);
		assertNotNull(result);
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
	}
	
	@Test
	void testCreateAndFindMultipleReferences() throws IOException {
		final ModuleRelationshipPojo[] beforeCreationResult = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.execute().getValue().get();

		Map<String, List<String>> beforeCreation = Stream.of(beforeCreationResult)
				.collect(Collectors.toMap(
						p -> getModuleName(p.getDstModule()),
						p -> Arrays.asList(getModuleName(p.getSrcModule()), Objects.toString(p.getRelationship())))
				);
		Map<String, List<String>> expectedBeforeCreation = Map.of("MMRS7101", Arrays.asList("PRG1", "CALLS"),
				"QBGPSLP1MMRS710A.STEP01.MMRS7102", Arrays.asList("PRG1", "CALLS"),
				"DPGM1", Arrays.asList("PRG1", "CALLS")
		);
		assertThat(beforeCreation, Matchers.equalTo(expectedBeforeCreation));

		createReference(RelationshipType.NONE);
		//this create ignores an existing reference due to missing unique constraint, see comparison below
		createReference(RelationshipType.CALLS);

		final ModuleRelationshipPojo[] afterCreationResult = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.execute().getValue().get();

		Map<String, List<String>> afterCreation = Stream.of(afterCreationResult)
				.sorted((o1, o2) -> o1.getRelationship().compareTo(o2.getRelationship()))
				.collect(Collectors.toMap(
						p -> getModuleName(p.getDstModule()),
						p -> new LinkedList<>(Arrays.asList(getModuleName(p.getSrcModule()), Objects.toString(p.getRelationship()))),
						(l1, l2) -> {
							l1.addAll(l2);
							return l1;
						})
				);
		Assertions.assertEquals(3, afterCreation.size());
		Assertions.assertEquals(Arrays.asList("PRG1", "CALLS"), afterCreation.get("MMRS7101"));
		//this shows that we have potential duplicates, as the comment above describes.
		Assertions.assertEquals(Arrays.asList("PRG1", "NONE", "PRG1", "CALLS", "PRG1", "CALLS"), afterCreation.get("QBGPSLP1MMRS710A.STEP01.MMRS7102"));
		Assertions.assertEquals(Arrays.asList("PRG1", "CALLS"), afterCreation.get("DPGM1"));
	}
	
	@Test
	void testCreateAndFindCallReference() throws IOException {
		final Result<ModuleRelationshipPojo[]> beforeCreationResult = referenceServiceProvider.findAllReferencesForModule()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(FIRST_CUSTOM_MODULE_ID)
			.setRelationship(RelationshipType.CALLS)
			.execute();

		createReference(RelationshipType.NONE);
		createReference(RelationshipType.CALLS);
		
		final Result<ModuleRelationshipPojo[]> afterCreationResult = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.setRelationship(RelationshipType.CALLS)
				.execute();
		
		final ModuleRelationshipPojo[] beforeCreationReferences = beforeCreationResult.getValue().get();
		final ModuleRelationshipPojo[] afterCreationReferences = afterCreationResult.getValue().get();
		
		assertEquals(afterCreationReferences.length, beforeCreationReferences.length + 1);
	}
	
	@Test
	void testFindReferenceById() throws IOException {
		final Result<ModuleRelationshipPojo> result = createReference(RelationshipType.CALLS);
		final var createdReference = result.getValue().get();
		
		final Result<ModuleRelationshipPojo> findResult = referenceServiceProvider.findReferenceById()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(FIRST_CUSTOM_MODULE_ID)
			.setReferenceId(createdReference.getId())
			.execute();
		assertNotNull(findResult);
		assertEquals(200, findResult.getStatusCode());
		assertTrue(findResult.getValue().isPresent());
		
		final var foundReference = findResult.getValue().get();
		assertEquals(createdReference, foundReference);
	}
	
	@Test 
	void testDeleteReference() throws IOException {
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(FIRST_CUSTOM_MODULE_ID)
				.setDstModule(SECOND_CUSTOM_MODULE_ID)
				.setSrcLocation(new ModuleLocation(100, 4));
		final Result<ModuleRelationshipPojo> createResult = referenceServiceProvider.createReference()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.setReference(reference)
				.execute();

		final var createdReferenceId = createResult.getValue().get().getId();
		/* get the from module location and verify that it contains the expected offset and length */
		final ModuleLocation fromModuleLocation = referenceServiceProvider.findReferenceById()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.setReferenceId(createdReferenceId)
				.execute()
				.getValue().get()
				.getSrcLocation().orElse(null);
		assertLocation(fromModuleLocation, 100, 4);
		
		/* delete the reference and verify that the from module location is also deleted */
		final Result<Void> result = referenceServiceProvider.deleteReference()
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setReferenceId(createdReferenceId)
				.execute();
		assertEquals(204, result.getStatusCode());
	}
	
	@Test
	void testDeleteFromAndToReference() throws IOException {
		/* get CALLS reference */
		final ModuleServiceProvider moduleService = MiningApiClient.moduleService(getConnectionInfo());
		final Result<ModulePojo[]> moduleResult = moduleService.findModuleByName().setProjectId(FIRST_CUSTOM_PROJECT_ID).setName("EXECSQL").execute();
		assertEquals(200, moduleResult.getStatusCode());
		
		final Result<ModuleRelationshipPojo[]> resultAllReferences = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(moduleResult.getValue().get()[0].identity())
				.setRelationship(RelationshipType.CALLS)
				.execute();
		assertEquals(200, resultAllReferences.getStatusCode());
		final ModuleRelationshipPojo[] references = resultAllReferences.getValue().get();
		assertEquals(1, references.length);
		final var reference = references[0];
		
		/* get the from and to module location and verify that they contain the expected offsets and lengths */
		assertLocation(reference.getSrcLocation().orElse(null), 100, 4);
		assertLocation(reference.getDstLocation().orElse(null), 200, 5);
		
		/* delete the reference and verify that the from and to module location is also deleted */
		final Result<Void> result = referenceServiceProvider.deleteReference().setModuleId(FIRST_CUSTOM_MODULE_ID).setProjectId(FIRST_CUSTOM_PROJECT_ID).setReferenceId(reference.getId()).execute();
		assertEquals(204, result.getStatusCode());
	}

	@Test
	void testCreateAndDeleteAllReferences() throws IOException {
		createReference(RelationshipType.NONE);
		createReference(RelationshipType.CALLS);
		final Result<ModuleRelationshipPojo[]> beforeDeletionResult = referenceServiceProvider.findAllReferencesForModule()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(FIRST_CUSTOM_MODULE_ID)
			.execute();
	
		final ModuleRelationshipPojo[] beforeDeletionReferences = beforeDeletionResult.getValue().get();
		assertTrue(beforeDeletionReferences.length >= 2);

		for (final var reference : beforeDeletionReferences) {
			final Result<Void> deletionResult = referenceServiceProvider.deleteReference()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.setReferenceId(reference.getId())
				.execute();
			
			assertEquals(204, deletionResult.getStatusCode());
		}

		final Result<ModuleRelationshipPojo[]> afterDeletionResult = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.execute();

		assertNotNull(afterDeletionResult);
		assertEquals(200, afterDeletionResult.getStatusCode());
		assertTrue(afterDeletionResult.getValue().isPresent());
		final ModuleRelationshipPojo[] afterDeletionReferences = afterDeletionResult.getValue().get();
		assertEquals(0, afterDeletionReferences.length);
	}
	
	@Test
	void testFindAllReferencesOnNonExistingProject() throws IOException {
		final Result<ModuleRelationshipPojo[]> result = referenceServiceProvider.findAllReferencesForModule()
			.setProjectId(NON_EXISTING_ID)
			.setModuleId(NON_EXISTING_ID)
			.execute();
		
		assertEquals(404, result.getStatusCode());
	}
	
	@Test
	void testFindAllReferencesOnNonExistingModule() throws IOException {
		final Result<ModuleRelationshipPojo[]> result = referenceServiceProvider.findAllReferencesForModule()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(NON_EXISTING_ID)
			.execute();
		
		assertEquals(404, result.getStatusCode());
	}
	
	@Test
	void testFindReferenceByIdOnNonExistingProject() throws IOException {
		final Result<ModuleRelationshipPojo> reference = createReference(RelationshipType.CALLS);
		final var referenceId = reference.getValue().get().getId();
		final Result<ModuleRelationshipPojo> result = referenceServiceProvider.findReferenceById()
			.setProjectId(NON_EXISTING_ID)
			.setModuleId(NON_EXISTING_ID)
			.setReferenceId(referenceId)
			.execute();
		
		assertEquals(404, result.getStatusCode());
	}
	
	@Test
	void testFindReferenceByIdOnNonExistingModule() throws IOException {
		final Result<ModuleRelationshipPojo> reference = createReference(RelationshipType.CALLS);
		final UUID referenceId = reference.getValue().get().getId();
		final Result<ModuleRelationshipPojo> result = referenceServiceProvider.findReferenceById()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(NON_EXISTING_ID)
			.setReferenceId(referenceId)
			.execute();
		
		assertEquals(404, result.getStatusCode());
	}
	
	@Test
	void testFindReferenceByIdOnNonExistingReference() throws IOException {
		createReference(RelationshipType.CALLS);
		final Result<ModuleRelationshipPojo> result = referenceServiceProvider.findReferenceById()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(FIRST_CUSTOM_MODULE_ID)
			.setReferenceId(UUID.randomUUID())
			.execute();
		
		assertEquals(404, result.getStatusCode());
	}
	
	@Test
	void testWithProperties() throws IOException {
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(FIRST_CUSTOM_MODULE_ID)
				.setDstModule(SECOND_CUSTOM_MODULE_ID);
		reference.setProperties(PROPERTIES);
		
		final Result<ModuleRelationshipPojo> result = referenceServiceProvider.createReference()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(FIRST_CUSTOM_MODULE_ID)
			.setReference(reference)
			.execute();
		final var resultReference = result.getValue().get();

		assertProperties(resultReference.getProperties());
		
		final var resultReferenceAfterFind = referenceServiceProvider.findReferenceById()
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setReferenceId(resultReference.getId())
				.execute()
				.getValue().get();
		
		assertProperties(resultReferenceAfterFind.getProperties());
	}
	
	@Test
	void testReferenceFromAndToNames() throws IOException {
		final String fromModuleName = "PRG1";
		final String toModuleName = "QBGPSLP1MMRS710A.STEP01.MMRS7102";
		
		final Result<ModuleRelationshipPojo> result = createReference(RelationshipType.CALLS, FIRST_CUSTOM_MODULE_ID);
		
		assertEquals(201, result.getStatusCode());
		UUID srcModule = result.getValue().get().getSrcModule();
		String srcName = moduleServiceProvider.findModuleById()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(EntityId.of(srcModule))
			.execute().getValue().get()
			.getName();
		assertEquals(fromModuleName, srcName);
		UUID dstModule = result.getValue().get().getDstModule();
		String dstName = moduleServiceProvider.findModuleById()
				.setProjectId(FIRST_CUSTOM_PROJECT_ID)
				.setModuleId(EntityId.of(dstModule))
				.execute().getValue().get()
				.getName();
		assertEquals(toModuleName, dstName);
	}
	
	private void assertProperties(final Optional<Map<String, Object>> props) {
		assertTrue(props.isPresent());
		final Map<String, Object> properties = props.get();
		assertThat(properties, not(anEmptyMap()));
		assertTrue(properties.containsKey(KEY1));
		assertEquals(VAL1, properties.get(KEY1));
		assertTrue(properties.containsKey(KEY2));
		assertEquals(VAL2, properties.get(KEY2));
		assertEquals(2, properties.size());
	}

	private Result<ModuleRelationshipPojo> createReference(final RelationshipType relationship) throws IOException {
		return createReference(relationship, FIRST_CUSTOM_MODULE_ID);
	}
	
	private Result<ModuleRelationshipPojo> createReference(final RelationshipType relationship, final EntityId moduleId) throws IOException {
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype()
				.setRelationship(relationship)
				.setSrcModule(moduleId)
				.setDstModule(SECOND_CUSTOM_MODULE_ID);
		final Result<ModuleRelationshipPojo> result = referenceServiceProvider.createReference()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(moduleId)
			.setReference(reference)
			.execute();
		
		assertEquals(HttpStatus.SC_CREATED, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		return result;
	}
	
	private Optional<ModuleRelationshipPojo> createReferenceWithLocationInformation(final RelationshipType relationship, final EntityId moduleId, 
			final int offset, final int length) throws IOException {
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype()
				.setRelationship(relationship)
				.setSrcModule(moduleId)
				.setDstModule(SECOND_CUSTOM_MODULE_ID);
		reference.setSrcLocation(new ModuleLocation(offset, length));
		final Result<ModuleRelationshipPojo> result = referenceServiceProvider.createReference()
			.setProjectId(FIRST_CUSTOM_PROJECT_ID)
			.setModuleId(moduleId)
			.setReference(reference)
			.execute();
		
		assertEquals(HttpStatus.SC_CREATED, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		return result.getValue();
	}

	private static void assertLocation(@Nullable final ModuleLocation location, final int expectedOffset, final int expectedLength) {
		assertNotNull("Location must exists", location);
		assertEquals("Offset must match", expectedOffset, location.getOffset().intValue());
		assertEquals("Length must match", expectedLength, location.getLength().intValue());
	}

	private boolean containsReference(final ModuleRelationshipPojo[] references, final ModuleRelationshipPojo expectedReference) {
		for (final var actualReference : references) {
			if (expectedReference.getId().equals(actualReference.getId())) {
				assertEquals(expectedReference.getSrcModule(), actualReference.getSrcModule());
				assertEquals(expectedReference.getDstModule(),actualReference.getDstModule());
				assertEquals(expectedReference.getRelationship(), actualReference.getRelationship());
				final Map<String, Object> expectedProperties = expectedReference.getProperties().orElse(null);
				final Map<String, Object> actualProperties = actualReference.getProperties().orElse(null);
				if (expectedProperties != null && ! expectedProperties.isEmpty() && actualProperties != null && ! actualProperties.isEmpty()) {
					final List<String> expectedDbAccessTypes = Arrays.asList(expectedProperties.get(ReferenceAttributes.DB_ACCESS_TYPES.getReferenceAttributeExcelName()).toString().split(","));
					final List<String> actualDbAccessTypes = Arrays.asList(actualProperties.get(ReferenceAttributes.DB_ACCESS_TYPES.getReferenceAttributeExcelName()).toString().split(","));
					assertEquals(Boolean.valueOf(expectedDbAccessTypes != null), Boolean.valueOf(actualDbAccessTypes != null));
					if (expectedDbAccessTypes != null && actualDbAccessTypes != null) {
						assertEquals(expectedDbAccessTypes.size(), actualDbAccessTypes.size());
						for (final String accessType : expectedDbAccessTypes) {
							assertTrue(actualDbAccessTypes.contains(accessType));
						}
					}
					
					final List<String> expectedDbStmts = Arrays.asList(expectedProperties.get(ReferenceAttributes.DB_ACCESS_OPERATIONS.getReferenceAttributeExcelName()).toString().split(","));
					final List<String> actualDbStmts = Arrays.asList(actualProperties.get(ReferenceAttributes.DB_ACCESS_OPERATIONS.getReferenceAttributeExcelName()).toString().split(","));
					assertEquals(Boolean.valueOf(expectedDbStmts != null), Boolean.valueOf(actualDbStmts != null));
					if (expectedDbStmts != null && actualDbStmts != null) {
						assertEquals(expectedDbStmts.size(), actualDbStmts.size());
						for (final String accessType : expectedDbStmts) {
							assertTrue(actualDbStmts.contains(accessType));
						}
					}
				}
				
				return true;
			}
		}
		return false;
	}
	
	private EntityId createModuleWithLocationInformation(final ModuleServiceProvider moduleService, final int moduleOffset, 
			final int moduleLength) throws IOException {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName("TEST MODULE FOR REFERECNE TEST");
		module.setProject(FIRST_CUSTOM_PROJECT_ID);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setLocation(new ModuleLocation(moduleOffset, moduleLength));
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.createModule().setProjectId(FIRST_CUSTOM_PROJECT_ID).setModule(module).execute().getValue().get().identity();
	}
}
