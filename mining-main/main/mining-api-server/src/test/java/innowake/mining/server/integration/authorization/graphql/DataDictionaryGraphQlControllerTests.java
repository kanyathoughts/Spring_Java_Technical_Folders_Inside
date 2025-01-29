/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.authorization.graphql;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.apache.commons.lang.math.NumberUtils.LONG_ONE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.graphql.test.tester.GraphQlTester.Response;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.lang.Nullable;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.test.web.servlet.client.MockMvcWebTestClient;
import org.springframework.web.context.WebApplicationContext;

import com.google.gson.Gson;

import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.server.graphql.controller.DataDictionaryGraphQlController;
import innowake.mining.server.util.UserNameUtil;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.DataDictionaryVariableAttribute;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Tests for the schema mapping methods of the {@link DataDictionaryGraphQlController}.
 * <p>Tests that GraphQL queries can reach other projects only through the GraphQL API if the user has access to them.</p>
 */
class DataDictionaryGraphQlControllerTests extends AbstractGraphQlControllerTests {

	private static final DataDictionaryPojo[] EXPECTED_DATA_DICTIONARY = new DataDictionaryPojo[6];

	private static EntityId projectId = EntityId.VOID;
	private static final EntityId projectId1 = EntityId.of(1l);

	private static final String QUERY_TEMPLATE = "query ($projectId: Long!, $filterObject: FilterObject_dataDictionaries, $sortObject: [SortObject_dataDictionaries]) {"
				+ "dataDictionaries(projectId: $projectId, filterObject: $filterObject, sortObject: $sortObject) {" +
					"totalElements," +
					"content {" +
						"id, " +
						"name, " +
						"description, " +
						"format, " +
						"length, " +
						"picClause, " +
						"definedLocation, " +
						"fieldLevel, " +
						"parentGroup, " +
						"initialValue, " +
						"usage, " +
						"state, " +
						"isBusiness, " +
						"fieldTransformation, " +
						"sourceInput, " +
						"targetOutput, " +
						"isReferenced, " +
						"createdByUserId, " +
						"updatedByUserId, " +
						"createdByUserName, " +
						"updatedByUserName " +
					"}" +
				"}" +
			"}";
	

	private static final String QUERY_TEMPLATE_SORT_FILTER = "query ($projectId: Long!, $filterObject: FilterObject_dataDictionaries, $sortObject: [SortObject_dataDictionaries]) {" +
												"dataDictionaries(projectId: $projectId, filterObject: $filterObject, sortObject: $sortObject) {" +
													"totalElements," +
													"content {" +
														"id," +
														"name" +
													"}" +
												"}" +
											"}";
	
	private static final String HAS_DATADICTIONARYENTRY_QUERY_TEMPLATE = "query ($projectId: Long!,"
			+ "$sortObject: [SortObject_dataDictionaries]) {" +
				"dataDictionaries(projectId: $projectId, sortObject: $sortObject) {" +
					"totalElements," +
					"content {" +
						"fieldType, " +
						"module { name }, " +
					"}" +
				"}" +
			"}";
	
	private static final String HAS_BUSINESSRULE_QUERY_TEMPLATE = "query ($projectId: Long!, $sortObject: [SortObject_dataDictionaries]) {" +
													"dataDictionaries(projectId: $projectId, sortObject: $sortObject) {" +
													"totalElements," +
													"content {" +
													"id, " +
													"linkedAnnotations {" +
														"id, " +
														"name" +
													"}" +
												"}" +
											"}" +
										"}";

	@Autowired
	private DataDictionaryService dataDictionaryService;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private WebApplicationContext webAppContext;
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	@Autowired
	private UserNameUtil userNameUtil;
	
	@AfterAll
	void tearDown() {
		projectService.deleteProjectCascading(LONG_ONE, projectId.getNid());
	}

	/**
	 * Creates the test data as following:
	 * <ul>
	 * <li>Creates a new project with the given {@code projectName}</li>
	 * <li>Creates three {@link Module Modules} in the new project</li>
	 * <li>Creates a reference with dummy module location</li>
	 * <li>Creates three {@link DataDictionaryPojo DataDictionaries} in the new project</li>
	 * </ul>
	 */
	@BeforeAll
	void init() {
		when(assertNotNull(userRoleService).isAdmin()).thenReturn(Boolean.TRUE);
		
		projectId = projectService.create(new ProjectPojoPrototype()
				.setName("Project_1")
				.setClient(EntityId.of(LONG_ONE))
				.setNatures(Collections.emptySet())
			).identity();
		
		final WebTestClient client = MockMvcWebTestClient.bindToApplicationContext(webAppContext)
				.configureClient()
				.baseUrl(GraphQlAuthorizationTests.GRAPHQL_ENDPOINT + "?projectId=" + projectId.getNid())
				.build();
		tester = HttpGraphQlTester.create(client);

		final ModulePojoPrototype module1 = new ModulePojoPrototype();
		module1.setProject(projectId);
		module1.setTechnology(Technology.COBOL);
		module1.setType(Type.PROGRAM);
		module1.setStorage(Storage.FILE);
		module1.setOrigin(Origin.CUSTOM);
		module1.setName("Module_1_" + projectId.getNid());
		module1.setIdentification(Identification.IDENTIFIED);
		module1.setCreator(Creator.DISCOVERY);

		final EntityId module1Id = moduleService.create(module1);

		final ModulePojoPrototype module2 = new ModulePojoPrototype();
		module2.setProject(projectId);
		module2.setTechnology(Technology.COBOL);
		module2.setType(Type.PROGRAM);
		module2.setStorage(Storage.FILE);
		module2.setOrigin(Origin.CUSTOM);
		module2.setName("Module_2_" + projectId.getNid());
		module2.setIdentification(Identification.IDENTIFIED);
		module2.setCreator(Creator.DISCOVERY);

		final EntityId module2Id = moduleService.create(module2);

		final ModulePojoPrototype module3 = new ModulePojoPrototype();
		module3.setProject(projectId);
		module3.setTechnology(Technology.COBOL);
		module3.setType(Type.PROGRAM);
		module3.setStorage(Storage.FILE);
		module3.setOrigin(Origin.CUSTOM);
		module3.setName("Module_3_" + projectId.getNid());
		module3.setIdentification(Identification.IDENTIFIED);
		module3.setCreator(Creator.DISCOVERY);

		final var module3Id = moduleService.create(module3);

		final ModulePojoPrototype module4 = new ModulePojoPrototype();
		module4.setProject(projectId1);
		module4.setTechnology(Technology.COBOL);
		module4.setType(Type.PROGRAM);
		module4.setStorage(Storage.FILE);
		module4.setOrigin(Origin.CUSTOM);
		module4.setName("Module_4_" + projectId1.getNid());
		module4.setIdentification(Identification.IDENTIFIED);
		module4.setCreator(Creator.DISCOVERY);

		final EntityId module4Id = moduleService.create(module4);

		DataDictionaryPojo dataDictionaryEntry1 = createFullDataDictionaryEntry("MY-PROGRAM-NAME", module1Id,
				"PICX", "COMP1", 56L, 58L, "XTAX-PRD", WorkingState.CANDIDATE, "SC", 0, 10);
		DataDictionaryPojo dataDictionaryEntry2 = createFullDataDictionaryEntry("MY-BIN-FIELDS", module2Id,
				"PIC9", "DEFAULT", 90L, 91L, "YEAR", WorkingState.INVALID, "OH", 20, 10);
		DataDictionaryPojo dataDictionaryEntry3 = createFullDataDictionaryEntry("MY-HEX-ORIGIN-LEN", module3Id,
				"GROUP", "COMP2", 40L, 41L, "TESTFIELD", WorkingState.APPROVED, "ZERO", 40, 10);

		final DataDictionaryPojo dataDictionaryEntry4 = createFullDataDictionaryEntry("MY-PROGRAM-FIELDS", module4Id,
				"PICX", "COMP1", 56L, 58L, "XTAX-PRD", WorkingState.CANDIDATE, "SC", 60, 10);
		final DataDictionaryPojo dataDictionaryEntry5 = createFullDataDictionaryEntry("MY-BIN-NAME", module4Id,
				"PIC9", "DEFAULT", 90L, 91L, "YEAR", WorkingState.INVALID, "OH", 80, 10);
		final DataDictionaryPojo dataDictionaryEntry6 = createFullDataDictionaryEntry("MY-HEX-NAME", module4Id,
				"GROUP", "COMP2", 40L, 41L, "TESTFIELD", WorkingState.APPROVED, "ZERO", 130, 10);

		final String entityName = "DataDictionaryEntry";
		final String className = entityName + "CustomProperties" + projectId.getNid();
		final String autoCompKey = "colorTagsAutoCompletionKey";
		
		/* Create new Tag type custom property */
		final CustomPropertyMetadata propertyMetaData = createCustomPropertyMetadata("ColorTags", "Color Tags", "Test Annotation Custom Property for graph ql test", "EMBEDDEDLIST",
				CustomPropertyFieldType.TAG, 0, false, autoCompKey, projectId, entityName, className);
		
		final List<String> colorTags = Arrays.asList("red", "blue", "green", "yellow", "pink");
		final Map<String, Set<String>> map = new HashMap<>();
		map.put(autoCompKey, new HashSet<>(colorTags));
		customPropertiesService.putEnumValues(projectId, map);
		assertTrue(customPropertiesService.getEnumValues(projectId, autoCompKey).containsAll(colorTags), "All entries should match for key: " + autoCompKey);
		
		/* Assign new property to DataDictionaryEntry */
		dataDictionaryEntry2 = dataDictionaryService.update(new DataDictionaryPojoPrototype()
				.setUid(dataDictionaryEntry2.getUid())
				.setUpdatedByUserId("admin")
				.setIsReferenced(false)
				.setCustomProperties(new NestedMap().set(className, propertyMetaData.getName(), Arrays.asList("red","blue","green"))));
	
		/* Create new String type custom property */
		final CustomPropertyMetadata propertyMetaData2 = createCustomPropertyMetadata("StringType",
				"String Type", "Test dde Custom Property for graph ql test", "STRING",
				CustomPropertyFieldType.DEFAULT, 1, false, null, projectId, entityName, className);
		
		/* Assign new property to DataDictionaryEntry */
		dataDictionaryEntry1 = dataDictionaryService.update(new DataDictionaryPojoPrototype()
				.setUid(dataDictionaryEntry1.getUid())
				.setUpdatedByUserId("admin")
				.setIsReferenced(false)
				.setCustomProperties(new NestedMap().set(className, propertyMetaData2.getName(), "Natural1")));
		
		/* Create new String Repeater type custom property */
		final CustomPropertyMetadata propertyMetaData3 = createCustomPropertyMetadata("StringRepeaterType",
				"String Repeater Type", "Test dde Custom Property for graph ql test", "EMBEDDEDLIST",
				CustomPropertyFieldType.DEFAULT, 1, false, null, projectId, entityName, className);

		
		dataDictionaryEntry3 = dataDictionaryService.update(new DataDictionaryPojoPrototype()
				.setUid(dataDictionaryEntry3.getUid())
				.setUpdatedByUserId("admin")
				.setIsReferenced(false)
				.setCustomProperties(new NestedMap()
						.set(className, propertyMetaData3.getName(), Arrays.asList("Orange", "Tomato"))
						.set(className, propertyMetaData2.getName(), "Natural2")));
		
		/* Create a Reference using a dummy location for Annotations */
		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(Integer.valueOf(0));
		dummyLocation.setLength(Integer.valueOf(0));
		
		final AnnotationPojo annotation1 = createAnnotation("Database Annotation 1", AnnotationType.DATABASE, WorkingState.CANDIDATE,
				"This is if-ELSE source attachment \n content", module1Id, dummyLocation, "1");
		final AnnotationPojo annotation2 = createAnnotation("Database Annotation 2", AnnotationType.DATABASE, WorkingState.FOR_REVIEW,
				"This is SUBTract source attachment content", module2Id, dummyLocation, "1");
		
		dataDictionaryService.linkAnnotations(dataDictionaryEntry1.identity(), annotation1.identity());
		dataDictionaryService.linkAnnotations(dataDictionaryEntry1.identity(), annotation2.identity());
		dataDictionaryService.linkAnnotations(dataDictionaryEntry2.identity(), annotation1.identity());
		dataDictionaryService.linkAnnotations(dataDictionaryEntry3.identity(), annotation2.identity());
		eventPublisher.publishEvent(new CustomPropertiesModifiedEvent(Optional.of(projectId)));

		final DataDictionaryPojo finalDataDictionaryEntry1 = dataDictionaryEntry1;
		final DataDictionaryPojo finalDataDictionaryEntry2 = dataDictionaryEntry2;
		final DataDictionaryPojo finalDataDictionaryEntry3 = dataDictionaryEntry3;
		EXPECTED_DATA_DICTIONARY[0] = dataDictionaryService.get(q -> q.byId(finalDataDictionaryEntry1.identity()));
		EXPECTED_DATA_DICTIONARY[1] = dataDictionaryService.get(q -> q.byId(finalDataDictionaryEntry2.identity()));
		EXPECTED_DATA_DICTIONARY[2] = dataDictionaryService.get(q -> q.byId(finalDataDictionaryEntry3.identity()));
		EXPECTED_DATA_DICTIONARY[3] = dataDictionaryService.get(q -> q.byId(dataDictionaryEntry4.identity()));
		EXPECTED_DATA_DICTIONARY[4] = dataDictionaryService.get(q -> q.byId(dataDictionaryEntry5.identity()));
		EXPECTED_DATA_DICTIONARY[5] = dataDictionaryService.get(q -> q.byId(dataDictionaryEntry6.identity()));
	}

	private CustomPropertyMetadata createCustomPropertyMetadata(final String name, final String label, final String description, final String dataType,
			final CustomPropertyFieldType customPropertyFieldType, final int customViewIndex, final boolean pluginVisible, @Nullable final String autoCompKey,
			final EntityId projectId, final String entityName, final String className) {
		final CustomPropertyMetadata propertyMetaData = new CustomPropertyMetadata();
		propertyMetaData.setName(name);
		propertyMetaData.setLabel(label);
		propertyMetaData.setDescription(description);
		propertyMetaData.setDataType(dataType);
		propertyMetaData.setFieldType(customPropertyFieldType);
		propertyMetaData.setCustomViewIndex(customViewIndex);
		propertyMetaData.setPluginVisible(pluginVisible);
		propertyMetaData.setAutoCompletionKey(autoCompKey);
		customPropertiesService.defineProperty(projectId, entityName, propertyMetaData.getName(), propertyMetaData);
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.ofParent(null).withName(className)), className + " should be existing in DB");
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.withParent(className).withName(propertyMetaData.getName())),
				propertyMetaData.getName() + " should exist in " + className);
		return propertyMetaData;
	}

	/**
	 * Tests that the GraphQL query returns expected sorted DataDictionaries.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with sorting")
	@Test
	void testSingleProjectWithSorting() {

		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		/* Execute GraphQL request to fetch the data sorted by name in ascending order and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId, List.of(Map.of("content_name", "ASC")),
				null), 3, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of created and fetched dataDictionaries are same */
		assertEquals(3, actualDataDictionaryEntries.length);
		/* Test that dataDictionary with name = "Database dataDictionary 1" is fetched as the first element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[1], actualDataDictionaryEntries[0]);
		/* Test that dataDictionary with name = "Database dataDictionary 2" is fetched as the second element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[2], actualDataDictionaryEntries[1]);
		/* Test that dataDictionary with name = "Database dataDictionary 0" is fetched as the third element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[0], actualDataDictionaryEntries[2]);
	}

	/**
	 * Tests that the GraphQL query returns all expected filtered dataDictionaries.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filters")
	@Test
	void testSingleProjectWithFilters() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		/* Execute GraphQL request and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId, null,
						new Gson().fromJson("{ content_name: { eq: \"MY-P\" } }", Object.class)),
				1, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of expected dataDictionaries and the fetched dataDictionaries is same */
		assertEquals(1, actualDataDictionaryEntries.length);
		/* Test that only dataDictionaries where DataDictionary dataElementName contains "PROGRAM" are fetched */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[0], actualDataDictionaryEntries[0]);
	}

	/**
	 * Tests that the GraphQL query returns all expected usage based filtered dataDictionaries.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with usage based filters")
	@Test
	void testSingleProjectWithUsageFilter() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		/* Execute GraphQL request and get response data */
		final DataDictionaryPojo[] ddEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId, null,
				new Gson().fromJson("{ content_usage : { in : ['COMP1', 'DEFAULT', 'COMP2'] } }", Object.class)),
				3, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of expected dataDictionaries and the fetched dataDictionaries is same */
		assertEquals(3, ddEntries.length);
		final var result = createResultDataDictionaryMap(ddEntries);
		/* Test that only dataDictionaries where DataDictionary Usage contains 'COMP1', 'DEFAULT', 'COMP2' are fetched */
		assertEquals(EXPECTED_DATA_DICTIONARY[0].getUsage(), result.get(EXPECTED_DATA_DICTIONARY[0].getName()).getUsage());
		assertEquals(EXPECTED_DATA_DICTIONARY[1].getUsage(), result.get(EXPECTED_DATA_DICTIONARY[1].getName()).getUsage());
		assertEquals(EXPECTED_DATA_DICTIONARY[2].getUsage(), result.get(EXPECTED_DATA_DICTIONARY[2].getName()).getUsage());
	}
	
	/**
	 * Tests that the GraphQL query returns all expected filtered dataDictionaries based on format attribute.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filters based on format")
	@Test
	void testSingleProjectWithFormatFilters() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		/* Execute GraphQL request and get response data for filter - 'format==PIC9' */
		DataDictionaryPojo[] actualDataDictionaryEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId, null,
				new Gson().fromJson("{ content_format : { eq : \"PIC9\" } }", Object.class)),
				1, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of expected dataDictionaries and the fetched dataDictionaries is same */
		assertEquals(1, actualDataDictionaryEntries.length);
		/* Test that only dataDictionaries where DataDictionary format contains "PIC9" are fetched */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[1], actualDataDictionaryEntries[0]);
		
		/* Execute GraphQL request and get response data for filter - 'format==GROUP' */
		actualDataDictionaryEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId, null,
				new Gson().fromJson("{ content_format : { eq : \"GROUP\" } }", Object.class)),
				1, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of expected dataDictionaries and the fetched dataDictionaries is same */
		assertEquals(1, actualDataDictionaryEntries.length);
		/* Test that only dataDictionaries where DataDictionary format contains "GROUP" are fetched */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[2], actualDataDictionaryEntries[0]);
	}

	/**
	 * Tests that the GraphQL query returns all expected filtered DataDictionaries.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object for module name")
	@Test
	void testSingleProjectWithFilterObject() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId1), Collections.singletonList(projectId1));

		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{ content_module_name: { eq: \"Module_4\" }}", Object.class);
		/* Execute GraphQL request and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId1, null, filterObject), 3, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of expected dataDictionaries and fetched dataDictionaries are same */
		assertEquals(3, actualDataDictionaryEntries.length);
		final var result = createResultDataDictionaryMap(actualDataDictionaryEntries);
		/* Test that only dataDictionaries where DataDictionary dataElementName contains "BIN" OR module name contains "Module_3" are fetched */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[4], result.get(EXPECTED_DATA_DICTIONARY[4].getName()));
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[5], result.get(EXPECTED_DATA_DICTIONARY[5].getName()));
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[3], result.get(EXPECTED_DATA_DICTIONARY[3].getName()));
	}

	@DisplayName("Test for a project with filter object for module name with wildcard")
	@Test
	void testModuleNameFilterObjectWithWildcard() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId1), Collections.singletonList(projectId1));

		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{ content_module_name: { eq: \"?o%le*4\" }}", Object.class);
		/* Execute GraphQL request and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId1, null, filterObject), 3, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of expected dataDictionaries and fetched dataDictionaries are same */
		assertEquals(3, actualDataDictionaryEntries.length);
		final var result = createResultDataDictionaryMap(actualDataDictionaryEntries);
		/* Test that only dataDictionaries where DataDictionary dataElementName contains "BIN" OR module name contains "Module_3" are fetched */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[4], result.get(EXPECTED_DATA_DICTIONARY[4].getName()));
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[5], result.get(EXPECTED_DATA_DICTIONARY[5].getName()));
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[3], result.get(EXPECTED_DATA_DICTIONARY[3].getName()));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected field Usage filtered DataDictionaries.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object as usage")
	@Test
	void testSingleProjectWithFilterObjectUsage() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{ content_usage: { in: [\"COMP1\",\"DEFAULT\"]}}", Object.class);
		
		/* Execute GraphQL request and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObject), 2, "dataDictionaries", DataDictionaryPojo.class);
		
		/* Test that the length of expected dataDictionaries and fetched dataDictionaries are same */
		assertEquals(2, actualDataDictionaryEntries.length);
		final List<String> actualDataElementNames = Arrays.stream(actualDataDictionaryEntries).map(DataDictionaryPojo::getName)
				.collect(Collectors.toList());
		
		/* Test that dataDictionaries where DataDictionary Usage as "COMP1" or "DEFAULT" are fetched */
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[0].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[0].getName()));
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[1].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[1].getName()));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected Referenced filtered DataDictionaries.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object as Reference")
	@Test
	void testSingleProjectWithFilterObjectReferenced() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{ content_isReferenced: {in: [true, false]}}", Object.class);
		
		/* Execute GraphQL request and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObject), 3, "dataDictionaries", DataDictionaryPojo.class);
		
		/* Test that the length of expected dataDictionaries and fetched dataDictionaries are same */
		assertEquals(3, actualDataDictionaryEntries.length);
		final List<String> actualDataElementNames = Arrays.stream(actualDataDictionaryEntries).map(DataDictionaryPojo::getName)
				.collect(Collectors.toList());
		
		/* Test that dataDictionaries where DataDictionary Reference as true or false are fetched */
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[0].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[0].getName()));
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[2].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[1].getName()));
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[2].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[2].getName()));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected Scope filtered DataDictionaries.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object as Scope")
	@Test
	void testSingleProjectWithFilterObjectScope() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{ content_scopes: { in: [\"PARAMETER\", \"NATURAL_DATABASE\"] }}",
				Object.class);
		
		/* Execute GraphQL request and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObject), 2, "dataDictionaries", DataDictionaryPojo.class);
		
		/* Test that the length of expected dataDictionaries and fetched dataDictionaries are same */
		assertEquals(2, actualDataDictionaryEntries.length);
		final List<String> actualDataElementNames = Arrays.stream(actualDataDictionaryEntries).map(DataDictionaryPojo::getName)
				.collect(Collectors.toList());

		/* Test that dataDictionaries where DataDictionary Scope as "PARAMETER" or "NATURAL_DATABASE" are fetched */
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[0].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[0].getName()));
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[2].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[2].getName()));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected State filtered DataDictionaries.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object State")
	@Test
	void testSingleProjectWithFilterObjectState() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{content_state : {in : [CANDIDATE, INVALID, APPROVED]}}", Object.class);
		
		/* Execute GraphQL request and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObject), 3, "dataDictionaries", DataDictionaryPojo.class);
		
		/* Test that the length of expected dataDictionaries and fetched dataDictionaries are same */
		assertEquals(3, actualDataDictionaryEntries.length);
		final List<String> actualDataElementNames = Arrays.stream(actualDataDictionaryEntries).map(DataDictionaryPojo::getName)
				.collect(Collectors.toList());
		
		/* Test that dataDictionaries where DataDictionary state as "CANDIDATE" or "INVALID" or "APPROVED" are fetched */
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[0].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[0].getName()));
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[1].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[1].getName()));
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[2].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[2].getName()));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected Defined Location filtered DataDictionaries.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object as Defined location")
	@Test
	void testSingleProjectWithFilterObjectDefinedLocation() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final Gson gson = new Gson();

		final Object filterObject = gson.fromJson("{ content_definedLocation: { in: [PROGRAM] }}", Object.class);
		
		/* Execute GraphQL request and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObject), 3, "dataDictionaries", DataDictionaryPojo.class);
		
		/* Test that the length of expected dataDictionaries and fetched dataDictionaries are same */
		assertEquals(3, actualDataDictionaryEntries.length);
		final List<String> actualDataElementNames = Arrays.stream(actualDataDictionaryEntries)
				.map(DataDictionaryPojo::getName)
				.collect(Collectors.toList());
		
		/* Test that dataDictionaries where DataDictionary DefinedLocation as "Program" or "Map" are fetched */
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[0].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[0].getName()));
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[1].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[1].getName()));
		assertTrue("actualDataElementNames should contain EXPECTED_DATA_DICTIONARY[2].getDataElementName()",
				actualDataElementNames.contains(EXPECTED_DATA_DICTIONARY[2].getName()));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected sorted and filtered DataDictionaries.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project without sorting and filters")
	@Test
	void testSingleProjectWithoutSortingAndFilters() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		/* Execute GraphQL request and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, null), 3, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of expected dataDictionaries and fetched dataDictionaries are same */
		assertEquals(3, actualDataDictionaryEntries.length);
		final var result = createResultDataDictionaryMap(actualDataDictionaryEntries);
		/* Test dataDictionaries where dataDictionary are in same order as they are saved in DB without any filters*/
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[0], result.get(EXPECTED_DATA_DICTIONARY[0].getName()));
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[1], result.get(EXPECTED_DATA_DICTIONARY[1].getName()));
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[2], result.get(EXPECTED_DATA_DICTIONARY[2].getName()));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected filtered dataDictionaries based on Scope
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filters based on scopes")
	@Test
	void testSingleProjectWithScopeFilters() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		/* Execute GraphQL request with scope filters where scopelink.name contains 'PARAMETER' and get response data */
		DataDictionaryPojo[] actualDataDictionaryEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId, null,
				new Gson().fromJson("{ content_scopes: { in: [\"PARAMETER\"] }}", Object.class)), 1, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of expected dataDictionaries and the fetched dataDictionaries is same */
		assertEquals(1, actualDataDictionaryEntries.length);
		/* Test that only dataDictionaries where DataDictionary.scopelink.name contains "PARAMETER" are fetched */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[0], actualDataDictionaryEntries[0]);
		

		/* Execute GraphQL request with scope filters where scopelink.name contains 'NATURAL_DATABASE' and get response data */
		actualDataDictionaryEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId, null,
				new Gson().fromJson("{ content_scopes: { in: [\"NATURAL_DATABASE\"] }}", Object.class)), 1, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of expected dataDictionaries and the fetched dataDictionaries is same */
		assertEquals(1, actualDataDictionaryEntries.length);
		/* Test that only dataDictionaries where DataDictionary.scopelink.name contains "NATURAL_DATABASE" are fetched */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[2], actualDataDictionaryEntries[0]);
		
		/* Execute GraphQL request with scope filters where scopelink.name contains 'CICS_UI' with sorting and get response data */
		actualDataDictionaryEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId, List.of(Map.of("content_name", "ASC")),
				new Gson().fromJson("{ content_scopes: { in: [\"CICS_UI\"] }}", Object.class)), 3, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of expected dataDictionaries and the fetched dataDictionaries is same */
		assertEquals(3, actualDataDictionaryEntries.length);
		/* Test that only dataDictionaries where DataDictionary.scopelink.name contains "CICS_UI" are fetched.
		Test that dataDictionary with name = "Database dataDictionary 1" is fetched as the first element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[1], actualDataDictionaryEntries[0]);
		/* Test that dataDictionary with name = "Database dataDictionary 2" is fetched as the second element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[2], actualDataDictionaryEntries[1]);
		/* Test that dataDictionary with name = "Database dataDictionary 0" is fetched as the third element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[0], actualDataDictionaryEntries[2]);
	}
	
	/**
	 * Tests that the GraphQL query returns the expected DataDictionary with Custom property Filter.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with String Type Custom property filter")
	@Test
	void testSingleProjectWithStringTypeCustomPropertyFilter() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));
		
		final Gson gson = new Gson();
		
		final DataDictionaryPojo[] actualDataDictionaryEntries =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, Collections.singletonList(Map.of("content_name", "ASC")),
								gson.fromJson("{content_customProperties_DataDictionaryEntryCustomProperties5_StringType : {eq: 'Natural1'}}", Object.class))
						, 1,
						"dataDictionaries", DataDictionaryPojo.class);
		assertEquals(1, actualDataDictionaryEntries.length);
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[0], actualDataDictionaryEntries[0]);
	}

	@DisplayName("Test for a project with String Type Custom property filter and sorting")
	@ParameterizedTest(name = "Test for a project with String Type Custom property filter and sorting in {0} order")
	@MethodSource("provideSortDirections")
	void testSingleProjectWithStringTypeCustomPropertyFilterWithSorting(final SortDirection sortDirection, final int firstExpectedIndex, final int secondExpectedIndex) {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final Response resp = assertNotNull(tester).document(QUERY_TEMPLATE_SORT_FILTER).variable("projectId", projectId.getNid())
				.variable("filterObject",
						Collections.singletonMap("content_customProperties_DataDictionaryEntryCustomProperties5_StringType",
								Collections.singletonMap("eq", "Natural")))
				.variable("sortObject", Collections.singletonMap("content_customProperties_DataDictionaryEntryCustomProperties5_StringType", sortDirection))
				.execute();

		final DataDictionaryPojo[] actualDataDictionaryEntries = getEntries(resp, 2, "dataDictionaries", DataDictionaryPojo.class);
		assertEquals(2, actualDataDictionaryEntries.length);
		assertEquals(EXPECTED_DATA_DICTIONARY[firstExpectedIndex].getId(), actualDataDictionaryEntries[0].getId());
		assertEquals(EXPECTED_DATA_DICTIONARY[secondExpectedIndex].getId(), actualDataDictionaryEntries[1].getId());
	}

	private static Stream<Arguments> provideSortDirections() {
		return Stream.of(Arguments.of(SortDirection.ASCENDING, 0, 2), Arguments.of(SortDirection.DESCENDING, 2, 0));
	}
	
	@DisplayName("Test for a project with Custom property filter")
	@ParameterizedTest
	@MethodSource("provideCustomPropertyFilters")
	void testSingleProjectWithCustomPropertyFilter(final Map<String, Object> customPropertyFilter, final int expectedDictionaryIndex) {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		/* Execute GraphQL request to fetch the data sorted by name in ascending order and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries = getEntries(
				executeQuery(QUERY_TEMPLATE, projectId, List.of(Map.of("content_name", "ASC")), customPropertyFilter), 1, "dataDictionaries",
				DataDictionaryPojo.class);

		assertEquals(1, actualDataDictionaryEntries.length);
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[expectedDictionaryIndex], actualDataDictionaryEntries[0]);
	}
	
	/**
	 * Tests that the GraphQL query returns the expected DataDictionary with Custom property Filter.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with None Custom property filter")
	@Test
	void testSingleProjectWithNoneCustomPropertyFilter() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final Gson gson = new Gson();
		
		final DataDictionaryPojo[] actualDataDictionaryEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId,
				Arrays.asList(Map.of("content_name", "ASC")),
				gson.fromJson("{content_customProperties_DataDictionaryEntryCustomProperties5_ColorTags : {is: null}}", Object.class)), 2, "dataDictionaries",
				DataDictionaryPojo.class);
	
		/* Test that the length of expected dataDictionaries and fetched dataDictionaries are same */
		assertEquals(2, actualDataDictionaryEntries.length);
		
		/* Test that expected data dictionaries are returned */
		
		final List<EntityId> expectedIds = Arrays.asList(EntityId.of(EXPECTED_DATA_DICTIONARY[0].getId()), EntityId.of(EXPECTED_DATA_DICTIONARY[2].getId()));
		final List<EntityId> actualIds = Arrays.stream(actualDataDictionaryEntries).map(DataDictionaryPojo::identity).collect(Collectors.toList());

		for (final EntityId expectedId : expectedIds) {
			assertTrue(actualIds.contains(expectedId), "Expected data dictionary ID not found in actual data dictionaries");
		}
	}

	/**
	 * Tests that the GraphQL query returns expected sorted data dictionaries by Module name.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with Module name sorting")
	@Test
	void testDataDictionarySortingByModuleName() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));
		
		/* Create expected JSON for DataDictionary sorting by module name in ascending order */
		final String expectedAscJson = "{\"dataDictionaries\":{\"totalElements\":3,\"content\":["
				+ "{\"fieldType\":\"ELEMENTARY\",\"module\":{\"name\":\"Module_1_5\"}},"
				+ "{\"fieldType\":\"ELEMENTARY\",\"module\":{\"name\":\"Module_2_5\"}},"
				+ "{\"fieldType\":\"GROUP\",\"module\":{\"name\":\"Module_3_5\"}}]}}";
		verifyReceivedDataDictionaryData(executeQuery(HAS_DATADICTIONARYENTRY_QUERY_TEMPLATE, projectId,
				List.of(Map.of("content_module_name", "ASC")), null), expectedAscJson);
		
		/* Create expected JSON for DataDictionary sorting by module name in descending order */
		final String expectedDescJson = "{\"dataDictionaries\":{\"totalElements\":3,\"content\":["
				+ "{\"fieldType\":\"GROUP\",\"module\":{\"name\":\"Module_3_5\"}},"
				+ "{\"fieldType\":\"ELEMENTARY\",\"module\":{\"name\":\"Module_2_5\"}},"
				+ "{\"fieldType\":\"ELEMENTARY\",\"module\":{\"name\":\"Module_1_5\"}}]}}";
		verifyReceivedDataDictionaryData(executeQuery(HAS_DATADICTIONARYENTRY_QUERY_TEMPLATE, projectId,
				List.of(Map.of("content_module_name", "DESC")), null), expectedDescJson);
	}
	
	/**
	 * Tests that the GraphQL query returns all expected scope filtered DataDictionaries.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with scope filter object")
	@Test
	void testSingleProjectWithFilterObjectOfScope() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		/* Execute GraphQL request to fetch the data sorted by name in ascending order and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId, List.of(Map.of("content_module_name", "DESC")),
				new Gson().fromJson("{ content_scopes: {in:[\"NATURAL_DATABASE\", \"PARAMETER\"]}}", Object.class)), 2, "dataDictionaries", DataDictionaryPojo.class);

		/* Test that the length of created and fetched dataDictionaries are same */
		assertEquals(2, actualDataDictionaryEntries.length);
		/* Test that dataDictionary with name = "Database dataDictionary 1" is fetched as the first element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[2], actualDataDictionaryEntries[0]);
		/* Test that dataDictionary with name = "Database dataDictionary 0" is fetched as the second element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[0], actualDataDictionaryEntries[1]);
	}
	
	/**
	 * Tests that the GraphQL query returns expected sorted data dictionaries by Length.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with sorting By length")
	@Test
	void testDataDictionarySortingByLength() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		/* Execute GraphQL request to fetch the data sorted by name in ascending order and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId, List.of(Map.of("content_length", "DESC")),
				null), 3, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of created and fetched dataDictionaries are same */
		assertEquals(3, actualDataDictionaryEntries.length);
		/* Test that dataDictionary with name = "Database dataDictionary 1" is fetched as the first element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[1], actualDataDictionaryEntries[0]);
		/* Test that dataDictionary with name = "Database dataDictionary 0" is fetched as the second element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[0], actualDataDictionaryEntries[1]);
		/* Test that dataDictionary with name = "Database dataDictionary 2" is fetched as the third element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[2], actualDataDictionaryEntries[2]);
	}

	/**
	 * Tests that the GraphQL query returns expected sorted data dictionaries by Field Level.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with sorting By Field Level")
	@Test
	void testDataDictonarySortingByFieldLevel() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		/* Execute GraphQL request to fetch the data sorted by name in ascending order and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId, List.of(Map.of("content_fieldLevel", "DESC")),
				null), 3, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of created and fetched dataDictionaries are same */
		assertEquals(3, actualDataDictionaryEntries.length);
		/* Test that dataDictionary with name = "Database dataDictionary 1" is fetched as the first element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[1], actualDataDictionaryEntries[0]);
		/* Test that dataDictionary with name = "Database dataDictionary 0" is fetched as the second element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[0], actualDataDictionaryEntries[1]);
		/* Test that dataDictionary with name = "Database dataDictionary 2" is fetched as the third element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[2], actualDataDictionaryEntries[2]);
	}

	/**
	 * Tests that the GraphQL query returns expected sorted data dictionaries by Group Field.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with sorting By Group Field")
	@Test
	void testDataDictonarySortingByGroupField() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		/* Execute GraphQL request to fetch the data sorted by name in Descending order and get response data */
		final DataDictionaryPojo[] actualDataDictionaryEntries = getEntries(executeQuery(QUERY_TEMPLATE, projectId, List.of(Map.of("content_parentGroup", "DESC")),
				null), 3, "dataDictionaries", DataDictionaryPojo.class);
		/* Test that the length of created and fetched dataDictionaries are same */
		assertEquals(3, actualDataDictionaryEntries.length);
		/* Test that dataDictionary with name = "Database dataDictionary 1" is fetched as the first element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[1], actualDataDictionaryEntries[0]);
		/* Test that dataDictionary with name = "Database dataDictionary 0" is fetched as the second element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[0], actualDataDictionaryEntries[1]);
		/* Test that dataDictionary with name = "Database dataDictionary 2" is fetched as the third element of expected dataDictionaries */
		assertDataDictionaries(EXPECTED_DATA_DICTIONARY[2], actualDataDictionaryEntries[2]);
	}
	
	/**
	 * Tests check whether list of linked Business Rules returned or not
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test linked Annotation")
	@Test
	void testlinkedAnnotationToDataDictionaryEntries() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		/* Execute GraphQL request to fetch the data sorted by name in Descending order and get response data */
		final Response response = executeQuery(HAS_BUSINESSRULE_QUERY_TEMPLATE, projectId, null, null);
		response.path("dataDictionaries.totalElements").entity(Long.class).isEqualTo(Long.valueOf(3));

		final Map<Long, List<String>> actualMap = valuesToMap(3,
						idx -> response.path("dataDictionaries.content[" + idx + "].id").entity(Long.class).get(),
						idx -> getAnnotations(response, idx).stream().sorted().collect(Collectors.toList()));

		final Map<Long, List<String>> expectedMap = valuesToMap(3,
						idx -> EXPECTED_DATA_DICTIONARY[idx].getId(),
						idx -> EXPECTED_DATA_DICTIONARY[idx].getAnnotations()
								.stream()
								.map(annotationService::get)
								.map(AnnotationPojo::getName)
								.sorted()
								.collect(Collectors.toList()));

		assertThat(expectedMap, Matchers.equalTo(actualMap));
	}
	
	@DisplayName("Test For DataDictionaryEntry Created By UserName")
	@Test
	void testDataDictionaryForCreatedByUserName() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));
		/* Execute GraphQL request and get response data */
		final Response response = executeQuery(QUERY_TEMPLATE, projectId, null, null);
		response.path("dataDictionaries.totalElements").entity(Long.class).isEqualTo(Long.valueOf(3));

		final Map<Long, List<String>> actualMap = valuesToMap(3,
						idx -> response.path("dataDictionaries.content[" + idx + "].id").entity(Long.class).get(),
						idx -> Stream.of(response.path("dataDictionaries.content[" + idx + "].createdByUserName").entity(String.class).get(),
											 response.path("dataDictionaries.content[" + idx + "].updatedByUserName").entity(String.class).get())
									.sorted().collect(Collectors.toList()));

		final Map<Long, List<String>> expectedMap = valuesToMap(3,
						idx -> EXPECTED_DATA_DICTIONARY[idx].getId(),
						idx -> Stream.of(userNameUtil.getUserName(EXPECTED_DATA_DICTIONARY[idx].getCreatedByUserId()),
										 userNameUtil.getUserName(EXPECTED_DATA_DICTIONARY[idx].getUpdatedByUserId()))
									.sorted().collect(Collectors.toList()));

		assertThat(expectedMap, Matchers.equalTo(actualMap));
	}

	/* Helper method to retrieve Annotation list using the given index */
	private List<String> getAnnotations(final Response response, final int index) {
		return response.path("dataDictionaries.content[" + index+ "].linkedAnnotations")
				.entityList(Object.class)
				.get()
				.stream()
				.filter(item -> item instanceof Map)
				.map(item -> (Map<?, ?>) item)
				.filter(innerMap -> innerMap.containsKey("name"))
				.map(innerMap -> innerMap.get("name").toString())
				.collect(Collectors.toList());
	}
	
	private static void verifyReceivedDataDictionaryData(final Response response, final String expectedJson) {
		response.path("").matchesJsonStrictly(expectedJson);
	}
	
	private void assertDataDictionaries(final DataDictionaryPojo expected, final DataDictionaryPojo actual) {
		assertEquals(expected.getName(), actual.getName());
		assertEquals(expected.getDescription(), actual.getDescription());
		assertEquals(expected.getFormat(), actual.getFormat());
		/* When the returned length from GraphQL is null then length is changed to 0 at time of parsing the response to DataDictionary in getEntries method */
		final Optional<Long> length = expected.getLength();
		if (length.isEmpty()) {
			assertEquals(0, actual.getLength().get());
		} else {
			assertEquals(length, actual.getLength());
		}
		assertEquals(expected.getCreatedByUserId(), actual.getCreatedByUserId());
		assertEquals(expected.getParentGroup(), actual.getParentGroup());
		expected.getUpdatedByUserId().ifPresent(updatedId -> assertEquals(updatedId, actual.getUpdatedByUserId().orElseThrow()));
		actual.getIsBusiness().ifPresent(isBusiness -> assertTrue(isBusiness, "Expected isBusiness to be True"));
		actual.getIsReferenced().ifPresent(isReferenced -> assertEquals(expected.getIsReferenced().orElseThrow(), isReferenced));
		assertEquals(expected.getPicClause(), actual.getPicClause());
		assertEquals(expected.getDefinedLocation(), actual.getDefinedLocation());
		assertEquals(expected.getState(), actual.getState());
		assertEquals(expected.getSourceInput(), actual.getSourceInput());
		assertEquals(expected.getTargetOutput(), actual.getTargetOutput());
		assertEquals(expected.getFieldTransformation(), actual.getFieldTransformation());
		assertEquals(expected.getFieldLevel(), actual.getFieldLevel());
		assertEquals(expected.getInitialValue(), actual.getInitialValue());
	}
	
	private DataDictionaryPojo createFullDataDictionaryEntry(final String dataElementName, final EntityId moduleId, final String format,
			final String usage, final Long fieldLevel, final Long length, final String parentGroup, @Nullable final WorkingState state,
			final String initialValue,final int locationOffset, final int locationLength) {
		final Map<String, String> attributes = new HashMap<>();
		attributes.put("A", "B");
		attributes.put("C", "D");

		final DataDictionaryPojoPrototype pojoPrototype = new DataDictionaryPojoPrototype()
				.setName(dataElementName)
				.setScopes(new HashMap<>(
						Map.of(DataDictionaryVariableAttribute.FILE_DATASET.getScope(),
								Map.of(DataDictionaryVariableAttribute.FILE_DATASET.getKey(), "My data set name"),
						DataDictionaryVariableScope.SQL_DATABASE, Map.of(),
						DataDictionaryVariableScope.CICS_UI, Map.of(),
						DataDictionaryVariableScope.OTHER, attributes)))
				.setModule(moduleId)
				.setLocation(new ModuleLocation(locationOffset, locationLength))
				.setDescription("MY description")
				.setFormat(format)
				.setLength(length)
				.setCreatedByUserId("admin")
				.setPicClause("TEST PIC CLAUSE")
				.setDefinedLocation(DefinedLocation.PROGRAM)
				.setIsBusiness(true)
				.setInitialValue(initialValue)
				.setFieldLevel(fieldLevel)
				.setState(state)
				.setParentGroup(parentGroup)
				.setUsage(usage)
				.setFieldTransformation("TEST TRANSFORMATION")
				.setSourceInput("TEST INPUT")
				.setTargetOutput("TEST OUTPUT");
		
		/* Set different scopes for data dictionary records based on their data element names*/
		if (dataElementName.equals("MY-PROGRAM-NAME")) {
			pojoPrototype.setIsReferenced(true);
			pojoPrototype.scopes.getNonNull().put(DataDictionaryVariableScope.PARAMETER, Map.of());
		} else if (dataElementName.equals("MY-HEX-ORIGIN-LEN")) {
			pojoPrototype.scopes.getNonNull().put(DataDictionaryVariableScope.NATURAL_DATABASE, Map.of());
		}
		
		return dataDictionaryService.create(pojoPrototype);
	}
	
	private AnnotationPojo createAnnotation(final String name, final AnnotationType annotationType, final WorkingState workingState,
			final String sourceAttachment, final EntityId moduleId, final ModuleLocation location, final String createdByUserId) {
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.setName(name);
		annotation.setType(annotationType);
		annotation.setState(workingState);
		annotation.setSourceAttachment(sourceAttachment);
		annotation.setLocation(location);
		annotation.setModule(moduleId);
		annotation.setCreatedByUserId(createdByUserId);
		return annotationService.get(annotationService.create(annotation));
	}

	private Map<String, DataDictionaryPojo> createResultDataDictionaryMap(final DataDictionaryPojo[] dataDictionaryEntries) {
		final Map<String, DataDictionaryPojo> dataDictionaryMap = new HashMap<>();
		for (final DataDictionaryPojo dataDictionaryEntry : dataDictionaryEntries) {
			dataDictionaryMap.put(dataDictionaryEntry.getName(), dataDictionaryEntry);
		}
		return dataDictionaryMap;
	}
	
	private static Stream<Arguments> provideCustomPropertyFilters() {
		final Gson gson = new Gson();

		return Stream.of(
				Arguments.of(gson.fromJson("{content_customProperties_DataDictionaryEntryCustomProperties5_StringRepeaterType : {eq: 'Tomato'}}",
						Object.class), 2),
				Arguments.of(Map.of("content_customProperties_DataDictionaryEntryCustomProperties5_ColorTags", Map.of("eq", "blue")), 1),
				Arguments.of(Map.of("content_customProperties_DataDictionaryEntryCustomProperties5_ColorTags", Map.of("in", List.of("red", "blue"))), 1)
		);
	}

}
