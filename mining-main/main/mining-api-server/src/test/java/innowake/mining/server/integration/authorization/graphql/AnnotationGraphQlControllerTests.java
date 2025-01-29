/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.authorization.graphql;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.apache.commons.lang.math.NumberUtils.LONG_ONE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
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
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.test.web.servlet.client.MockMvcWebTestClient;
import org.springframework.web.context.WebApplicationContext;

import com.google.gson.Gson;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.graphql.controller.AnnotationsGraphQlController;
import innowake.mining.server.util.UserNameUtil;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Tests for the schema mapping methods of the {@link AnnotationsGraphQlController}.
 * <p>Tests that GraphQL queries can reach other projects only through the GraphQL API if the user has access to them.</p>
 */
class AnnotationGraphQlControllerTests extends AbstractGraphQlControllerTests {

	private static final EntityId[] EXPECTED_ANNOTATIONS = new EntityId[4];
	
	private static EntityId projectId = EntityId.VOID;

	private static final String QUERY_TEMPLATE = "query ($projectId: Long!, $filterObject: FilterObject_annotations, $sortObject: [SortObject_annotations]) {" + 
											"annotations(projectId: $projectId, filterObject: $filterObject, sortObject: $sortObject) {" + 
												"totalElements," +
												"content {" + 
													"id, "+
													"name, "+
													"createdByUserName, " +
													"updatedByUserName" +
												"}" + 
											"}" + 
										"}";

	private static final String HAS_ANNOTATION_QUERY_TEMPLATE = "query ($projectId: Long!, $sortObject: [SortObject_annotations]) {" +
											"annotations(projectId: $projectId, sortObject: $sortObject) {" +
												"totalElements," +
												"content {" +
													"module {" +
														"id, " + 
														"name" +
													"}" +
												"}" +
											"}" +
										"}";
	private static final String REFERENCED_VARIABLE_QUERY_TEMPLATE = "query ($projectId: Long!, $sortObject: [SortObject_annotations]) {" +
			"annotations(projectId: $projectId, sortObject: $sortObject) {" +
				"content {" +
					"linkedBusinessDataDictionary : linkedDataDictionaries(isBusiness: true) { name }," +
					"linkedNonBusinessDataDictionary: linkedDataDictionaries(isBusiness: false) { name }," +
					"name," +
					"id," +
				"}" +
				"totalElements" +
			"}" +
		 "}";

	private static final String FUNCTIONAL_GROUP = "query ($projectId: Long!, $filterObject: FilterObject_annotations, $sortObject: [SortObject_annotations]) {"
				+ "annotations(projectId: $projectId, filterObject: $filterObject, sortObject: $sortObject) {" +
					"totalElements," +
					"content {" +
					"id, "+
					"name, " +
					"createdByUserName, " +
					"updatedByUserName," +
					"functionalGroups {" +
						"uid," +
						"name,"+
						"annotationSequenceNumber" +
						"}" +
					"}" +
				"}" +
			"}";
	private static final String ANNOTATION_TAXONOMIES = "query($projectId: Long!, $filterObject: FilterObject_annotations, $sortObject: [SortObject_annotations]) { "
				+ "annotations(projectId: $projectId, filterObject: $filterObject, sortObject: $sortObject) { "
					+ "totalElements "
					+ "content { "
						+ "module { "
							+ "name "
						+ "} "
						+ "type "
						+ "categoryName "
						+ "state "
						+ "name "
					+ "} "
				+ "} "
			+ "}";

	@Autowired
	private ModuleService moduleService;
	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private TaxonomyService taxonomyService;
	@Autowired
	private WebApplicationContext webAppContext;
	@Autowired
	private DataDictionaryService dataDictionaryService;
	@Autowired
	private ApplicationEventPublisher eventPublisher;

	@Nullable
	//Initialized in #init()
	private Long businessRuleCategoryId = null;

	@Autowired
	private FunctionalBlockService functionalBlockService;
	
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
	 * <li>Creates two {@link Module Modules} in the new project</li>
	 * <li>Creates a reference with dummy module location</li>
	 * <li>Creates four {@link AnnotationPojo Annotations} in the new project</li>
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
		
		final EntityId moduleId1 = createTestModule("Module_1_", projectId);
		final EntityId moduleId2 = createTestModule("Module_2_", projectId);
		
		businessRuleCategoryId = annotationService.createCategory(projectId, "Business Rule", Arrays.asList(AnnotationType.RULE));

		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(Integer.valueOf(0));
		dummyLocation.setLength(Integer.valueOf(0));

		final AnnotationPojoPrototype annotation1 = new AnnotationPojoPrototype()
//				No category set for testing that
//				.setCategoryId(businessRuleCategoryId)
				.setName("Database Annotation 1")
				.setType(AnnotationType.DATABASE)
				.setState(WorkingState.CANDIDATE)
				.setSourceAttachment("This is if-ELSE source attachment \n content")
				.setModule(moduleId1)
				.setLocation(dummyLocation)
				.setCreatedByUserId("1")
				.setUpdatedByUserId("");
		final var aid1 = annotationService.create(annotation1);

		/* Creating a String repeater type custom property and adding this custom property to annotation 2*/
		final String stringRepeaterEntityName = "Annotation";
		final String stringRepeaterClassName = stringRepeaterEntityName + "CustomProperties" + projectId.getNid();
		
		final CustomPropertyMetadata propertyMetaData1 = createCustomPropertyMetadata("testField1",
				"Test Annotation String Repeater Custom Label", "Test Annotation Custom Property for graph ql test", "EMBEDDEDLIST",
				CustomPropertyFieldType.DEFAULT, 1, false, null, projectId, stringRepeaterEntityName, stringRepeaterClassName);

		final AnnotationPojoPrototype annotation2 = new AnnotationPojoPrototype()
				.setCategoryId(businessRuleCategoryId)
				.setName("Database Annotation 2")
				.setType(AnnotationType.DATABASE)
				.setState(WorkingState.FOR_REVIEW)
				.setSourceAttachment("This is SUBTract source attachment content")
				.setModule(moduleId2)
				.setLocation(new ModuleLocation(0, 2))
				.setCreatedByUserId("1")
				.setUpdatedByUserId("")
				.setCustomProperties(new HashMap<>(Map.of(stringRepeaterClassName, new HashMap<>(Map.of(propertyMetaData1.getName(), Arrays.asList("red", "blue", "green"))))));
		final var aid2 = annotationService.create(annotation2);

		/* Creating a Tag type custom property and adding this custom property to annotation 3*/
		final String tagEntityName = "Annotation";
		final String tagClassName = tagEntityName + "CustomProperties" + projectId.getNid();
		final String autoCompKey = "AnnotationAutoCompletionKey";
		
		final CustomPropertyMetadata propertyMetaData2 = createCustomPropertyMetadata("fieldName0",
				"Test Annotation TAGS Custom Label", "Test Annotation Custom Property for graph ql test", "EMBEDDEDLIST",
				CustomPropertyFieldType.TAG, 0, false, autoCompKey, projectId, tagEntityName, tagClassName);
		
		/* Creating a String type custom property and adding this custom property to annotation 4*/
		final String stringEntityName = "Annotation";
		final String stringClassName = stringEntityName + "CustomProperties" + projectId.getNid();
		
		final CustomPropertyMetadata propertyMetaData3 = createCustomPropertyMetadata("testField2",
				"Test Annotation String Custom Label", "Test Annotation Custom Property for graph ql test", "STRING",
				CustomPropertyFieldType.DEFAULT, 1, false, null, projectId, stringEntityName, stringClassName);
		
		final AnnotationPojoPrototype annotation3 = new AnnotationPojoPrototype()
				.setCategoryId(businessRuleCategoryId)
				.setName("Annotation 1")
				.setType(AnnotationType.RULE)
				.setState(WorkingState.CANDIDATE)
				.setSourceAttachment("This is if and WHILE source attachment content")
				.setModule(moduleId1)
				.setLocation(new ModuleLocation(0, 3))
				.setCreatedByUserId("1")
				.setUpdatedByUserId("")
				.setReasons(Arrays.asList("LOOP_CONDITION", "OTHER_CONDITION"))
				.setCustomProperties(new NestedMap()
						.set(tagClassName, propertyMetaData2.getName(), Arrays.asList("pl", "natural"))
						.set(stringClassName, propertyMetaData3.getName(), "PL2"));
		
		final var aid3 = annotationService.create(annotation3);
		
		final List<String> tags = Arrays.asList("c", "pl", "natural", "java", "kotlin");
		final Map<String, Set<String>> map = new HashMap<>();
		map.put(autoCompKey, new HashSet<>(tags));
		customPropertiesService.putEnumValues(projectId, map);
		assertTrue(customPropertiesService.getEnumValues(projectId, autoCompKey).containsAll(tags), "All entries should match for key: " + autoCompKey);
		
		final AnnotationPojoPrototype annotation4 = new AnnotationPojoPrototype()
				.setCategoryId(businessRuleCategoryId)
				.setName("DeadCode Annotation")
				.setType(AnnotationType.DEAD_CODE)
				.setState(WorkingState.INVALID)
				.setSourceAttachment("This is Function source attachment \\n content")
				.setModule(moduleId1)
				.setLocation(new ModuleLocation(0, 4))
				.setCreatedByUserId("2")
				.setUpdatedByUserId("")
				.setCustomProperties(new HashMap<>(Map.of(stringClassName,new HashMap<>(Map.of(propertyMetaData3.getName(), "PL1")))));
		final var aid4 = annotationService.create(annotation4);

		final DataDictionaryPojo dataDictionaryEntry1 = createDataDictionaryEntry(moduleId1, "Data Dictionary 1", true, new ModuleLocation(0, 10));
		final DataDictionaryPojo dataDictionaryEntry2 = createDataDictionaryEntry(moduleId1, "Data Dictionary 2", false, new ModuleLocation(20, 10));
		final DataDictionaryPojo dataDictionaryEntry3 = createDataDictionaryEntry(moduleId1, "Data Dictionary 3", true, new ModuleLocation(35, 15));
		final DataDictionaryPojo dataDictionaryEntry4 = createDataDictionaryEntry(moduleId2, "Data Dictionary 2_1", false, new ModuleLocation(0, 10));
		final DataDictionaryPojo dataDictionaryEntry5 = createDataDictionaryEntry(moduleId2, "Data Dictionary 2_2", false, new ModuleLocation(20, 10));
		final DataDictionaryPojo dataDictionaryEntry6 = createDataDictionaryEntry(moduleId2, "Data Dictionary 2_3", true, new ModuleLocation(35, 10));

		dataDictionaryService.linkAnnotations(dataDictionaryEntry1.identity(), aid1);
		dataDictionaryService.linkAnnotations(dataDictionaryEntry2.identity(), aid1);
		dataDictionaryService.linkAnnotations(dataDictionaryEntry3.identity(), aid1);

		dataDictionaryService.linkAnnotations(dataDictionaryEntry4.identity(), aid2);
		dataDictionaryService.linkAnnotations(dataDictionaryEntry5.identity(), aid2);
		dataDictionaryService.linkAnnotations(dataDictionaryEntry6.identity(), aid2);

		dataDictionaryService.linkAnnotations(dataDictionaryEntry1.identity(), aid3);
		dataDictionaryService.linkAnnotations(dataDictionaryEntry2.identity(), aid3);

		EXPECTED_ANNOTATIONS[0] = aid1;
		EXPECTED_ANNOTATIONS[1] = aid2;
		EXPECTED_ANNOTATIONS[2] = aid3;
		EXPECTED_ANNOTATIONS[3] = aid4;
		
		eventPublisher.publishEvent(new CustomPropertiesModifiedEvent(Optional.of(projectId)));
	}

	/**
	 * Tests that the GraphQL query returns expected sorted annotations.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with sort object")
	@Test
	void testSingleProjectWithSortObject() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		/* Execute GraphQL request to fetch the data sorted by name in ascending order and get response data */
		final AnnotationPojo[] actualAnnotations = getEntries(executeQuery(QUERY_TEMPLATE, projectId,
				Arrays.asList(Map.of("content_name", "ASC")), null), EXPECTED_ANNOTATIONS.length, "annotations", AnnotationPojo.class);
		/* Test that the length of created and fetched annotations are same */
		assertEquals(EXPECTED_ANNOTATIONS.length, actualAnnotations.length);
		/* Test that Annotation with name = "Annotation 1" is fetched as the first element of expected annotations */
		assertEquals(EXPECTED_ANNOTATIONS[2], actualAnnotations[0].identity());
		/* Test that Annotation with name = "Database Annotation 1" is fetched as the second element of expected annotations */
		assertEquals(EXPECTED_ANNOTATIONS[0], actualAnnotations[1].identity());
		/* Test that Annotation with name = "Database Annotation 2" is fetched as the third element of expected annotations */
		assertEquals(EXPECTED_ANNOTATIONS[1], actualAnnotations[2].identity());
		/* Test that Annotation with name = "DeadCode Annotation" is fetched as the fourth element of expected annotations */
		assertEquals(EXPECTED_ANNOTATIONS[3], actualAnnotations[3].identity());

	}
	
	/**
	 * Tests that the GraphQL query returns all expected filtered annotations.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object")
	@Test
	void testSingleProjectWithFilterObject() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{content_type : {eq: DATABASE}, content_state : {eq: FOR_REVIEW}}", Object.class);
		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObject), 1, "annotations", AnnotationPojo.class);
		/* Test that only Annotations where Annotation type "Database" and state "FOR_REVIEW" are fetched */
		assertEquals(EXPECTED_ANNOTATIONS[1], actualAnnotations[0].identity());
	}

	/**
	 * Tests that the GraphQL query returns all expected filtered annotations for sourceAttachmentLink.content.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filters related to sourceAttachmentLink.content")
	@Test
	void testSingleProjectWithSourceAttachmentLinkContentFilters() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		final Object filterObject = new Gson().fromJson("{content_sourceAttachment : {eq: 'WHILE'}}", Object.class);
		/* Execute GraphQL request where string passed in request as filters is present in the content in the sourceAttachmentLink.content
		 * and sourceAttachmentLink.content also contains new line character */
		AnnotationPojo[] actualAnnotations = getEntries(executeQuery(
				QUERY_TEMPLATE, projectId, null, filterObject), 1, "annotations", AnnotationPojo.class);
		/* Test that the length of expected Annotations and the fetched Annotations is same */
		assertEquals(1, actualAnnotations.length);
		/* Test that only Annotations where Annotation sourceAttachmentLink.content contains "WHILE" are fetched */
		assertEquals(EXPECTED_ANNOTATIONS[2], actualAnnotations[0].identity());

		final Object filterObject2 = new Gson().fromJson("{content_sourceAttachment : {eq: 'else'}}", Object.class);
		/* Execute GraphQL request where string passed in request as filters is lowercase string and present in the
		 * sourceAttachmentLink.content as uppercase string */
		actualAnnotations = getEntries(executeQuery(
				QUERY_TEMPLATE, projectId, null, filterObject2), 1, "annotations", AnnotationPojo.class);
		/* Test that the length of expected Annotations and the fetched Annotations is same */
		assertEquals(1, actualAnnotations.length);
		/* Test that only Annotations where Annotation sourceAttachmentLink.content contains "else" are fetched */
		assertEquals(EXPECTED_ANNOTATIONS[0], actualAnnotations[0].identity());

		final Object filterObject3 = new Gson().fromJson("{content_sourceAttachment : {eq: 'IF'}}", Object.class);
		/* Execute GraphQL request where string passed in request as filters is uppercase string and present in the
		 * sourceAttachmentLink.content as lowercase string */
		actualAnnotations = getEntries(executeQuery(
				QUERY_TEMPLATE, projectId, null, filterObject3), 2, "annotations", AnnotationPojo.class);
		/* Test that the length of expected Annotations and the fetched Annotations is same */
		assertEquals(2, actualAnnotations.length);
		/* Test that only Annotations where Annotation sourceAttachmentLink.content contains "IF" are fetched */
		assertThat(Arrays.asList(actualAnnotations[0].identity(), actualAnnotations[1].identity()), Matchers.containsInAnyOrder(EXPECTED_ANNOTATIONS[0], EXPECTED_ANNOTATIONS[2]));

		final Object filterObject4 = new Gson().fromJson("{content_sourceAttachment : {eq: 'subtRACT'}}", Object.class);
		/* Execute GraphQL request where string passed in request as filters is mixedcase string and present in the
		 * sourceAttachmentLink.content as mixedcase string */
		actualAnnotations = getEntries(executeQuery(
				QUERY_TEMPLATE, projectId, null, filterObject4), 1, "annotations", AnnotationPojo.class);
		/* Test that the length of expected Annotations and the fetched Annotations is same */
		assertEquals(1, actualAnnotations.length);
		/* Test that only Annotations where Annotation sourceAttachmentLink.content contains "subtRACT" are fetched */
		assertEquals(EXPECTED_ANNOTATIONS[1], actualAnnotations[0].identity());
		
		/* Test for the source attachment with wildcards */
		final Object filterObject5 = new Gson().fromJson("{content_sourceAttachment : {eq: 'This is if*'}}", Object.class);
		actualAnnotations = getEntries(executeQuery(
				QUERY_TEMPLATE, projectId, null, filterObject5), 2, "annotations", AnnotationPojo.class);
		/* Test that the length of expected Annotations and the fetched Annotations is same */
		assertEquals(2, actualAnnotations.length);
		assertThat(Arrays.asList(actualAnnotations[0].identity(), actualAnnotations[1].identity()), Matchers.containsInAnyOrder(EXPECTED_ANNOTATIONS[0], EXPECTED_ANNOTATIONS[2]));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected sorted and filtered annotations.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with sorting and filters")
	@Test
	void testSingleProjectWithSortingAndFilters() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		final Object filterObject = new Gson().fromJson("{content_name : {eq: '*1'}}", Object.class);

		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, Arrays.asList(Map.of("content_name", "ASC")), filterObject), 2, "annotations", AnnotationPojo.class);
		/* Test that the length of expected Annotations and fetched Annotations are same */
		assertEquals(2, actualAnnotations.length);
		/* Test that only Annotations where Annotation name contains "1" are fetched sorted by name in ASC order */
		assertEquals(EXPECTED_ANNOTATIONS[2], actualAnnotations[0].identity());
		assertEquals(EXPECTED_ANNOTATIONS[0], actualAnnotations[1].identity());
	}
	
	/**
	 * Tests that the GraphQL query returns the expected Annotation with Custom property.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with Custom property filter")
	@ParameterizedTest(name = "{index} => propertyFilter={0}, expectedCount={1}")
	@MethodSource("parametersProvider")
	void testProjectWithCustomPropertyFilter(final String propertyFilter, final int expectedCount) {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));
		final Gson gson = new Gson();

		/* Test for Annotation with Custom Property */
		assertAnnotationWithCustomProperty(gson.fromJson(propertyFilter, Object.class), expectedCount);
	}

	/**
	 * Tests that the GraphQL query returns the expected Annotation with No Custom property.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with Custom property filter None")
	@Test
	void testSingleProjectWithCustomPropertyFilterNone() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));
		final Gson gson = new Gson();
		
		/* Execute GraphQL request for Annotation and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, Arrays.asList(Map.of("content_name", "ASC")),
						gson.fromJson("{content_customProperties_AnnotationCustomProperties5_fieldName0 : {is: null}}", Object.class)), 3, "annotations",
						AnnotationPojo.class);
		
		/* Test that the length of expected annotation and fetched annotation are same */
		assertEquals(3, actualAnnotations.length);
		
		/* Test that expected Annotations are returned */
		final List<EntityId> expectedIds = Arrays.asList(EXPECTED_ANNOTATIONS[0], EXPECTED_ANNOTATIONS[1], EXPECTED_ANNOTATIONS[3]);
		final List<EntityId> actualIds = Arrays.stream(actualAnnotations).map(AnnotationPojo::identity).collect(Collectors.toList());

		for (final EntityId expectedId : expectedIds) {
			assertTrue(actualIds.contains(expectedId), "Expected annotation ID not found in actual annotations");
		}
	}
	
	@DisplayName("Test for a project with Module name sorting using sortObject")
	@Test
	void testAnnotationSortingByModuleNameUsingSortObject() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		/* Create expected JSON for Annotation sorting by module name in ascending order */
		final String expectedAscJson = "{annotations={totalElements=4, content=["
				+ "{module={id=2000, name=Module_1_5}}, "
				+ "{module={id=2000, name=Module_1_5}}, "
				+ "{module={id=2000, name=Module_1_5}}, "
				+ "{module={id=2001, name=Module_2_5}}]}}";
		verifyReceivedAnnotationData(
				executeQuery(HAS_ANNOTATION_QUERY_TEMPLATE, projectId, Collections.singletonList(Map.of("content_module_name", "ASC")), null),
				expectedAscJson);

		/* Create expected JSON for Annotation sorting by module name in descending order */
		final String expectedDescJson = "{annotations={totalElements=4, content=["
				+ "{module={id=2001, name=Module_2_5}}, "
				+ "{module={id=2000, name=Module_1_5}}, "
				+ "{module={id=2000, name=Module_1_5}}, "
				+ "{module={id=2000, name=Module_1_5}}]}}";
		verifyReceivedAnnotationData(
				executeQuery(HAS_ANNOTATION_QUERY_TEMPLATE, projectId, Collections.singletonList(Map.of("content_module_name", "DESC")), null),
				expectedDescJson);
	}
	
	/**
	 * Tests that the GraphQL query returns the expected Annotation with Meta data.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with Meta data filters")
	@Test
	void testSingleProjectWithMetaDataFilter() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));
		
		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{content_reasons: {eq: LOOP_CONDITION}}", Object.class);
		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObject), 1, "annotations", AnnotationPojo.class);
		/* Test that only Annotations where meta data reason as 'LOOP_CONDITION' is fetched */
		assertEquals(EXPECTED_ANNOTATIONS[2], actualAnnotations[0].identity());
	}
	
	private void assertAnnotationWithCustomProperty(final Object filterObject, final int index) {
		/* Execute GraphQL request for Annotation and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, Arrays.asList(Map.of("content_name", "ASC")), filterObject), 1, "annotations", AnnotationPojo.class);
		/* Test that the length of expected annotation and fetched annotation are same */
		assertEquals(1, actualAnnotations.length);
		
		/* Test that expected Annotation is returned */
		assertEquals(EXPECTED_ANNOTATIONS[index], actualAnnotations[0].identity());
	}
	
	/**
	 * Tests that the GraphQL query returns all expected Annotation type filtered annotations.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object as Annotation type")
	@Test
	void testSingleProjectWithFilterObjectForAnnotationType() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		final Gson gson = new Gson();

		final Object filterObject = gson.fromJson("{ content_type: { in: [\"DATABASE\", \"DEAD_CODE\"] }}",
				Object.class);
		
		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObject), 3, "annotations", AnnotationPojo.class);
		
		/* Test that Annotations where Annotation type "DATABASE" or "DEAD_CODE" are fetched */
		assertEquals(3, actualAnnotations.length);
		final List<EntityId> actualIds = Arrays.stream(actualAnnotations).map(AnnotationPojo::identity).collect(Collectors.toList());
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[0]", actualIds.contains(EXPECTED_ANNOTATIONS[0]));
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[1]", actualIds.contains(EXPECTED_ANNOTATIONS[1]));
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[3]", actualIds.contains(EXPECTED_ANNOTATIONS[3]));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected type filtered annotations.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object as type")
	@Test
	void testSingleProjectFilterObjectForType() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		final Object filterObjectForType = new Gson().fromJson("{content_module_type : {in : [PROGRAM]}}", Object.class);
		
		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObjectForType), 4, "annotations", AnnotationPojo.class);
		
		/* Test that only Annotations where type "PROGRAM" are fetched */
		assertEquals(4, actualAnnotations.length);
		final List<EntityId> actualIds = Arrays.stream(actualAnnotations).map(AnnotationPojo::identity).collect(Collectors.toList());
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[0]", actualIds.contains(EXPECTED_ANNOTATIONS[0]));
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[1]", actualIds.contains(EXPECTED_ANNOTATIONS[1]));
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[2]", actualIds.contains(EXPECTED_ANNOTATIONS[2]));
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[3]", actualIds.contains(EXPECTED_ANNOTATIONS[3]));
	}

	/**
	 * Tests that the GraphQL query returns all expected is null Identification Reason filtered annotations.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object as Identification Reason")
	@Test
	void testSingleProjectWithIsNullFilterObjectForIdentificationReason() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		final Gson gson = new Gson();
			
		final Object filterObjectForIdentificationReason = gson.fromJson("{content_reasons : {\"is\": null}}", Object.class);
		
		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObjectForIdentificationReason), 3, "annotations", AnnotationPojo.class);
		
		/* Test that only Annotations where Identification Reason is null are fetched */
		assertEquals(3, actualAnnotations.length);
		final List<EntityId> actualIds = Arrays.stream(actualAnnotations).map(AnnotationPojo::identity).collect(Collectors.toList());
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[0]", actualIds.contains(EXPECTED_ANNOTATIONS[0]));
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[1]", actualIds.contains(EXPECTED_ANNOTATIONS[1]));
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[3]", actualIds.contains(EXPECTED_ANNOTATIONS[3]));
		}
	
	/**
	 * Tests that the GraphQL query returns all expected State filtered annotations.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object as state")
	@Test
	void testSingleProjectWithFilterObjectForState() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		final Gson gson = new Gson();
		
		/* filterObject as stateLink */
		final Object filterObjectForState = gson.fromJson("{content_state : {in : [CANDIDATE, INVALID]}}",
				Object.class);
		
		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObjectForState), 3, "annotations", AnnotationPojo.class);
		
		/* Test that only Annotations where state "CANDIDATE" or "INVALID" are fetched */
		assertEquals(3, actualAnnotations.length);
		final List<EntityId> actualIds = Arrays.stream(actualAnnotations).map(AnnotationPojo::identity).collect(Collectors.toList());
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[0]", actualIds.contains(EXPECTED_ANNOTATIONS[0]));
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[2]", actualIds.contains(EXPECTED_ANNOTATIONS[2]));
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[3]", actualIds.contains(EXPECTED_ANNOTATIONS[3]));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected is null Category filtered annotations.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object as Category name")
	@Test
	void testSingleProjectWithIsNullFilterObjectForCategory() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));
		
		//create an Annotation without referencing a category
		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(Integer.valueOf(0));
		dummyLocation.setLength(Integer.valueOf(0));
		
		/* filterObject as none category */
		final Object filterObjectForCategory = new Gson().fromJson("{content_categoryName : {\"is\": null}, content_type: { in: [\"DATABASE\", \"DEAD_CODE\"] }}", Object.class);
		
		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObjectForCategory), 1, "annotations", AnnotationPojo.class);
		
		/* Test that only Annotations where Annotation category is null are fetched */
		assertEquals(1, actualAnnotations.length);
		final List<EntityId> actualIds = Arrays.stream(actualAnnotations).map(AnnotationPojo::identity).collect(Collectors.toList());
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[0]", actualIds.contains(EXPECTED_ANNOTATIONS[0]));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected Technology filtered annotations.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for a project with filter object as Technology")
	@Test
	void testSingleProjectWithFilterObjectForTechnology() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		final Gson gson = new Gson();
		
		final Object filterObjectForTechnology = gson.fromJson("{content_module_technology : "
				+ "{eq : COBOL}}", Object.class);
		
		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, filterObjectForTechnology), 4, "annotations", AnnotationPojo.class);
		
		/* Test that only Annotations where Technology "COBOL" are fetched */
		assertEquals(4, actualAnnotations.length);
		final List<EntityId> actualIds = Arrays.stream(actualAnnotations).map(AnnotationPojo::identity).collect(Collectors.toList());
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[0]", actualIds.contains(EXPECTED_ANNOTATIONS[0]));
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[1]", actualIds.contains(EXPECTED_ANNOTATIONS[1]));
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[2]", actualIds.contains(EXPECTED_ANNOTATIONS[2]));
		assertTrue("actualIds should contain id of EXPECTED_ANNOTATIONS[3]", actualIds.contains(EXPECTED_ANNOTATIONS[3]));	
	}
	
	@DisplayName("Test For Annotation Created By UserName")
	@Test
	void testAnnotationForCreatedByUserName() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));
		/* Execute GraphQL request and get response data */
		final Response response = executeQuery(QUERY_TEMPLATE, projectId, null, null);
		response.path("annotations.totalElements").entity(Long.class).isEqualTo(Long.valueOf(4));

		final Map<Long, List<String>> actualMap = valuesToMap(4,
						idx -> response.path("annotations.content[" + idx + "].id").entity(EntityId.class).get().getNid(),
						idx -> Stream.of(response.path("annotations.content[" + idx + "].createdByUserName").entity(String.class).get(),
										 response.path("annotations.content[" + idx + "].updatedByUserName").entity(String.class).get())
									.sorted().collect(Collectors.toList()));

		final Map<Long, List<String>> expectedMap = valuesToMap(4,
						idx -> EXPECTED_ANNOTATIONS[idx].getNid(),
						idx -> Stream.of(userNameUtil.getUserName(annotationService.get(EXPECTED_ANNOTATIONS[idx]).getCreatedByUserId()),
										 "SYSTEM")
									.sorted().collect(Collectors.toList()));

		assertThat(actualMap, Matchers.equalTo(expectedMap));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected annotations for a module.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for filtering annotations for a module")
	@Test
	void testAnnotationsForModuleWithoutSorting() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));
		
		final EntityId moduleId = moduleService.findModuleIds(q -> q.ofProject(projectId).withName("Module_1_" + projectId.getNid())).get(0);

		final var filterObject = Map.of("content_module_id", Map.of("eq", moduleId.getNid()));
		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations = getEntries(
				executeQuery(QUERY_TEMPLATE, projectId, null, filterObject), 3, "annotations", AnnotationPojo.class);
		/* Test that only Annotations for Module are found */
		
		assertEquals(new HashSet<Long>(Arrays.asList(EXPECTED_ANNOTATIONS[0].getNid(), EXPECTED_ANNOTATIONS[2].getNid(), EXPECTED_ANNOTATIONS[3].getNid())),
				new HashSet<Long>(Arrays.asList(actualAnnotations[0].getId(), actualAnnotations[1].getId(), actualAnnotations[2].getId())));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected annotations for a module sorted based on annotation type in descending order.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for filtering annotations for a module with sorting")
	@Test
	void testAnnotationsForModuleWithSorting() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));
		
		
		final EntityId moduleId = moduleService.findAnyModule(q -> q.ofProject(projectId).withName("Module_1_" + projectId.getNid())).get().identity();

		final var filterObject = Map.of("content_module_id", Map.of("eq", moduleId.getNid()));

		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations = getEntries(
				executeQuery(QUERY_TEMPLATE, projectId, Arrays.asList(Map.of("content_type", "DESC")), filterObject), 3, "annotations", AnnotationPojo.class);
		/* Test that only Annotations for Module are found sorted based on annotation type descending order */
		assertEquals(EXPECTED_ANNOTATIONS[2], actualAnnotations[0].identity());
		assertEquals(EXPECTED_ANNOTATIONS[3], actualAnnotations[1].identity());
		assertEquals(EXPECTED_ANNOTATIONS[0], actualAnnotations[2].identity());
	}
	
	/**
	 * Tests check whether list of linked Business DataDictionary Entries associated with Annotation returned or not
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test linked Business Data Dictionary Entries")
	@Test
	void testLinkedBusinessDataDictionaryEntries() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));
		/* Execute GraphQL request and get response data */
		final Response response = executeQuery(REFERENCED_VARIABLE_QUERY_TEMPLATE, projectId, null, null);
		response.path("annotations.totalElements").entity(Long.class).isEqualTo(Long.valueOf(4));

		/* The order of the Annotations can differ. Find the correct index of the modules in the response */
		final List<EntityId> annotationList = Arrays.asList(EXPECTED_ANNOTATIONS).stream().collect(Collectors.toList());
		final int indexAnnotation1 = annotationList.indexOf(response.path("annotations.content[0].id").entity(EntityId.class).get());
		final int indexAnnotation2 = annotationList.indexOf(response.path("annotations.content[1].id").entity(EntityId.class).get());
		final int indexAnnotation3 = annotationList.indexOf(response.path("annotations.content[2].id").entity(EntityId.class).get());
		final int indexAnnotation4 = annotationList.indexOf(response.path("annotations.content[3].id").entity(EntityId.class).get());

		@SuppressWarnings("unchecked")
		final List<String>[] businessDdeLists = new List[4];
		businessDdeLists[indexAnnotation1] = getDataDictionaryEntries(response, "annotations.content[0].linkedBusinessDataDictionary");
		businessDdeLists[indexAnnotation2] = getDataDictionaryEntries(response, "annotations.content[1].linkedBusinessDataDictionary");
		businessDdeLists[indexAnnotation3] = getDataDictionaryEntries(response, "annotations.content[2].linkedBusinessDataDictionary");
		businessDdeLists[indexAnnotation4] = getDataDictionaryEntries(response, "annotations.content[3].linkedBusinessDataDictionary");
		
		assertEquals(2, businessDdeLists[0].size());
		assertEquals(1, businessDdeLists[1].size());
		assertEquals(1, businessDdeLists[2].size());
		assertEquals(0, businessDdeLists[3].size());
		
		/* the Link DDE should have same businessRule Element name as Expected*/
		assertTrue("Annotation1 Should Contain DataDictionary1 and DataDictionary3",
				CollectionUtils.isEqualCollection(Arrays.asList("Data Dictionary 1", "Data Dictionary 3"), businessDdeLists[0]));
		assertTrue("Annotation2 Should Contain DataDictionary2_3",
				CollectionUtils.isEqualCollection(Collections.singletonList("Data Dictionary 2_3"), businessDdeLists[1]));
		assertTrue("Annotation3 Should Contain DataDictionary1",
				CollectionUtils.isEqualCollection(Collections.singletonList("Data Dictionary 1"), businessDdeLists[2]));
		assertTrue("Annotation4 should have no datatdictionary", businessDdeLists[3].isEmpty());
	}
	
	/**
	 * Tests check whether list of linked Non Business DataDictionary Entries associated with this Annotation returned or not
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test linked Non Business Data Dictionary Entries")
	@Test
	void testLinkedNonBusinessDataDictionaryEntries() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));
		/* Execute GraphQL request and get response data */
		final Response response = executeQuery(REFERENCED_VARIABLE_QUERY_TEMPLATE, projectId, null, null);
		response.path("annotations.totalElements").entity(Long.class).isEqualTo(Long.valueOf(4));

		/* The order of the Annotations can differ. Find the correct index of the modules in the response */
		final List<EntityId> annotationList = Arrays.asList(EXPECTED_ANNOTATIONS).stream().collect(Collectors.toList());
		final int indexAnnotation1 = annotationList.indexOf(response.path("annotations.content[0].id").entity(EntityId.class).get());
		final int indexAnnotation2 = annotationList.indexOf(response.path("annotations.content[1].id").entity(EntityId.class).get());
		final int indexAnnotation3 = annotationList.indexOf(response.path("annotations.content[2].id").entity(EntityId.class).get());
		final int indexAnnotation4 = annotationList.indexOf(response.path("annotations.content[3].id").entity(EntityId.class).get());

		@SuppressWarnings("unchecked")
		final List<String>[] nonBusinessDdeLists = new List[4];
		nonBusinessDdeLists[indexAnnotation1] = getDataDictionaryEntries(response, "annotations.content[0].linkedNonBusinessDataDictionary");
		nonBusinessDdeLists[indexAnnotation2] = getDataDictionaryEntries(response, "annotations.content[1].linkedNonBusinessDataDictionary");
		nonBusinessDdeLists[indexAnnotation3] = getDataDictionaryEntries(response, "annotations.content[2].linkedNonBusinessDataDictionary");
		nonBusinessDdeLists[indexAnnotation4] = getDataDictionaryEntries(response, "annotations.content[3].linkedNonBusinessDataDictionary");

		assertEquals(1, nonBusinessDdeLists[0].size());
		assertEquals(2, nonBusinessDdeLists[1].size());
		assertEquals(1, nonBusinessDdeLists[2].size());
		assertEquals(0, nonBusinessDdeLists[3].size());

		/* the Link DDE should have same businessRule Element name as Expected*/
		assertTrue("Annotation1 Should Contain DataDictionary2",
				CollectionUtils.isEqualCollection(Collections.singletonList("Data Dictionary 2"), nonBusinessDdeLists[0]));
		assertTrue("Annotation2 Should Contain DataDictionary2_1 and 2_2",
				CollectionUtils.isEqualCollection(Arrays.asList("Data Dictionary 2_1", "Data Dictionary 2_2"), nonBusinessDdeLists[1]));
		assertTrue("Annotation3 Should Contain DataDictionary2",
				CollectionUtils.isEqualCollection(Collections.singletonList("Data Dictionary 2"), nonBusinessDdeLists[2]));
		assertTrue("Annotation4 should have no datatdictionary", nonBusinessDdeLists[3].isEmpty());
	}
	
	@DisplayName("Test filter and sort by functional group")
	@Test
	void tesAnnotationFilterAndSortToFunctionalBlock() {
		createFunctionalBlock();
		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{ content_functionalGroups_name: { eq: \"Test Group 1\" } }", Object.class);
		final Object filterObject2 = gson.fromJson("{ content_functionalGroups_name: { eq: \"Test Group *\" } }", Object.class);
		final Object filterObject3 = gson.fromJson("{ content_functionalGroups_name: { eq: \"* Group 1\" } }", Object.class);
		final Object filterObject4 = gson.fromJson("{ content_functionalGroups_name: { eq: \"Test * 1\" } }", Object.class);
		
		getEntries(executeQuery(FUNCTIONAL_GROUP, projectId, Collections.singletonList(Map.of("content_functionalGroups_name", "ASC")), filterObject), 2, "annotations",
				AnnotationPojo.class);
		getEntries(executeQuery(FUNCTIONAL_GROUP, projectId, Collections.singletonList(Map.of("content_functionalGroups_name", "ASC")), filterObject3), 2, "annotations",
				AnnotationPojo.class);
		getEntries(executeQuery(FUNCTIONAL_GROUP, projectId, Collections.singletonList(Map.of("content_functionalGroups_name", "ASC")), filterObject4), 2, "annotations",
				AnnotationPojo.class);
		final AnnotationPojo[] actualAnnotations = getEntries(
				executeQuery(FUNCTIONAL_GROUP, projectId, Collections.singletonList(Map.of("content_name", "ASC", "content_functionalGroups_name", "ASC")), filterObject2), 3, "annotations",
				AnnotationPojo.class);
		assertEquals(EXPECTED_ANNOTATIONS[2], actualAnnotations[0].identity());
		assertEquals(EXPECTED_ANNOTATIONS[0], actualAnnotations[1].identity());
		assertEquals(EXPECTED_ANNOTATIONS[1], actualAnnotations[2].identity());

		final List<String> fbNamesAsc = executeQuery(FUNCTIONAL_GROUP, projectId, Collections.singletonList(Map.of("content_functionalGroups_name", "ASC")), null)
				.path("annotations.content[*].functionalGroups[*].name").entityList(String.class).get();
		assertEquals(List.of("check Group 2", "Test Group 1", "Test Group 1", "Test Group 2"), fbNamesAsc);

		final List<String> fbNamesDesc = executeQuery(FUNCTIONAL_GROUP, projectId, Collections.singletonList(Map.of("content_functionalGroups_name", "DESC")), null)
				.path("annotations.content[*].functionalGroups[*].name").entityList(String.class).get();
		assertEquals(List.of("Test Group 2", "Test Group 1", "Test Group 1", "check Group 2"), fbNamesDesc);
	}

	@DisplayName("Test Annotations with Equal id")
	@Test
	void testAnnotationIdEqFilter() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, null, Map.of("content_id", Map.of("eq", 1))), 1, "annotations", AnnotationPojo.class);
		/* Test that only Annotations where Annotation id == 1 is fetched */
		assertEquals(EXPECTED_ANNOTATIONS[0].getNid(), actualAnnotations[0].getId());
	}

	@DisplayName("Test Annotations with Greater Than or Equal id")
	@Test
	void testAnnotationIdGteFilter() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, List.of(Map.of("content_id", "ASC")),
						Map.of("content_id", Map.of("gte", 2))), 3, "annotations", AnnotationPojo.class);
		/* Test that only Annotations where Annotation id>=2 are fetched */
		assertEquals(EXPECTED_ANNOTATIONS[1].getNid(), actualAnnotations[0].getId());
		assertEquals(EXPECTED_ANNOTATIONS[2].getNid(), actualAnnotations[1].getId());
		assertEquals(EXPECTED_ANNOTATIONS[3].getNid(), actualAnnotations[2].getId());
	}

	@DisplayName("Test Annotations with Less Than or Equal id")
	@Test
	void testAnnotationIdLteFilter() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));

		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId, List.of(Map.of("content_module_id", "DESC")),
						Map.of("content_id", Map.of("lte", 2))), 2, "annotations", AnnotationPojo.class);
		/* Test that only Annotations where Annotation id<=2 are fetched */
		assertEquals(EXPECTED_ANNOTATIONS[1].getNid(), actualAnnotations[0].getId());
		assertEquals(EXPECTED_ANNOTATIONS[0].getNid(), actualAnnotations[1].getId());
	}

	@DisplayName("Test filter and sort by functional group does not return duplicates when assigned two functional groups to single annotation")
	@Test
	@SuppressWarnings("unchecked")
	void tesAnnotationFilterAndSortToSingleFunctionalBlock() {
		final EntityId projectId2 = createProject("Test Project ", 2L).identity();
		final EntityId moduleId = createTestModule("Module_3_", projectId2);
		final AnnotationPojo annotation = createTestAnnotation(moduleId, "Test Ann1");
		final AnnotationPojo annotation2 = createTestAnnotation(moduleId, "Test Ann3");
		final AnnotationPojo annotation3 = createTestAnnotation(moduleId, "Test Ann2");
		final ModulePojo module = moduleService.getModule(moduleId);

		final UUID functionalblockUID = createFunctionalBlockPojoPrototype(projectId2, null, "fu1", "fuDesc1",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)));

		createFunctionalBlockPojoPrototype(projectId2, Arrays.asList(functionalblockUID),
				"FG1", "Fg1Desc", Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)));
		functionalBlockService.setGeneratedFrom(functionalblockUID, GeneratedFrom.fromAnnotation(annotation.identity()));

		createFunctionalBlockPojoPrototype(projectId2, Arrays.asList(functionalblockUID),
				"FG2", "Fg2Desc", Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)));
		functionalBlockService.setGeneratedFrom(functionalblockUID, GeneratedFrom.fromAnnotation(annotation.identity()));

		final UUID functionalblockUID2 = createFunctionalBlockPojoPrototype(projectId2, null, "fu2", "fu2Desc",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)));
		createFunctionalBlockPojoPrototype(projectId2, Arrays.asList(functionalblockUID2),
				"FG3", "Fg3Desc", Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)));
		functionalBlockService.setGeneratedFrom(functionalblockUID2, GeneratedFrom.fromAnnotation(annotation2.identity()));

		final UUID functionalblockUID3 = createFunctionalBlockPojoPrototype(projectId2, null, "fu3", "fu3Desc",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)));

		createFunctionalBlockPojoPrototype(projectId2, Arrays.asList(functionalblockUID3),
				"FG4", "fg4Desc", Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)));
		functionalBlockService.setGeneratedFrom(functionalblockUID3, GeneratedFrom.fromAnnotation(annotation3.identity()));

		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{ content_module_name: { eq: \""+ module.getName() + "\" } }", Object.class);
		final Response response = executeQuery(FUNCTIONAL_GROUP, projectId2, Arrays.asList(Map.of("content_functionalGroups_name", "ASC")), filterObject);

		final List<AnnotationPojo> content = response.path("annotations.content").entity(List.class).get();
		assertEquals(3, content.size());

		final List<String> groupNames = new ArrayList<>();
		groupNames.add(response.path("annotations.content[0].functionalGroups[0].name").entity(String.class).get());
		groupNames.add(response.path("annotations.content[0].functionalGroups[1].name").entity(String.class).get());
		groupNames.add(response.path("annotations.content[1].functionalGroups[0].name").entity(String.class).get());
		groupNames.add(response.path("annotations.content[2].functionalGroups[0].name").entity(String.class).get());

		assertTrue(groupNames.equals(Arrays.asList("FG1", "FG2", "FG3", "FG4")) || groupNames.equals(Arrays.asList("FG2", "FG1", "FG3", "FG4")),
				"content[0] has two functionalGroups order must be one of the two expected");
	}

	@DisplayName("Test Ungrouped Annotations")
	@Test
	void testUngroupedAnnotations() {
		final EntityId projectId2 = createProject("Test Ungrouped ", 2L).identity();
		final EntityId moduleId = createTestModule("TestModule", projectId2);
		final String moduleName = "TestModule" + projectId2.getNid();
		final AnnotationPojo annotation1 = createTestAnnotation(moduleId, "Test Ann1");
		final AnnotationPojo annotation2 = createTestAnnotation(moduleId, "Test Ann2");
		final AnnotationPojo annotation3 = createTestAnnotation(moduleId, "Test Ann3");
		final AnnotationPojo annotation4 = createTestAnnotation(moduleId, "Test Ann4");
		final List<Long> expectedAnnotationIds = Arrays.asList(annotation1.getId(), annotation3.getId());

		final UUID functionalBlockAnnotation1 = createFunctionalBlockPojoPrototype(projectId, null, "Test Unit 1", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)));
		final UUID functionalBlockAnnotation2 = createFunctionalBlockPojoPrototype(projectId, null, "Test Unit 2", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)));
		final UUID functionalBlockAnnotation3 = createFunctionalBlockPojoPrototype(projectId,	null, "Test Unit 3", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)));
		final UUID  functionalBlockAnnotation4 = createFunctionalBlockPojoPrototype(projectId, null, "Test Unit 4", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)));

		final List<UUID> childUIDs = new ArrayList<>();
		childUIDs.add(functionalBlockAnnotation2);
		childUIDs.add(functionalBlockAnnotation4);

		createFunctionalBlockPojoPrototype(projectId, childUIDs, "Test Group 1", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)));
		functionalBlockService.setGeneratedFrom(functionalBlockAnnotation1, GeneratedFrom.fromAnnotation(annotation1.identity()));
		functionalBlockService.setGeneratedFrom(functionalBlockAnnotation2, GeneratedFrom.fromAnnotation(annotation2.identity()));
		functionalBlockService.setGeneratedFrom(functionalBlockAnnotation3, GeneratedFrom.fromAnnotation(annotation3.identity()));
		functionalBlockService.setGeneratedFrom(functionalBlockAnnotation4, GeneratedFrom.fromAnnotation(annotation4.identity()));

		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{ content_functionalGroups_name: { is: null }, "
				+ "content_module_name: { eq: \"" + moduleName + "\" } }", Object.class);
		final Response response = executeQuery(FUNCTIONAL_GROUP, projectId2,
				Arrays.asList(Map.of("content_functionalGroups_name", "ASC")), filterObject);

		final List<AnnotationPojo> content = response.path("annotations.content").entityList(AnnotationPojo.class).get();
		assertEquals(2, content.size());

		final List<Long> actualAnnotationIds = new ArrayList<>();
		for (int i = 0; i < content.size(); i++) {
			actualAnnotationIds.add(response.path("annotations.content[" + i + "].id").entity(Long.class).get());
		}
		assertTrue(actualAnnotationIds.containsAll(expectedAnnotationIds), "actualAnnotationIds should contain id of annotation1 and annotation2");
	}

	@DisplayName("Test Annotations with Taxonomies")
	@Test
	void testAnnotationsWithTaxonomies() {
		final EntityId moduleId1 = createTestModule("TestModule1_", projectId);
		final EntityId moduleId2 = createTestModule("TestModule2_", projectId);
		projectId.getNid();
		projectId.getNid();
		final AnnotationPojo annotation1 = createTestAnnotation(moduleId1, "Test Ann1");
		final AnnotationPojo annotation2 = createTestAnnotation(moduleId1, "Test Ann2");
		final AnnotationPojo annotation3 = createTestAnnotation(moduleId2, "Test Ann3");
		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(Integer.valueOf(1));
		dummyLocation.setLength(Integer.valueOf(1));
		final AnnotationPojo annotation4 = createAnnotation("Test Ann4", AnnotationType.RULE, WorkingState.CANDIDATE,
				"content", moduleId2, dummyLocation, "1", null);

		final TaxonomyCategoryPojo taxonomyCategory1 = taxonomyService.findCategories(q -> q.withName("Business Taxonomies")).get(0);
		final UUID taxonomyType1 = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("BusinessProcess").setProject(projectId).setCategoryId(taxonomyCategory1.getId()));
		final UUID taxonomyType2 = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("BusinessSubsystem").setProject(projectId).setCategoryId(taxonomyCategory1.getId()));
		final EntityId taxonomy1 = taxonomyService.create(new TaxonomyPojoPrototype().setProject(projectId).setName("Taxonomy1").setType(taxonomyType1));
		final EntityId taxonomy2 = taxonomyService.create(new TaxonomyPojoPrototype().setProject(projectId).setName("Taxonomy2").setType(taxonomyType1));
		final EntityId taxonomy3 = taxonomyService.create(new TaxonomyPojoPrototype().setProject(projectId).setName("Taxonomy3").setType(taxonomyType2));

		//later we'll filter on the type, the BusinessProcess name
		taxonomyService.createModuleLink(moduleId1.getUid(), taxonomy1);
		taxonomyService.createModuleLink(moduleId1.getUid(), taxonomy2);
		taxonomyService.createModuleLink(moduleId1.getUid(), taxonomy3);
		taxonomyService.createModuleLink(moduleId2.getUid(), taxonomy1);
		taxonomyService.createModuleLink(moduleId2.getUid(), taxonomy3);

		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));

		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{content_module_taxonomy_businessProcess: {eq: \"Taxonomy1\"}, content_type: {eq: RULE}}", Object.class);
		final Response response = executeQuery(ANNOTATION_TAXONOMIES, projectId,
				Arrays.asList(Map.of("content_module_name", "ASC")), filterObject);

		final Long totalElements = response.path("annotations.totalElements").entity(Long.class).get();
		assertEquals(1, totalElements);
		final String annotationName = response.path("annotations.content[0].name").entity(String.class).get();
		assertEquals("Test Ann4", annotationName);

		annotationService.delete(projectId, annotation1.identity());
		annotationService.delete(projectId, annotation2.identity());
		annotationService.delete(projectId, annotation3.identity());
		annotationService.delete(projectId, annotation4.identity());
		moduleService.deleteModule(moduleId1, true);
		moduleService.deleteModule(moduleId2, true);
	}

	@DisplayName("Test For Annotation Modified By UserName is SYSTEM When no modification is made after creation")
	@Test
	void testAnnotationForModifiedByUserNameWhenNoModificationMadeAfterCreation() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final EntityId projectEid2 = createProject("TestProject5", 2L).identity();
		final EntityId moduleId = createTestModule("TestModule", projectEid2);
		final String moduleName = "TestModule" + projectEid2.getNid();
		final AnnotationPojo annotation1 = createTestAnnotation(moduleId, "Test Ann1");
		final AnnotationPojo annotation2 = createTestAnnotation(moduleId, "Test Ann2");
		final AnnotationPojo annotation3 = createTestAnnotation(moduleId, "Test Ann3");
		final AnnotationPojo annotation4 = createTestAnnotation(moduleId, "Test Ann4");

		/* Execute GraphQL request and get response data */
		final Gson gson = new Gson();
		final Object filterObject = gson.fromJson("{content_module_name: { eq: \"" + moduleName + "\" } }", Object.class);
		final Response response = executeQuery(QUERY_TEMPLATE, projectEid2, null, filterObject);

		/* The order of the Annotations can differ. Find the correct index of the modules in the response */
		final List<Long> expectedAnnotationIds =
				Arrays.asList(annotation1, annotation2, annotation3, annotation4).stream().map(AnnotationPojo :: getId).collect(Collectors.toList());

		final List<AnnotationPojo> content = response.path("annotations.content").entityList(AnnotationPojo.class).get();
		assertEquals(4, content.size());

		final List<Long> actualAnnotationIds = new ArrayList<>();
		for (int i = 0; i < content.size(); i++) {
			actualAnnotationIds.add(response.path("annotations.content[" + i + "].id").entity(Long.class).get());
			final String updatedByUserName = response.path("annotations.content[" + i + "].updatedByUserName").entity(String.class).get();
			assertEquals("SYSTEM", updatedByUserName, "updatedByUserName should be SYSTEM");
		}
		assertTrue(actualAnnotationIds.containsAll(expectedAnnotationIds), "actualAnnotationIds should contain id "
				+ "of annotation1, annotation2, annotation3, annotation4");
	}
	
	/**
	 * Tests that the GraphQL query returns all expected annotations for a module sorted based on annotation offset in ascending order.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for filtering annotations for a module with sorting by annotationOffset")
	@Test
	void testAnnotationsForModuleWithOffsetSorting() {
		final EntityId moduleId3 = createTestModule("Module_3_", projectId);
		try {
			final var a1 = annotationService.create(new AnnotationPojoPrototype()
					.setModule(moduleId3)
					.setLocation(new ModuleLocation(10, 2))
					.setName("A1").setType(AnnotationType.DATABASE).setState(WorkingState.CANDIDATE).setCreatedByUserId("1"));
			final var a2 = annotationService.create(new AnnotationPojoPrototype()
					.setModule(moduleId3)
					.setLocation(new ModuleLocation(1, 2))
					.setName("A2").setType(AnnotationType.DATABASE).setState(WorkingState.CANDIDATE).setCreatedByUserId("1"));
			final var a3 = annotationService.create(new AnnotationPojoPrototype()
					.setModule(moduleId3)
					.setLocation(new ModuleLocation(100, 2))
					.setName("A3").setType(AnnotationType.DATABASE).setState(WorkingState.CANDIDATE).setCreatedByUserId("1"));
	
			/* Set full user accesses to project */
			setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));
	
			final var filter = Map.of("content_module_id", Map.of("eq", moduleId3.getNid()));
			final var sort = List.of(Map.of("content_annotationOffset", "ASC"));
	
			/* Execute GraphQL request and get response data */
			final AnnotationPojo[] result = getEntries(executeQuery(QUERY_TEMPLATE, projectId, sort, filter), 3, "annotations", AnnotationPojo.class);
	
			/* Test that only Annotations for Module are found sorted based on annotation type descending order */
			assertEquals(a2, result[0].identity());
			assertEquals(a1, result[1].identity());
			assertEquals(a3, result[2].identity());
		} finally {
			moduleService.deleteModule(moduleId3, false);
		}
	}

	@DisplayName("Test for a project with MultiSelect Meta data filters")
	@Test
	void testSingleProjectWithMetaDataFilterForMultiSelectFilter() {
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final EntityId projectId2 = createProject("TestProject", 2L).identity();
		final EntityId moduleId = createTestModule("TestModule", projectId2);
		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(Integer.valueOf(1));
		dummyLocation.setLength(Integer.valueOf(1));
		createAnnotation("Test Anno1", AnnotationType.EXCLUDE, WorkingState.CANDIDATE,
				"content", moduleId, dummyLocation, "1", Arrays.asList("COBOL_EVALUATE_CONDITION"));
		final AnnotationPojo annotation2 = createAnnotation("Test Anno2", AnnotationType.EXCLUDE, WorkingState.CANDIDATE,
				"content", moduleId, dummyLocation, "1", Arrays.asList("OTHER_CONDITION", "COBOL_EVALUATE_CONDITION"));
		final AnnotationPojo annotation3 = createAnnotation("Test Anno3", AnnotationType.EXCLUDE, WorkingState.CANDIDATE,
				"content", moduleId, dummyLocation, "1", Arrays.asList("LOOP_CONDITION", "COBOL_EVALUATE_CONDITION"));
		createAnnotation("Test Anno4", AnnotationType.EXCLUDE, WorkingState.CANDIDATE,
				"content", moduleId, dummyLocation, "1", Arrays.asList("IF_ELSE_CONDITION"));
		final AnnotationPojo annotation5 = createAnnotation("Test Anno5", AnnotationType.EXCLUDE, WorkingState.CANDIDATE,
				"content", moduleId, dummyLocation, "1", Arrays.asList("LOOP_CONDITION", "OTHER_CONDITION", "SELECT_CONDITION"));
		final Gson gson = new Gson();
		final Object filterObjectForMetaData = gson.fromJson("{content_reasons : {in : [LOOP_CONDITION, OTHER_CONDITION, SELECT_CONDITION]}}", Object.class);
		/* Execute GraphQL request and get response data */
		final AnnotationPojo[] actualAnnotations =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId2, Arrays.asList(Map.of("content_name", "ASC")), filterObjectForMetaData), 3, "annotations", AnnotationPojo.class);
		/* Test that only Annotations where meta data reason as 'LOOP_CONDITION','OTHER_CONDITION' , 'SELECT_CONDITION' is fetched */
		final List<Long> expectedAnnotationIds =
				Arrays.asList(annotation2, annotation3, annotation5).stream().map(AnnotationPojo :: getId).collect(Collectors.toList());
		final List<Long> actualAnnotationIds = 
				Arrays.asList(actualAnnotations).stream().map(AnnotationPojo :: getId).collect(Collectors.toList());
		assertTrue(actualAnnotationIds.containsAll(expectedAnnotationIds), "actualAnnotationIds should contain id of annotation2, annotation4, annotation5");
	}
	
	@DisplayName("Test for a project with Reasons containing NULL")
	@Test
	void testSingleProjectForReasonContainingNullWithMultiSelectFilter() {
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final EntityId projectId3 = createProject("TestProjectReason", 2L).identity();
		final EntityId moduleId = createTestModule("TestModule", projectId3);
		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(Integer.valueOf(1));
		dummyLocation.setLength(Integer.valueOf(1));
		final AnnotationPojo annotation1 = createAnnotation("Test Anno1", AnnotationType.EXCLUDE, WorkingState.CANDIDATE,
				"content", moduleId, dummyLocation, "1", null);
		final AnnotationPojo annotation2 = createAnnotation("Test Anno2", AnnotationType.EXCLUDE, WorkingState.CANDIDATE,
				"content", moduleId, dummyLocation, "1", Arrays.asList("IF_ELSE_CONDITION"));
		final AnnotationPojo annotation3 = createAnnotation("Test Anno3", AnnotationType.EXCLUDE, WorkingState.CANDIDATE,
				"content", moduleId, dummyLocation, "1", Arrays.asList("COBOL_EVALUATE_CONDITION"));
		final AnnotationPojo annotation4 = createAnnotation("Test Anno4", AnnotationType.EXCLUDE, WorkingState.CANDIDATE,
				"content", moduleId, dummyLocation, "1", null);
		final Gson gson = new Gson();
		
		final Object filterObjectForReason1 = gson.fromJson("{content_reasons : {eq : null}}", Object.class);
		final AnnotationPojo[] actualAnnotations1 =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId3, Arrays.asList(Map.of("content_name", "ASC")), filterObjectForReason1), 2, "annotations", AnnotationPojo.class);
		/* Test that only Annotations where meta data reason is null is fetched */
		final List<Long> expectedAnnotationIds1 =
				Arrays.asList(annotation1, annotation4).stream().map(AnnotationPojo :: getId).collect(Collectors.toList());
		final List<Long> actualAnnotationIds1 = 
				Arrays.asList(actualAnnotations1).stream().map(AnnotationPojo :: getId).collect(Collectors.toList());
		assertTrue(actualAnnotationIds1.containsAll(expectedAnnotationIds1), "actualAnnotationIds should contain id of annotation1, annotation4");
		
		final Object filterObjectForReasons2 = gson.fromJson("{content_reasons : {in : [null, IF_ELSE_CONDITION]}}", Object.class);
		final AnnotationPojo[] actualAnnotations2 =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId3, Arrays.asList(Map.of("content_name", "ASC")), filterObjectForReasons2), 3, "annotations", AnnotationPojo.class);
		/* Test that only Annotations where meta data reason is null or 'IF_ELSE_CONDITION' is fetched */
		final List<Long> expectedAnnotationIds2 =
				Arrays.asList(annotation1, annotation2, annotation4).stream().map(AnnotationPojo :: getId).collect(Collectors.toList());
		final List<Long> actualAnnotationIds2 = 
				Arrays.asList(actualAnnotations2).stream().map(AnnotationPojo :: getId).collect(Collectors.toList());
		assertTrue(actualAnnotationIds2.containsAll(expectedAnnotationIds2), "actualAnnotationIds should contain id of all the Annotation");
		
		final Object filterObjectForReasons3 = gson.fromJson("{content_reasons : {eq : COBOL_EVALUATE_CONDITION}}", Object.class);
		final AnnotationPojo[] actualAnnotations3 =
				getEntries(executeQuery(QUERY_TEMPLATE, projectId3, Arrays.asList(Map.of("content_name", "ASC")), filterObjectForReasons3), 1, "annotations", AnnotationPojo.class);
		/* Test that only Annotations where meta data reason is 'COBOL_EVALUATE_CONDITION' is fetched */
		final List<Long> expectedAnnotationIds3 =
				Arrays.asList(annotation3).stream().map(AnnotationPojo :: getId).collect(Collectors.toList());
		final List<Long> actualAnnotationIds3 = 
				Arrays.asList(actualAnnotations3).stream().map(AnnotationPojo :: getId).collect(Collectors.toList());
		assertTrue(actualAnnotationIds3.containsAll(expectedAnnotationIds3), "actualAnnotationIds should contain id of Annotation3");
	}

	@DisplayName("Test for a project with Custom property filter and sort")
	@ParameterizedTest
	@MethodSource("provideFilterAndExpectedAnnotations")
	void testProjectWithCustomPropertyFilterAndSorting(int firstIndex, int secondIndex, SortDirection sortDirection) {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final Response resp = assertNotNull(tester).document(QUERY_TEMPLATE).variable("projectId", projectId.getNid())
				.variable("filterObject", Collections.singletonMap("content_customProperties_AnnotationCustomProperties5_testField2", Collections.singletonMap("eq", "PL")))
				.variable("sortObject", Collections.singletonMap("content_customProperties_AnnotationCustomProperties5_testField2", sortDirection)).execute();
		final AnnotationPojo[] actualAnnotations = getEntries(resp, 2, "annotations", AnnotationPojo.class);
		assertEquals(2, actualAnnotations.length);
		assertEquals(EXPECTED_ANNOTATIONS[firstIndex].getNid(), actualAnnotations[0].getId());
		assertEquals(EXPECTED_ANNOTATIONS[secondIndex].getNid(), actualAnnotations[1].getId());
	}

	private static Stream<Arguments> provideFilterAndExpectedAnnotations() {
		return Stream.of(
				Arguments.of(3, 2, SortDirection.ASCENDING),
				Arguments.of(2, 3, SortDirection.DESCENDING)
		);
	}

	private Stream<Arguments> parametersProvider() {
		return Stream.of(Arguments.of("{content_customProperties_AnnotationCustomProperties5_testField1 : {eq: 'red'}}", 1),
				Arguments.of("{content_customProperties_AnnotationCustomProperties5_testField2 : {eq: 'PL1'}}", 3),
				Arguments.of("{content_customProperties_AnnotationCustomProperties5_fieldName0 : {eq: 'natural'}}", 2),
				Arguments.of("{content_customProperties_AnnotationCustomProperties5_fieldName0 : {in: ['pl', 'natural']}}", 2));
	}


	private AnnotationPojo createTestAnnotation(final EntityId moduleId, final String annotationName) {
		/* Create a Reference using a dummy location for Annotations */
		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(Integer.valueOf(1));
		dummyLocation.setLength(Integer.valueOf(1));
		final AnnotationPojo annotation = createAnnotation(annotationName, AnnotationType.EXCLUDE, WorkingState.CANDIDATE,
				"content", moduleId, dummyLocation, "1", null);
		assertNotNull(annotation);
		return annotation;
	}

	private ProjectPojo createProject(final String name, final Long clientId) {
		return projectService.create(new ProjectPojoPrototype()
				.setName(name)
				.setClient(EntityId.of(clientId))
				.setNatures(Collections.singleton(ProjectNature.MINING)));
	}

	/* Helper method to retrieve DataDictionaryEntry list using the given index */
	private List<String> getDataDictionaryEntries(final Response response, final String path) {
		return response.path(path)
				.entityList(DataDictionaryPojo.class)
				.get()
				.stream()
				.map(DataDictionaryPojo::getName)
				.collect(Collectors.toList());
	}
	
	private static void verifyReceivedAnnotationData(final Response response, final String expectedJson) {
		try {
			response.path("").matchesJsonStrictly(expectedJson);
		} catch (final AssertionError e) {
			String readableMessage = e.getMessage();
			if (readableMessage.contains("If expected, please")) {
				readableMessage = readableMessage.replaceAll("([:,])", "$1\n");
				throw new AssertionError(readableMessage, e);
			}
			throw e;
		}
	}

	private AnnotationPojo createAnnotation(final String name, final AnnotationType annotationType, final WorkingState workingState,
			final String sourceAttachment, final EntityId moduleId, final ModuleLocation location, final String createdByUserId, @Nullable final Collection<String> reasons) {
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
//		annotation.setCategoryName("Business Rule");
		annotation.setName(name);
		annotation.setType(annotationType);
		annotation.setState(workingState);
		annotation.setSourceAttachment(sourceAttachment);
		annotation.setLocation(location);
		annotation.setCreatedByUserId(createdByUserId);
		annotation.setModule(moduleId);
		if (reasons != null) {
			annotation.setReasons(reasons);
		}
		return annotationService.get(annotationService.create(annotation));
	}

	private EntityId createTestModule(final String name, final EntityId projectId) {
		final ModulePojoPrototype module = new ModulePojoPrototype()
			.setProject(projectId)
			.setTechnology(Technology.COBOL)
			.setType(Type.PROGRAM)
			.setStorage(Storage.FILE)
			.setOrigin(Origin.CUSTOM)
			.setName(name + projectId.getNid())
			.setType(Type.PROGRAM)
			.setTechnology(Technology.COBOL)
			.setIdentification(Identification.IDENTIFIED)
			.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
	
	private CustomPropertyMetadata createCustomPropertyMetadata(final String name, final String label, final String description, final String dataType,
			final CustomPropertyFieldType customPropertyFieldType, final int customViewIndex, final boolean pluginVisible, @Nullable final String autoCompKey,
			final EntityId projectId, final String entityName, final String className) {
		final CustomPropertyMetadata customPropertyMetadata = new CustomPropertyMetadata();
		customPropertyMetadata.setName(name);
		customPropertyMetadata.setLabel(label);
		customPropertyMetadata.setDescription(description);
		customPropertyMetadata.setDataType(dataType);
		customPropertyMetadata.setFieldType(customPropertyFieldType);
		customPropertyMetadata.setCustomViewIndex(customViewIndex);
		customPropertyMetadata.setPluginVisible(pluginVisible);
		customPropertyMetadata.setAutoCompletionKey(autoCompKey);
		customPropertiesService.defineProperty(projectId, entityName, customPropertyMetadata.getName(), customPropertyMetadata);
		
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.ofParent(null).withName(className)), className + " should be existing in DB");
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.withParent(className).withName(customPropertyMetadata.getName())),
				customPropertyMetadata.getName() + " should exist in " + className);
		
		return customPropertyMetadata;
	}
	
	private DataDictionaryPojo createDataDictionaryEntry(final EntityId moduleId, final String dataDictionaryEntryName,
			final Boolean isBusiness, final ModuleLocation modulelocation) {
		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype()
				.setName(dataDictionaryEntryName)
				.setDescription("Data Dictionary Entry")
				.setModule(moduleId)
				.setLocation(new ModuleLocation(47, 11))
				.setCreatedByUserId("admin")
				.setIsBusiness(isBusiness)
				.setLocation(modulelocation)
				.setScopes(Map.of(DataDictionaryVariableScope.FILE, Map.of(), DataDictionaryVariableScope.SQL_DATABASE, Map.of()));
		return dataDictionaryService.create(dataDictionaryEntry);
	}

	private void createFunctionalBlock() {
		final UUID functionalblockUID3 = createFunctionalBlockPojoPrototype(projectId, null, "Test Unit 1", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)));

		final UUID  functionalblockUID4 = createFunctionalBlockPojoPrototype(projectId, null, "Test Unit 2", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)));

		final UUID  functionalblockUID5 = createFunctionalBlockPojoPrototype(projectId, null, "Test Unit 3", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)));
		final UUID  functionalblockUID6 = createFunctionalBlockPojoPrototype(projectId, null, "Test Unit 3", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)));
		final List<UUID> childUIDs = new ArrayList<>();
		childUIDs.add(functionalblockUID3);
		childUIDs.add(functionalblockUID4);

		final UUID  functionalblockUID1 = createFunctionalBlockPojoPrototype(projectId, childUIDs, "Test Group 1", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)));
		final List<UUID> parentUID = new ArrayList<>();
		parentUID.add(functionalblockUID1);

		createFunctionalBlockPojoPrototype(projectId, Arrays.asList(functionalblockUID5),
				"Test Group 2", "", Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)));
		createFunctionalBlockPojoPrototype(projectId, Arrays.asList(functionalblockUID6),
				"check Group 2", "", Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)));

		functionalBlockService.setGeneratedFrom(functionalblockUID3, GeneratedFrom.fromAnnotation(EXPECTED_ANNOTATIONS[0]));
		functionalBlockService.setGeneratedFrom(functionalblockUID4, GeneratedFrom.fromAnnotation(EXPECTED_ANNOTATIONS[1]));
		functionalBlockService.setGeneratedFrom(functionalblockUID5, GeneratedFrom.fromAnnotation(EXPECTED_ANNOTATIONS[2]));
		functionalBlockService.setGeneratedFrom(functionalblockUID6, GeneratedFrom.fromAnnotation(EXPECTED_ANNOTATIONS[3]));

	}

	private UUID createFunctionalBlockPojoPrototype(
			 final EntityId project,
			 final @Nullable List<UUID> children,
			 final String name,
			 final String description,
			 final @Nullable Map<String, Object> flags) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype= new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototype.setProject(project);
		functionalBlockPojoPrototype.setName(name);
		functionalBlockPojoPrototype.setDescription(description);

		if(children != null) {
			functionalBlockPojoPrototype.setChildren(children);
		}
		if (flags != null) {
			functionalBlockPojoPrototype.setFlags(flags);
		}
		return functionalBlockService.create(functionalBlockPojoPrototype);
	}
}
