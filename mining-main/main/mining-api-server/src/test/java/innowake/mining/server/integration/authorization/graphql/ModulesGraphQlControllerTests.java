/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.authorization.graphql;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.apache.commons.lang.math.NumberUtils.LONG_ONE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.BDDMockito.given;

import java.io.IOException;
import java.lang.reflect.Array;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.mining.data.core.storeast.DefaultStoreAstExecutor;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.entities.ErrorMarkerPojo;
import innowake.mining.shared.model.*;
import innowake.mining.shared.model.discovery.Severity;
import innowake.mining.shared.model.job.NullParsingSummarizer;
import org.apache.commons.compress.utils.Sets;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.internal.verification.Calls;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.graphql.test.tester.GraphQlTester.Response;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.graphql.test.tester.WebGraphQlTester;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.test.web.servlet.client.MockMvcWebTestClient;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.context.WebApplicationContext;

import com.google.gson.Gson;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.config.security.MiningRole;
import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.functionalblocks.service.FunctionalBlockServiceImpl;
import innowake.mining.server.graphql.controller.ControllerLocalContext;
import innowake.mining.server.graphql.controller.ModulesGraphQlController;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.MiningPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.service.UserRoleService;
import innowake.mining.tags.AuthorizationTest;

/**
 * Tests for the schema mapping methods of the {@link ModulesGraphQlController}.
 * <p>Tests that GraphQL queries can reach other projects only through the GraphQL API if the user has access to them.</p>
 */
@ActiveProfiles(value= Profiles.AUTH_TEST, inheritProfiles = false)
@AuthorizationTest
class ModulesGraphQlControllerTests extends DatabaseResettingTest {

	private static final Map<String, Long> ANNOTATION_CATEGORY_MAP = new HashMap<>();

	private EntityId projectId = EntityId.VOID;
	private static final String ENTITY_NAME = "Module";
	protected static final String RESOURCE_PATH = "/test-resources/innowake/mining/server/job/identification/";
	private static final String QUERY_TEMPLATE = "query ($projectId: Long!, $type: RelationshipType, $filterObject: FilterObject_modules) {" +
										     "modules(projectId: $projectId, filterObject: $filterObject) {" +
										       "totalElements," +
										       "content {" +
										         "id," +
										         "path," +
										         "out: dependencies(direction: OUT, type: [ $type ]) {" +
										           "type, module {" +
										             "id," +
										             "projectId" +
										           "}" +
										         "}," +
										         "in: dependencies(direction: IN, type: [ $type ]) {" +
									               "type, module {" +
									                 "id," +
									                 "projectId" +
									               "}" +
									             "}," +
												" reachabilityBlocks { " +
													"name" +
												"}" +
										       "}" +
										     "}" +
										   "}";
	
	private static final String QUERY_TEMPLATE_2 = "query ($projectId: Long!) {" +
										      "modules(projectId: $projectId) {" +
										       "totalElements," +
										       "content {" +
										         "id," +
										         "path," +
										         "inCodebase," +
										         "inboundCallsCount," +
										         "dependencies(direction: IN, type: CONTAINS) {" +
									               "module {" +
									                 "id," +
									                 "content" +
									               "}" +
									             "}" +
										       "}" +
										     "}" +
										   "}";
	
	private static final String QUERY_TEMPLATE_3 = "query ($projectId: Long!, $filterObject: FilterObject_modules) {" +
												"modules(projectId: $projectId, filterObject: $filterObject) {" +
												"totalElements," +
												"content {" +
										         "id, " +
										         "inboundCallsCount," +
										         "dependencies(direction: IN, type: CALLS) {" +
									               "module {" +
									                 "id," +
									                 "content" +
									               "}" +
									             "}" +
										       "}" +
										     "}" +
										   "}";

	private static final String ID_QUERY_TEMPLATE = "query($projectId: Long!, $filterObject: FilterObject_modules) { " +
													"modules(projectId: $projectId, filterObject: $filterObject) {" +
														"content { id name %s }" +
													"}" +
												"}";

	private static final String QUERY_TEMPLATE_SORT_FILTER = "query ($projectId: Long!, $filterObject: FilterObject_modules, $sortObject: [SortObject_modules]) {" +
												"modules(projectId: $projectId, filterObject: $filterObject, sortObject: $sortObject) {" +
													"totalElements," +
													"content {" +
														"id," +
														"name" +
													"}" +
												"}" +
											"}";

	private static final String QUERY_TEMPLATE_SORT_FILTER_OBJECT = 
			"query ($projectId: Long!, $sortObject: [SortObject_modules], $filterObject: FilterObject_modules) {" +
												"modules(projectId: $projectId, sortObject: $sortObject, filterObject: $filterObject) {" +
													"totalElements," +
													"content {" +
														"id," +
														"name," +
														"description" +
													"}" +
												"}" +
											"}";

	private static final String QUERY_FOR_ANNOTATIONS_COUNT = "query ($projectId: Long!, $filterObject: FilterObject_modules, "
			+ "$sortObject: [SortObject_modules]) {" +
										"modules(projectId: $projectId, sortObject: $sortObject, filterObject: $filterObject) {"
												+"content {"
													+ "name, "
													+ "annotationCount,"
													+ "numberOfBusinessRules,"
													+ "numberOfValidationRules,"
													+ "numberOfTechnicalRules,"
													+ "numberOfFieldComputationRules,"
													+ "numberOfErrorProcessingRules,"
													+ "numberOfDeclareDatabaseOperation,"
													+ "numberOfCloseDatabaseOperation,"
													+ "numberOfReadDatabaseOperation,"
													+ "numberOfWriteDatabaseOperation,"
													+ "numberOfAnnotatedStatementNodes,"
													+ "id"
											+ " } "
											+ " } "
										+ "}";

	private static final String QUERY_FOR_ANNOTATIONS_COVERAGE = "query ($projectId: Long!, $filterObject: FilterObject_modules, "
			+ "$sortObject: [SortObject_modules]) {" +
			"modules(projectId: $projectId, sortObject: $sortObject, filterObject: $filterObject) {"
			+"content {"
			+ "numberOfAnnotatedStatementNodes"
			+ " } "
			+ " } "
			+ "}";

	private static final String ERROR_QUERY_TEMPLATE = "query($projectId: Long!, $filterObject: FilterObject_modules) { " +
														"modules(projectId: $projectId, filterObject: $filterObject, sortObject: { content_id: ASC }) {" +
														"totalElements," +
														"content {" +
															"name, " +
															"errorCount," +
														"}" +
														"}" +
													"}";
	private static final String ERROR_MARKER_QUERY_TEMPLATE = "query($projectId : Long!, $filterObject: FilterObject_errorMarkers) {" +
														"errorMarkers(projectId: $projectId, filterObject: $filterObject) {" +
															"totalElements," +
															"content {" +
																"severity, " +
																"errorText ," +
																"cause " +
															"}" +
														"}" +
													"}";

	private static final String MOD5_CONTENT = "       IDENTIFICATION DIVISION.\r\n"
			+ "       PROGRAM-ID. MAINPROGRAM.\r\n"
			+ "\r\n"
			+ "       DATA DIVISION.\r\n"
			+ "       WORKING-STORAGE SECTION.\r\n"
			+ "           COPY CPY.\r\n"
			+ "           01 TEST2 PIC X(10) VALUE 'Hello'.\r\n"
			+ "\r\n"
			+ "       PROCEDURE DIVISION.\r\n"
			+ "           DISPLAY TEST1.\r\n"
			+ "           DISPLAY TEST2.\r\n"
			+ "\r\n"
			+ "           STOP RUN.\r\n"
			+ "";

	@Nullable
	@MockBean
	private UserRoleService userRoleService;
	@MockBean
	@Nullable
	RestTemplate oauthRestTemplate;
	@Nullable
	private WebGraphQlTester tester;
	@Autowired
	private WebApplicationContext webAppContext;
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private TaxonomyService taxonomyService;
	@Autowired
	private FunctionalBlockServiceImpl blockServiceImpl;
	@Autowired
	private MiningDataCoreService core;
	
	@BeforeAll
	void init() {
		final WebTestClient client = MockMvcWebTestClient.bindToApplicationContext(webAppContext)
				.configureClient()
				.baseUrl(GraphQlAuthorizationTests.GRAPHQL_ENDPOINT + "?projectId=5") /* that's the custom project we create */
				.build();

		tester = HttpGraphQlTester.create(client);
	}

	/**
	 * Tests that the GraphQL query returns all expected modules 
	 * The Module with no source attachment but a Containing Module that has source has inCodebase = true 
	 * The Module with no source attachment and no Containing Module with source has inCodebase = false
	 *
	 */
	@DisplayName("Test inCodebase data point for Virtual Module")
	@Test
	void testInCodeBaseModules() {
		projectId = createProject("Test_Project");
		
		/* Create a Module with Content */
		final String name = "Module1_" + projectId.getNid();
		final var module1 = createTestModule(projectId, name, "programs/" + name + ".cbl", "Test Data content", Technology.COBOL, Type.PROGRAM, null);
		
		/* Create a Module with No Content with Module 1 as the parent */
		final var module2 = createTestModule(projectId, "Module2_" + projectId.getNid(), null, null, Technology.COBOL, Type.PROGRAM, null, module1, Identification.IDENTIFIED);
		
		/* Create a Module with No Content */
		final var module3 = createTestModule(projectId, "Module3_" + projectId.getNid(), null, null, Technology.COBOL, Type.PROGRAM, null);
		
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);
		
		final Response response = executeQueryBasedOnTemplate(projectId, QUERY_TEMPLATE_2, null);
		response.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(3));

		/* The order of the modules can differ. Find the correct index of the modules in the response */
		final Long id0 = response.path("modules.content[0].id").entity(Long.class).get();
		final Long id1 = response.path("modules.content[1].id").entity(Long.class).get();
		final Long id2 = response.path("modules.content[2].id").entity(Long.class).get();
		final Set<Long> returnedIds = Sets.newHashSet(id0, id1, id2);
		assertTrue(returnedIds.contains(module1.getNid()), "Id of module 1 must have be present");
		assertTrue(returnedIds.contains(module2.getNid()), "Id of module 2 must have be present");
		assertTrue(returnedIds.contains(module3.getNid()), "Id of module 3 must have be present");
		final int indexModule1 = id0.equals(module1.getNid()) ? 0 : (id1.equals(module1.getNid()) ? 1 : 2);
		final int indexModule2 = id0.equals(module2.getNid()) ? 0 : (id1.equals(module2.getNid()) ? 1 : 2);
		final int indexModule3 = id0.equals(module3.getNid()) ? 0 : (id1.equals(module3.getNid()) ? 1 : 2);

		/* Check that Module with source attachment has inCodebase = true */
		assertTrue(response.path("modules.content[" + indexModule1 + "].inCodebase").entity(Boolean.class).get(),
				"The inCodebase of Module 1 must be true as it has a source by itself");

		/* Check that Module with no source attachment but a Containing Module that has source has inCodebase = true */
		assertTrue(response.path("modules.content[" + indexModule2 + "].inCodebase").entity(Boolean.class).get(),
				"The inCodebase of Module 2 must be true as it has no source itself but its containing module has a source ");
		assertEquals(module1.getNid(), response.path("modules.content[" + indexModule2 + "].dependencies[0].module.id").entity(Long.class).get());
		assertNotNull(response.path("modules.content[" + indexModule2 + "].dependencies[0].module.content").entity(String.class).get());
		
		/* Check that Module with no source attachment and no Containing Module with source has inCodebase = false */
		assertFalse(response.path("modules.content[" + indexModule3 + "].inCodebase").entity(Boolean.class).get(),
				"The inCodebase of Module 3 must be false as it had no source and no Containing Module");
	}
	
	/**
	 * Tests that the GraphQL query returns all expected modules 
	 * The Module with inSize=0 has empty inCalls
	 * The Module with inSize=1 has non-empty inCalls
	 *
	 */
	@DisplayName("Test inSizeRSQLOperator for inSize=0 and inSize=1")
	@Test
	void testInSizeRsqlOperator() {
		projectId = createProject("Test_Project");
		
		/* Create a Module with Content */
		final String name = "ModuleA_" + projectId.getNid();
		final var moduleA = createTestModule(projectId, name, "programs/" + name + ".cbl", "Test Data content", Technology.COBOL, Type.PROGRAM, null);
		
		/* Create a Module with No Content */
		final var moduleB = createTestModule(projectId, "ModuleB_" + projectId.getNid(), null, null, Technology.COBOL, Type.PROGRAM, null);
		
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);
		
		assertNotNull(moduleA);
		assertNotNull(moduleB);
		
		/* Create a Reference CALLS between Module A and Module B */
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(moduleA)
				.setDstModule(moduleB));
		
		final Response responseA = executeQueryBasedOnTemplate(projectId, QUERY_TEMPLATE_3, 
				Collections.singletonMap("content_inboundCallsCount", Collections.singletonMap("eq", 0)));
		responseA.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(1));
		responseA.path("modules.content[0].dependencies").entityList(Object.class).hasSize(0);
		
		final Response responseB = executeQueryBasedOnTemplate(projectId, QUERY_TEMPLATE_3,
				Collections.singletonMap("content_inboundCallsCount", Collections.singletonMap("eq", 1)));
		responseB.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(1));
		responseB.path("modules.content[0].dependencies").entityList(Object.class).hasSizeGreaterThan(0);
	}
	
	@DisplayName("Test path for given module")
	@Test
	void testPath() {
		projectId = createProject("Test_Project");
		
		/* Create a Module with Content */
		final String name = "ModuleA_" + projectId.getNid();
		final String moduleApath="programs/" + name + ".cbl";
		final var moduleA = createTestModule(projectId, name, moduleApath, "Test Data content", Technology.COBOL, Type.PROGRAM, null);
		final var moduleB = createTestModule(projectId, "ModuleB_" + projectId.getNid(), null, null, Technology.COBOL, Type.PROGRAM, null, moduleA, Identification.IDENTIFIED);
		
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);

		assertNotNull(moduleA);
		assertNotNull(moduleB);

		final Response responseA = executeQueryBasedOnTemplate(projectId, QUERY_TEMPLATE_2, 
				Collections.singletonMap("content_inboundCallsCount", Collections.singletonMap("eq", 0)));

		final Long id0 = responseA.path("modules.content[0].id").entity(Long.class).get();
		final Long id1 = responseA.path("modules.content[1].id").entity(Long.class).get();
		final int indexModuleA = id0.equals(moduleA.getNid()) ? 0 : (id1.equals(moduleA.getNid()) ? 1 : 2);
		final int indexModuleB = id0.equals(moduleB.getNid()) ? 0 : (id1.equals(moduleB.getNid()) ? 1 : 2);
		
		assertEquals(moduleApath, responseA.path("modules.content[" + indexModuleA + "].path").entity(String.class).get(),
				"The path should be ModuleA's path");
		assertEquals(moduleApath, responseA.path("modules.content[" + indexModuleB + "].path").entity(String.class).get(),
				"The path of ModuleB should be path of ModuleA");
	}
	
	@DisplayName("Test Description for given module")
	@Test
	void testDescription() {
		projectId = createProject("Test_Project");
		
		/* Create a Module with Content */
		final String name = "ModuleX_" + projectId.getNid();
		final String moduleApath="programs/" + name + ".cbl";
		final EntityId moduleX = createTestModule(projectId, name, moduleApath, "Test Data content", Technology.COBOL, Type.PROGRAM, null);
		
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);
		moduleService.update(new ModulePojoPrototype()
				.withId(moduleX)
				.setDescription("updated Description"));
		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_SORT_FILTER_OBJECT)
				.variable("projectId", projectId.getNid())
				.variable("sortObject", Map.of("content_name", "ASC"))
				.execute();
		
		response.errors().verify();

		assertEquals(moduleX.getNid(), response.path("modules.content[0].id").entity(Long.class).get());
		assertEquals("updated Description", response.path("modules.content[0].description").entity(String.class).get());

	}
	
	@DisplayName("Test annotations for module")
	@Test
	void testAnnotation() {
		projectId = createProject("Annotation_Project");
		if (ANNOTATION_CATEGORY_MAP.isEmpty()) {
			loadRuleAnnotationCategories(ANNOTATION_CATEGORY_MAP);
		}
		createModuleWithAnnotation(projectId);
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);
		final Response responseA = assertNotNull(tester).document(QUERY_FOR_ANNOTATIONS_COUNT)
				.variable("projectId", projectId.getNid())
				.execute();

		assertEquals(13, responseA.path("modules.content[0].annotationCount").entity(Long.class).get(),
				"number of numberOfBusinessRules should be as assigned");
		assertEquals(1, responseA.path("modules.content[0].numberOfBusinessRules").entity(Long.class).get(),
				"number of numberOfBusinessRules should be as assigned");
		assertEquals(1, responseA.path("modules.content[0].numberOfValidationRules").entity(Long.class).get(),
				"number of numberOfValidationRules should be as assigned");
		assertEquals(1, responseA.path("modules.content[0].numberOfTechnicalRules").entity(Long.class).get(),
				"number of numberOfTechnicalRules should be as assigned");
		assertEquals(1, responseA.path("modules.content[0].numberOfFieldComputationRules").entity(Long.class).get(),
				"number of numberOfFieldComputationRules should be as assigned");
		assertEquals(1, responseA.path("modules.content[0].numberOfErrorProcessingRules").entity(Long.class).get(),
				"number of numberOfErrorProcessingRules should be as assigned");
		assertEquals(2, responseA.path("modules.content[0].numberOfDeclareDatabaseOperation").entity(Long.class).get(),
				"number of numberOfDeclareDatabaseOperation should be as assigned");
		assertEquals(1, responseA.path("modules.content[0].numberOfWriteDatabaseOperation").entity(Long.class).get(),
				"number of numberOfWriteDatabaseOperation should be as assigned");
		assertEquals(1, responseA.path("modules.content[0].numberOfReadDatabaseOperation").entity(Long.class).get(),
				"number of numberOfReadDatabaseOperation should be as assigned");
		assertEquals(4, responseA.path("modules.content[0].numberOfCloseDatabaseOperation").entity(Long.class).get(),
				"number of numberOfCloseDatabaseOperation should be as assigned");
		assertEquals(0.5, responseA.path("modules.content[0].numberOfAnnotatedStatementNodes").entity(Double.class).get(),
				"number of numberOfCloseDatabaseOperation should be as assigned");
	}

	@DisplayName("Test annotation coverage for module")
	@Test
	void testAnnotationCoverage() {
		projectId = createProject("Annotation_Coverage_Project");
		if (ANNOTATION_CATEGORY_MAP.isEmpty()) {
			loadRuleAnnotationCategories(ANNOTATION_CATEGORY_MAP);
		}

		final String name = "Module" + projectId.getNid();
		final String moduleApath = "programs/" + name + ".cbl";
		final var moduleA = createTestModule(projectId, name, moduleApath, MOD5_CONTENT, Technology.COBOL, Type.PROGRAM, null);

		final EntityId anno1 = createAnnotation("Annotation 12", AnnotationType.DATABASE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.DatabaseAnnotationCategory.CLOSE.getName()), new ModuleLocation(233, 13));

		createAnnotation("Annotation 12", AnnotationType.DATABASE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.DatabaseAnnotationCategory.CLOSE.getName()), new ModuleLocation(260, 13));

		new DefaultStoreAstExecutor(NullParsingSummarizer.INSTANCE).execute(moduleA, core);

		setupProjectAccesses(singletonList(projectId), singletonList(projectId), Collections.emptyList(), false);
		final Response responseA = assertNotNull(tester).document(QUERY_FOR_ANNOTATIONS_COVERAGE)
				.variable("projectId", projectId.getNid())
				.execute();

		assertEquals(0.5, responseA.path("modules.content[0].numberOfAnnotatedStatementNodes").entity(Double.class).get(),
				"number of numberOfCloseDatabaseOperation should be as assigned");

		/* Delete one annotation -> coverage should go down to 0.25 */
		annotationService.delete(projectId, anno1);

		final Response responseB = assertNotNull(tester).document(QUERY_FOR_ANNOTATIONS_COVERAGE)
				.variable("projectId", projectId.getNid())
				.execute();

		assertEquals(0.25, responseB.path("modules.content[0].numberOfAnnotatedStatementNodes").entity(Double.class).get());
	}

	private void testNumberOfXAnnotationDataPointsSortingAndFiltering(final AnnotationType type, final String category, final String sort, final String filter) {
		projectId = createProject("Annotation_Project");
		if (ANNOTATION_CATEGORY_MAP.isEmpty()) {
			loadRuleAnnotationCategories(ANNOTATION_CATEGORY_MAP);
		}
		createModuleWithAnnotation("Test Module1" + category, projectId, type, category, 1);
		createModuleWithAnnotation("Test Module2" + category, projectId, type, category, 2);
		createModuleWithAnnotation("Test Module3" + category, projectId, type, category, 3);
		String aliasName;
		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, String> sortObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();
		filterOperator.put(filter, "2");
		if (category.equals(AnnotationCategory.RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName())) {
			aliasName = "numberOfFieldComputationRules";
			filterObject.put("content_numberOfFieldComputationRules", filterOperator);
		} else if (type == AnnotationType.RULE) {
			aliasName = "numberOf" + category.replace(" ", "") + "s";
			filterObject.put("content_" + aliasName, filterOperator);
		} else {
			aliasName = "numberOf" + category + "DatabaseOperation";
			filterObject.put("content_" + aliasName, filterOperator);
		}
		sortObject.put("content_" + aliasName, sort);
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);
		final Response response = assertNotNull(tester).document(QUERY_FOR_ANNOTATIONS_COUNT)
				.variable("projectId", projectId.getNid())
				.variable("sortObject", new Object[] { sortObject })
				.variable("filterObject", filterObject)
				.execute();

		if (sort.equals("DESC") && filter.equals("gte")) {
			assertEquals(3L, response.path("modules.content[0]." + aliasName).entity(Long.class).get(),
					"number of " + category + " should be as assigned");
			assertEquals(2L, response.path("modules.content[1]." + aliasName).entity(Long.class).get(),
					"number of " + category + " should be as assigned");
		} else if (sort.equals("ASC") && filter.equals("lte")) {
			assertEquals(1L, response.path("modules.content[0]." + aliasName).entity(Long.class).get(),
					"number of " + category + " should be as assigned");
			assertEquals(2L, response.path("modules.content[1]." + aliasName).entity(Long.class).get(),
					"number of " + category + " should be as assigned");
		} else {
			assertEquals(2L, response.path("modules.content[0]." + aliasName).entity(Long.class).get(),
					"number of " + category + " should be as assigned");
		}
	}
	
	@Test void testQueryModuleAnnotations0001() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.BUSINESS_RULE.getName(), "DESC", "gte"); }
	@Test void testQueryModuleAnnotations0002() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.BUSINESS_RULE.getName(), "ASC", "lte"); }
	@Test void testQueryModuleAnnotations0003() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.BUSINESS_RULE.getName(), "ASC", "eq"); }
	@Test void testQueryModuleAnnotations0004() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.TECHNICAL_RULE.getName(), "DESC", "gte"); }
	@Test void testQueryModuleAnnotations0005() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.TECHNICAL_RULE.getName(), "ASC", "lte"); }
	@Test void testQueryModuleAnnotations0006() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.TECHNICAL_RULE.getName(), "ASC", "eq"); }
	@Test void testQueryModuleAnnotations0007() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.VALIDATION_RULE.getName(), "DESC", "gte"); }
	@Test void testQueryModuleAnnotations0008() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.VALIDATION_RULE.getName(), "ASC", "lte"); }
	@Test void testQueryModuleAnnotations0009() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.VALIDATION_RULE.getName(), "ASC", "eq"); }
	@Test void testQueryModuleAnnotations0010() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.ERROR_PROCESSING_RULE.getName(), "DESC", "gte"); }
	@Test void testQueryModuleAnnotations0011() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.ERROR_PROCESSING_RULE.getName(), "ASC", "lte"); }
	@Test void testQueryModuleAnnotations0012() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.ERROR_PROCESSING_RULE.getName(), "ASC", "eq"); }
	@Test void testQueryModuleAnnotations0013() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName(), "DESC", "gte"); }
	@Test void testQueryModuleAnnotations0014() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName(), "ASC", "lte"); }
	@Test void testQueryModuleAnnotations0015() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.RULE, AnnotationCategory.RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName(), "ASC", "eq"); }
	@Test void testQueryModuleAnnotations0016() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.DATABASE, AnnotationCategory.DatabaseAnnotationCategory.CLOSE.getName(), "DESC", "gte"); }
	@Test void testQueryModuleAnnotations0017() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.DATABASE, AnnotationCategory.DatabaseAnnotationCategory.CLOSE.getName(), "ASC", "lte"); }
	@Test void testQueryModuleAnnotations0018() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.DATABASE, AnnotationCategory.DatabaseAnnotationCategory.CLOSE.getName(), "ASC", "eq"); }
	@Test void testQueryModuleAnnotations0019() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.DATABASE, AnnotationCategory.DatabaseAnnotationCategory.DECLARE.getName(), "DESC", "gte"); }
	@Test void testQueryModuleAnnotations0020() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.DATABASE, AnnotationCategory.DatabaseAnnotationCategory.DECLARE.getName(), "ASC", "lte"); }
	@Test void testQueryModuleAnnotations0021() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.DATABASE, AnnotationCategory.DatabaseAnnotationCategory.DECLARE.getName(), "ASC", "eq"); }
	@Test void testQueryModuleAnnotations0022() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.DATABASE, AnnotationCategory.DatabaseAnnotationCategory.READ.getName(), "DESC", "gte"); }
	@Test void testQueryModuleAnnotations0023() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.DATABASE, AnnotationCategory.DatabaseAnnotationCategory.READ.getName(), "ASC", "lte"); }
	@Test void testQueryModuleAnnotations0024() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.DATABASE, AnnotationCategory.DatabaseAnnotationCategory.READ.getName(), "ASC", "eq"); }
	@Test void testQueryModuleAnnotations0025() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.DATABASE, AnnotationCategory.DatabaseAnnotationCategory.WRITE.getName(), "DESC", "gte"); }
	@Test void testQueryModuleAnnotations0026() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.DATABASE, AnnotationCategory.DatabaseAnnotationCategory.WRITE.getName(), "ASC", "lte"); }
	@Test void testQueryModuleAnnotations0027() { testNumberOfXAnnotationDataPointsSortingAndFiltering(AnnotationType.DATABASE, AnnotationCategory.DatabaseAnnotationCategory.WRITE.getName(), "ASC", "eq"); }
	
	private List<ModulePojo> getModules(final EntityId projectId, final EntityId moduleId) {
		return moduleService.findModules(q -> q.ofProject(projectId).byId(moduleId));
	}
	
	/**
	 * Tests that the GraphQL query returns all expected modules including the incoming and outgoing references between them.
	 * @param relType {@code CALLS} for references of type {@link Calls}, {@code INCLUDES} for for references of type {@link Includes}, {@code READSWRITES} for
	 * references of type {@link ReadsWrites} or {@code REFERENCES} for references of type {@link References}
	 * @param withAccess If the user has full access to the created test project.
	 */
	private void testSingleProject(final RelationshipType relType, final boolean withAccess) {
		/* Create test data in DB as following:
		 * Project_1 (Project for client with id=1)
		 *    |
		 *    |--- Module_1_${project1Id}  --- out --- ${refName}
		 *    |                                           |  |
		 *    |--- Module_2_${project1Id} <--- in --------+  |
		 *    |                                              |
		 *    |--- Module_3_${project1Id} <--- in -----------+
		 *    |
		 */

		final EntityId[] projectModules1 = createProjectWithModules("Project_1");
		createRelationship(relType, projectModules1, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] project1Modules = new ModulePojo[projectModules1.length];
		for(int i = 0; i < projectModules1.length; i++) {
			final List<ModulePojo> moduleList = getModules(project1Id, projectModules1[i]);
			assertEquals(1, moduleList.size());
			project1Modules[i] = moduleList.get(0);
		}
		
		if (withAccess) {
			/* Set full user accesses to project */
			setupProjectAccesses(Arrays.asList(project1Id), Arrays.asList(project1Id), Collections.emptyList(), false);
		} else {
			/* Set authentication accesses to project but not in UserRoleService to test that GraphQL will not return references for its modules */
			setupProjectAccesses(Arrays.asList(project1Id), Arrays.asList(EntityId.of(0l)), Collections.emptyList(), false);
		}

		/* Execute GraphQL request and check response data */
		final var response = executeQuery(relType, project1Id);
		response.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(project1Modules.length));
		/* test Module_1 is present and that it has no incoming references and two outgoing references to Module_2 and Module_3 */
		assertRefs(response, 0, project1Id, 0, 2, relType);
		/* test Module_2 is present and that it has one incoming reference from Module_1 and no outgoing references */
		assertRefs(response, 1, project1Id, 1, 0, relType);
		/* test Module_3 is present and that it has one incoming reference from Module_1 and no outgoing references */
		assertRefs(response, 2, project1Id, 1, 0, relType);
		/* test Module_4 is present and that it has no incoming or outgoing references */
		assertRefs(response, 3, project1Id, 0, 0, relType);
	}
	
	@Test void testSingleProjectCalls() { testSingleProject(RelationshipType.CALLS, true); }
	@Test void testSingleProjectIncludes() { testSingleProject(RelationshipType.INCLUDES, true); }
	@Test void testSingleProjectReferences() { testSingleProject(RelationshipType.REFERENCES, true); }
	@Test void testSingleProjectAccesses() { testSingleProject(RelationshipType.ACCESSES, true); }
	
	@Test void testSingleProjectNoAccessCalls() { testSingleProject(RelationshipType.CALLS, false); }
	@Test void testSingleProjectNoAccessIncludes() { testSingleProject(RelationshipType.INCLUDES, false); }
	@Test void testSingleProjectNoAccessReferences() { testSingleProject(RelationshipType.REFERENCES, false); }
	@Test void testSingleProjectNoAccessAccesses() { testSingleProject(RelationshipType.ACCESSES, false); }
	
	@Test
	void testQueryIdsOnly() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}

		projectId = modules[0].getProject();
		final Set<Long> moduleIds = Arrays.stream(modules).map(ModulePojo::getId).collect(Collectors.toSet());

		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);

		final Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.execute();

		response.errors().verify();

		assertEquals(moduleIds, response.path("modules.content").entityList(ModulePojo.class).get().stream().map(ModulePojo::getId).collect(Collectors.toSet()));
	}

	@Test
	void testQueryIdsOnlyWithFilter() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		projectId = assertNotNull(assertNotNull(modules[0].getProject()));
		final List<Long> moduleIds = Arrays.stream(modules)
				.filter(module -> ("Module_1_" + projectId.getNid()).equals(module.getName())).map(ModulePojo::getId)
				.collect(Collectors.toList());

		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);

		final Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", Collections.singletonMap("content_name", Collections.singletonMap("eq", "Module_1_" + projectId.getNid())))
				.execute();

		response.errors().verify();

		assertEquals(moduleIds, response.path("modules.content[*].id").entityList(Long.class).get());
	}

	@Test
	void testQueryIdsOnlyWithFilterObject() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		projectId = modules[0].getProject();
		final Set<Long> moduleIds = Arrays.stream(modules).filter(module -> ("Module_1_" + projectId.getNid()).equals(module.getName())).map(ModulePojo::getId)
				.collect(Collectors.toSet());

		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);

		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();

		filterOperator.put("eq", "Module_1_" + projectId.getNid());
		filterObject.put("content_name", filterOperator);

		final Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();

		assertEquals(moduleIds, response.path("modules.content").entityList(ModulePojo.class).get().stream().map(ModulePojo::getId).collect(Collectors.toSet()));
	}
	
	/**
	 * Tests that the GraphQL query returns single matching modules by String Type Custom property filter.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test querying for single matching modules by string type custom property with eq filter")
	@Test
	void testSingleMatchingModulesWithStringTypeCustomPropertyFilter() {
		final EntityId[] expectedModules = createTestData();
		
		/* Execute GraphQL request for Module with String type Custom Property and get response data */
		assertModuleWithCustomProperty(Collections.singletonMap("content_customProperties_ModuleCustomProperties5_StringType",
				Collections.singletonMap("eq", "PL1")), expectedModules[2]);
	}
	
	/**
	 * Tests that the GraphQL query returns single matching modules by String Repeater Custom property with eq filter.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test querying for single matching modules by string repeater custom property with eq filter")
	@Test
	void testSingleMatchingModulesWithStringRepeaterCustomPropertyFilter() {
		final EntityId[] expectedModules = createTestData();

		/* Test for Module with String Repeater type Custom Property*/
		assertModuleWithCustomProperty(Collections.singletonMap("content_customProperties_ModuleCustomProperties5_StringRepeaterType",
				Collections.singletonMap("eq", "black")), expectedModules[5]);
	}
	
	/**
	 * Tests that the GraphQL query returns single matching modules by Tag Type Custom property with eq filter.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test querying for single matching modules by tag type custom property with eq filter")
	@Test
	void testSingleMatchingModulesWithTagTypeEqCustomPropertyFilter() {
		final EntityId[] expectedModules = createTestData();
		
		/* Test for Module with TAG type Custom Property and get response data */
		assertModuleWithCustomProperty(Collections.singletonMap("content_customProperties_ModuleCustomProperties5_TagType",
				Collections.singletonMap("eq","java")), expectedModules[7]);
	}
	
	/**
	 * Tests that the GraphQL query returns single matching modules by tag type Custom property with in filter.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test querying for single matching modules by in tag type custom property with in filter")
	@Test
	void testSingleMatchingModulesWithTagTypeInCustomPropertyFilter() {
		final EntityId[] expectedModules = createTestData();
		
		/* Test for Module with TAG type Custom Property and get response data */
		assertModuleWithCustomProperty(Collections.singletonMap("content_customProperties_ModuleCustomProperties5_TagType",
				Collections.singletonMap("in", Arrays.asList("pl", "natural"))), expectedModules[1]);
	}
	
	/**
	 * Tests that the GraphQL query returns no matching Module by Custom property.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test querying for no matching modules by custom properties")
	@Test
	void testNoneMatchingModulesWithCustomPropertyFilter() {
		createTestData();
		/* Execute GraphQL request for Module with String Repeater type Custom Property*/
		final ModulePojo[] actualModulesWithStringRepeaterFilter = getEntries(executeQueryBasedOnTemplate(projectId, QUERY_TEMPLATE_SORT_FILTER, 
				Collections.singletonMap("content_customProperties_ModuleCustomProperties5_StringRepeaterType",
						Collections.singletonMap("in", Arrays.asList("orange")))), 0,
				"modules", ModulePojo.class);
		assertEquals(0, actualModulesWithStringRepeaterFilter.length,
				"actualModulesWithStringRepeaterFilter must be empty since no matching module should be found for the queried custom property");
		/* Execute GraphQL request for Module with TAG type Custom Property and get response data */
		final ModulePojo[] actualModulesWithTagTypeFilter = getEntries(executeQueryBasedOnTemplate(projectId, QUERY_TEMPLATE_SORT_FILTER,
				Collections.singletonMap("content_customProperties_ModuleCustomProperties5_TagType",
						Collections.singletonMap("in", Arrays.asList("assembler")))), 0,
				"modules", ModulePojo.class);
		assertEquals(0, actualModulesWithTagTypeFilter.length,
				"actualModulesWithTagTypeFilter must be empty since no matching module should be found for the queried custom property");
		/* Execute GraphQL request for Module with String type Custom Property and get response data */
		final ModulePojo[] actualModulesWithStringTypeFilter = getEntries(executeQueryBasedOnTemplate(projectId, QUERY_TEMPLATE_SORT_FILTER, 
				Collections.singletonMap("content_customProperties_ModuleCustomProperties5_StringType",
					Collections.singletonMap("eq", "assembler"))), 0,
				"modules", ModulePojo.class);
		assertEquals(0, actualModulesWithStringTypeFilter.length,
				"actualModulesWithStringTypeFilter must be empty since no matching module should be found for the queried custom property");
	}
	
	/**
	 * Tests that the GraphQL query returns multiple matching modules by Custom property.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test querying for multiple matching modules by custom properties")
	@Test
	void testMultipleMatchingModulesWithCustomPropertyFilter() {
		final EntityId[] modules = createTestData();
		final EntityId[] expectedModules = {modules[4], modules[5]};
		
		/* Test for Module with String Repeater type Custom Property*/
		assertModuleWithCustomProperty(Collections.singletonMap("content_customProperties_ModuleCustomProperties5_StringRepeaterType",
				Collections.singletonMap("in", Arrays.asList("yellow"))), expectedModules);

		/* Test for for Module with TAG type Custom Property and get response data */
		assertModuleWithCustomProperty(Collections.singletonMap("content_customProperties_ModuleCustomProperties5_TagType",
				Collections.singletonMap("in", Arrays.asList("yellow"))), expectedModules);
		
		/* Execute GraphQL request for Module with String type Custom Property and get response data */
		assertModuleWithCustomProperty(Collections.singletonMap("content_customProperties_ModuleCustomProperties5_StringType",
				Collections.singletonMap("eq", "yellow")), expectedModules);
	}

	@DisplayName("Test filtering Modules by Taxonomy")
	@Test
	void testFilterByTaxonomy() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		final ModulePojo expectedModule1 = modules[0];
		final ModulePojo expectedModule2 = modules[1];

		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);

		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();

		/* Test #1: filter by { content_taxonomy_test: { eq: "foo" } } should return Module 1 */
		filterObject.put("content_taxonomy_test", filterOperator);
		filterOperator.put("eq", "foo");

		Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, "taxonomy { test }"))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();

		assertEquals(Collections.singleton(expectedModule1.getId()),
				response.path("modules.content").entityList(ModulePojo.class).get().stream().map(ModulePojo::getId).collect(Collectors.toSet()));

		/* Test #2: filter by { content_taxonomy_test: { eq: "bar" } } should return Module 2 */
		filterOperator.put("eq", "bar");

		response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, "taxonomy { test }"))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();

		assertEquals(Collections.singletonList(expectedModule2.getId()),
				response.path("modules.content[*].id").entityList(Long.class).get());

		/* Test #3: filter by { content_taxonomy_test: { eq: "asdf" } } should not return any Module */
		filterOperator.put("eq", "asdf");

		response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, "taxonomy { test }"))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();
		response.path("modules.content").entityList(Object.class).hasSize(0);
		
		/* Test #4: filter by { content_taxonomy_test: {is: null }} should return the modules without taxonomy type test*/
		final Object filterObject1 = (new Gson()).fromJson("{content_taxonomy_test: { is: null }}", Object.class);

		response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, "taxonomy { test }"))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject1)
				.execute();

		response.errors().verify();
		response.path("modules.content").entityList(Object.class).hasSize(2);

		final Map<Long, ModulePojo> expectedModules = new HashMap<>(2);
		expectedModules.put(modules[2].getId(), modules[2]);
		expectedModules.put(modules[3].getId(), modules[3]);

		final Map<Long, ModulePojo> actualModules = new HashMap<>(2);
		for (int i = 0; i < expectedModules.size(); i++) {
			final var path = String.format("modules.content[%d]", Integer.valueOf(i));
			final var respModule = response.path(path).entity(ModulePojo.class).get();
			actualModules.put(respModule.getId(), respModule);
		}

		for (final var expected : expectedModules.entrySet()) {
			final var actual = actualModules.get(expected.getKey());
			assertNotNull(actual, "Module must exists in DB: " + expected.getKey());
			assertEquals(expected.getValue().getName(), actual.getName());
		}
	}
	
	@DisplayName("Test filtering Modules by Multiple Taxonomy")
	@Test
	void testFilterByMultipleTaxonomies() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);

		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();

		filterObject.put("content_taxonomy_test", filterOperator);
		filterOperator.put("in", Arrays.asList("foo", "bar"));
		
		final Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, "taxonomy { test }"))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();
		
		final Set<Long> moduleIds= Sets.newHashSet(modules[0].getId(), modules[1].getId());
		assertEquals(moduleIds, response.path("modules.content[*].id").entityList(Long.class).get().stream().collect(Collectors.toSet()));
	}
	
	@DisplayName("Test filtering Modules by Taxonomy filter as NONE")
	@Test
	void testFilterByNoneTaxonomyFilter() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);

		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();
		
		filterOperator.put("is", null);
		filterObject.put("content_taxonomy_test", filterOperator);
		filterObject.put("content_name", Map.of("eq", modules[2].getName()));
		
		final Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, "taxonomy { test }"))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();

		assertEquals(Sets.newHashSet(modules[2].getId()), response.path("modules.content").entityList(ModulePojo.class).get()
				.stream().map(ModulePojo::getId).collect(Collectors.toSet()));
	}
	
	@DisplayName("Test filtering Modules by Multiple Taxonomy and None")
	@Test
	void testFilterByMultipleTaxonomiesWithNone() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for (int i = 0; i < projectModules.length; i++) {
			final int index = i;
			final List <ModulePojo> moduleList = moduleService.findModules(q -> q.ofProject(projectId).byId(projectModules[index]));
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);
		
		final Object filterObject2 = (new Gson()).fromJson("{content_technology : {eq: \"COBOL\" },"
				+ "content_type: {in: [\"PROGRAM\", \"COPYBOOK\"]} , content_taxonomy_test: { in: [\"foo\", null] }}", Object.class);
		
		final Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject2)
				.execute();

		response.errors().verify();
		final Set<Long> moduleIds= Sets.newHashSet(modules[0].getId(), modules[3].getId()); 
		assertEquals(moduleIds, response.path("modules.content").entityList(ModulePojo.class).get().stream().map(ModulePojo::getId).collect(Collectors.toSet()));
	}
	
	/**
	 * Tests that the GraphQL query returns all Modules without Tag type of custom properties assigned.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test querying for Modules by None of Tag type of custom properties")
	@Test
	void testModulesWithNoneCustomPropertyFilterObject() {
		final EntityId[] projectModules = createTestData();
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		/* Execute GraphQL request for Module to fetch all modules don't have TAG type Custom Property*/
		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();
		filterObject.put("content_customProperties_ModuleCustomProperties5_TagType", filterOperator);
		filterOperator.put("is", null);

		final Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();
		response.path("modules.content").entityList(Object.class).hasSize(3);

		final Map<Long, ModulePojo> expectedModules = new HashMap<>(3);
		expectedModules.put(modules[0].getId(), modules[0]);
		expectedModules.put(modules[2].getId(), modules[2]);
		expectedModules.put(modules[3].getId(), modules[3]);

		final Map<Long, ModulePojo> actualModules = new HashMap<>(3);
		for (int i = 0; i < expectedModules.size(); i++) {
			final var path = String.format("modules.content[%d]", Integer.valueOf(i));
			final var respModule = response.path(path).entity(ModulePojo.class).get();
			actualModules.put(respModule.getId(), respModule);
		}

		for (final var expected : expectedModules.entrySet()) {
			final var actual = actualModules.get(expected.getKey());
			assertNotNull(actual, "Module must exists in DB: " + expected.getKey());
			assertEquals(expected.getValue().getName(), actual.getName());
		}
	}

	@DisplayName("Test filtering Modules by Name with trailing or leading whitespace")
	@Test
	void testFilterByNameWithWhitespace() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		final ModulePojo expectedModule = modules[0];

		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);

		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();

		/* Test #1: filter without whitespace */
		filterObject.put("content_name", filterOperator);
		filterOperator.put("eq", "Module_1");

		Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();

		assertEquals(Collections.singletonList(expectedModule.getId()), response.path("modules.content[*].id").entityList(Long.class).get());

		/* Test #2: filter with trailing whitespace */
		filterOperator.put("eq", "Module_1 ");

		response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();

		assertEquals(Collections.singletonList(expectedModule.getId()), response.path("modules.content[*].id").entityList(Long.class).get());

		/* Test #3: filter with leading whitespace */
		filterOperator.put("eq", " Module_1");

		response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();

		assertEquals(Collections.singletonList(expectedModule.getId()),
				response.path("modules.content[*].id").entityList(Long.class).get());
	}

	@DisplayName("Test filtering Modules by Name with Wildcard")
	@Test
	void testFilterModuleWithPattern() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;

		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		final ModulePojo expectedModule = modules[0];

		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);

		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();

		/* Test #1: filter with trailing multi wildcard */
		filterObject.put("content_name", filterOperator);
		filterOperator.put("eq", "Module*");

		Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();

		//should find all modules with name Module*
		assertEquals(Arrays.stream(modules).map(ModulePojo::getId).collect(Collectors.toList()),
				response.path("modules.content[*].id").entityList(Long.class).get());

		/* Test #2: filter with leading single wildcard */
		filterOperator.put("eq", "?odule_1");

		response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();

		//should find all modules with name ?odule_1
		assertEquals(Collections.singletonList(expectedModule.getId()),
				response.path("modules.content[*].id").entityList(Long.class).get());

		/* Test #2: filter with contained multi and leading single wildcard */
		filterOperator.put("eq", "_od*e*2");

		response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();

		//should find all modules with name Module*
		assertEquals(Collections.singletonList(modules[1].getId()),
				response.path("modules.content[*].id").entityList(Long.class).get());

		/* Test #3: filter with contained multi wildcard % */
		filterOperator.put("eq", "Module%2");

		response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();

		assertEquals(singletonList(modules[1].getId()),
				response.path("modules.content[*].id").entityList(Long.class).get());
	}

	@DisplayName("Test filtering of Modules by MultiSelect filter object of Technology")
	@Test
	void testSingleProjectWithMultiSelectFilterObjectForTechnology() {
		createProjectWithModules("Project_1");
		
		/* Create more Modules*/
		final String name = "Module1_" + projectId.getNid();
		createTestModule(projectId, name, "programs/" + name + ".cbl", "Test Data content", Technology.CICS, Type.BMS_MAPSET, null);
		createTestModule(projectId, "Module2_" + projectId.getNid(), null, null, Technology.NATURAL, Type.PROGRAM, null);
		createTestModule(projectId, "Module3_" + projectId.getNid(), null, null, Technology.CICS, Type.BMS_MAP, null);
		
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);
		
		/* Filter Modules using Filter Object */
		final Object filterObject = (new Gson()).fromJson("{content_technology: {in: [ \"CICS\", \"NATURAL\" ]}}", Object.class);
		final Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();
		response.path("modules.content").entityList(Object.class).hasSize(4);
	}
	
	@DisplayName("Test filtering of Modules by MultiSelect filter object of type")
	@Test
	void testSingleProjectWithMultiSelectFilterObjectForType() {
		createProjectWithModules("Project_1");
		
		/* Create more Modules*/
		final String name = "Module1_" + projectId.getNid();
		createTestModule(projectId, name, "programs/" + name + ".cbl", "Test Data content", Technology.COBOL, Type.COPYBOOK, null);
		createTestModule(projectId, "Module2_" + projectId.getNid(), null, null, Technology.NATURAL, Type.PROGRAM, null);
		createTestModule(projectId, "Module3_" + projectId.getNid(), null, null, Technology.CICS, Type.BMS_MAP, null);
		
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);
		
		/* filter Modules By type using Filter Object */
		final Object filterObject = (new Gson()).fromJson("{content_type: {in: [ \"PROGRAM\", \"BMS_MAP\" ]}}", Object.class);
		
		final Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();
		response.path("modules.content").entityList(Object.class).hasSize(5);
	}
	
	@DisplayName("Test filtering of Modules by MultiSelect filter object of Creator, FileIdentification and requiresReview")
	@Test
	void testSingleProjectWithMultiSelectFilterObjectForCreatorAndFileIdentification() {
		createProjectWithModules("Project_1");
		
		final Gson gson = new Gson();
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);
		/* Test 1: filter Modules using multiple select Filter Object for Creator */
		final Object filterObject = gson.fromJson("{content_creator: {in: [ \"DISCOVERY\", \"API\" ]}}", Object.class);
		
		Response response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();
		
		response.errors().verify();
		response.path("modules.content").entityList(Object.class).hasSize(4);
		
		/* Test 2: filter Modules using using multiple select for Identification Link Object */
		final Object filterObject1 = gson.fromJson("{content_identification: {in: [ \"IDENTIFIED\", \"MISSING\"]}}", Object.class);
		response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject1)
				.execute();

		response.errors().verify();
		response.path("modules.content").entityList(Object.class).hasSize(4);
		
		/* Test 3: filter Modules By requiresReview using _none conjunction */
		final Object filterObject2 = gson.fromJson("{content_requiresReview: {\"in\": [ false, true ]}}", Object.class);
		response = assertNotNull(tester).document(String.format(ID_QUERY_TEMPLATE, ""))
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject2)
				.execute();

		response.errors().verify();
		response.path("modules.content").entityList(Object.class).hasSize(4);
	}
	
	/**
	 * Tests that the GraphQL query returns modules sorted by Source Lines Of Code
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test Sort Modules by Source Lines Of Code")
	@Test
	void testSortBySourceLinesOfCode() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		
		final ModulePojo[] expectedModules = {modules[0], modules[3], modules[2], modules[1]};
		
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);
		
		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_SORT_FILTER)
						.variable("projectId", projectId.getNid())
						.variable("sortObject", Collections.singletonMap("content_sourceMetrics_codeLines", SortDirection.DESCENDING))
						.execute();
		
		response.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(expectedModules.length));
		
		final ModulePojo[] actualModules = new ModulePojo[expectedModules.length];
		for (int i = 0; i < expectedModules.length; i++) {
			final String path = String.format("modules.content[%d]", Integer.valueOf(i));
			final ModulePojo respModule = response.path(path).entity(ModulePojo.class).get();
			actualModules[i] = respModule;
		}
		
		for (int i = 0; i < expectedModules.length; i++) {
			assertEquals(expectedModules[i].getId(), actualModules[i].getId(), "Expected and Actual module should be same post sorting");
		}
	}
	
	/**
	 * Tests that the GraphQL query returns modules sorted by Physical Lines Of Code
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test Sort Modules by Physical Lines Of Code")
	@Test
	void testSortByPhysicalLinesOfCode() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		final ModulePojo[] expectedModules = {modules[0], modules[3], modules[2], modules[1]};
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);
		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_SORT_FILTER)
						.variable("projectId", projectId.getNid())
						.variable("sortObject", Collections.singletonMap("content_sourceMetrics_physicalLines", SortDirection.DESCENDING))
						.execute();
		response.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(expectedModules.length));
		final ModulePojo[] actualModules = new ModulePojo[expectedModules.length];
		for (int i = 0; i < expectedModules.length; i++) {
			final String path = String.format("modules.content[%d]", Integer.valueOf(i));
			final ModulePojo respModule = response.path(path).entity(ModulePojo.class).get();
			actualModules[i] = respModule;
		}
		
		for (int i = 0; i < expectedModules.length; i++) {
			assertEquals(expectedModules[i].getId(), actualModules[i].getId(), "Expected and Actual module should be same post sorting");
		}
	}
	
	/**
	 * Tests that the GraphQL query returns modules sorted by Program Complexity
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test Sort Modules by Program Complexity")
	@Test
	void testSortByProgramComplexityOfModule() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		
		final ModulePojo[] expectedModules = {modules[3], modules[1], modules[2], modules[0]};
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_SORT_FILTER)
				.variable("projectId", projectId.getNid())
				.variable("sortObject", Collections.singletonMap("content_sourceMetrics_complexityMcCabe", SortDirection.DESCENDING))
				.execute();

		response.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(expectedModules.length));

		for (int i = 0; i < expectedModules.length; i++) {
			final String path = String.format("modules.content[%d]", Integer.valueOf(i));
			final var respModule = response.path(path).entity(ModulePojo.class).get();
			assertEquals(expectedModules[i].getId(), respModule.getId(), "Expected and Actual module should be same post sorting");
		}
	}
	
	/**
	 * Tests that the GraphQL query returns modules sorted by name ignoring case
	 * <p>The user has full access to the created test project.</p>
	 * */

	@DisplayName("Test Sort Modules by name ASC using sortObject")
	@Test
	void testSortByModuleNameUsingSortObjectAscending() {
		testSortByModuleNameUsingSortObject(true);
	}

	@DisplayName("Test Sort Modules by name DESC using sortObject")
	@Test
	void testSortByModuleNameUsingSortObjectDescending() {
		testSortByModuleNameUsingSortObject(false);
	}

	private void testSortByModuleNameUsingSortObject(final boolean ascending) {
		projectId = createProject("Test_Project");

		final String name1 = "BEN_CON_OUX_" + projectId.getNid();
		final var basicModule1 = createTestModule(projectId, name1, null, "Test Data content", Technology.BASIC, Type.PROGRAM, null);

		final String name2 = "BEN_PRD_BUD_" + projectId.getNid();
		final var basicModule2 = createTestModule(projectId, name2, "src/basic/" + name2 + ".bas", "Test Data content", Technology.BASIC, Type.PROGRAM,
				null);

		final String name3 = "ben_con_ouy_" + projectId.getNid();
		final var basicModule3 = createTestModule(projectId, name3, "src/basic/" + name3 + ".bas", "Test Data content", Technology.BASIC, Type.OBJECT, null);

		final Long[] expectedModules = ascending ? new Long[] {basicModule1.getNid(), basicModule3.getNid(), basicModule2.getNid()}
				: new Long[] {basicModule2.getNid(), basicModule3.getNid(), basicModule1.getNid()};

		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_SORT_FILTER_OBJECT)
				.variable("projectId", projectId.getNid())
				.variable("sortObject", Map.of("content_name", ascending ? "ASC" : "DESC"))
				.execute();

		response.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(expectedModules.length));

		final Long[] actualModules = new Long[expectedModules.length];
		for (int i = 0; i < expectedModules.length; i++) {
			final String path = String.format("modules.content[%d].id", Integer.valueOf(i));
			final var respModule = response.path(path).entity(Long.class).get();
			actualModules[i] = respModule;
		}

		for (int i = 0; i < expectedModules.length; i++) {
			assertEquals(expectedModules[i], actualModules[i], "Expected and Actual module should be same post sorting");
		}
	}
	
	/**
	 * Tests that the GraphQL query returns modules sorted by Comment Lines Of Code
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test Sort Modules by Comment Lines Of Code")
	@Test
	void testSortByCommentLinesOfCode() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		final List<Long> expectedModules = Arrays.asList(modules[3].getId(), modules[1].getId(), modules[2].getId(), modules[0].getId());
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);
		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_SORT_FILTER)
						.variable("projectId", projectId.getNid())
						.variable("sortObject", Collections.singletonMap("content_sourceMetrics_commentLines", SortDirection.DESCENDING))
						.execute();
		response.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(expectedModules.size()));	
		assertEquals(expectedModules, response.path("modules.content[*].id").entityList(Long.class).get(),
				"Expected and Actual module should be same post sorting");
	}
	
	@DisplayName("Test Sort Modules by Modified Date")
	@Test
	void testSortByModifiedDate() {
		projectId = createProject("Test_Project");
		final String name1 = "BEN_CON_OUX_" + projectId.getNid();
		/* Created a basic Module with Last modified date - April 25, 2017, 18:39:27.894 GMT */
		final var basicModule1 = createTestModule(projectId, name1, null, "Test Data content", Technology.BASIC, Type.PROGRAM, new Date(1494487567894L));
		
		final String name2 = "BEN_PRD_BUD_" + projectId.getNid();
		/* Created a basic Module with Last modified date - November 24, 2020, 00:00:00 GMT */
		final var basicModule2 = createTestModule(projectId, name2, "src/basic/" + name2 + ".bas", "Test Data content", Technology.BASIC, Type.PROGRAM,
				new Date(1609459200000L));
		
		final String name3 = "ben_con_oux_" + projectId.getNid();
		/* Created a basic Module with Last modified date - January 1, 2012, 00:00:00 GMT */
		final var basicModule3 = createTestModule(projectId, name3, "src/basic/" + name3 + ".bas", "Test Data content", Technology.BASIC, Type.OBJECT,
				new Date(1325376000000L));
		
		final EntityId[] expectedModules = {basicModule2, basicModule1, basicModule3};
		
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);
		
		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_SORT_FILTER)
						.variable("projectId", projectId.getNid())
						.variable("sortObject", Arrays.asList(Collections.singletonMap("content_modifiedDate", SortDirection.DESCENDING)))
						.execute();
		
		response.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(expectedModules.length));
		
		final ModulePojo[] actualModules = new ModulePojo[expectedModules.length];
		for (int i = 0; i < expectedModules.length; i++) {
			final String path = String.format("modules.content[%d]", Integer.valueOf(i));
			final var respModule = response.path(path).entity(ModulePojo.class).get();
			actualModules[i] = respModule;
		}
		
		for (int i = 0; i < expectedModules.length; i++) {
			assertEquals(expectedModules[i], actualModules[i].identity(), "Expected and Actual module should be same post sorting");
		}
	}

	
	/**
	 * Tests that the GraphQL query returns modules sorted by Lines Of Dead Code
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test Sort Modules by Lines Of Dead Code")
	@Test
	void testSortByLinesOfDeadCode() {
		final EntityId[] projectModules = createProjectWithModules("Project_1");
		createRelationship(RelationshipType.CALLS, projectModules, null);
		final EntityId project1Id = projectId;
		
		final ModulePojo[] modules = new ModulePojo[projectModules.length];
		for(int i = 0; i < projectModules.length; i++) {
			final List <ModulePojo> moduleList = getModules(project1Id, projectModules[i]);
			assertEquals(1, moduleList.size());
			modules[i] = moduleList.get(0);
		}
		
		final ModulePojo[] expectedModules = {modules[0], modules[2], modules[1], modules[3]};
		
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);
		
		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_SORT_FILTER)
						.variable("projectId", projectId.getNid())
						.variable("sortObject", Collections.singletonMap("content_sourceMetrics_deadCodeLines", SortDirection.DESCENDING))
						.execute();
		
		response.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(expectedModules.length));
		
		final ModulePojo[] actualModules = new ModulePojo[expectedModules.length];
		for (int i = 0; i < expectedModules.length; i++) {
			final String path = String.format("modules.content[%d]", Integer.valueOf(i));
			final ModulePojo respModule = response.path(path).entity(ModulePojo.class).get();
			actualModules[i] = respModule;
		}
		
		for (int i = 0; i < expectedModules.length; i++) {
			assertEquals(expectedModules[i].getId(), actualModules[i].getId(), "Expected and Actual module should be same post sorting");
		}
	}

	/**
	 * Tests that the GraphQL query returns all expected modules 
	 * The Module with supporting languages has isActuallySupported = true 
	 *
	 */
	@DisplayName("Test isActuallySupported data point for different modules")
	@Test
	void testIsActuallySuportedModules() {

		 final String QUERY_TEMPLATE_CHECK_SUPPORTED_MODULE = "query ($projectId: Long!) {" +
														        "modules(projectId: $projectId) {" +
														          "totalElements," +
														          "size," +
														          "content {" +
														            "name," +
														            "id," +
														            "actuallySupported," +
														            "technology," + 
														            "type" +
														          "}"+
														        "}" +
														      "}";

		projectId = createProject("Test_Project");
		setupProjectAccesses(singletonList(projectId), singletonList(projectId), emptyList(), false);

		/* Create test data */
		final List<EntityId> expectedModuleIds = Arrays.asList(
				createModule(projectId, Technology.COBOL, Type.PROGRAM),
				createModule(projectId, Technology.PL1, Type.PROGRAM),
				createModule(projectId, Technology.PL1, Type.MAINPROGRAM),
				createModule(projectId, Technology.NATURAL, Type.PROGRAM),
				createModule(projectId, Technology.NATURAL, Type.SUBPROGRAM),
				createModule(projectId, Technology.NATURAL, Type.SUBROUTINE),
				createModule(projectId, Technology.C, Type.PROGRAM),
				createModule(projectId, Technology.JCL, Type.JOB),
				createModule(projectId, Technology.JAVA, Type.COMPILATION_UNIT),
				createModule(projectId, Technology.SQL, Type.TABLE)
		);

		/* Query the test data from GraphQL */
		final Response response = executeQueryBasedOnTemplate(projectId, QUERY_TEMPLATE_CHECK_SUPPORTED_MODULE, null);
		final List<Long> actualModuleIds = response.path("modules.content[*].id").entityList(Long.class).get();

		/* Assert validity of response */
		final var expectedModuleNids = EntityId.allNids(expectedModuleIds);
		assertTrue(actualModuleIds.containsAll(expectedModuleNids), "All the created modules should be part of the response modules");

		for (int n = 0; n < actualModuleIds.size(); n++) {
			final Technology technology = response.path(String.format("modules.content[%d].technology", n)).entity(Technology.class).get();
			final Boolean isActuallySupported = response.path(String.format("modules.content[%d].actuallySupported", n)).entity(Boolean.class).get();
			if (technology == Technology.SQL) {
				assertFalse(isActuallySupported, "The isActuallySupported must be false for SQL Table");
			} else {
				assertTrue(isActuallySupported, "The isActuallySupported must be true for " + technology);
			}
		}
	}

	/**
	 * Creates a new module within the specified project, using the given technology and type.
	 * This method generates a new UUID for the module name and calls the createTestModule method.
	 *
	 * @param projectId    The ID of the project to which the module will be added.
	 * @param technology   The technology used in the module.
	 * @param type         The type of the module.
	 * @return             The ID of the newly created module.
	 */
	private EntityId createModule(final EntityId projectId, final Technology technology, final Type type) {
		return createTestModule(projectId, UUID.randomUUID().toString(), null, null, technology, type, null);
	}

	/**
	* Tests that the GraphQL query returns expected module which has the errorCount>=1
	* <p>The user has full access to the created test project.</p>
	*/
	@Test
	void testErrorCountGteOperator() {
		projectId = createProject("Test_Project");

		/* Creating Modules */
		final String name = "Module_" + projectId.getNid();
		final var module1 = createTestModule(projectId, name, "programs/" + name + ".cbl", "Test Data content", Technology.COBOL, Type.PROGRAM, null);
		final var module2 = createTestModule(projectId, name, "programs/" + name + ".cbl", null, Technology.COBOL, Type.PROGRAM, null);

		moduleService.createErrorMarker(new ErrorMarker().setCause("Error1").setModuleId(module1.getUid()).convertToPojoPrototype().setProject(projectId));
		moduleService.createErrorMarker(new ErrorMarker().setCause("Error2").setModuleId(module1.getUid()).convertToPojoPrototype().setProject(projectId));
		moduleService.createErrorMarker(new ErrorMarker().setCause("Error3").setModuleId(module2.getUid()).convertToPojoPrototype().setProject(projectId));
		
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);
		

		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();

		filterOperator.put("gte", "1");
		filterObject.put("content_errorCount", filterOperator);

		final Response response = assertNotNull(tester).document(ERROR_QUERY_TEMPLATE)
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();
		/* Testing that total 2 modules are fetched which has errorCount>=1*/
		response.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(2));
		
		/* Testing the errorCount of the module1 is 2 */
		response.path("modules.content[0].errorCount").entity(Long.class).isEqualTo(Long.valueOf(2));
		
		/* Testing the errorCount of the module2 is 1 */
		response.path("modules.content[1].errorCount").entity(Long.class).isEqualTo(Long.valueOf(1));
	}
	
	/**
	* Tests that the GraphQL query returns expected module which has the errorCount=1
	* <p>The user has full access to the created test project.</p>
	*/
	@DisplayName("Test Module with errorCount=1")
	@Test
	void testErrorCountEqOperator() {
		projectId = createProject("Test_Project");

		/* Creating Modules */
		final String name = "Module_" + projectId.getNid();
		final var module = createTestModule(projectId, name, "programs/" + name + ".cbl", "Test Data content", Technology.COBOL, Type.PROGRAM, null);

		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);

		assertNotNull(module);
		
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorD").setModuleId(module.getUid()).convertToPojoPrototype().setProject(projectId));

		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();

		filterOperator.put("eq", "1");
		filterObject.put("content_errorCount", filterOperator);

		final Response response = assertNotNull(tester).document(ERROR_QUERY_TEMPLATE)
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();
		/* Testing that total 1 module is fetched which has errorCount=1*/
		response.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(1));
		
		/* Testing the errorCount of the module is 1 */
		response.path("modules.content[0].errorCount").entity(Long.class).isEqualTo(Long.valueOf(1));
	}
	
	@DisplayName("Test querying for single matching modules by string type custom property with eq filter and sort order")
	@ParameterizedTest
	@MethodSource("provideSortDirections")
	void testSingleMatchingModulesWithStringTypeCustomPropertyFilterAndSort(final SortDirection direction, final int firstExpectedModuleIndex,
			final int secondExpectedModuleIndex) {
		final EntityId[] expectedModules = createTestData();
		final Response resp = assertNotNull(tester).document(QUERY_TEMPLATE_SORT_FILTER).variable("projectId", projectId.getNid())
				.variable("filterObject",
						Collections.singletonMap("content_customProperties_ModuleCustomProperties5_StringType", Collections.singletonMap("eq", "PL")))
				.variable("sortObject", Collections.singletonMap("content_customProperties_ModuleCustomProperties5_StringType", direction)).execute();

		final ModulePojo[] actualModules = getEntries(resp, 2, "modules", ModulePojo.class);
		assertEquals(2, actualModules.length);
		assertEquals(expectedModules[firstExpectedModuleIndex], actualModules[0].identity());
		assertEquals(expectedModules[secondExpectedModuleIndex], actualModules[1].identity());
	}
	
	/**
	* Tests that the GraphQL query returns expected module which has the errorCount<=1
	* <p>The user has full access to the created test project.</p>
	*/
	@DisplayName("Test Modules with errorCount<=1")
	@Test
	void testErrorCountLteOperator() {
		projectId = createProject("Test_Project");

		/* Creating Modules */
		final String name = "Module_" + projectId.getNid();
		final var module1 = createTestModule(projectId, name, "programs/" + name + ".cbl", "Test Data content", Technology.COBOL, Type.PROGRAM, null);
		final var module2 = createTestModule(projectId, name, "programs/" + name + ".cbl", null, Technology.COBOL, Type.PROGRAM, null);
		
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorA").setModuleId(module1.getUid()).convertToPojoPrototype().setProject(projectId));
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorB").setModuleId(module1.getUid()).convertToPojoPrototype().setProject(projectId));
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorC").setModuleId(module2.getUid()).convertToPojoPrototype().setProject(projectId));
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorD").setModuleId(module2.getUid()).convertToPojoPrototype().setProject(projectId));
		
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);
		

		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();

		filterOperator.put("lte", "2");
		filterObject.put("content_errorCount", filterOperator);

		final Response response = assertNotNull(tester).document(ERROR_QUERY_TEMPLATE)
				.variable("projectId", projectId.getNid())
				.variable("filterObject", filterObject)
				.execute();

		response.errors().verify();
		/* Testing that total 2 modules are fetched which has errorCount<=1*/
		response.path("modules.totalElements").entity(Long.class).isEqualTo(Long.valueOf(2));
		
		/* Testing the errorCount of the module1 is 2 */
		response.path("modules.content[0].errorCount").entity(Long.class).isEqualTo(Long.valueOf(2));
		
		/* Testing the errorCount of the module2 is 2 */
		response.path("modules.content[1].errorCount").entity(Long.class).isEqualTo(Long.valueOf(2));
	}

	@Test
	void testReachabilityBlockDataPointForModules() {
		projectId = createProject("Test_Project");
		final EntityId module1 = createTestModule(projectId, "Module_1",
				"ModuleReachabilityBlocks1", " Module content1", Technology.COBOL, Type.PROGRAM, null);
		final EntityId module2 = createTestModule(projectId, "Module_2",
				"ModuleReachabilityBlocks2", " Module content2", Technology.COBOL, Type.PROGRAM, null);
		createTestModule(projectId, "Module_3",
				"ModuleReachabilityBlocks3", " Module content3", Technology.COBOL, Type.PROGRAM, null);

		final UUID functionalBlockPojoUUID1 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Tp_Functional_block1",
				"Functional_block_description_Top_Down1", new ModuleLocation(100, 250), Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)), projectId));
		final UUID functionalBlockPojoUUID2 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Tp_Functional_block2",
				"Functional_block_description_Top_Down2", new ModuleLocation(100, 250), Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)), projectId));

		blockServiceImpl.setResolvedModuleParts(functionalBlockPojoUUID1, List.of(new ResolvedModulePart(module1), new ResolvedModulePart(module2)));
		blockServiceImpl.setResolvedModuleParts(functionalBlockPojoUUID2, List.of(new ResolvedModulePart(module1)));

		setupProjectAccesses(singletonList(projectId), singletonList(projectId), emptyList(), false);
		final Response response1 = executeQueryBasedOnTemplate(projectId, QUERY_TEMPLATE, null);
		assertEquals(3L, response1.path("modules.totalElements").entity(Long.class).get());

		final Map<String, List<FunctionalBlockPojo>> functionalBlockMap = new HashMap<>();
		functionalBlockMap.put(response1.path("modules.content[0].path").entity(String.class).get(),
				response1.path("modules.content[0].reachabilityBlocks").entityList(FunctionalBlockPojo.class).get());
		functionalBlockMap.put(response1.path("modules.content[1].path").entity(String.class).get(),
				response1.path("modules.content[1].reachabilityBlocks").entityList(FunctionalBlockPojo.class).get());
		functionalBlockMap.put(response1.path("modules.content[2].path").entity(String.class).get(),
				response1.path("modules.content[2].reachabilityBlocks").entityList(FunctionalBlockPojo.class).get());

		assertEquals(2L, functionalBlockMap.get("ModuleReachabilityBlocks1").size());
		assertEquals(1L, functionalBlockMap.get("ModuleReachabilityBlocks2").size());
		assertEquals(0L, functionalBlockMap.get("ModuleReachabilityBlocks3").size());

		assertEquals(Set.of("Tp_Functional_block1", "Tp_Functional_block2"),
				functionalBlockMap.get("ModuleReachabilityBlocks1").stream().map(FunctionalBlockPojo::getName).collect(Collectors.toSet()));
		assertEquals("Tp_Functional_block1", functionalBlockMap.get("ModuleReachabilityBlocks2").get(0).getName());

		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();
		filterOperator.put("eq", "Tp_Functional_block1");
		filterObject.put("content_reachabilityBlockNames_name", filterOperator);

		final Response response2 = executeQueryBasedOnTemplate(projectId, QUERY_TEMPLATE, filterObject);
		assertEquals(2L, response2.path("modules.totalElements").entity(Long.class).get());

		final Set<String> modulePaths = new HashSet<>();
		modulePaths.add(response2.path("modules.content[0].path").entity(String.class).get());
		modulePaths.add(response2.path("modules.content[1].path").entity(String.class).get());
		assertTrue(modulePaths.containsAll(List.of("ModuleReachabilityBlocks1", "ModuleReachabilityBlocks2")));
	}

	@DisplayName("Test missing dependencies are determined and mapped correctly")
	@Test
	void testMissingDependencies() {
		final String queryTemplate = """
				query ($projectId: Long!) {
				  modules(projectId: $projectId) {
				    content {
				      name
				      missingDependencies {
				        name
				      }
				    }
				  }
				}
				""";

		projectId = createProject("Test_Project");
		setupProjectAccesses(singletonList(projectId), singletonList(projectId), emptyList(), false);

		final EntityId module1 = createTestModule(projectId, "module1", null, null, Technology.COBOL, Type.PROGRAM, null, null, Identification.IDENTIFIED);
		final EntityId module2 = createTestModule(projectId, "module2", null, null, Technology.COBOL, Type.PROGRAM, null, null, Identification.MISSING);
		final EntityId module3 = createTestModule(projectId, "module3", null, null, Technology.COBOL, Type.PROGRAM, null, null, Identification.IDENTIFIED);
		final EntityId module4 = createTestModule(projectId, "module4", null, null, Technology.COBOL, Type.PROGRAM, null, null, Identification.MISSING);

		moduleService.createRelationship(
				new ModuleRelationshipPojoPrototype().setSrcModule(module1).setDstModule(module2).setType(RelationshipType.CALLS.name()));
		moduleService.createRelationship(
				new ModuleRelationshipPojoPrototype().setSrcModule(module1).setDstModule(module3).setType(RelationshipType.CALLS.name()));
		moduleService.createRelationship(
				new ModuleRelationshipPojoPrototype().setSrcModule(module3).setDstModule(module4).setType(RelationshipType.CALLS.name()));

		/* Query the test data from GraphQL */
		final Response response = executeQueryBasedOnTemplate(projectId, queryTemplate, null);
		final List<String> sourceModule = response.path("modules.content[*].name").entityList(String.class).get();

		assertEquals(4, sourceModule.size());
		for (int i = 0; i < sourceModule.size(); i++) {
			final List<String> destinationModule = response.path("modules.content[" + i + "].missingDependencies[*].name").entityList(String.class).get();
			if (sourceModule.get(i).equals("module1")) {
				assertEquals(1, destinationModule.size());
				assertEquals("module2", destinationModule.get(0));
			} else if (sourceModule.get(i).equals("module3")) {
				assertEquals(1, destinationModule.size());
				assertEquals("module4", destinationModule.get(0));
			} else {
				assertEquals(0, destinationModule.size());
			}
		}
	}

	@DisplayName("Test Modules Error Marker with Severity, Cause And Text")
	@Test
	void testModuleErrorMarkerSeverityAndCauseAndText() {
		projectId = createProject("Test_Project");

		/* Creating Modules */
		final var module1 = createTestModule(projectId, "moduleError1", "programs/moduleError1.cbl",
				getContent("DBSCOPE.cbl", RESOURCE_PATH), Technology.COBOL, Type.PROGRAM, null);
		final var module2 = createTestModule(projectId, "moduleError2", "programs/moduleError2.cbl",
				getContent("MULTISCOPE.cbl", RESOURCE_PATH), Technology.COBOL, Type.PROGRAM, null);
		final var module3 = createTestModule(projectId, "moduleError3", "programs/moduleError3.cbl",
				getContent("PARAM.cbl", RESOURCE_PATH), Technology.COBOL, Type.PROGRAM, null);

		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorA").setSeverity(Severity.ERROR)
						.setModuleLocation(new AstNodeLocation(494, 28, 494, 28, 494, 28, null, null))
				.setModuleId(module1.getUid()).convertToPojoPrototype().setProject(projectId));
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorB").setSeverity(Severity.ERROR)
						.setModuleLocation(new AstNodeLocation(1158, 25, 1158, 25, 1158, 25, null, null))
				.setModuleId(module1.getUid()).convertToPojoPrototype().setProject(projectId));
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorC").setSeverity(Severity.ERROR)
						.setModuleLocation(new AstNodeLocation(461, 15, 461, 15, 461, 15, null, null))
				.setModuleId(module2.getUid()).convertToPojoPrototype().setProject(projectId));
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorD").setSeverity(Severity.WARNING)
						.setModuleLocation(new AstNodeLocation(515, 349, 515, 349, 515, 349, null, null))
				.setModuleId(module2.getUid()).convertToPojoPrototype().setProject(projectId));
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorE").setSeverity(Severity.WARNING)
						.setModuleLocation(new AstNodeLocation(625, 0, 625, 0, 625, 0, null, null))
				.setModuleId(module3.getUid()).convertToPojoPrototype().setProject(projectId));

		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);

		final Response response1 = assertNotNull(tester).document(ERROR_MARKER_QUERY_TEMPLATE)
				.variable("projectId", projectId.getNid())
				.variable("filterObject", Collections.singletonMap("content_module", Collections.singletonMap("eq", module1.getNid())))
				.execute();

		final List<ErrorMarkerPojo> errorMarkers1 = response1.path("errorMarkers.content").entityList(ErrorMarkerPojo.class).get();
		assertEquals(2, errorMarkers1.size());
		assertEquals(Set.of("ErrorA", "ErrorB"), errorMarkers1.stream().map(ErrorMarkerPojo::getCause).collect(Collectors.toSet()));
		assertEquals(Set.of(Severity.ERROR), errorMarkers1.stream().map(ErrorMarkerPojo::getSeverity).collect(Collectors.toSet()));
		assertEquals(Set.of("COORDINATOR-TYPE     PIC X(1)", "INTO :DCL.COORDINATOR-TYPE"),
				Set.of(response1.path("errorMarkers.content[0].errorText").entity(String.class).get(),
						response1.path("errorMarkers.content[1].errorText").entity(String.class).get()));

		final Response response2 = assertNotNull(tester).document(ERROR_MARKER_QUERY_TEMPLATE)
				.variable("projectId", projectId.getNid())
				.variable("filterObject", Collections.singletonMap("content_module", Collections.singletonMap("eq", module2.getNid())))
				.execute();

		final List<ErrorMarkerPojo> errorMarkers2 = response2.path("errorMarkers.content").entityList(ErrorMarkerPojo.class).get();
		assertEquals(2, errorMarkers2.size());
		assertEquals(Set.of("ErrorC", "ErrorD"), errorMarkers2.stream().map(ErrorMarkerPojo::getCause).collect(Collectors.toSet()));
		assertEquals(Set.of(Severity.ERROR, Severity.WARNING), errorMarkers2.stream().map(ErrorMarkerPojo::getSeverity).collect(Collectors.toSet()));
		assertEquals(Set.of("0000-INITIALIZE.", "SELECT TABLE1.COORDINATOR_TYPE,\n" +
						"                   TABLE1.ANNUAL_DELIV_FEE,\n" +
						"                   TABLE1.FREE_DELIVERY_IND\n" +
						"                   INTO :DCL.COORDINATOR-TYPE,\n" +
						"                        :DCL.ANNUAL-DELIV-FEE,\n" +
						"                        :DCL.FREE-DELIVERY-IND\n" +
						"                   FROM TABLE1\n" +
						"                   WHERE TABLE1.SUBJECT_ID = :W-SUBJECT-ID"),
				Set.of(response2.path("errorMarkers.content[0].errorText").entity(String.class).get(),
						response2.path("errorMarkers.content[1].errorText").entity(String.class).get()));

		final Response response3 = assertNotNull(tester).document(ERROR_MARKER_QUERY_TEMPLATE)
				.variable("projectId", projectId.getNid())
				.variable("filterObject", Collections.singletonMap("content_module", Collections.singletonMap("eq", module3.getNid())))
				.execute();

		final List<ErrorMarkerPojo> errorMarkers3 = response3.path("errorMarkers.content").entityList(ErrorMarkerPojo.class).get();
		assertEquals(1, errorMarkers3.size());
		assertEquals(Set.of("ErrorE"), errorMarkers3.stream().map(ErrorMarkerPojo::getCause).collect(Collectors.toSet()));
		assertEquals(Set.of(Severity.WARNING), errorMarkers3.stream().map(ErrorMarkerPojo::getSeverity).collect(Collectors.toSet()));
		assertEquals(".", response3.path("errorMarkers.content[0].errorText").entity(String.class).get());
	}

	@DisplayName("Test Modules Error Marker with Severity, Cause And Text for all edge cases")
	@Test
	void testErrorMarkerTextAllEdgeCases() {
		projectId = createProject("Test_Project");
		final EntityId module1 = createTestModule(projectId, "moduleError1", "programs/moduleError1.cbl", null,
				Technology.COBOL, Type.PROGRAM, null);
		final EntityId module2 = createTestModule(projectId, "moduleError2", "programs/moduleError2.cbl", null,
				Technology.COBOL, Type.PROGRAM, null);

		/* creating errorMarker with No location */
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorA").setSeverity(Severity.ERROR).setModuleId(module1.getUid())
				.convertToPojoPrototype().setProject(projectId));

		/* creating errorMarker with location inside include component */
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorB").setSeverity(Severity.ERROR)
				.setModuleLocation(new AstNodeLocation(-1, -1, -1, -1, 67, 78, null, null)).setModuleId(module1.getUid()).convertToPojoPrototype()
				.setProject(projectId));

		/* creating errorMarker with no root relative offset and length */
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorC").setSeverity(Severity.ERROR)
				.setModuleLocation(new AstNodeLocation(0, 0, 0, 0, null, null, null, null)).setModuleId(module2.getUid()).convertToPojoPrototype()
				.setProject(projectId));

		/* creating errorMarker with null module content */
		moduleService.createErrorMarker(new ErrorMarker().setCause("ErrorD").setSeverity(Severity.WARNING)
				.setModuleLocation(new AstNodeLocation(0, 0, 0, 0, 0, 0, null, null)).setModuleId(module2.getUid()).convertToPojoPrototype()
				.setProject(projectId));

		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), Collections.emptyList(), false);

		final Response response1 = assertNotNull(tester).document(ERROR_MARKER_QUERY_TEMPLATE)
				.variable("projectId", projectId.getNid())
				.variable("filterObject", Collections.singletonMap("content_module", Collections.singletonMap("eq", module1.getNid())))
				.execute();

		final List<ErrorMarkerPojo> errorMarkers1 = response1.path("errorMarkers.content").entityList(ErrorMarkerPojo.class).get();
		assertEquals(2, errorMarkers1.size());
		assertEquals(Set.of("ErrorA", "ErrorB"), errorMarkers1.stream().map(ErrorMarkerPojo::getCause).collect(Collectors.toSet()));
		assertEquals(Set.of(Severity.ERROR), errorMarkers1.stream().map(ErrorMarkerPojo::getSeverity).collect(Collectors.toSet()));
		assertEquals(List.of("No Data", "No Data"), List.of(response1.path("errorMarkers.content[0].errorText").entity(String.class).get(),
				response1.path("errorMarkers.content[1].errorText").entity(String.class).get()));

		final Response response2 = assertNotNull(tester).document(ERROR_MARKER_QUERY_TEMPLATE)
				.variable("projectId", projectId.getNid())
				.variable("filterObject", Collections.singletonMap("content_module", Collections.singletonMap("eq", module2.getNid())))
				.execute();

		final List<ErrorMarkerPojo> errorMarkers2 = response2.path("errorMarkers.content").entityList(ErrorMarkerPojo.class).get();
		assertEquals(2, errorMarkers2.size());
		assertEquals(Set.of("ErrorC", "ErrorD"), errorMarkers2.stream().map(ErrorMarkerPojo::getCause).collect(Collectors.toSet()));
		assertEquals(Set.of(Severity.ERROR, Severity.WARNING), errorMarkers2.stream().map(ErrorMarkerPojo::getSeverity).collect(Collectors.toSet()));
		assertEquals(List.of("No Data", "No Data"), List.of(response2.path("errorMarkers.content[0].errorText").entity(String.class).get(),
				response2.path("errorMarkers.content[1].errorText").entity(String.class).get()));
	}
	
	static Stream<Arguments> provideSortDirections() {
		return Stream.of(Arguments.of(SortDirection.DESCENDING, 6, 2), Arguments.of(SortDirection.ASCENDING, 2, 6));
	}
	
	/**
	 * Tests that all expected incoming or outgoing references are present.
	 *
	 * @param refListGetter Supplier that returns the list of reference to check
	 * @param moduleGetter Supplier that returns the referenced {@link ModulePojo} in the reference
	 * @param expProjectId The expected projectId of the referenced {@link ModulePojo} in the reference
	 * @param expCount expected amount of incoming or outgoing references
	 * @param refName One of {@link #CALLS}, {@link #INCLUDES}, {@link #READSWRITES} or {@link #REFERENCES}
	 */
	private static void assertRefs(final Response response, final int idx, final String field,
			final EntityId expProjectId, final int expCount, final RelationshipType expRef) {
		final int count = response.path(String.format("modules.content[%d].%s", idx, field)).entityList(Object.class).get().size();
		assertEquals(expCount, count, () -> "Number of \"" + field + "\" dependencies must match");
		for (int n = 0; n < count; n++) {
			assertEquals(expRef, response.path(String.format("modules.content[%d].%s[%d].type", idx, field, n)).<RelationshipType>entity(RelationshipType.class).get(),
				() -> "All dependencies must be " + expRef);
			assertEquals(expProjectId.getNid(), response.path(String.format("modules.content[%d].%s[%d].module.projectId", idx, field, n)).<Long>entity(Long.class).get(),
					"Project ID must match");
		}
	}
	
	/**
	 * Tests that all expected incoming and outgoing references are present in the given {@code module}.
	 *
	 * @param module The {@link ModulePojo} whose references have to be tested
	 * @param expProjectId The expected projectId of the referenced {@link ModulePojo} in the reference
	 * @param expInCount expected amount of incoming references
 	 * @param expOutCount expected amount of outgoing references
	 * @param expRef One of {@link #CALLS}, {@link #INCLUDES}, {@link #READSWRITES} or {@link #REFERENCES}
	 */
	private void assertRefs(final Response response, final int idx, final EntityId expProjectId, final int expInCount, final int expOutCount, final RelationshipType expRef) {
		assertRefs(response, idx, "in", expProjectId, expInCount, expRef);
		assertRefs(response, idx, "out", expProjectId, expOutCount, expRef);
	}

	/**
	 * Creates the test data as following:
	 * <ul>
	 * <li>Creates a new project with the given {@code projectName}</li>
	 * <li>Module 4 has no references</li>
	 * <li>Module 1 has Taxonomy test.foo</li>
	 * <li>Module 2 has Taxonomy test.bar</li>
	 * </ul>
	 *
	 * @param projectName The name of the project
	 * @return The four created {@link Module Modules}
	 */
	private EntityId[] createProjectWithModules(final String projectName) {
		projectId = createProject(projectName);
		final var module1 = new ModulePojoPrototype();
		module1.setName("Module_1_" + projectId.getNid());
		module1.setTechnology(Technology.COBOL);
		module1.setStorage(Storage.FILE);
		module1.setOrigin(Origin.CUSTOM);
		module1.setType(Type.PROGRAM);
		module1.setCreator(Creator.API);
		module1.setProject(projectId);
		module1.setIdentification(Identification.MISSING);
		
		final var sourceMetrics1 = new SourceMetricsPojoPrototype();
		sourceMetrics1.setCodeLines(Integer.valueOf(789));
		sourceMetrics1.setCommentLines(Integer.valueOf(9));
		sourceMetrics1.setPhysicalLines(Integer.valueOf(788));
		sourceMetrics1.setDeadCodeLines(Integer.valueOf(14));
		sourceMetrics1.setComplexityMcCabe(Integer.valueOf(1));
		module1.setSourceMetrics(sourceMetrics1);
		final var m1 = moduleService.create(module1);
		
		final var module2 = new ModulePojoPrototype();
		module2.setName("Module_2_" + projectId.getNid());
		module2.setTechnology(Technology.COBOL);
		module2.setStorage(Storage.FILE);
		module2.setOrigin(Origin.CUSTOM);
		module2.setType(Type.PROGRAM);
		module2.setCreator(Creator.DISCOVERY);
		module2.setProject(projectId);
		module2.setIdentification(Identification.IDENTIFIED);
		
		final var sourceMetrics2 = new SourceMetricsPojoPrototype();
		sourceMetrics2.setCodeLines(Integer.valueOf(7));
		sourceMetrics2.setCommentLines(Integer.valueOf(692));
		sourceMetrics2.setPhysicalLines(Integer.valueOf(7));
		sourceMetrics2.setDeadCodeLines(Integer.valueOf(8));
		sourceMetrics2.setComplexityMcCabe(Integer.valueOf(3));
		module2.setSourceMetrics(sourceMetrics2);

		final var m2 = moduleService.create(module2);

		final var module3 = new ModulePojoPrototype();
		module3.setName("Module_3_" + projectId.getNid());
		module3.setTechnology(Technology.NATURAL);
		module3.setStorage(Storage.FILE);
		module3.setOrigin(Origin.CUSTOM);
		module3.setType(Type.PROGRAM);
		module3.setCreator(Creator.API);
		module3.setProject(projectId);
		module3.setIdentification(Identification.IDENTIFIED);
		
		final var sourceMetrics3 = new SourceMetricsPojoPrototype();
		sourceMetrics3.setCodeLines(Integer.valueOf(79));
		sourceMetrics3.setCommentLines(Integer.valueOf(677));
		sourceMetrics3.setPhysicalLines(Integer.valueOf(79));
		sourceMetrics3.setDeadCodeLines(Integer.valueOf(10));
		sourceMetrics3.setComplexityMcCabe(Integer.valueOf(2));
		module3.setSourceMetrics(sourceMetrics3);
		final var m3 = moduleService.create(module3);
		
		final var module4 = new ModulePojoPrototype();
		module4.setName("Module_4_" + projectId.getNid());
		module4.setTechnology(Technology.COBOL);
		module4.setStorage(Storage.FILE);
		module4.setOrigin(Origin.CUSTOM);
		module4.setType(Type.COPYBOOK);
		module4.setCreator(Creator.API);
		module4.setProject(projectId);
		module4.setIdentification(Identification.MISSING);
		
		final var sourceMetrics4 = new SourceMetricsPojoPrototype();
		sourceMetrics4.setCodeLines(Integer.valueOf(779));
		sourceMetrics4.setCommentLines(Integer.valueOf(890));
		sourceMetrics4.setPhysicalLines(Integer.valueOf(779));
		sourceMetrics4.setDeadCodeLines(Integer.valueOf(4));
		sourceMetrics4.setComplexityMcCabe(Integer.valueOf(5));
		module4.setSourceMetrics(sourceMetrics4);
		final var m4 = moduleService.create(module4);

		final UUID tt = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("test").setProject(projectId));
		final EntityId fooTaxonomy = taxonomyService.create(new TaxonomyPojoPrototype().setName("foo").setProject(projectId).setType(tt));
		final EntityId barTaxonomy = taxonomyService.create(new TaxonomyPojoPrototype().setName("bar").setProject(projectId).setType(tt));

		taxonomyService.createModuleLink(m1.getUid(), fooTaxonomy);
		taxonomyService.createModuleLink(m2.getUid(), barTaxonomy);
		
		/* TaxonomiesModifiedEvent needs to get published so data points and GraphQL schema are re-calculated */
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));

		return new EntityId[] { m1, m2, m3, m4 };
	}

	/**
	 * Executes the GraphQL query {@link #QUERY_TEMPLATE} for the given {@code refName}.
	 *
	 * @param relType One of {@link #CALLS}, {@link #INCLUDES}, {@link #READSWRITES} or {@link #REFERENCES}
	 * @param projectId The project to query
	 * @return {@link Response} from GraphQL with the query result
	 */
	private Response executeQuery(final RelationshipType relType, final EntityId projectId) {
		return assertNotNull(tester)
					.document(QUERY_TEMPLATE)
					.variable("projectId", projectId.getNid())
					.variable("type", relType)
					.execute();
	}
	
	/**
	 * Executes the GraphQL queries {@link #QUERY_TEMPLATE_2} and {@link #QUERY_TEMPLATE_3} and {@link #QUERY_TEMPLATE_SORT_FILTER}
	 * 
	 * @param projectId The project to query
	 * @param queryTemplate The query
	 * @param filter filtering field
	 * @return {@link Response} from GraphQL with the query result
	 */
	private Response executeQueryBasedOnTemplate(final EntityId projectId, final String queryTemplate, @Nullable final Map<String, Object> filter) {
		return assertNotNull(tester)
					.document(queryTemplate)
					.variable("projectId", projectId.getNid())
					.variable("filterObject", filter)
					.execute();
	}

	/**
	 * Returns the method reference of the method that adds new references for the given {@code refName}.
	 *
	 * @param relType One of {@link #CALLS}, {@link #INCLUDES}, {@link #READSWRITES} or {@link #REFERENCES}
	 */
	private void getRefCreator(final RelationshipType relType, final EntityId module1, final EntityId module2) {
		switch (relType) {
			case CALLS:
			case INCLUDES:
			case ACCESSES:
			case REFERENCES:
				createReference(relType, module1, module2);
				break;
			default:
				throw new AssertionError("Unsupported reference: " + relType);
		}
	}

	/**
	 * <ul>
	 * <li>Creates a reference from module 1 to module 2 by using the given {@code referenceCreator}</li>
	 * <li>Creates a reference from module 1 to module 3 by using the given {@code referenceCreator}</li>
	 * <li>Creates a reference from module 1 to all {@code externalModules} by using the given {@code referenceCreator}</li>
	 * <li>Module 4 has no references</li>
	 * </ul>
	 *
	 * @param projectName The name of the project
	 * @param refCreator The method for the creation of the references
	 * @param externalModules Modules from other project(s)
	 */
	private void createRelationship(final RelationshipType relType, final EntityId[] module, @Nullable final EntityId externalModule) {
		final var module1 = module[0];
		for(int i = 1; i < module.length - 1; i++) {
			getRefCreator(relType, module1, module[i]);
		}
		if (externalModule != null) {
			getRefCreator(relType, module1, externalModule);
		}
	}

	private EntityId createTestModule(final EntityId projectId, final String name, @Nullable final String path, @Nullable final String content,
			final Technology technology, final Type type, @Nullable final Date lastModified) {
		return createTestModule(projectId, name, path, content, technology, type, lastModified, null, Identification.IDENTIFIED);
	}

	/**
	 * Create a Module in database
	 *
	 * @param projectId project ID of the project
	 * @param name Module name
	 * @param path Module path
	 * @param content The content of Module
	 * @return Created module
	 */
	private EntityId createTestModule(final EntityId projectId, final String name, @Nullable final String path, @Nullable final String content,
			final Technology technology, final Type type, @Nullable final Date lastModified, @Nullable final EntityId parent,
			final Identification identification) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(name);
		module.setProject(projectId);
		module.setTechnology(technology);
		module.setType(type);
		module.setStorage(Storage.FILE);
		module.setIdentification(identification);
		module.setOrigin(Origin.CUSTOM);
		module.setDescription("Creating Module " + name);
		module.setCreator(Creator.DISCOVERY);
		if (content != null) {
			module.setContent(content);
			module.setPath(path);
		}
		if (name.equals("Module_A_")) {
			final var sourceMetrics1 = new SourceMetricsPojoPrototype();
			sourceMetrics1.setCodeLines(Integer.valueOf(789));
			sourceMetrics1.setCommentLines(Integer.valueOf(9));
			sourceMetrics1.setPhysicalLines(Integer.valueOf(788));
			sourceMetrics1.setDeadCodeLines(Integer.valueOf(14));
			sourceMetrics1.setComplexityMcCabe(Integer.valueOf(1));
			module.setSourceMetrics(sourceMetrics1);
		}
		
		if (lastModified != null) {
			module.setModifiedDate(lastModified.toInstant());
		}
		
		final var newModule = moduleService.create(module);
		
		if (parent != null) {
			moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
					.setSrcModule(parent)
					.setRelationship(RelationshipType.CONTAINS)
					.setDstModule(newModule));
		}
		
		return newModule;
	}

	private void createReference(final RelationshipType relationship, final EntityId module1, final EntityId module2) {
		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(Integer.valueOf(0));
		dummyLocation.setLength(Integer.valueOf(0));
		
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(relationship)
				.setSrcModule(module1)
				.setSrcLocation(dummyLocation)
				.setDstModule(module2)
				.setDstLocation(dummyLocation));
	}

	/**
	 * Sets the project accesses for the current user. The given {@code authProjectIds} are used for setting the user's {@link Authentication}. The given
	 * {@code userRoleProjectIds} are used for the {@link UserRoleService}. Both lists can differ.
	 * 
	 * <p>Usually the associated project IDs in the UserRoleService should match the project authorities. If a user has no authorization for a project
	 * ({@link MiningRole}), the server responses with access {@code FORBIDDEN}. For explicitly testing of {@link ControllerLocalContext}
	 * and {@link UserRoleService} both project ID lists can differ.</p>
	 *
	 * @param authProjectIds project IDs for the {@link Authentication}
	 * @param userRoleProjectIds project IDs for the {@link UserRoleService}
	 */
	private void setupProjectAccesses(final List<EntityId> authProjectIds, final List<EntityId> userRoleProjectIds, final List<Long> userClientAdminIds,
			final boolean isAdmin) {
		final UserRoleService userRoleService = Objects.requireNonNull(this.userRoleService);
		given(userRoleService.getProjectIds()).willReturn(EntityId.allNids(userRoleProjectIds));
		given(userRoleService.getClientAdminIds()).willReturn(userClientAdminIds);
		given(userRoleService.isAdmin()).willReturn(isAdmin);
		
		final List<GrantedAuthority> authorities = new ArrayList<>(authProjectIds.size() * 2);
		authProjectIds.forEach(projectId -> {
			authorities.add(new MiningRole(String.format("client-1-project-%d-viewer", projectId.getNid())));
			authorities.add(new MiningRole(String.format("client-1-project-%d-mining", projectId.getNid())));
		});
		userClientAdminIds.forEach(clientId -> {
			authorities.add(new MiningRole(String.format("client-%d-admin", clientId)));
		});
		if (isAdmin) {
			authorities.add(new MiningRole(String.format("admin")));
		}

		final Authentication auth = new UsernamePasswordAuthenticationToken("", "", authorities);
		final SecurityContext context = SecurityContextHolder.createEmptyContext();
		context.setAuthentication(auth);
		SecurityContextHolder.setContext(context);
	}
	
	private void assertModuleWithCustomProperty(final Map<String, Object> filter, final EntityId... expectedModule) {
		/* Execute GraphQL request for Module and get response data */
		final ModulePojo[] actualModules =
				getEntries(executeQueryBasedOnTemplate(projectId, QUERY_TEMPLATE_SORT_FILTER, filter), expectedModule.length, "modules", ModulePojo.class);
		
		/* Test that the length of expected module and fetched module are same */
		assertEquals(expectedModule.length, actualModules.length);
		
		/* Test that expected Modules are returned */
		for (int i = 0; i < expectedModule.length; i++) {
			assertEquals(expectedModule[i], actualModules[i].identity());
		}
	}
	
	/**
	 * Get array of entities from Response
	 *
	 * @param response the {@linkplain Response}
	 * @param noOfElements the expected number of elements
	 * @param typeName the name of the GraphQl type
	 * @param clazz the Entity class
	 * @return array of parsed Entity objects
	 */
	private static <T extends MiningPojo> T[] getEntries(final Response response, final int noOfElements, final String typeName,
			final Class<T> clazz) {
		response.path(typeName + ".totalElements").entity(Long.class).isEqualTo(Long.valueOf(noOfElements));
		@SuppressWarnings("unchecked")
		final T[] result = (T[]) Array.newInstance(clazz, noOfElements);
		for (int i = 0; i < noOfElements; i++) {
			final T respModule = response.path(String.format(typeName + ".content[%d]", Integer.valueOf(i))).entity(clazz).get();
			result[i] = respModule;
		}
		return result;
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
		eventPublisher.publishEvent(new CustomPropertiesModifiedEvent(Optional.of(projectId)));
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.ofParent(null).withName(className)), className + " should be existing in DB");
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.withParent(className).withName(customPropertyMetadata.getName())),
				customPropertyMetadata.getName() + " should exist in " + className);
		return customPropertyMetadata;
	}
	
	private EntityId[] createTestData() {
		projectId = createProject("Project_1");

		var moduleA = createTestModule(projectId, "Module_A_", "programs/Module_A_.cbl", "Test Data contentA", Technology.COBOL, Type.PROGRAM, null);
		var moduleB = createTestModule(projectId, "Module_B_", "programs/Module_B_.cbl", "Test Data contentB", Technology.COBOL, Type.PROGRAM, null);
		var moduleC = createTestModule(projectId, "Module_C_", "programs/Module_C_.cbl", "Test Data contentC", Technology.COBOL, Type.PROGRAM, null);
		final var moduleD = createTestModule(projectId, "Module_D_", "programs/Module_D_.cbl", "Test Data contentD", Technology.COBOL, Type.PROGRAM, null);
		var moduleE = createTestModule(projectId, "Module_E_", "programs/Module_E_.cbl", "Test Data contentE", Technology.COBOL, Type.PROGRAM, null);
		var moduleF = createTestModule(projectId, "Module_F_", "programs/Module_F_.cbl", "Test Data contentF", Technology.COBOL, Type.PROGRAM, null);
		var moduleG = createTestModule(projectId, "Module_G_", "programs/Module_G_.cbl", "Test Data contentG", Technology.COBOL, Type.PROGRAM, null);
		var moduleH = createTestModule(projectId, "Module_H_", "programs/Module_H_.cbl", "Test Data contentH", Technology.COBOL, Type.PROGRAM, null);
		
		final String className = ENTITY_NAME + "CustomProperties" + projectId.getNid();
		
		/* Creating a String repeater type custom property and adding this custom property to moduleA */
		final CustomPropertyMetadata propertyMetaData1 = createCustomPropertyMetadata("StringRepeaterType",
				"Test Module String Repeater Custom Label", "Test Module Custom Property for graph ql test", "EMBEDDEDLIST",
				CustomPropertyFieldType.DEFAULT, 1, false, null, projectId, ENTITY_NAME, className);
		
		/* Assign new String Repeater type property to moduleA */
		moduleA = createCustomProperty(propertyMetaData1.getName(), Arrays.asList("red", "blue", "green"), moduleA);
		
		/* Creating a Tag type custom property and adding this custom property to moduleB, moduleG and moduleH*/
		final String autoCompKey = "ModuleAutoCompletionKey";
		
		final CustomPropertyMetadata propertyMetaData2 = createCustomPropertyMetadata("TagType",
				"Test Module TAGS Custom Label", "Test Module Custom Property for graph ql test", "EMBEDDEDLIST",
				CustomPropertyFieldType.TAG, 0, false, autoCompKey, projectId, ENTITY_NAME, className);
		
		final List<String> tags = Arrays.asList("c", "pl", "natural", "java", "kotlin");
		final Map<String, Set<String>> map = new HashMap<>();
		map.put(autoCompKey, new HashSet<>(tags));
		customPropertiesService.putEnumValues(projectId, map);
		assertTrue(customPropertiesService.getEnumValues(projectId, autoCompKey).containsAll(tags), "All entries should match for key: " + autoCompKey);
		
		/* Assign new Tag type property to moduleB, moduleG, ModuleH */
		moduleB = createCustomProperty(propertyMetaData2.getName(), Arrays.asList("pl", "natural"), moduleB);
		moduleG = createCustomProperty(propertyMetaData2.getName(), Arrays.asList("c"), moduleG);
		moduleH = createCustomProperty(propertyMetaData2.getName(), Arrays.asList("java", "kotlin"), moduleH);
		
		/* Creating a String type custom property and adding this custom property to moduleC*/
		final CustomPropertyMetadata propertyMetaData3 = createCustomPropertyMetadata("StringType",
				"Test Module String Custom Label", "Test Module Custom Property for graph ql test", "STRING",
				CustomPropertyFieldType.DEFAULT, 1, false, null, projectId, ENTITY_NAME, className);
		
		/* Assign new String type property to moduleC */
		moduleC = createCustomProperty(propertyMetaData3.getName(), "PL1", moduleC);
		
		/* Assign new String Repeater type property to moduleE and moduleF */
		moduleE = createCustomProperty(propertyMetaData1.getName(), Arrays.asList("yellow"), moduleE);
		moduleF = createCustomProperty(propertyMetaData1.getName(), Arrays.asList("yellow", "black"), moduleF);
		
		/* Assign new Tag type property to moduleE and moduleF */
		moduleE = createCustomProperty(propertyMetaData2.getName(), Arrays.asList("yellow", "black"), moduleE);
		moduleF = createCustomProperty(propertyMetaData2.getName(), Arrays.asList("yellow", "black"), moduleF);
		
		/* Assign new String type property to moduleE and moduleF */
		moduleE = createCustomProperty(propertyMetaData3.getName(), "yellow black", moduleE);
		moduleF = createCustomProperty(propertyMetaData3.getName(), "yellow black", moduleF);
		/* Assign new String type property to moduleC */
		moduleG= createCustomProperty(propertyMetaData3.getName(), "PL2", moduleG);

		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId), Collections.emptyList(), false);
		return new EntityId[] { moduleA, moduleB, moduleC, moduleD, moduleE, moduleF, moduleG, moduleH };
	}
	
	private EntityId createCustomProperty(final String propertyMetaData, final Object value, final EntityId module) {
		return moduleService.update(new ModulePojoPrototype()
				.setUid(module.getUid())
				.setNid(module.getNid())
				.setCustomProperties(new NestedMap().set(ENTITY_NAME + "CustomProperties" + projectId.getNid(), propertyMetaData, value)));
	}
	
	private EntityId createProject(final String projectName) {
		given(assertNotNull(userRoleService).getClientAdminIds()).willReturn(Arrays.asList(LONG_ONE));
		given(assertNotNull(userRoleService).isAdmin()).willReturn(Boolean.TRUE);
		return projectService.create(new ProjectPojoPrototype()
			.setName(projectName)
			.setClient(EntityId.of(LONG_ONE))
			.setNatures(new HashSet<>(Arrays.asList(ProjectNature.MINING)))).identity();
	}
	
	private void createModuleWithAnnotation(final EntityId projectId) {
		/* Create a Module with Content */
		final String name = "Module" + projectId.getNid();
		final String moduleApath="programs/" + name + ".cbl";
		final var moduleA = createTestModule(projectId, name, moduleApath, MOD5_CONTENT, Technology.COBOL, Type.PROGRAM, null);
		/* Create a Annotation */
		createAnnotation("Annotation 1", AnnotationType.RULE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.RuleAnnotationCategory.BUSINESS_RULE.getName()));
		createAnnotation("Annotation 2", AnnotationType.RULE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.RuleAnnotationCategory.TECHNICAL_RULE.getName()));
		createAnnotation("Annotation 3", AnnotationType.RULE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.RuleAnnotationCategory.VALIDATION_RULE.getName()));
		createAnnotation("Annotation 4", AnnotationType.RULE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.RuleAnnotationCategory.ERROR_PROCESSING_RULE.getName()));
		createAnnotation("Annotation 5", AnnotationType.RULE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName()));
		createAnnotation("Annotation 6", AnnotationType.DATABASE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.DatabaseAnnotationCategory.READ.getName()));
		createAnnotation("Annotation 7", AnnotationType.DATABASE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.DatabaseAnnotationCategory.WRITE.getName()));
		createAnnotation("Annotation 8", AnnotationType.DATABASE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.DatabaseAnnotationCategory.DECLARE.getName()));
		createAnnotation("Annotation 8", AnnotationType.DATABASE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.DatabaseAnnotationCategory.DECLARE.getName()));
		createAnnotation("Annotation 9", AnnotationType.DATABASE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.DatabaseAnnotationCategory.CLOSE.getName()));
		createAnnotation("Annotation 10", AnnotationType.DATABASE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.DatabaseAnnotationCategory.CLOSE.getName()));
		createAnnotation("Annotation 11", AnnotationType.DATABASE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.DatabaseAnnotationCategory.CLOSE.getName()));
		createAnnotation("Annotation 12", AnnotationType.DATABASE, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
				ANNOTATION_CATEGORY_MAP.get(AnnotationCategory.DatabaseAnnotationCategory.CLOSE.getName()), new ModuleLocation(234, 30));

		new DefaultStoreAstExecutor(NullParsingSummarizer.INSTANCE).execute(moduleA, core);
	}
	
	private void createModuleWithAnnotation(final String moduleName, final EntityId projectId, final AnnotationType annotationType, final String category,
			final int count) {
		final String moduleApath="programs/" + moduleName + ".cbl";
		final var moduleA = createTestModule(projectId, moduleName, moduleApath, "Test Data", Technology.COBOL, Type.PROGRAM, null);
		for (int i=1; i<=count ; i++) {
			createAnnotation(category + count, annotationType, WorkingState.CANDIDATE, StringUtils.EMPTY, moduleA,
					ANNOTATION_CATEGORY_MAP.get(category));
		}
	}

	private EntityId createAnnotation(final String name, final AnnotationType annotationType, final WorkingState workingState,
			final String sourceAttachment, final EntityId moduleId, final Long categoryId, final ModuleLocation location) {
		final var annotation = new AnnotationPojoPrototype();
		annotation.setName(name);
		annotation.setType(annotationType);
		annotation.setState(workingState);
		annotation.setCategoryId(categoryId);
		annotation.setSourceAttachment(new BinaryString(sourceAttachment));
		annotation.setLocation(location);
		annotation.setModule(moduleId);
		annotation.setCreatedByUserId("1");
		return annotationService.create(annotation);
	}

	private EntityId createAnnotation(final String name, final AnnotationType annotationType, final WorkingState workingState,
			final String sourceAttachment, final EntityId moduleId, final Long categoryId) {
		return createAnnotation(name, annotationType, workingState, sourceAttachment, moduleId, categoryId, new ModuleLocation(0,0));
	}

	private Map<String, Long> loadRuleAnnotationCategories(final Map<String, Long> annotationCategoryMap) {
		annotationService.findCategories(q -> q.ofProject(EntityId.of(0L)).withTypes(Arrays.asList(AnnotationType.RULE, AnnotationType.DATABASE)))
			.forEach(a -> annotationCategoryMap.put(a.getName(), a.getId()));
		return annotationCategoryMap;
	}

	private FunctionalBlockPojoPrototype createFunctionalBlockPojoPrototype(final String name, final String desc, final ModuleLocation moduleLocation,
			@Nullable final List<UUID> children, @Nullable final Map<String, Object> flags, final EntityId projectId) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototype.setProject(projectId);
		functionalBlockPojoPrototype.setName(name);
		functionalBlockPojoPrototype.setDescription(desc);
		final List<ModulePart> moduleParts = new ArrayList<>();
		final ModulePart functionalBlockModulePart = new ModulePart("DummyHash1", moduleLocation);
		moduleParts.add(functionalBlockModulePart);
		functionalBlockPojoPrototype.setModuleParts(moduleParts);
		if (children != null) {
			functionalBlockPojoPrototype.setChildren(children);
		}
		if (flags != null) {
			functionalBlockPojoPrototype.setFlags(flags);
		}
		return functionalBlockPojoPrototype;
	}

	protected String getContent(final String file, final String dir) {
		final Path path = Paths.get(System.getProperty("user.dir"), dir, file);
		try {
			return new String(Files.readAllBytes(path), StandardCharsets.UTF_8).replaceAll("\\r\\n", "\n").replaceAll("\\r", "\n");
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			throw new IllegalStateException("Exception occured while file reading", e);
		}
	}
	
	@AfterAll
	void cleanUp() {
		//resetCustomProperties("Module", projectId.getNid());
	}
}
