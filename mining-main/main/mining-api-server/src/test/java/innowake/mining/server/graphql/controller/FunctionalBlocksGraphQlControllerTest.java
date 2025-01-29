/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import com.google.gson.Gson;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.ModuleBlockGeneration;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.authorization.graphql.GraphQlAuthorizationTests;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkCondition;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockStatus;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.tags.IntegrationTest;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.test.tester.GraphQlTester.Response;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.graphql.test.tester.WebGraphQlTester;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.test.web.servlet.client.MockMvcWebTestClient;
import org.springframework.web.context.WebApplicationContext;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Integration tests for {@link FunctionalBlocksGraphQlController}.
 */
@IntegrationTest
@TestMethodOrder(OrderAnnotation.class)
@TestPropertySource(properties = {"dbcutter-db-postgres.enabled=true"})
@TestPropertySource(properties = {"dbcutter-db-postgres.datasource.jdbc-url=${postgres.datasource.jdbc-url}"})
@TestPropertySource(properties = {"debug=true"})
class FunctionalBlocksGraphQlControllerTest extends DatabaseRelatedTest {
	@Autowired
	private WebApplicationContext webAppContext;
	@Autowired
	private FunctionalBlockService blockService;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private TaxonomyService taxonomyService;
	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private DataDictionaryService dataDictionaryService;
	@Autowired
	private FunctionalBlockGenerationService functionalBlockGenerationService;

	private static final EntityId PROJECT_ID_1 = EntityId.of(1L);
	private static final EntityId PROJECT_ID_2 = EntityId.of(2L);
	private static final EntityId PROJECT_ID_3 = EntityId.of(3L);
	private static final EntityId PROJECT_ID_4 = EntityId.of(4L);

	private EntityId projectId5 = EntityId.VOID;
	private EntityId projectId6 = EntityId.VOID;

	@Nullable
	private WebGraphQlTester tester;

	final Gson gson = new Gson();

	private static final String QUERY_TEMPLATE_WITH_MODULE_PART = "query ($projectId: Long!, $page: Int, $size: Int"
			+ " $filterObject: FilterObject_functionalBlocks) {"
			+ "functionalBlocks(projectId: $projectId,  page : $page, size: $size, filterObject: $filterObject) {" +
					"totalElements," +
					"content {" +
						"name," +
						"description," +
						"uid," +
						"status," +
						"project{" +
							"uid," +
							"id" + 
						"}" +
						"moduleParts{" +
							"moduleLinkHash," +
							"location{" +
								"length," +
								"offset" +
							"}" +
						"}" +
						"flags," +
						"type," +
						"outdatedBlock," +
						"blocksWithDeletedUB," +
						"lowerBoundAccessTypes," +
						"}" +
					"size" +
				"}" +
			"}";
	
	private static final String QUERY_TEMPLATE_BY_UID = "query ($projectId: Long, $uid: UUID) {"
			+ "functionalBlock(projectId: $projectId, uid: $uid) {" +
						"name," +
						"description," +
						"uid," +
						"project{" +
							"uid," +
							"id" +
						"}" +
						"flags," +
						"type" +
				"}" +
			"}";
	
	private static final String QUERY_TEMPLATE_CHILDREN = "query ($projectId: Long!, $filterObject: FilterObject_functionalBlocks) {"
			+ "functionalBlocks(projectId: $projectId, filterObject: $filterObject) {" +
					"totalElements," +
					"content {" +
						"children {" +
							"content{" +
								"name," +
								"uid" +
							"}" +
						"}" +
						"parents{" +
							"content{" +
								"name," + 
								"uid" +
							"}" +
						"}" +
						"childrenDeep{" +
							"size," +
							"content{" +
								"uid," +
								"name" +
								"}" +
							"}" +
						"}" +
					"}" +
				"}";
	
	private static final String QUERY_TEMPLATE_LINK_CONDITION = "query ($projectId: Long!, $uid: UUID) {"
			+ "functionalBlock(projectId: $projectId, uid: $uid) {" +
						"name," +
						"description," +
						"uid," +
						"links {" +
							"childA {" +
								"flags," +
								"description," +
								"uid," +
								"name" +
							"}" +
							"childB { " +
								"flags," +
								"description, " +
								"uid," +
								"name" +
							"}" +
							"condition {" +
								"uid," +
								"label" +
							"}" +
						"}" +
					"}" +
				"}";

	private static final String QUERY_TEMPLATE_BY_RESOLVED_MODULE_PART = "query ($projectId: Long!, $uid: UUID) {"
			+ "functionalBlock(projectId: $projectId, uid: $uid) {" +
					"resolvedModuleParts{" +
						"referencedTaxonomies{" +
							"project," +
							"name" +
							"}" +
						"module{" +
							"linkHash," +
							"name" +
						"}" +
					"}" +
				"}" +
			"}";

	private static final String QUERY_TEMPLATE_FILTER_BY_RESOLVED_MODULE_PART_NAME =
			"query ($projectId: Long!, $filterObject: FilterObject_functionalBlocks) {" +
				"functionalBlocks(projectId: $projectId, filterObject: $filterObject) {" +
					"content {" +
						"name " +
						"peers {" +
							"content {" +
								"name" +
							"}" +
							"totalElements" +
						"}" +
						"resolvedModuleParts{" +
							"module{" +
								"linkHash," +
								"name" +
							"}" +
						"} " +
					"}" +
					"totalElements" +
				"}" +
			"}";
	
	private static final String QUERY_TEMPLATE_BY_GENERATED_FROM = "query ($projectId: Long!, $uid: UUID) {"
			+ "functionalBlock(projectId: $projectId, uid: $uid) {" +
				"generatedFrom{" +
					"moduleLinkHash," +
					"annotationId," +
					"moduleContentHash," +
					"module{" +
						"linkHash" +
						"}" +
					"}" +
				"}" +
			"}";

	private static final String QUERY_TEMPLATE_GROUP = "query ($uid: UUID, $filterObject: FilterObject_functionalBlocks) {" +
			  "functionalBlock(uid: $uid) { " +
			    "resolvedModuleParts {" +
			      "module {" +
			        "name" +
			      "}" +
			    "}" +
			    "childrenDeep(filterObject: $filterObject) {" +
			      "content {" +
			        "generatedFrom {" +
			          "annotation {" +
			            "location {" +
			                "offset, " +
			                "length" +
			            "}" +
						"sourceAttachment " +
						"linkedDataDictionaryEntries { " +
							"name " +
						"} " +
			          "}" +
			        "}" +
			      "}" +
			    "}" +
			  "}" +
			 "}" ;
	
	private static final String ANNOTATION_BLOCK_QUERY_TEMPLATE = "query ($uid: UUID){" +
			  "functionalBlock(uid: $uid) { " +
			    "resolvedModuleParts {" +
			      "module {" +
			        "name" +
			      "}" +
			    "}" +
			    "generatedFrom {" +
			     "annotation {" +
			       "location {" +
						"offset, " +
			            "length" +
			        "}" +
			        "sourceAttachment " +
			        "linkedDataDictionaryEntries { " +
						"name " +
					"} " +
			     "}" +
			    "}" +
			  "}" +
			 "}" ;
	
	private static final String ANNOTATIONS_BLOCK_WITH_FILTER_QUERY_TEMPLATE = "query ($projectId: Long!, $filterObject: FilterObject_functionalBlocks) { " +
			   "functionalBlocks(projectId: $projectId, filterObject: $filterObject ) {" +
			    "content {" +
			      "name, " +
		           "generatedFrom {" +
		             "annotation {" +
		              "sourceAttachment " +
						"linkedDataDictionaryEntries { " +
							"name " +
						"} " +
		             "}" +
		           "}" +
		          "}" +
		         "}" + 
		       "}";
	
	private static final String QUERY_TEMPLATE_AGGREGATION = "query ($projectId: Long!) { " +
			"functionalBlocks(projectId: $projectId) {" +
				" totalElements, " +
				" aggregationCount :aggregations {" +
					" groupBy {" +
						" TYPE" +
						"}" +
					" fields {" +
						" UID {" +
							" COUNT" +
							" }" +
						" }" +
					" }" +
				" aggregationList :aggregations {" +
					" groupBy {" +
						" TYPE" +
						"}" +
					" fields {" +
						" UID {" +
							" LIST" +
							" }" +
						" }" +
					" }" +
				" }" +
			" }";

	private static final String QUERY_TEMPLATE_MODULE_AGGREGATION = "query ($projectId: Long!) { " +
			"functionalBlocks(projectId: $projectId) {" +
				" aggregations {" +
					" groupBy {" +
						" REFERENCED_MODULE_TYPE" +
						" REFERENCED_MODULE_TECHNOLOGY" +
					" }" +
					" fields {" +
						" UID {" +
							" COUNT" +
						" }" +
					" }" +
				" }" +
			" }" +
		" }";

	private static final String QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK = "  query ($projectId: Long, $functionalBlocks: [UUID], " +
			" $sortObject: [SortObject_reachabilityData], $includeIntermediateModulesPerAccessModule : Boolean, " +
			" $filterObject: FilterObject_reachabilityData) {" +
			" reachabilityData( projectId: $projectId, functionalBlocks: $functionalBlocks," +
			" sortObject: $sortObject, includeIntermediateModulesPerAccessModule: $includeIntermediateModulesPerAccessModule, filterObject: $filterObject){" +
			" content {" +
				" upperBoundModules {" +
					" name" +
					" technology " +
					" type" +
					" }" +
				" dataAccessType" +
				" lowerBoundModules {" +
					" name" +
					" technology " +
					" type" +
				" }" +
				" accessModules {" +
					" name" +
				" }" +
				"taxonomy_lower_bound: moduleTaxonomies(moduleType: \"LowerBound\") " +
				"taxonomy_upper_bound: moduleTaxonomies(moduleType: \"UpperBound\") " +
				"intermediateModulesData {" +
					" name" +
				"}" +
			" }" +
			" totalElements" +
			" size" +
			" }" +
			"}";

	private static final String QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK_WITH_SQL_DETAILS = "  query ($projectId: Long, $functionalBlocks: [UUID], "
			+ "$sortObject: [SortObject_reachabilityData], $includeIntermediateModulesPerAccessModule : Boolean) {" +
			" reachabilityData( projectId: $projectId, functionalBlocks: $functionalBlocks," +
			" sortObject: $sortObject, includeIntermediateModulesPerAccessModule: $includeIntermediateModulesPerAccessModule){" +
			" content {" +
			" upperBoundModules {" +
			" name" +
			" technology " +
			" type" +
			" }" +
			" dataAccessType" +
			" lowerBoundModules {" +
			" name" +
			" technology " +
			" type" +
			" }" +
			" accessModules {" +
			" name" +
			" }" +
			"sqlDetails {" +
			" query" +
			" tableName" +
			" conditional" +
			" nonconditional" +
			" queryType" +
			" }" +
			"taxonomy_lower_bound: moduleTaxonomies(moduleType: \"LowerBound\") " +
			"taxonomy_upper_bound: moduleTaxonomies(moduleType: \"UpperBound\") " +
			"intermediateModulesData {" +
			" name" +
			"}" +
			" }" +
			" totalElements" +
			" size" +
			" }" +
			"}";

	private static final String QUERY_TEMPLATE_BY_UID_CHILDREN_DEEP_WITH_AGGREGATION = "query ($projectId: Long, $uid: UUID) {"
			+ "functionalBlock(projectId: $projectId, uid: $uid) {" +
						"childrenDeep{" +
							"aggregations{" +
							  "fields{" +
							   "UID{" +
							    "LIST" +
							   "}" +
							  "}" +
							 "}" +
						"}" +
				"}" +
			"}";

	private static final String QUERY_TEMPLATE_FOR_CONTEXT_SHARING_BY_UID_CHILDREN =
			"query ($filterObject: FilterObject_functionalBlocks, $projectId : Long, $uid : UUID) {" +
					"functionalBlock(projectId: $projectId, uid: $uid) {" +
						"childrenDeep(filterObject: $filterObject) {" +
							"content {" +
								"children {" +
									"aggregations{" +
										"fields{" +
											"UID{" +
												"LIST" +
												"}" +
											"}" +
										"}" +
									"}" +
								"}" +
							"}" +
						"}" +
					"}";

	private static final String QUERY_TEMPLATE_FOR_CONTEXT_SHARING_BY_UID_PARENT =
			"query ($filterObject: FilterObject_functionalBlocks, $projectId : Long, $uid : UUID) {" +
					"functionalBlock(projectId: $projectId, uid: $uid) {" +
						"childrenDeep(filterObject: $filterObject) {" +
							"content {" +
								"parents {" +
									"aggregations{" +
										"fields{" +
											"UID{" +
												"LIST" +
												"}" +
											"}" +
										"}" +
									"}" +
								"}" +
							"}" +
						"}" +
					"}";

	private static final String QUERY_TEMPLATE_FUNCTIONAL_BLOCKS_WITH_FILTER =
			"query ($projectId : Long, $page: Int, $size: Int, $sortObject: [SortObject_functionalBlocks], $filterObject: FilterObject_functionalBlocks) {" +
					"functionalBlocks(projectId: $projectId, page : $page, size: $size, sortObject: $sortObject, filterObject: $filterObject) {" +
					"totalElements, " +
					"content {" +
						"name, " +
						"uid, " +
						"type, " +
						"childrenDeep {" +
							"content {" +
								"name," +
								"uid," +
								"flags" +
							"}" +
						"}" +
					"}" +
				"}" +
			"}";

	private static final String QUERY_TEMPLATE_FOR_FUNCTIONAL_BLOCKS_CHILDREN_DEEP_WITH_FILTERS =
			"query ($projectId : Long, $filterObject: FilterObject_functionalBlocks,  $childFilterObject: FilterObject_functionalBlocks) {" +
					"functionalBlocks(projectId: $projectId, filterObject: $filterObject) {" +
						"totalElements, " +
						"content {" +
							"name, " +
							"type, " +
							"childrenDeep (filterObject: $childFilterObject) {" +
								"size," +
								"content {" +
									"name," +
									"uid," +
									"flags" +
								"}" +
							"}" +
						"}" +
					"}" +
				"}";

	private static final String QUERY_TEMPLATE_FOR_FUNCTIONAL_BLOCKS_CHILDREN_WITH_FILTERS =
			"query ($projectId : Long, $filterObject: FilterObject_functionalBlocks,  $childFilterObject: FilterObject_functionalBlocks) {" +
					"functionalBlocks(projectId: $projectId, filterObject: $filterObject) {" +
						"totalElements, " +
						"content {" +
							"name, " +
							"uid, " +
							"type, " +
							"children(filterObject: $childFilterObject) {" +
								"size," +
								"content {" +
									"name," +
									"uid," +
									"flags" +
								"}" +
							"}" +
						"}" +
					"}" +
				"}";

	private static final String FILTER_OBJECT_FUNCTIONAL_GROUP = "{content_type: { eq: FUNCTIONAL_GROUP }}";
	private static final String FILTER_OBJECT_FUNCTIONAL_UNIT = "{content_type: { eq: FUNCTIONAL_UNIT }}";
	private static final String FILTER_OBJECT_FOR_FUNCTIONAL_UNITS_WHICH_ARE_NOT_PART_OF_ANY_GROUP = "{content_type: {eq: FUNCTIONAL_UNIT},"
			+ "content_parents: {notEq: {content_type: {eq: FUNCTIONAL_GROUP}}}}";
	private static final String FILTER_OBJECT_FOR_FUNCTIONAL_GROUPS_WHICH_ARE_NOT_PART_OF_ANY_OTHER_GROUP = "{content_type: {eq: FUNCTIONAL_GROUP},"
			+ "content_parents: {notEq: {content_type: {eq: FUNCTIONAL_GROUP}}}}";

	@BeforeAll
	void init() {
		final WebTestClient client = MockMvcWebTestClient.bindToApplicationContext(webAppContext)
				.configureClient()
				.baseUrl(GraphQlAuthorizationTests.GRAPHQL_ENDPOINT)
				.build();

		tester = HttpGraphQlTester.create(client);
		projectId5 = createProject(5L).identity();
		projectId6 = createProject(6L).identity();
	}

	@Override
	protected List<String> getAdditionalPgScriptFile() {
		return List.of("test-data-module-sqldetails");
	}

	@Order(1)
	@DisplayName("Test for Functional Blocks with children and children deep")
	@Test
	void testFunctionalBlockWithChildren() {
		/* The hierarchy of functional Blocks is as follow:
		 *
		 *            rootBlock
		 *               |
		 *           childBlock
		 *               |
		 *          deepCHildBlock
		 */
		final FunctionalBlockPojoPrototype deepCHildBlock = createFunctionalBlockPojoPrototype("deep child block",
				"description", new ModuleLocation(100, 20), null, null, PROJECT_ID_1);
		final UUID childUid1 = blockService.create(deepCHildBlock);

		final List<UUID> childofChild = Arrays.asList(childUid1);

		final FunctionalBlockPojoPrototype childBlock = createFunctionalBlockPojoPrototype("child block",
				"description", new ModuleLocation(100, 20), childofChild, null, PROJECT_ID_1);

		final UUID childUid2 = blockService.create(childBlock);

		final List<UUID> children = Arrays.asList(childUid2);

		final FunctionalBlockPojoPrototype rootBlock = createFunctionalBlockPojoPrototype("root Block",
				"root Block description", new ModuleLocation(150, 30), children,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), PROJECT_ID_1);
		blockService.create(rootBlock);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_CHILDREN)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("filterObject", gson.fromJson(FILTER_OBJECT_FUNCTIONAL_GROUP, Object.class))
				.execute();
		response.path("functionalBlocks.content[0].children.content[0].uid").matchesJsonStrictly("\"" + childUid2.toString() + "\"");
		response.path("functionalBlocks.content[0].childrenDeep.size").matchesJsonStrictly("2");
	}

	@Order(2)
	@DisplayName("Test for Functional Blocks with Module part")
	@Test
	void testFunctionalBlockWithModulePart() {
		final FunctionalBlockPojoPrototype functionalBlock = createFunctionalBlockPojoPrototype(" Functional_block",
				" description", new ModuleLocation(200, 30), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), PROJECT_ID_1);
		final UUID functionalBlockPojoUid = blockService.create(functionalBlock);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson(FILTER_OBJECT_FUNCTIONAL_UNIT, Object.class))
				.execute();

		final FunctionalBlockPojo[] funtionalBlocks = getFunctionalBlocksFromJson(response, 1);
		assertEquals(functionalBlockPojoUid, funtionalBlocks[0].getUid(), "The Functional block uid is not correct");
		assertEquals("DummyHash1", funtionalBlocks[0].getModuleParts().get(0).getModuleLinkHash(),
				"The linkHash of Module part is not correct for the functionalBlock.");
	}

	@Order(3)
	@DisplayName("Test for Functional Blocks with UID")
	@Test
	void testFunctionalBlockWithUid() {
		final FunctionalBlockPojoPrototype functionalBlock = createFunctionalBlockPojoPrototype("functionalBlock",
				"1st description", new ModuleLocation(100, 20), null, null, PROJECT_ID_1);

		final UUID uid = blockService.create(functionalBlock);
		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_BY_UID)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("uid", uid.toString())
				.execute();
		final FunctionalBlockPojo funtionalBlocks = getFunctionalBlockFromJson(response);
		assertEquals(uid, funtionalBlocks.getUid(), "The UUID of functional Block in response is not correct.");
		assertEquals(functionalBlock.name.get(), funtionalBlocks.getName(), "The Functional block's name is not correct.");
	}

	@Order(4)
	@DisplayName("Test for Functional Blocks with UID and without projectId")
	@Test
	void testFunctionalBlockWithUidAndWithoutProjectId() {
		/* Note: this works only in no-authorization profile as otherwise you would get ACCESS_DENIED when not specifying projectId */
		final FunctionalBlockPojoPrototype functionalBlock = createFunctionalBlockPojoPrototype("functionalBlock",
				"1st description", new ModuleLocation(100, 20), null, null, PROJECT_ID_1);

		final UUID uid = blockService.create(functionalBlock);
		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_BY_UID)
				.variable("uid", uid.toString())
				.execute();
		final FunctionalBlockPojo funtionalBlocks = getFunctionalBlockFromJson(response);
		assertEquals(uid, funtionalBlocks.getUid(), "The UUID of functional Block in response is not correct.");
		assertEquals(functionalBlock.name.get(), funtionalBlocks.getName(), "The Functional block's name is not correct.");
	}

	@Order(5)
	@DisplayName("Test for Functional Blocks with Links")
	@Test
	void testFunctionalBlocksLinks() {
		/* The hierarchy of functional Blocks is as follow:
		 *
		 *                       parentBlock
		 *                           |
		 *               --------------------------
		 *              |                           |
		 *        childBlockA                   childBlockBW
		 */
		final FunctionalBlockPojoPrototype childBlockA = createFunctionalBlockPojoPrototype("1st Child",
				"1st Child description", new ModuleLocation(100, 20), null, null, PROJECT_ID_1);

		final UUID functionalblockUidA = blockService.create(childBlockA);

		final FunctionalBlockPojoPrototype childBlockB = createFunctionalBlockPojoPrototype("2nd Child",
				"2nd Child description", new ModuleLocation(100, 20), null, null, PROJECT_ID_1);

		final UUID functionalblockUidB = blockService.create(childBlockB);

		final List<UUID> children = Arrays.asList(functionalblockUidA, functionalblockUidB);

		final UUID conditionUid = UUID.randomUUID();
		final FunctionalBlockLinkCondition condition = new FunctionalBlockLinkCondition(conditionUid, "condition label");

		final FunctionalBlockPojoPrototype parentBlock = createFunctionalBlockPojoPrototype("Functional_block", "description",
				new ModuleLocation(150, 30), children, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), PROJECT_ID_1);

		final UUID functionalBlockUid = blockService.create(parentBlock);

		blockService.setLinks(functionalBlockUid, Collections.singletonList(
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockUid, functionalblockUidA, functionalblockUidB,
						null, Collections.singletonMap(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.REACHABILITY), condition)));

		final Response response = assertNotNull(tester)
				.document(QUERY_TEMPLATE_LINK_CONDITION)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("uid", functionalBlockUid)
				.execute();

		response.path("functionalBlock.links[0].condition")
				.matchesJsonStrictly("{\"uid\": \"" + conditionUid.toString() + "\", \"label\": \"condition label\" }");
		final String childA = "{\"flags\":{},\"description\":\"1st Child description\"," +
				"\"uid\":\"" + functionalblockUidA.toString() + "\",\"name\":\"1st Child\"}";
		response.path("functionalBlock.links[0].childA").matchesJsonStrictly(childA);

		final String childB = "{\"flags\":{},\"description\":\"2nd Child description\"," +
				"\"uid\":\"" + functionalblockUidB.toString() + "\",\"name\":\"2nd Child\"}";
		response.path("functionalBlock.links[0].childB").matchesJsonStrictly(childB);
	}

	@Order(6)
	@DisplayName("Test for Functional Blocks with resolved Module part")
	@Test
	void testFunctionalBlocksResolvedModulePart() {
		final var module1 = createTestModule("MODULE_RESOLVED", "src/Testresolved.cbl", PROJECT_ID_1, null, null);

		final List<ResolvedModulePart> resolvedModuleParts = new ArrayList<>();

		final FunctionalBlockPojoPrototype functionalBlock = createFunctionalBlockPojoPrototype("functional block",
				"description", new ModuleLocation(100, 20), null, null, PROJECT_ID_1);

		final UUID uid = blockService.create(functionalBlock);

		final ResolvedModulePart resolvedModulePart = new ResolvedModulePart(module1.identity(), new ModuleLocation(100, 250));
		resolvedModuleParts.add(resolvedModulePart);
		blockService.setResolvedModuleParts(uid, resolvedModuleParts);

		final Long category = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype()
				.setName("Business Taxonomies")
				.setProject(PROJECT_ID_1));
		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype()
				.setName("DataDomain2")
				.setProject(PROJECT_ID_1)
				.setCategoryId(category));
		final var taxonomy1 = taxonomyService.get(taxonomyService.create(new TaxonomyPojoPrototype()
				.setName("Taxonomy A")
				.setProject(PROJECT_ID_1)
				.setType(type)));
		taxonomyService.createModuleLink(module1.getUid(), taxonomy1.identity());

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_BY_RESOLVED_MODULE_PART)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("uid", uid)
				.execute();
		response.path("functionalBlock.resolvedModuleParts[0].referencedTaxonomies[0].name").matchesJsonStrictly("\"" + taxonomy1.getName() + "\"");
		response.path("functionalBlock.resolvedModuleParts[0].module.name").matchesJsonStrictly("\"" + module1.getName() + "\"");
	}

	@Order(7)
	@DisplayName("Test for Functional Blocks with Generated From")
	@Test
	void testFunctionalBlocksGeneratedFrom() {
		final var module1 = createTestModule("Generated From", "src/Test.cbl", PROJECT_ID_1, null, null);

		final var annotation1 = createAnnotation("annotation", AnnotationType.DATABASE, WorkingState.CANDIDATE,
				"", module1.identity(), "1", 0, 1);

		final FunctionalBlockPojoPrototype functionalBlock= createFunctionalBlockPojoPrototype("functional block",
				"description", new ModuleLocation(100, 20), null, null, PROJECT_ID_1);

		final UUID functionalBlockUid = blockService.create(functionalBlock);
		final GeneratedFrom generatedFrom1 = GeneratedFrom.fromAnnotation(annotation1.identity());
		blockService.setGeneratedFrom(functionalBlockUid, generatedFrom1);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_BY_GENERATED_FROM)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("uid", functionalBlockUid).execute();
		response.path("functionalBlock.generatedFrom.annotationId").matchesJsonStrictly(annotation1.getId().toString());
	}

	@Order(8)
	@Disabled("Flaky (WMIN-11291)")
	@DisplayName("Retrieving functional groups with all associated information including Annotation and linked Data Dictionary of that annotation")
	@Test
	void retrieveFunctionalGroupsWithAnnotationAndLinkedDataDictionaryInfo() {
		final var module1 = createTestModule("Group", "src/Group.cbl", PROJECT_ID_3, null, null);

		final var annotation1 = createAnnotation("annotation1", AnnotationType.DATABASE, WorkingState.CANDIDATE,
				"New Annotation", module1.identity(), "createdByUser", 2900, 500);
		final var annotation2 = createAnnotation("annotation2", AnnotationType.DATABASE, WorkingState.CANDIDATE,
				"New Annotation2", module1.identity(), "createdByUser", 2800, 90);
		assertNotNull(annotation2);

		final var dataDictionaryEntry = createDataDictionaryEntry(module1.identity(), "DataDictionaryEntry1", false,
				new ModuleLocation(3000, 100));

		dataDictionaryService.linkAnnotations(dataDictionaryEntry.identity(), annotation1.identity());
		dataDictionaryService.linkAnnotations(dataDictionaryEntry.identity(), annotation2.identity());

		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_3),
				module1.identity());

		final List<FunctionalBlockPojo> results = blockService.find(q -> q.withType(FunctionalBlockType.FUNCTIONAL_UNIT));
		final List<UUID> filteredResults = results.stream().filter(block -> block.getName().equals("annotation1") ||
				block.getName().equals("annotation2")).map(FunctionalBlockPojo::getUid).collect(Collectors.toList());
		assertEquals(2, filteredResults.size());

		final UUID groupFunctionalBlock = blockService.create(createFunctionalBlockPojoPrototype("Group functional block",
				"group type fb", new ModuleLocation(100, 20), filteredResults,
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)), PROJECT_ID_3));
		assertNotNull(groupFunctionalBlock);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_GROUP)
				.variable("uid", groupFunctionalBlock)
				.variable("filterObject", gson.fromJson(FILTER_OBJECT_FUNCTIONAL_UNIT, Object.class))
				.execute();

		final List<Integer> offsets = new ArrayList<>();
		offsets.add(response.path("functionalBlock.childrenDeep.content[0].generatedFrom.annotation.location.offset")
				.entity(Integer.class).get());
		offsets.add(response.path("functionalBlock.childrenDeep.content[1].generatedFrom.annotation.location.offset")
				.entity(Integer.class).get());
		assertTrue(offsets.contains(assertNotNull(annotation1.getLocation().orElse(null)).getOffset()));
		assertTrue(offsets.contains(assertNotNull(annotation2.getLocation().orElse(null)).getOffset()));

		final List<String> dataElements = new ArrayList<>();
		//linkedDataDictionaryEntries or dataDictionaryEntries
		dataElements.add(response.path("functionalBlock.childrenDeep.content[0].generatedFrom.annotation.linkedDataDictionaryEntries[0].name")
				.entity(String.class).get());
		dataElements.add(response.path("functionalBlock.childrenDeep.content[1].generatedFrom.annotation.linkedDataDictionaryEntries[0].name")
				.entity(String.class).get());
		assertTrue(dataElements.contains(dataDictionaryEntry.getName()));

		final List<String> sourceAttachments = new ArrayList<>();
		sourceAttachments.add(response.path("functionalBlock.childrenDeep.content[0].generatedFrom.annotation.sourceAttachment")
				.entity(String.class).get());
		assertTrue(sourceAttachments.contains(assertNotNull(annotation1.getSourceAttachment()).toString()));
	}

	@Order(9)
	@DisplayName("Single Annotation is Selected")
	@Test
	void retrieveSingleFunctionalUnitWithAnnotationAndLinkedDataDictionaryInfo() {
		final var module1 = createTestModule("Group2", "src/Group2.cbl", PROJECT_ID_3, null, null);

		final var annotation = createAnnotation("annotationA", AnnotationType.DATABASE, WorkingState.CANDIDATE,
				"Source Attachment", module1.identity(), "createdByUser", 2900, 500);

		final var dataDictionaryEntry = createDataDictionaryEntry(module1.identity(), "DataDictionaryEntryA", false,
				new ModuleLocation(3000, 100));

		dataDictionaryService.linkAnnotations(dataDictionaryEntry.identity(), annotation.identity());

		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_3),
				module1.identity());
		final List<FunctionalBlockPojo> results = blockService.find(q -> q.withType(FunctionalBlockType.FUNCTIONAL_UNIT));
		final List<FunctionalBlockPojo> filteredResults = results.stream().filter(block -> block.getName().equals("annotationA")).collect(Collectors.toList());
		assertEquals(1, filteredResults.size());

		final Response response = assertNotNull(tester).document(ANNOTATION_BLOCK_QUERY_TEMPLATE)
				.variable("uid", filteredResults.get(0).getUid())
				.execute();

		response.path("functionalBlock.generatedFrom.annotation.location.offset")
				.matchesJsonStrictly(assertNotNull(annotation.getLocation().orElse(null)).getOffset().toString());
		response.path("functionalBlock.generatedFrom.annotation.location.length")
				.matchesJsonStrictly(assertNotNull(annotation.getLocation().orElse(null)).getLength().toString());
		response.path("functionalBlock.generatedFrom.annotation.linkedDataDictionaryEntries[0].name")
				.matchesJsonStrictly("\"" + dataDictionaryEntry.getName() + "\"");
		response.path("functionalBlock.generatedFrom.annotation.sourceAttachment")
				.matchesJsonStrictly("\"" + assertNotNull(annotation.getSourceAttachment().get()).toString() + "\"");
	}

	@Order(10)
	@DisplayName("Functional block units with filter where the type equals FUNCTIONAL_UNIT and not having parent where the type equals FUNCTIONAL_GROUP")
	@Test
	void functionalBlockUnitsWhichAreNotPartOfAnyGroup() {
		final var module = createTestModule("test", "src/test.cbl", PROJECT_ID_4, null, null);

		final var annotation = createAnnotation("NotPartOfGroup", AnnotationType.DATABASE, WorkingState.CANDIDATE,
				"Annotations SourceAttachment", module.identity(), "createdByUser", 2900, 500);
		assertNotNull(annotation);
		final var dataDictionaryEntry = createDataDictionaryEntry(module.identity(), "NewDataDictionaryEntry", false,
				new ModuleLocation(3000, 100));

		dataDictionaryService.linkAnnotations(dataDictionaryEntry.identity(), annotation.identity());

		final var annotation2 = createAnnotation("PartOfGroup", AnnotationType.DATABASE, WorkingState.CANDIDATE,
				"PartOfGroupContent", module.identity(), "createdByUser", 290, 50);
		assertNotNull(annotation2);

		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_4),
				module.identity());

		final List<FunctionalBlockPojo> results = blockService.find(q -> q.withType(FunctionalBlockType.FUNCTIONAL_UNIT));
		final List<FunctionalBlockPojo> filteredResults = results.stream().filter(block -> block.getName().equals("PartOfGroup")).collect(Collectors.toList());
		assertEquals(1, filteredResults.size());

		final UUID groupFunctionalBlock = blockService.create(createFunctionalBlockPojoPrototype("Group functional block",
				"group type fb", new ModuleLocation(100, 20), Collections.singletonList(filteredResults.get(0).getUid()),
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)), PROJECT_ID_4));
		assertNotNull(groupFunctionalBlock);

		final Response response = assertNotNull(tester).document(ANNOTATIONS_BLOCK_WITH_FILTER_QUERY_TEMPLATE)
				.variable("projectId", PROJECT_ID_4.getNid())
				.variable("filterObject", gson.fromJson(FILTER_OBJECT_FOR_FUNCTIONAL_UNITS_WHICH_ARE_NOT_PART_OF_ANY_GROUP, Object.class))
				.execute();
		response.path("data.functionalBlocks.content[0].name").matchesJsonStrictly("\"" + annotation.getName().toString() + "\"");
		response.path("data.functionalBlocks.content[0].generatedFrom.annotation.sourceAttachment")
				.matchesJsonStrictly("\"" + assertNotNull(annotation.getSourceAttachment().get()).toString() + "\"");
		response.path("data.functionalBlocks.content[0].generatedFrom.annotation.linkedDataDictionaryEntries[0].name")
				.matchesJsonStrictly("\"" + dataDictionaryEntry.getName().toString() + "\"");
	}

	@Order(11)
	@DisplayName("Functional block units with filter FUNCTIONAL_GROUPs that are not part of another FUNCTIONAL_GROUP")
	@Test
	void functionalBlockGroupsWhichAreNotPartOfAnyOtherGroups() {
		final var module = createTestModule("test2", "src/test2.cbl", PROJECT_ID_4, null, null);

		final var annotation = createAnnotation("Annotation Block1", AnnotationType.DATABASE, WorkingState.CANDIDATE,
				"", module.identity(), "createdByUser", 2900, 500);
		assertNotNull(annotation);

		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_4),
				module.identity());

		final List<FunctionalBlockPojo> results = blockService.find(q -> q.withType(FunctionalBlockType.FUNCTIONAL_UNIT));
		final List<FunctionalBlockPojo> filteredResults = results.stream().filter(block -> block.getName().equals("Annotation Block1"))
				.collect(Collectors.toList());
		assertEquals(1, filteredResults.size());

		final UUID groupFunctionalBlock1 = blockService.create(createFunctionalBlockPojoPrototype("Group functional block1",
				"group type fb", new ModuleLocation(100, 20), Collections.singletonList(filteredResults.get(0).getUid()),
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)), PROJECT_ID_4));

		final UUID groupFunctionalBlock2 = blockService.create(createFunctionalBlockPojoPrototype("Group functional block2",
				"group type fb", new ModuleLocation(100, 20), Collections.singletonList(groupFunctionalBlock1),
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)), PROJECT_ID_4));
		assertNotNull(groupFunctionalBlock2);

		final Response response = assertNotNull(tester).document(ANNOTATIONS_BLOCK_WITH_FILTER_QUERY_TEMPLATE)
				.variable("projectId", PROJECT_ID_4.getNid())
				.variable("filterObject", gson.fromJson(FILTER_OBJECT_FOR_FUNCTIONAL_GROUPS_WHICH_ARE_NOT_PART_OF_ANY_OTHER_GROUP, Object.class))
				.execute();
		final List<String> groupNames = new ArrayList<>();
		groupNames.add(response.path("data.functionalBlocks.content[0].name").entity(String.class).get());
		groupNames.add(response.path("data.functionalBlocks.content[1].name").entity(String.class).get());
		assertTrue(groupNames.contains("Group functional block2"));
		assertFalse(groupNames.contains("Group functional block1"));
	}

	@Order(12)
	@Test
	void testFunctionalBlockAggregation() {
		blockService.create(createFunctionalBlockPojoPrototype("Functional_block1test",
				"Functional_block_description1", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), PROJECT_ID_1));

		final UUID functionalBlockPojoUUID1 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block1",
				"Functional_block_description1", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), PROJECT_ID_2));
		final UUID functionalBlockPojoUUID2 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block2",
				"Functional_block_description2", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), PROJECT_ID_2));
		final UUID functionalBlockPojoUUID3 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block3",
				"Functional_block_description3", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), PROJECT_ID_2));
		final UUID functionalBlockPojoUUID4 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block4",
				"Functional_block_description4", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY)), PROJECT_ID_2));
		final UUID functionalBlockPojoUUID5 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block5",
				"Functional_block_description5", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY)), PROJECT_ID_2));
		final UUID functionalBlockPojoUUID6 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block6",
				"Functional_block_description6", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MODULE)), PROJECT_ID_2));

		/* testing Count aggregation for functionalBlock. */
		final Response response1 = assertNotNull(tester).document(QUERY_TEMPLATE_AGGREGATION).variable("projectId", PROJECT_ID_2.getNid()).execute();
		final List<Object> aggregationList1 = response1.path("functionalBlocks.aggregationCount").entityList(Object.class).get();
		assertEquals(3, aggregationList1.size());
		final Map<String, Long> aggregationResult1 = IntStream.range(0, aggregationList1.size()).boxed().collect(Collectors.toMap(
				agg -> response1.path("functionalBlocks.aggregationCount[" + agg +"].groupBy.TYPE").entityList(String.class).get().get(0),
				agg -> response1.path("functionalBlocks.aggregationCount[" + agg +"].fields.UID.COUNT").entity(Long.class).get()
		));
		assertEquals(1, aggregationResult1.get("MODULE"));
		assertEquals(3, aggregationResult1.get("FUNCTIONAL_UNIT"));
		assertEquals(2, aggregationResult1.get("REACHABILITY"));

		/* testing List Of UUID aggregation for functionalBlock. */
		final Response response2 = assertNotNull(tester).document(QUERY_TEMPLATE_AGGREGATION).variable("projectId", PROJECT_ID_2.getNid()).execute();
		final List<Object> aggregationList2 = response2.path("functionalBlocks.aggregationList").entityList(Object.class).get();
		assertEquals(3, aggregationList2.size());
		final Map<String, Set<UUID>> aggregationResult2 = IntStream.range(0, aggregationList2.size()).boxed().collect(Collectors.toMap(
				agg -> response2.path("functionalBlocks.aggregationList[" + agg +"].groupBy.TYPE").entityList(String.class).get().get(0),
				agg -> response2.path("functionalBlocks.aggregationList[" + agg +"].fields.UID.LIST").entityList(UUID.class).get()
						.stream().collect(Collectors.toSet())
		));
		assertEquals(3, aggregationResult2.size());
		assertEquals(1, aggregationResult2.get("MODULE").size());
		assertEquals(Set.of(functionalBlockPojoUUID6), aggregationResult2.get("MODULE"));
		assertEquals(3, aggregationResult2.get("FUNCTIONAL_UNIT").size());
		assertEquals(Set.of(functionalBlockPojoUUID1, functionalBlockPojoUUID2, functionalBlockPojoUUID3), aggregationResult2.get("FUNCTIONAL_UNIT"));
		assertEquals(2, aggregationResult2.get("REACHABILITY").size());
		assertEquals(Set.of(functionalBlockPojoUUID4, functionalBlockPojoUUID5), aggregationResult2.get("REACHABILITY"));

		/* testing count Of UUID aggregation for functionalBlock and modules */
		final Response response3 = assertNotNull(tester).document(QUERY_TEMPLATE_MODULE_AGGREGATION).variable("projectId", PROJECT_ID_1.getNid()).execute();
		assertEquals(1, response3.path("functionalBlocks.aggregations").entityList(Object.class).get().size());
		assertEquals(1, response3.path("functionalBlocks.aggregations[0].fields.UID.COUNT").entity(Long.class).get());

		final Response response4 = assertNotNull(tester).document(QUERY_TEMPLATE_MODULE_AGGREGATION).variable("projectId", PROJECT_ID_2.getNid()).execute();
		assertEquals(0, response4.path("functionalBlocks.aggregations").entityList(Object.class).get().size());

		blockService.delete(functionalBlockPojoUUID1);
		blockService.delete(functionalBlockPojoUUID2);
		blockService.delete(functionalBlockPojoUUID3);
		blockService.delete(functionalBlockPojoUUID4);
		blockService.delete(functionalBlockPojoUUID5);
		blockService.delete(functionalBlockPojoUUID6);
	}

	@Order(13)
	@DisplayName("Test Datapoints for Reachability Blocks")
	@Test
	void testReachabilityBlockUpperBound() {
		final var lowerModule = createTestModule("lowerModule", "src/cobol/programs/lowerModule.cbl", PROJECT_ID_1, null, null);
		final var upperModule = createTestModule("upperModule1", "src/cobol/programs/upperModule1.cbl", PROJECT_ID_1, null, null);
		final var accessModule1 = createTestModule("accessModule1", "src/cobol/programs/accessModule1.cbl", PROJECT_ID_1, null, null);
		final var accessModule2 = createTestModule("accessModule2", "src/cobol/programs/accessModule2.cbl", PROJECT_ID_1, null, null);

		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype =
				createFunctionalBlockPojoPrototype("Functional_block", "Functional_block_description", new ModuleLocation(100, 20), null, null, PROJECT_ID_1);
		final UUID functionalBlockPojoUUID = blockService.create(functionalBlockPojoPrototype);
		final ReachabilityDataPojoPrototype reachabilityDataPojoPrototype1 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperModule.identity())
				.setLowerBoundModuleId(lowerModule.identity()).setAccessModuleId(accessModule1.identity())
				.setAccessTypes(List.of("AccessType1", "AccessType2"));
		final ReachabilityDataPojoPrototype reachabilityDataPojoPrototype2 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperModule.identity())
				.setLowerBoundModuleId(lowerModule.identity()).setAccessModuleId(accessModule2.identity())
				.setAccessTypes(List.of("AccessType1", "AccessType2"));

		blockService.setReachabilityData(functionalBlockPojoUUID, List.of(reachabilityDataPojoPrototype1, reachabilityDataPojoPrototype2));

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Collections.emptyMap())
				.execute();
		response.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(Long.valueOf(1));

		assertEquals(upperModule.getName(), response.path("reachabilityData.content[0].upperBoundModules.name").entity(String.class).get());
		assertEquals(upperModule.getTechnology().name(),
				response.path("reachabilityData.content[0].upperBoundModules.technology").entity(String.class).get());
		assertEquals(upperModule.getType().name(),
				response.path("reachabilityData.content[0].upperBoundModules.type").entity(String.class).get());
		assertEquals(lowerModule.getName(), response.path("reachabilityData.content[0].lowerBoundModules.name").entity(String.class).get());
		assertEquals(lowerModule.getTechnology().name(),
				response.path("reachabilityData.content[0].lowerBoundModules.technology").entity(String.class).get());
		assertEquals(lowerModule.getType().name(),
				response.path("reachabilityData.content[0].upperBoundModules.type").entity(String.class).get());
		final Set<String> accessModules = response.path("reachabilityData.content[0].accessModules").entityList(ModulePojo.class).get()
				.stream().map(ModulePojo::getName).collect(Collectors.toSet());
		assertEquals(2, accessModules.size());
		assertEquals(Set.of("accessModule1", "accessModule2"), accessModules);
		final List<String> dataAccessType = response.path("reachabilityData.content[0].dataAccessType").entityList(String.class).get();
		assertEquals(2, dataAccessType.size());
		assertEquals(Set.of("AccessType1", "AccessType2"), dataAccessType.stream().collect(Collectors.toSet()));
	}

	@Test
	@DisplayName("Test Datapoints sorting for Reachability Blocks")
	@Order(14)
	void testReachabilityBlockDataPointSortingForLowerBoundAndUpperBoundModuleName() {
		final var lowerModuleA = createTestModule("lowerModuleA", "src/cobol/programs/lowerModuleA.cbl", PROJECT_ID_1, null, null);
		final var lowerModuleB = createTestModule("lowerModuleB", "src/cobol/programs/lowerModuleB.cbl", PROJECT_ID_1, null, null);
		final var lowerModuleC = createTestModule("lowerModuleC", "src/cobol/programs/lowerModuleC.cbl", PROJECT_ID_1, null, null);
		final var upperModuleA = createTestModule("upperModuleA", "src/cobol/programs/upperModuleA.cbl", PROJECT_ID_1, null, null);
		final var upperModuleB = createTestModule("upperModuleB", "src/cobol/programs/upperModuleB.cbl", PROJECT_ID_1, null, null);
		final var upperModuleC = createTestModule("upperModuleC", "src/cobol/programs/upperModuleC.cbl", PROJECT_ID_1, null, null);
		final var accessModule = createTestModule("accessModule", "src/cobol/programs/accessModule.cbl", PROJECT_ID_1, null, null);


		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype =
				createFunctionalBlockPojoPrototype("Functional_block", "Functional_block_description", new ModuleLocation(100, 20), null, null, PROJECT_ID_1);
		final UUID functionalBlockPojoUUID = blockService.create(functionalBlockPojoPrototype);
		blockService.setReachabilityData(functionalBlockPojoUUID, List.of(
				new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
						.setUpperBoundModuleId(upperModuleA.identity()).setLowerBoundModuleId(lowerModuleA.identity())
						.setAccessModuleId(accessModule.identity()).setAccessTypes(List.of("AccessType1")),
				new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
						.setUpperBoundModuleId(upperModuleA.identity()).setLowerBoundModuleId(lowerModuleB.identity())
						.setAccessModuleId(accessModule.identity()).setAccessTypes(List.of("AccessType2")),
				new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
						.setUpperBoundModuleId(upperModuleA.identity()).setLowerBoundModuleId(lowerModuleC.identity())
						.setAccessModuleId(accessModule.identity()).setAccessTypes(List.of("AccessType3")),
				new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
						.setUpperBoundModuleId(upperModuleB.identity()).setLowerBoundModuleId(lowerModuleC.identity())
						.setAccessModuleId(accessModule.identity()).setAccessTypes(List.of("AccessType1")),
				new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
						.setUpperBoundModuleId(upperModuleC.identity()).setLowerBoundModuleId(lowerModuleC.identity())
						.setAccessModuleId(accessModule.identity()).setAccessTypes(List.of("AccessType3"))
		));

		/* sorting by lowerBound ASC */
		Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_lowerBoundModuleName_name", "ASC"))
				.execute();

		response.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(5L);

		for (int i = 0; i < 5; i++) {
			final String path = String.format("reachabilityData.content[%d].lowerBoundModules.name", i);
			final String moduleName = response.path(path).entity(String.class).get();
			assertEquals("lowerModule" + (char) ('A' + (i < 3 ? i : 2)), moduleName);
		}

		/* sorting by lowerBound DESC */
		response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_lowerBoundModuleName_name", "DESC"))
				.execute();

		response.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(5L);

		for (int i = 0; i < 5; i++) {
			final String path = String.format("reachabilityData.content[%d].lowerBoundModules.name", i);
			final String moduleName = response.path(path).entity(String.class).get();
			assertEquals("lowerModule" + (char) ('A' + (i < 3 ? 2 : (4 - i))), moduleName);
		}

		/* sorting by upperBound ASC */
		response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_upperBoundModuleName_name", "ASC"))
				.execute();

		response.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(5L);

		for (int i = 0; i < 5; i++) {
			final String path = String.format("reachabilityData.content[%d].upperBoundModules.name", i);
			final String moduleName = response.path(path).entity(String.class).get();
			assertEquals("upperModule" + (char) ('A' + (i < 3 ? 0 : (i - 2))), moduleName);
		}

		/* sorting by upperBound DESC */
		response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_upperBoundModuleName_name", "DESC"))
				.execute();

		response.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(5L);

		for (int i = 0; i < 5; i++) {
			final String path = String.format("reachabilityData.content[%d].upperBoundModules.name", i);
			final String moduleName = response.path(path).entity(String.class).get();
			assertEquals("upperModule" + (char) ('A' + (i < 3 ? (2 - i) : 0)), moduleName);
		}
	}

	@Test
	@DisplayName("Test Filter FB By Status value")
	@Order(15)
	void testFilterFunctionalBlockByStatus() {
		final var activeBlock = createFunctionalBlockPojoPrototype("Active_Block", "Functional_block_description1",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MODULE)), PROJECT_ID_1);
		final var inactiveBlock = createFunctionalBlockPojoPrototype("InActive_Block", "Functional_block_description2",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.STATUS.name(), FunctionalBlockStatus.INACTIVE, FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.MODULE)), PROJECT_ID_1);

		activeBlock.setUid(blockService.create(activeBlock));
		inactiveBlock.setUid(blockService.create(inactiveBlock));

		Response response = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_status:{eq: INACTIVE}}", Object.class))
				.execute();

		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(1L);
		response.path("functionalBlocks.content[0].name").matchesJsonStrictly(
				"\"" + inactiveBlock.name.get() + "\"");
		response.path("functionalBlocks.content[0].status").matchesJsonStrictly("\"INACTIVE\"");

		response = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_status: {notEq: INACTIVE}, content_type: {eq: MODULE}}", Object.class))
				.execute();

		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(1L);
		response.path("functionalBlocks.content[0].name").matchesJsonStrictly("\"" + activeBlock.name.get() + "\"");
		response.path("functionalBlocks.content[0].status").valueIsNull();

		blockService.delete(activeBlock.uid.getNonNull());
		blockService.delete(inactiveBlock.uid.getNonNull());
	}

	@SuppressWarnings("unchecked")
	@Order(16)
	@DisplayName("Test for Functional Block children deep with aggregation For root block")
	@Test
	void testFunctionalBlockChildrenDeepWithAggregation() {
		/* The hierarchy of functional Blocks is as follow:
		 *
		 *            +---------------rootBlock-------------+
		 *            |                                     |
		 *            |                                     |
		 *         ChildFB1                              ChildFB2
		 *            |                                     |
		 *            |                                     |
		 *  SubChildFB1,SubChildFB2,SubChildFB3        SubChildFB4,SubChildFB5
		 */
		final FunctionalBlockPojoPrototype SubChildFB1 = createFunctionalBlockPojoPrototype("SubChildFB1",
				"description", new ModuleLocation(100, 20), null, null, projectId5);
		final UUID subChildFbUid1 = blockService.create(SubChildFB1);

		final FunctionalBlockPojoPrototype SubChildFB2 = createFunctionalBlockPojoPrototype("SubChildFB2",
				"description", new ModuleLocation(200, 20), null, null, projectId5);
		final UUID subChildFbUid2 = blockService.create(SubChildFB2);

		final FunctionalBlockPojoPrototype SubChildFB3 = createFunctionalBlockPojoPrototype("SubChildFB3",
				"description", new ModuleLocation(300, 20), null, null, projectId5);
		final UUID subChildFbUid3 = blockService.create(SubChildFB3);

		final FunctionalBlockPojoPrototype SubChildFB4 = createFunctionalBlockPojoPrototype("SubChildFB4",
				"description", new ModuleLocation(330, 20), null, null, projectId5);
		final UUID subChildFbUid4 = blockService.create(SubChildFB4);

		final FunctionalBlockPojoPrototype SubChildFB5 = createFunctionalBlockPojoPrototype("SubChildFB5",
				"description", new ModuleLocation(360, 20), null, null, projectId5);
		final UUID subChildFbUid5 = blockService.create(SubChildFB5);

		final FunctionalBlockPojoPrototype childBlock1 = createFunctionalBlockPojoPrototype("ChildFB1",
				"description", new ModuleLocation(100, 20), Arrays.asList(subChildFbUid1, subChildFbUid2, subChildFbUid3), null, projectId5);

		final UUID childUid1 = blockService.create(childBlock1);

		final FunctionalBlockPojoPrototype childBlock2 = createFunctionalBlockPojoPrototype("ChildFB2",
				"description", new ModuleLocation(100, 20), Arrays.asList(subChildFbUid4, subChildFbUid5), null, projectId5);

		final UUID childUid2 = blockService.create(childBlock2);

		final FunctionalBlockPojoPrototype rootBlock = createFunctionalBlockPojoPrototype("Root Block",
				"root Block description", new ModuleLocation(150, 30),  Arrays.asList(childUid1, childUid2),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final UUID rootBlockId = blockService.create(rootBlock);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_BY_UID_CHILDREN_DEEP_WITH_AGGREGATION)
				.variable("projectId", projectId5.getNid())
				.variable("uid", rootBlockId)
				.execute();
		response.path("data.functionalBlock.childrenDeep.aggregations[0].fields.UID.LIST.length()").matchesJsonStrictly("7");
		final List<String> uidList = new ArrayList<>();
		uidList.addAll(response.path("data.functionalBlock.childrenDeep.aggregations[0].fields.UID.LIST").entity(List.class).get());
		final List<String> expectedList = Arrays.asList(childUid1.toString(), childUid2.toString(), subChildFbUid1.toString(), subChildFbUid2.toString(),
				subChildFbUid3.toString(), subChildFbUid4.toString(), subChildFbUid5.toString());
		assertTrue(uidList.containsAll(expectedList));
		blockService.delete(List.of(childUid1, childUid2, rootBlockId, subChildFbUid1, subChildFbUid2, subChildFbUid3, subChildFbUid4, subChildFbUid5));
	}

	@SuppressWarnings("unchecked")
	@Order(17)
	@DisplayName("Test for Functional Block children deep with aggregation For child block uids")
	@Test
	void testFunctionalBlockChildrenDeepWithAggregationForChildBlocks() {
		/* The hierarchy of functional Blocks is as follow:
		 *
		 *            +---------------rootBlock-------------+
		 *            |                                     |
		 *            |                                     |
		 *         ChildFB1                              ChildFB2
		 *            |                                     |
		 *            |                                     |
		 *  SubChildFB1,SubChildFB2,SubChildFB3        SubChildFB4,SubChildFB5
		 */
		final FunctionalBlockPojoPrototype SubChildFB1 = createFunctionalBlockPojoPrototype("SubChildFB1",
				"description", new ModuleLocation(100, 20), null, null, projectId5);
		final UUID subChildFbUid1 = blockService.create(SubChildFB1);

		final FunctionalBlockPojoPrototype SubChildFB2 = createFunctionalBlockPojoPrototype("SubChildFB2",
				"description", new ModuleLocation(200, 20), null, null, projectId5);
		final UUID subChildFbUid2 = blockService.create(SubChildFB2);

		final FunctionalBlockPojoPrototype SubChildFB3 = createFunctionalBlockPojoPrototype("SubChildFB3",
				"description", new ModuleLocation(300, 20), null, null, projectId5);
		final UUID subChildFbUid3 = blockService.create(SubChildFB3);

		final FunctionalBlockPojoPrototype SubChildFB4 = createFunctionalBlockPojoPrototype("SubChildFB4",
				"description", new ModuleLocation(330, 20), null, null, projectId5);
		final UUID subChildFbUid4 = blockService.create(SubChildFB4);

		final FunctionalBlockPojoPrototype SubChildFB5 = createFunctionalBlockPojoPrototype("SubChildFB5",
				"description", new ModuleLocation(360, 20), null, null, projectId5);
		final UUID subChildFbUid5 = blockService.create(SubChildFB5);

		final FunctionalBlockPojoPrototype childBlock1 = createFunctionalBlockPojoPrototype("ChildFB1",
				"description", new ModuleLocation(100, 20), Arrays.asList(subChildFbUid1, subChildFbUid2, subChildFbUid3), null, projectId5);

		final UUID childUid1 = blockService.create(childBlock1);

		final FunctionalBlockPojoPrototype childBlock2 = createFunctionalBlockPojoPrototype("ChildFB2",
				"description", new ModuleLocation(100, 20), Arrays.asList(subChildFbUid4, subChildFbUid5), null, projectId5);

		final UUID childUid2 = blockService.create(childBlock2);

		final FunctionalBlockPojoPrototype rootBlock = createFunctionalBlockPojoPrototype("Root Block",
				"root Block description", new ModuleLocation(150, 30),  Arrays.asList(childUid1, childUid2),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final UUID rootBlockUid = blockService.create(rootBlock);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_BY_UID_CHILDREN_DEEP_WITH_AGGREGATION)
				.variable("projectId", projectId5.getNid())
				.variable("uid", childUid1)
				.execute();
		response.path("data.functionalBlock.childrenDeep.aggregations[0].fields.UID.LIST.length()").matchesJsonStrictly("3");
		final List<String> uidList = new ArrayList<>();
		uidList.addAll(response.path("data.functionalBlock.childrenDeep.aggregations[0].fields.UID.LIST").entity(List.class).get());
		assertTrue(uidList.containsAll(Arrays.asList(subChildFbUid1.toString(), subChildFbUid2.toString(), subChildFbUid3.toString())),
				"must return the subChildFbUid1, subChildFbUid2, subChildFbUid3");

		final Response response2 = assertNotNull(tester).document(QUERY_TEMPLATE_BY_UID_CHILDREN_DEEP_WITH_AGGREGATION)
				.variable("projectId", projectId5.getNid())
				.variable("uid", childUid2)
				.execute();
		response2.path("data.functionalBlock.childrenDeep.aggregations[0].fields.UID.LIST.length()").matchesJsonStrictly("2");
		final List<String> uidList2 = new ArrayList<>();
		uidList2.addAll(response2.path("data.functionalBlock.childrenDeep.aggregations[0].fields.UID.LIST").entity(List.class).get());
		assertTrue(uidList2.containsAll(Arrays.asList(subChildFbUid4.toString(), subChildFbUid5.toString())), "must return the subChildFbUid4, subChildFbUid5");
		blockService.delete(List.of(rootBlockUid, childUid1, childUid2, subChildFbUid1, subChildFbUid2, subChildFbUid3, subChildFbUid4, subChildFbUid5));
	}

	@SuppressWarnings("unchecked")
	@Order(18)
	@DisplayName("Test for Functional Block children deep with aggregation For Context Sharing for child block uids")
	@Test
	void testFunctionalBlockChildrenDeepWithAggregationForContextSharingForChild() {
		final UUID subChildFbUid1 = blockService.create(createFunctionalBlockPojoPrototype("SubChildFB1",
				"description", new ModuleLocation(100, 20), null,
				null, projectId5));
		final UUID subChildFbUid2 = blockService.create(createFunctionalBlockPojoPrototype("SubChildFB2",
				"description", new ModuleLocation(200, 20), null, null, projectId5));
		final UUID subChildFbUid3 = blockService.create(createFunctionalBlockPojoPrototype("SubChildFB3",
				"description", new ModuleLocation(300, 20), null, null, projectId5));
		final UUID subChildFbUid4 = blockService.create(createFunctionalBlockPojoPrototype("SubChildFB4",
				"description", new ModuleLocation(330, 20), null, null, projectId5));
		final UUID subChildFbUid5 = blockService.create(createFunctionalBlockPojoPrototype("SubChildFB5",
				"description", new ModuleLocation(360, 20), null, null, projectId5));

		final UUID childUid1 = blockService.create(createFunctionalBlockPojoPrototype("ChildFB1",
				"description", new ModuleLocation(100, 20), Arrays.asList(subChildFbUid1, subChildFbUid2, subChildFbUid3),
				Map.of(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.FUNCTIONAL_UNIT.name()), projectId5));
		final UUID childUid2 = blockService.create(createFunctionalBlockPojoPrototype("ChildFB2",
				"description", new ModuleLocation(100, 20), Arrays.asList(subChildFbUid4, subChildFbUid5),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5));

		final UUID rootBlockUid = blockService.create(createFunctionalBlockPojoPrototype("Root Block",
				"root Block description", new ModuleLocation(150, 30),  Arrays.asList(childUid1, childUid2),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5));

		final Response response1 = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_CONTEXT_SHARING_BY_UID_CHILDREN)
				.variable("projectId", projectId5.getNid())
				.variable("uid", rootBlockUid)
				.variable("filterObject", gson.fromJson(FILTER_OBJECT_FUNCTIONAL_UNIT, Object.class))
				.execute();
		response1.path("data.functionalBlock.childrenDeep.content.length()").matchesJsonStrictly("1");

		final Set<String> uidSet1 = new HashSet<>();
		uidSet1.addAll(response1.path("data.functionalBlock.childrenDeep.content[0].children.aggregations[0].fields.UID.LIST").entity(List.class).get());
		assertTrue(uidSet1.containsAll(Arrays.asList(subChildFbUid1.toString(), subChildFbUid2.toString(), subChildFbUid3.toString())),
				"must return the subChildFbUid1, subChildFbUid2, subChildFbUid3");

		final Response response2 = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_CONTEXT_SHARING_BY_UID_CHILDREN)
				.variable("projectId", projectId5.getNid())
				.variable("uid", rootBlockUid)
				.variable("filterObject", gson.fromJson(FILTER_OBJECT_FUNCTIONAL_GROUP, Object.class))
				.execute();
		response2.path("data.functionalBlock.childrenDeep.content.length()").matchesJsonStrictly("1");

		final Set<String> uidSet2 = new HashSet<>();
		uidSet2.addAll(response2.path("data.functionalBlock.childrenDeep.content[0].children.aggregations[0].fields.UID.LIST").entity(List.class).get());
		assertTrue(uidSet2.containsAll(Arrays.asList(subChildFbUid4.toString(), subChildFbUid5.toString())), "must return the subChildFbUid4, subChildFbUid5");
		blockService.delete(List.of(rootBlockUid, childUid1, childUid2, subChildFbUid1, subChildFbUid2, subChildFbUid3, subChildFbUid4, subChildFbUid5));
	}

	@SuppressWarnings("unchecked")
	@Order(19)
	@DisplayName("Test for Functional Block children deep Context Sharing For Parents block uids")
	@Test
	void testFunctionalBlockChildrenDeepWithAggregationContextSharingForParent() {
		final UUID subChildFbUid1 = blockService.create(createFunctionalBlockPojoPrototype("SubChildFB1", "description",
				new ModuleLocation(100, 20), null, Map.of(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.FUNCTIONAL_GROUP.name()), projectId5));
		final UUID subChildFbUid2 = blockService.create(createFunctionalBlockPojoPrototype("SubChildFB2",
				"description", new ModuleLocation(200, 20), null, null, projectId5));
		final UUID subChildFbUid3 = blockService.create(createFunctionalBlockPojoPrototype("SubChildFB3",
				"description", new ModuleLocation(300, 20), null, null, projectId5));
		final UUID subChildFbUid4 = blockService.create(createFunctionalBlockPojoPrototype("SubChildFB4", "description",
				new ModuleLocation(330, 20), null, Map.of(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.FUNCTIONAL_UNIT.name()), projectId5));
		final UUID subChildFbUid5 = blockService.create(createFunctionalBlockPojoPrototype("SubChildFB5",
				"description", new ModuleLocation(360, 20), null, null, projectId5));

		final UUID childUid1 = blockService.create(createFunctionalBlockPojoPrototype("ChildFB1",
				"description", new ModuleLocation(100, 20), Arrays.asList(subChildFbUid1, subChildFbUid2, subChildFbUid3),
				Map.of(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.FUNCTIONAL_GROUP.name()), projectId5));
		final UUID childUid2 = blockService.create(createFunctionalBlockPojoPrototype("ChildFB2",
				"description", new ModuleLocation(100, 20), Arrays.asList(subChildFbUid1, subChildFbUid4, subChildFbUid5),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT.name())), projectId5));

		final UUID rootBlockUid = blockService.create(createFunctionalBlockPojoPrototype("Root Block",
				"root Block description", new ModuleLocation(150, 30),  Arrays.asList(childUid1, childUid2),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5));

		final Response response1 = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_CONTEXT_SHARING_BY_UID_PARENT)
				.variable("projectId", projectId5.getNid())
				.variable("uid", childUid1)
				.variable("filterObject", gson.fromJson(FILTER_OBJECT_FUNCTIONAL_GROUP, Object.class))
				.execute();
		response1.path("data.functionalBlock.childrenDeep.content.length()").matchesJsonStrictly("1");

		final Set<String> uidSet1 = new HashSet<>();
		uidSet1.addAll(response1.path("data.functionalBlock.childrenDeep.content[0].parents.aggregations[0].fields.UID.LIST").entity(List.class).get());
		assertTrue(uidSet1.containsAll(Arrays.asList(childUid1.toString(), childUid2.toString())), "must return the ChildUid1, childUid2");

		final Response response2 = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_CONTEXT_SHARING_BY_UID_PARENT)
				.variable("projectId", projectId5.getNid())
				.variable("uid", childUid2)
				.variable("filterObject", gson.fromJson(FILTER_OBJECT_FUNCTIONAL_UNIT, Object.class))
				.execute();
		response2.path("data.functionalBlock.childrenDeep.content.length()").matchesJsonStrictly("1");

		final Set<String> uidSet2 = new HashSet<>();
		uidSet2.addAll(response2.path("data.functionalBlock.childrenDeep.content[0].parents.aggregations[0].fields.UID.LIST").entity(List.class).get());
		assertTrue(uidSet2.containsAll(Arrays.asList(childUid2.toString())), "must return the ChildUid2");
		blockService.delete(List.of(rootBlockUid, childUid1, childUid2, subChildFbUid1, subChildFbUid2, subChildFbUid3, subChildFbUid4, subChildFbUid5));
	}

	@Order(20)
	@DisplayName("Test for Functional Block filtering by name for peers and resolved module parts")
	@Test
	void testFunctionalBlockFilteringByNameForPeersAndResolvedModuleParts() {
		final var module1 = createTestModule("module1", "src/cobol/programs/module1.cbl", projectId5, null, null);
		final var module2 = createTestModule("module2", "src/cobol/programs/module2.cbl", projectId5, null, null);
		final var module3 = createTestModule("module3", "src/cobol/programs/module3.cbl", projectId5, null, null);

		final var functionalBlock1 = createFunctionalBlockPojoPrototype("Functional_block1", "Functional_block_description1",
				new ModuleLocation(100, 250), null, null, projectId5);
		final var functionalBlock2 = createFunctionalBlockPojoPrototype("Functional_block2", "Functional_block_description2",
				new ModuleLocation(100, 250), null, null, projectId5);
		final var functionalBlock3 = createFunctionalBlockPojoPrototype("Functional_block3", "Functional_block_description3",
				new ModuleLocation(100, 250), null, null, projectId5);

		final var functionalBlockUid1 = blockService.create(functionalBlock1);
		final var functionalBlockUid2 = blockService.create(functionalBlock2);
		final var functionalBlockUid3 = blockService.create(functionalBlock3);

		final var resolvedModulePart1 = new ResolvedModulePart(EntityId.of(module1.getId()), new ModuleLocation(100, 250));
		final var resolvedModulePart2 = new ResolvedModulePart(EntityId.of(module2.getId()), new ModuleLocation(100, 250));
		final var resolvedModulePart3 = new ResolvedModulePart(EntityId.of(module3.getId()), new ModuleLocation(100, 250));

		blockService.setResolvedModuleParts(functionalBlockUid1, List.of(resolvedModulePart1, resolvedModulePart2));
		blockService.setResolvedModuleParts(functionalBlockUid2, List.of(resolvedModulePart3, resolvedModulePart2));

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FILTER_BY_RESOLVED_MODULE_PART_NAME)
				.variable("projectId", projectId5.getNid())
				.variable("filterObject", gson.fromJson("{content_name: {eq: \"Functional_block1\"}}", Object.class))
				.execute();

		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(1L);
		response.path("functionalBlocks.content[0].name").matchesJsonStrictly("\"Functional_block1\"");

		final Response response5 = assertNotNull(tester).document(QUERY_TEMPLATE_FILTER_BY_RESOLVED_MODULE_PART_NAME)
				.variable("projectId", projectId5.getNid())
				.variable("filterObject", gson.fromJson("{content_name: {eq: \"*block1\"}}", Object.class))
				.execute();

		response5.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(1L);
		response5.path("functionalBlocks.content[0].name").matchesJsonStrictly("\"Functional_block1\"");

		final Response response2 = assertNotNull(tester).document(QUERY_TEMPLATE_FILTER_BY_RESOLVED_MODULE_PART_NAME)
				.variable("projectId", projectId5.getNid())
				.variable("filterObject", gson.fromJson("{content_name: {eq: \"Functional_block*\"}}", Object.class))
				.execute();

		response2.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(3L);
		final Map<String, Integer> index = new HashMap<>();
		for (int i = 0; i < 3; i++) {
			index.put(response2.path("functionalBlocks.content[" + i + "].name").entity(String.class).get(), i);
		}
		assertTrue(index.keySet().containsAll(Set.of("Functional_block1", "Functional_block2", "Functional_block3")));
		response2.path("functionalBlocks.content[" + index.get("Functional_block1") + "].peers.totalElements").entity(Long.class).isEqualTo(1L);
		response2.path("functionalBlocks.content[" + index.get("Functional_block1") + "].peers.content[0].name").matchesJsonStrictly("\"Functional_block2\"");
		response2.path("functionalBlocks.content[" + index.get("Functional_block2") + "].peers.totalElements").entity(Long.class).isEqualTo(1L);
		response2.path("functionalBlocks.content[" + index.get("Functional_block2") + "].peers.content[0].name").matchesJsonStrictly("\"Functional_block1\"");
		response2.path("functionalBlocks.content[" + index.get("Functional_block3") + "].peers").valueIsNull();

		final Response response6 = assertNotNull(tester).document(QUERY_TEMPLATE_FILTER_BY_RESOLVED_MODULE_PART_NAME)
				.variable("projectId", projectId5.getNid())
				.variable("filterObject", gson.fromJson("{content_name: {in: [\"Functional_block1\", \"Functional_block2\"]}}", Object.class))
				.execute();

		response6.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(2L);
		assertTrue(Set.of(response6.path("functionalBlocks.content[0].name").entity(String.class).get(),
						response6.path("functionalBlocks.content[1].name").entity(String.class).get())
				.containsAll(Set.of("Functional_block1", "Functional_block2")));

		final Response response3 = assertNotNull(tester).document(QUERY_TEMPLATE_FILTER_BY_RESOLVED_MODULE_PART_NAME)
				.variable("projectId", projectId5.getNid())
				.variable("filterObject", gson.fromJson("{content_resolvedModuleParts_module_name: {eq: \"module2\"}}", Object.class))
				.execute();

		response3.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(2L);
		assertTrue(Set.of(response3.path("functionalBlocks.content[0].name").entity(String.class).get(),
						response3.path("functionalBlocks.content[1].name").entity(String.class).get())
				.containsAll(Set.of("Functional_block1", "Functional_block2")));

		final Response response7 = assertNotNull(tester).document(QUERY_TEMPLATE_FILTER_BY_RESOLVED_MODULE_PART_NAME)
				.variable("projectId", projectId5.getNid())
				.variable("filterObject", gson.fromJson("{content_resolvedModuleParts_module_name: {in: [\"module1\",\"module3\"]}}", Object.class))
				.execute();

		response7.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(2L);
		assertTrue(Set.of(response7.path("functionalBlocks.content[0].name").entity(String.class).get(),
						response7.path("functionalBlocks.content[1].name").entity(String.class).get())
				.containsAll(Set.of("Functional_block1", "Functional_block2")));

		final Response response4 = assertNotNull(tester).document(QUERY_TEMPLATE_FILTER_BY_RESOLVED_MODULE_PART_NAME)
				.variable("projectId", projectId5.getNid())
				.variable("filterObject", gson.fromJson("{content_resolvedModuleParts_module_name: {eq: \"module3\"}}", Object.class))
				.execute();

		response4.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(1L);
		response4.path("functionalBlocks.content[0].name").matchesJsonStrictly("\"Functional_block2\"");

		final Response response8 = assertNotNull(tester).document(QUERY_TEMPLATE_FILTER_BY_RESOLVED_MODULE_PART_NAME)
				.variable("projectId", projectId5.getNid())
				.variable("filterObject", gson.fromJson("{content_peers: {eq: { content_name: { eq: \"Functional_block1\"}}}}", Object.class))
				.execute();

		response8.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(1L);
		response8.path("functionalBlocks.content[0].name").matchesJsonStrictly("\"Functional_block2\"");

		final Response response9 = assertNotNull(tester).document(QUERY_TEMPLATE_FILTER_BY_RESOLVED_MODULE_PART_NAME)
				.variable("projectId", projectId5.getNid())
				.variable("filterObject", gson.fromJson("{content_peers: {eq: { content_name: { eq: \"*block2\"}}}}", Object.class))
				.execute();

		response9.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(1L);
		response9.path("functionalBlocks.content[0].name").matchesJsonStrictly("\"Functional_block1\"");

		blockService.delete(functionalBlockUid1);
		blockService.delete(functionalBlockUid2);
		blockService.delete(functionalBlockUid3);
	}

	@Order(21)
	@DisplayName("Test for Reachability Block Lower and Upper Bound Taxonomies")
	@Test
	void testReachabilityBlockModuleTaxonomies() {
		final ModulePojo lowerModule = createTestModule("lowerModule", "src/cobol/programs/lowerMod.cbl", PROJECT_ID_1, null, null);
		final ModulePojo upperModule = createTestModule("upperModule1", "src/cobol/programs/upperMod.cbl", PROJECT_ID_1, null, null);
		final ModulePojo accessModule = createTestModule("accessModule1", "src/cobol/programs/accessMod.cbl", PROJECT_ID_1, null, null);

		final Long category = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype()
				.setName("Business Taxonomies")
				.setProject(PROJECT_ID_1));
		final UUID technicalTaxonomy = taxonomyService.createType(new TaxonomyTypePojoPrototype()
				.setName("technicalTaxonomy")
				.setProject(PROJECT_ID_1)
				.setCategoryId(category));
		final var taxonomy1 = taxonomyService.get(taxonomyService.create(new TaxonomyPojoPrototype()
				.setName("Taxonomy1")
				.setProject(PROJECT_ID_1)
				.setType(technicalTaxonomy)));
		taxonomyService.createModuleLink(lowerModule.getUid(), taxonomy1.identity());
		taxonomyService.createModuleLink(upperModule.getUid(), taxonomy1.identity());

		final UUID businessTaxonomy = taxonomyService.createType(new TaxonomyTypePojoPrototype()
				.setName("businessTaxonomy")
				.setProject(PROJECT_ID_1)
				.setCategoryId(category));
		final var taxonomy2 = taxonomyService.get(taxonomyService.create(new TaxonomyPojoPrototype()
				.setName("Taxonomy2")
				.setProject(PROJECT_ID_1)
				.setType(businessTaxonomy)));
		taxonomyService.createModuleLink(lowerModule.getUid(), taxonomy2.identity());

		final UUID processTaxonomy = taxonomyService.createType(new TaxonomyTypePojoPrototype()
				.setName("processTaxonomy")
				.setProject(PROJECT_ID_1)
				.setCategoryId(category));
		final var taxonomy3 = taxonomyService.get(taxonomyService.create(new TaxonomyPojoPrototype()
				.setName("Taxonomy3")
				.setProject(PROJECT_ID_1)
				.setType(processTaxonomy)));
		taxonomyService.createModuleLink(upperModule.getUid(), taxonomy3.identity());

		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = createFunctionalBlockPojoPrototype("Functional_block", "Functional_block_description",
				new ModuleLocation(100, 20), null, null, PROJECT_ID_1);
		final UUID functionalBlockPojoUUID = blockService.create(functionalBlockPojoPrototype);
		blockService.setReachabilityData(functionalBlockPojoUUID, List.of(
				new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
						.setUpperBoundModuleId(upperModule.identity()).setLowerBoundModuleId(lowerModule.identity())
						.setAccessModuleId(accessModule.identity()).setAccessTypes(List.of("AccessType1", "AccessType2"))
		));

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.execute();

		response.path("data.reachabilityData.totalElements").entity(Long.class).isEqualTo(1L);
		final Set<String> lowerBoundTaxonomy = new HashSet<>();
		lowerBoundTaxonomy.addAll(response.path("data.reachabilityData.content[0].taxonomy_lower_bound").entityList(String.class).get());
		assertEquals(2, lowerBoundTaxonomy.size());
		assertEquals(Set.of("technicalTaxonomy: Taxonomy1", "businessTaxonomy: Taxonomy2"), lowerBoundTaxonomy);
		final Set<String> upperBoundTaxonomy = new HashSet<>();
		upperBoundTaxonomy.addAll(response.path("data.reachabilityData.content[0].taxonomy_upper_bound").entityList(String.class).get());
		assertEquals(2, upperBoundTaxonomy.size());
		assertEquals(Set.of("technicalTaxonomy: Taxonomy1", "processTaxonomy: Taxonomy3"), upperBoundTaxonomy);
	}

	@Order(22)
	@DisplayName("Test for Functional Blocks with page and size")
	@Test
	void testFunctionalBlocksWithPageAndSize() {
		final FunctionalBlockPojoPrototype functionalBlock = createFunctionalBlockPojoPrototype(" Functional_block",
				" description", new ModuleLocation(200, 30), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), PROJECT_ID_1);
		final UUID functionalBlockPojoUid = blockService.create(functionalBlock);

		createFunctionalBlockPojoPrototype(" Dummy_Functional_block",
				" description", new ModuleLocation(20, 30), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), PROJECT_ID_1);
		final UUID functionalBlockPojoUid2 = blockService.create(functionalBlock);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("page", 0)
				.variable("size", 1)
				.variable("filterObject", gson.fromJson(FILTER_OBJECT_FUNCTIONAL_UNIT, Object.class))
				.execute();

		assertEquals("1", response.path("data.functionalBlocks.size").entity(String.class).get());
		assertEquals("3", response.path("data.functionalBlocks.totalElements").entity(String.class).get());
		blockService.delete(functionalBlockPojoUid);
		blockService.delete(functionalBlockPojoUid2);
	}

	@Order(23)
	@DisplayName("Find functional groups with wildcard search by name.")
	@Test
	void findFunctionalBlocksWildCardSearchByName() {

		final var functionalBlock1 = createFunctionalBlockPojoPrototype("Functional_block1", "Functional_block_description1",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock2 = createFunctionalBlockPojoPrototype("Functional_block2", "Functional_block_description2",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock3 = createFunctionalBlockPojoPrototype("Example", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock4 = createFunctionalBlockPojoPrototype("Functional_Unit1", "Functional_block_description4",
				new ModuleLocation(10, 25), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), projectId5);

		final var functionalBlockUid1 = blockService.create(functionalBlock1);
		final var functionalBlockUid2 = blockService.create(functionalBlock2);
		final var functionalBlockUid3 = blockService.create(functionalBlock3);
		final var functionalBlockUid4 = blockService.create(functionalBlock4);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FUNCTIONAL_BLOCKS_WITH_FILTER)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_name: {eq: \"F*\"}}", Object.class))
				.execute();

		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(2L);
		final List<String> names = new ArrayList<>();
		names.add(response.path("functionalBlocks.content[0].name").entity(String.class).get());
		names.add(response.path("functionalBlocks.content[1].name").entity(String.class).get());
		assertTrue(names.contains("Functional_block1"));
		assertTrue(names.contains("Functional_block2"));
		blockService.delete(functionalBlockUid1);
		blockService.delete(functionalBlockUid2);
		blockService.delete(functionalBlockUid3);
		blockService.delete(functionalBlockUid4);
	}

	@Order(24)
	@DisplayName("Find functional units with wildcard search by name.")
	@Test
	void findFunctionalUnitsWithWildCardSearchByName() {

		final var functionalBlock1 = createFunctionalBlockPojoPrototype("Functional_block1", "Functional_block_description1", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), projectId5);
		final var functionalBlock2 = createFunctionalBlockPojoPrototype("Functional_block2", "Functional_block_description2", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), projectId5);
		final var functionalBlock3 = createFunctionalBlockPojoPrototype("Example", "Functional_block_description3", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), projectId5);
		final var functionalBlockUid1 = blockService.create(functionalBlock1);
		final var functionalBlockUid2 = blockService.create(functionalBlock2);
		final var functionalBlockUid3 = blockService.create(functionalBlock3);

		final var functionalBlock4 = createFunctionalBlockPojoPrototype("Functional_block4", "Functional_block_description4", new ModuleLocation(10, 25),
				Collections.singletonList(functionalBlockUid2), Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)),
				projectId5);
		final var functionalBlockUid4 = blockService.create(functionalBlock4);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FUNCTIONAL_BLOCKS_WITH_FILTER).variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"FUNCTIONAL_UNIT\"}, content_name: {eq: \"F*\"}}", Object.class)).execute();

		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(2L);
		final List<String> names = new ArrayList<>();
		names.add(response.path("functionalBlocks.content[0].name").entity(String.class).get());
		names.add(response.path("functionalBlocks.content[1].name").entity(String.class).get());
		assertTrue(names.contains("Functional_block1"));
		assertTrue(names.contains("Functional_block2"));
		blockService.delete(functionalBlockUid1);
		blockService.delete(functionalBlockUid2);
		blockService.delete(functionalBlockUid3);
		blockService.delete(functionalBlockUid4);
	}

	@Order(25)
	@DisplayName("Filter functional blocks and their childrenDeep by name, applying distinct word filters to each.")
	@Test
	void findFunctionalBlocksWildCardSearchByNameAndChildrenDeepName() {

		/* The hierarchy of functional Blocks is as follows:
		 *
		 *            rootBlock
		 *               |
		 *           childBlock
		 *               |
		 *          deepChildBlock
		 * 				|
		 * 		  deeperChildBlock
		 */
		final FunctionalBlockPojoPrototype deeperChildBlock = createFunctionalBlockPojoPrototype("sub child block2",
				"description", new ModuleLocation(100, 20), null, Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final UUID deeperChildUid = blockService.create(deeperChildBlock);

		final FunctionalBlockPojoPrototype deepChildBlock = createFunctionalBlockPojoPrototype("sub child block",
				"description", new ModuleLocation(100, 20), Collections.singletonList(deeperChildUid), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final UUID childUid1 = blockService.create(deepChildBlock);

		final FunctionalBlockPojoPrototype childBlock = createFunctionalBlockPojoPrototype("child block",
				"description", new ModuleLocation(100, 20), Collections.singletonList(childUid1), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);

		final UUID childUid2 = blockService.create(childBlock);

		final FunctionalBlockPojoPrototype rootBlock = createFunctionalBlockPojoPrototype("Parent Block",
				"root Block description", new ModuleLocation(150, 30), Collections.singletonList(childUid2),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final UUID rootBlockUid = blockService.create(rootBlock);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_FUNCTIONAL_BLOCKS_CHILDREN_DEEP_WITH_FILTERS)
				.variable("projectId", projectId5.getNid())
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_name: {eq: \"P*\"},"
						+ "content_parents:{notEq: {content_type: {eq: \"FUNCTIONAL_GROUP\"}}}}", Object.class))
				.variable("childFilterObject", gson.fromJson("{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_name: {eq: \"s*\"}}", Object.class))
				.execute();
		response.path("functionalBlocks.content[0].name").matchesJsonStrictly("\"" + rootBlock.name.getNonNull() + "\"");
		response.path("functionalBlocks.content[0].childrenDeep.size").matchesJsonStrictly("2");
		final List<String> names = new ArrayList<>();
		names.add(response.path("functionalBlocks.content[0].childrenDeep.content[0].name").entity(String.class).get());
		names.add(response.path("functionalBlocks.content[0].childrenDeep.content[1].name").entity(String.class).get());
		assertTrue(names.contains(deeperChildBlock.name.getNonNull()));
		assertTrue(names.contains(deepChildBlock.name.getNonNull()));
		blockService.delete(deeperChildUid);
		blockService.delete(childUid1);
		blockService.delete(childUid2);
		blockService.delete(rootBlockUid);
	}

	@Order(26)
	@DisplayName("Filter functional blocks and their children by name, applying distinct word filters to each.")
	@Test
	void findFunctionalBlocksWildCardSearchByNameAndChildrenName() {

		/* The hierarchy of functional Blocks is as follows:
		 *
		 *            +---------------rootBlock-------------+
		 *            |                                     |
		 *            |                                    	|
		 *         childBlock1                         childBlock2
		 *            |                                     |
		 *            |                                     |
		 *  	deepChildBlock                        deepChildBlock
		 */
		final FunctionalBlockPojoPrototype deepChildBlock = createFunctionalBlockPojoPrototype("child block 1", "description", new ModuleLocation(100, 20),
				null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final UUID childUid1 = blockService.create(deepChildBlock);

		final List<UUID> childOfChild = Arrays.asList(childUid1);

		final FunctionalBlockPojoPrototype childBlock2 = createFunctionalBlockPojoPrototype("child block 2", "description", new ModuleLocation(100, 20),
				childOfChild, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);

		final UUID childUid2 = blockService.create(childBlock2);

		final FunctionalBlockPojoPrototype childBlock3 = createFunctionalBlockPojoPrototype("child block 3", "description", new ModuleLocation(100, 20),
				childOfChild, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), projectId5);

		final UUID childUid3 = blockService.create(childBlock3);

		final FunctionalBlockPojoPrototype rootBlock = createFunctionalBlockPojoPrototype("Parent Block", "root Block description", new ModuleLocation(150, 30),
				Arrays.asList(childUid2, childUid3), Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final UUID rootBlockUid = blockService.create(rootBlock);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_FUNCTIONAL_BLOCKS_CHILDREN_WITH_FILTERS)
				.variable("projectId", projectId5.getNid()).variable("filterObject", gson.fromJson(
						"{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_name: {eq: \"P*\"},"
								+ "content_parents:{notEq: {content_type: {eq: \"FUNCTIONAL_GROUP\"}}}}", Object.class))
				.variable("childFilterObject", gson.fromJson("{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_name: {eq: \"c*\"}}", Object.class)).execute();
		response.path("functionalBlocks.content[0].name").matchesJsonStrictly("\"" + rootBlock.name.getNonNull() + "\"");
		response.path("functionalBlocks.content[0].children.size").matchesJsonStrictly("1");
		response.path("functionalBlocks.content[0].children.content[0].name").matchesJsonStrictly("\"" + childBlock2.name.getNonNull() + "\"");
		blockService.delete(childUid1);
		blockService.delete(childUid2);
		blockService.delete(childUid3);
		blockService.delete(rootBlockUid);
	}

	@Test
	@DisplayName("Test Datapoints sorting for Reachability Blocks Having no Lower Bound")
	@Order(27)
	void testReachabilityBlockDataPointForNoLowerBound() {
		final ModulePojo lowerModuleA = createTestModule("lowerModuleA", "src/cobol/lowerModuleA.cbl", PROJECT_ID_1, null, null);
		final ModulePojo lowerModuleB = createTestModule("lowerModuleB", "src/cobol/lowerModuleB.cbl", PROJECT_ID_1, null, null);
		final ModulePojo lowerModuleC = createTestModule("lowerModuleC", "src/cobol/lowerModuleC.cbl", PROJECT_ID_1, null, null);
		final ModulePojo upperModuleA = createTestModule("upperModuleA", "src/cobol/upperModuleA.cbl", PROJECT_ID_1, null, null);
		final ModulePojo upperModuleB = createTestModule("upperModuleB", "src/cobol/upperModuleB.cbl", PROJECT_ID_1, null, null);
		final ModulePojo upperModuleC = createTestModule("upperModuleC", "src/cobol/upperModuleC.cbl", PROJECT_ID_1, null, null);
		final ModulePojo upperModuleD = createTestModule("upperModuleD", "src/cobol/upperModuleD.cbl", PROJECT_ID_1, null, null);
		final ModulePojo accessModule = createTestModule("accessModule", "src/cobol/accessModule.cbl", PROJECT_ID_1, null, null);

		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype =
				createFunctionalBlockPojoPrototype("Functional_block", "Functional_block_description", new ModuleLocation(100, 20), null, null, PROJECT_ID_1);
		final UUID functionalBlockPojoUUID = blockService.create(functionalBlockPojoPrototype);
		blockService.setReachabilityData(functionalBlockPojoUUID, List.of(
				new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
						.setUpperBoundModuleId(upperModuleA.identity()).setLowerBoundModuleId(lowerModuleA.identity())
						.setAccessModuleId(accessModule.identity()).setAccessTypes(List.of("AccessType1")),
				new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
						.setUpperBoundModuleId(upperModuleB.identity()).setLowerBoundModuleId(lowerModuleB.identity())
						.setAccessModuleId(accessModule.identity()).setAccessTypes(List.of("AccessType2")),
				new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
						.setUpperBoundModuleId(upperModuleC.identity()).setLowerBoundModuleId(lowerModuleC.identity()),
				new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
						.setUpperBoundModuleId(upperModuleD.identity())
		));

		/* sorting by lowerBound ASC */
		Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_lowerBoundModuleName_name", "ASC"))
				.execute();

		response.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(4L);
		assertEquals(lowerModuleA.getName(), response.path("reachabilityData.content[0].lowerBoundModules.name").entity(String.class).get());
		assertEquals(lowerModuleB.getName(), response.path("reachabilityData.content[1].lowerBoundModules.name").entity(String.class).get());
		assertEquals(lowerModuleC.getName(), response.path("reachabilityData.content[2].lowerBoundModules.name").entity(String.class).get());
		assertEquals(upperModuleD.getName(), response.path("reachabilityData.content[3].upperBoundModules.name").entity(String.class).get());

		/* sorting by lowerBound DESC */
		response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_lowerBoundModuleName_name", "DESC"))
				.execute();

		response.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(4L);
		assertEquals(lowerModuleC.getName(), response.path("reachabilityData.content[0].lowerBoundModules.name").entity(String.class).get());
		assertEquals(lowerModuleB.getName(), response.path("reachabilityData.content[1].lowerBoundModules.name").entity(String.class).get());
		assertEquals(lowerModuleA.getName(), response.path("reachabilityData.content[2].lowerBoundModules.name").entity(String.class).get());
		assertEquals(upperModuleD.getName(), response.path("reachabilityData.content[3].upperBoundModules.name").entity(String.class).get());
		blockService.delete(functionalBlockPojoUUID);
	}

	@Order(28)
	@DisplayName("Test for FunctionalBlock outdated flag filtering")
	@Test
	void testFunctionalBlockOutdatedFlagFiltering() {
		final var functionalBlockUid1 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block1",
				"Functional_block_description1", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.OUTDATED.name(), true), projectId6));
		final var functionalBlockUid2 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block2",
				"Functional_block_description2", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.OUTDATED.name(), true), projectId6));
		final var functionalBlockUid3 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block3",
				"Functional_block_description3", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.OUTDATED.name(), true), projectId6));
		final var functionalBlockUid4 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block4",
				"Functional_block_description4", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.OUTDATED.name(), false), projectId6));
		final var functionalBlockUid5 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block5",
				"Functional_block_description5", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.OUTDATED.name(), false), projectId6));

		final Response response1 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId6.getNid())
				.execute();

		final Map<UUID, Boolean> functionalBlockPojoMap = new HashMap<>();
		functionalBlockPojoMap.put(response1.path("functionalBlocks.content[0].uid").entity(UUID.class).get(),
				response1.path("functionalBlocks.content[0].outdatedBlock").entity(Boolean.class).get());
		functionalBlockPojoMap.put(response1.path("functionalBlocks.content[1].uid").entity(UUID.class).get(),
				response1.path("functionalBlocks.content[1].outdatedBlock").entity(Boolean.class).get());
		functionalBlockPojoMap.put(response1.path("functionalBlocks.content[2].uid").entity(UUID.class).get(),
				response1.path("functionalBlocks.content[2].outdatedBlock").entity(Boolean.class).get());
		functionalBlockPojoMap.put(response1.path("functionalBlocks.content[3].uid").entity(UUID.class).get(),
				response1.path("functionalBlocks.content[3].outdatedBlock").entity(Boolean.class).get());
		functionalBlockPojoMap.put(response1.path("functionalBlocks.content[4].uid").entity(UUID.class).get(),
				response1.path("functionalBlocks.content[4].outdatedBlock").entity(Boolean.class).get());
		assertTrue(functionalBlockPojoMap.get(functionalBlockUid1));
		assertTrue(functionalBlockPojoMap.get(functionalBlockUid2));
		assertTrue(functionalBlockPojoMap.get(functionalBlockUid3));
		assertFalse(functionalBlockPojoMap.get(functionalBlockUid4));
		assertFalse(functionalBlockPojoMap.get(functionalBlockUid5));

		final Response response2 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId6.getNid())
				.variable("filterObject", gson.fromJson("{content_outdatedBlock: {eq: true}}", Object.class))
				.execute();

		final FunctionalBlockPojo[] functionalBlockList1 = getFunctionalBlocksFromJson(response2, 3);
		assertEquals(Set.of(functionalBlockUid1, functionalBlockUid2, functionalBlockUid3),
				Set.of(functionalBlockList1[0].getUid(), functionalBlockList1[1].getUid(), functionalBlockList1[2].getUid()));

		final Response response3 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId6.getNid())
				.variable("filterObject", gson.fromJson("{content_outdatedBlock: {eq: false}}", Object.class))
				.execute();
		final FunctionalBlockPojo[] functionalBlockList2 = getFunctionalBlocksFromJson(response3, 2);
		assertEquals(Set.of(functionalBlockUid4, functionalBlockUid5), Set.of(functionalBlockList2[0].getUid(), functionalBlockList2[1].getUid()));
		blockService.delete(List.of(functionalBlockUid1, functionalBlockUid2, functionalBlockUid3, functionalBlockUid4, functionalBlockUid5));
	}

	@Order(29)
	@DisplayName("Test for Reachability Block Intermediate Modules")
	@Test
	void testReachabilityBlockIntermediateModules() {
		final ModulePojo lowerModule = createTestModule("lowerModule", "path/lowerModule.cbl", PROJECT_ID_1, null, null);
		final ModulePojo upperModule = createTestModule("upperModule", "path/upperModule.cbl", PROJECT_ID_1, null, null);
		final ModulePojo accessModule1 = createTestModule("accessModule1", "path/accessModule1", PROJECT_ID_1, null, null);
		final ModulePojo accessModule2 = createTestModule("accessModule2", "path/accessModule2", PROJECT_ID_1, null, null);

		final ModulePojo intermediateModule1 = createTestModule("intermediateModule1", "path/intermediateModule1.cbl", PROJECT_ID_1, null, null);
		final ModulePojo intermediateModule2 = createTestModule("intermediateModule2", "path/intermediateModule2.cbl", PROJECT_ID_1, null, null);
		final ModulePojo intermediateModule3 = createTestModule("intermediateModule3", "path/intermediateModule3.cbl", PROJECT_ID_1, null, null);
		final ModulePojo intermediateModule4 = createTestModule("intermediateModule4", "path/intermediateModule4.cbl", PROJECT_ID_1, null, null);

		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = createFunctionalBlockPojoPrototype("Functional_block", "Functional_block_description",
				new ModuleLocation(100, 20), null, null, PROJECT_ID_1);
		final UUID functionalBlockPojoUUID = blockService.create(functionalBlockPojoPrototype);
		blockService.setReachabilityData(functionalBlockPojoUUID, List.of(
				new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
						.setUpperBoundModuleId(upperModule.identity()).setLowerBoundModuleId(lowerModule.identity())
						.setAccessModuleId(accessModule1.identity()).setAccessTypes(List.of("AccessType1", "AccessType2"))
						.setIntermediateModules(List.of(EntityId.of(intermediateModule1.getId()), EntityId.of(intermediateModule2.getId()))),
				new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
						.setUpperBoundModuleId(upperModule.identity()).setLowerBoundModuleId(lowerModule.identity())
						.setAccessModuleId(accessModule2.identity()).setAccessTypes(List.of("AccessType3"))
						.setIntermediateModules(List.of(intermediateModule3.identity(), intermediateModule4.identity()))
		));

		Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("includeIntermediateModulesPerAccessModule", false)
				.execute();

		response.path("data.reachabilityData.totalElements").entity(Long.class).isEqualTo(1L);

		assertEquals(upperModule.getName(), response.path("reachabilityData.content[0].upperBoundModules.name").entity(String.class).get());
		assertEquals(lowerModule.getName(), response.path("reachabilityData.content[0].lowerBoundModules.name").entity(String.class).get());
		final Set<String> accessModules = response.path("reachabilityData.content[0].accessModules").entityList(ModulePojo.class).get()
				.stream().map(ModulePojo::getName).collect(Collectors.toSet());
		assertEquals(2, accessModules.size());
		assertEquals(Set.of("accessModule1", "accessModule2"), accessModules);
		final List<String> dataAccessType = response.path("reachabilityData.content[0].dataAccessType").entityList(String.class).get();
		assertEquals(3, dataAccessType.size());
		assertEquals(Set.of("AccessType1", "AccessType2", "AccessType3"), new HashSet<>(dataAccessType));
		final List<String> intermediateModules = response.path("reachabilityData.content[0].intermediateModulesData").entityList(ModulePojo.class).get()
				.stream().map(ModulePojo::getName).collect(Collectors.toList());
		assertEquals(4, intermediateModules.size());
		assertEquals(Set.of("intermediateModule1", "intermediateModule2", "intermediateModule3", "intermediateModule4"), new HashSet<>(intermediateModules));

		response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("includeIntermediateModulesPerAccessModule", true)
				.execute();

		response.path("data.reachabilityData.totalElements").entity(Long.class).isEqualTo(2L);
		final List<String> accessModules1 = response.path("reachabilityData.content[0].accessModules").entityList(ModulePojo.class).get()
				.stream().map(ModulePojo::getName).collect(Collectors.toList());
		assertEquals(1, accessModules1.size());
		assertEquals("accessModule1", accessModules1.get(0));
		final List<String> dataAccessType1 = response.path("reachabilityData.content[0].dataAccessType").entityList(String.class).get();
		assertEquals(2, dataAccessType1.size());
		assertEquals(Set.of("AccessType1", "AccessType2"), new HashSet<>(dataAccessType1));
		final List<String> intermediateModules1 = response.path("reachabilityData.content[0].intermediateModulesData").entityList(ModulePojo.class).get()
				.stream().map(ModulePojo::getName).collect(Collectors.toList());
		assertEquals(2, intermediateModules1.size());
		assertEquals(Set.of("intermediateModule1", "intermediateModule2"), new HashSet<>(intermediateModules1));

		final List<String> accessModules2 = response.path("reachabilityData.content[1].accessModules").entityList(ModulePojo.class).get()
				.stream().map(ModulePojo::getName).collect(Collectors.toList());
		assertEquals(1, accessModules2.size());
		assertEquals("accessModule2", accessModules2.get(0));
		final List<String> dataAccessType2 = response.path("reachabilityData.content[1].dataAccessType").entityList(String.class).get();
		assertEquals(1, dataAccessType2.size());
		assertEquals("AccessType3", dataAccessType2.get(0));
		final List<String> intermediateModules2 = response.path("reachabilityData.content[1].intermediateModulesData").entityList(ModulePojo.class).get()
				.stream().map(ModulePojo::getName).collect(Collectors.toList());
		assertEquals(2, intermediateModules2.size());
		assertEquals(Set.of("intermediateModule3", "intermediateModule4"), new HashSet<>(intermediateModules2));
	}

	@Order(30)
	@DisplayName("Test for FunctionalBlock Deleted flag filtering")
	@Test
	void testFunctionalBlockDeletedFlagFiltering() {
		final var functionalBlockUid1 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block1",
				"Functional_block_description1", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.DELETED.name(), true), projectId6));
		final var functionalBlockUid2 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block2",
				"Functional_block_description2", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.DELETED.name(), true), projectId6));
		final var functionalBlockUid3 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block3",
				"Functional_block_description3", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.DELETED.name(), true), projectId6));
		final var functionalBlockUid4 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block4",
				"Functional_block_description4", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.DELETED.name(), false), projectId6));
		final var functionalBlockUid5 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block5",
				"Functional_block_description5", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.DELETED.name(), false), projectId6));

		final Response response1 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId6.getNid())
				.execute();

		final Map<UUID, Boolean> functionalBlockPojoMap = new HashMap<>();
		functionalBlockPojoMap.put(response1.path("functionalBlocks.content[0].uid").entity(UUID.class).get(),
				response1.path("functionalBlocks.content[0].blocksWithDeletedUB").entity(Boolean.class).get());
		functionalBlockPojoMap.put(response1.path("functionalBlocks.content[1].uid").entity(UUID.class).get(),
				response1.path("functionalBlocks.content[1].blocksWithDeletedUB").entity(Boolean.class).get());
		functionalBlockPojoMap.put(response1.path("functionalBlocks.content[2].uid").entity(UUID.class).get(),
				response1.path("functionalBlocks.content[2].blocksWithDeletedUB").entity(Boolean.class).get());
		functionalBlockPojoMap.put(response1.path("functionalBlocks.content[3].uid").entity(UUID.class).get(),
				response1.path("functionalBlocks.content[3].blocksWithDeletedUB").entity(Boolean.class).get());
		functionalBlockPojoMap.put(response1.path("functionalBlocks.content[4].uid").entity(UUID.class).get(),
				response1.path("functionalBlocks.content[4].blocksWithDeletedUB").entity(Boolean.class).get());
		assertTrue(functionalBlockPojoMap.get(functionalBlockUid1));
		assertTrue(functionalBlockPojoMap.get(functionalBlockUid2));
		assertTrue(functionalBlockPojoMap.get(functionalBlockUid3));
		assertFalse(functionalBlockPojoMap.get(functionalBlockUid4));
		assertFalse(functionalBlockPojoMap.get(functionalBlockUid5));

		final Response response2 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId6.getNid())
				.variable("filterObject", gson.fromJson("{content_blocksWithDeletedUB: {eq: true}}", Object.class))
				.execute();

		final FunctionalBlockPojo[] functionalBlockList1 = getFunctionalBlocksFromJson(response2, 3);
		assertEquals(Set.of(functionalBlockUid1, functionalBlockUid2, functionalBlockUid3),
				Set.of(functionalBlockList1[0].getUid(), functionalBlockList1[1].getUid(), functionalBlockList1[2].getUid()));

		final Response response3 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId6.getNid())
				.variable("filterObject", gson.fromJson("{content_blocksWithDeletedUB: {eq: false}}", Object.class))
				.execute();
		final FunctionalBlockPojo[] functionalBlockList2 = getFunctionalBlocksFromJson(response3, 2);
		assertEquals(Set.of(functionalBlockUid4, functionalBlockUid5), Set.of(functionalBlockList2[0].getUid(), functionalBlockList2[1].getUid()));
		blockService.delete(List.of(functionalBlockUid1, functionalBlockUid2, functionalBlockUid3, functionalBlockUid4, functionalBlockUid5));
	}

	@Order(31)
	@DisplayName("Test Datapoints for SQL details in Reachability Blocks")
	@Test
	void testReachabilityBlockSqlDetails() {
		final var upperBoundModule = moduleService.findAnyModuleLightweight(x -> x.ofProject(EntityId.of(1L))
						.withLinkHash("DummyHashPgm1SqlDetails"))
				.orElseThrow(() -> new MiningEntityNotFoundException(
						"Upper bound module not found. Check if the module is created in test-data-module-sqldetails.sql"));
		final var accessModule = moduleService.findAnyModuleLightweight(x -> x.ofProject(EntityId.of(1L))
						.withLinkHash("DummyHashPgm2SqlDetails"))
				.orElseThrow(() -> new MiningEntityNotFoundException(
						"Access module not found. Check if the module is created in test-data-module-sqldetails.sql"));
		final var lowerBoundModule1 = moduleService.findAnyModuleLightweight(x -> x.ofProject(EntityId.of(1L))
						.withLinkHash("DummyHashtable7SqlDetails"))
				.orElseThrow(() -> new MiningEntityNotFoundException(
						"Lower bound module 1 not found. Check if the module is created in test-data-module-sqldetails.sql"));
		final var lowerBoundModule2 = moduleService.findAnyModuleLightweight(x -> x.ofProject(EntityId.of(1L))
						.withLinkHash("DummyHashtable8SqlDetails"))
				.orElseThrow(() -> new MiningEntityNotFoundException(
						"Lower bound module 2 not found. Check if the module is created in test-data-module-sqldetails.sql"));
		final var lowerBoundModule3 = moduleService.findAnyModuleLightweight(x -> x.ofProject(EntityId.of(1L))
						.withLinkHash("DummyHashtable9SqlDetails"))
				.orElseThrow(() -> new MiningEntityNotFoundException(
						"Lower bound module 3 not found. Check if the module is created in test-data-module-sqldetails.sql"));

		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = createFunctionalBlockPojoPrototype("Functional_block", "Functional_block_description",
				new ModuleLocation(100, 20), null, null, PROJECT_ID_1);
		final UUID functionalBlockPojoUUID = blockService.create(functionalBlockPojoPrototype);
		final ReachabilityDataPojoPrototype reachabilityDataPojoPrototype1 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperBoundModule.identity())
				.setLowerBoundModuleId(lowerBoundModule1.identity())
				.setAccessModuleId(accessModule.identity())
				.setAccessTypes(List.of("AccessType1", "AccessType2"));
		final ReachabilityDataPojoPrototype reachabilityDataPojoPrototype2 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperBoundModule.identity())
				.setLowerBoundModuleId(lowerBoundModule2.identity())
				.setAccessModuleId(accessModule.identity())
				.setAccessTypes(List.of("AccessType1", "AccessType2"));
		final ReachabilityDataPojoPrototype reachabilityDataPojoPrototype3 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperBoundModule.identity())
				.setLowerBoundModuleId(lowerBoundModule3.identity())
				.setAccessModuleId(accessModule.identity())
				.setAccessTypes(List.of("AccessType1", "AccessType2"));

		blockService.setReachabilityData(functionalBlockPojoUUID,
				List.of(reachabilityDataPojoPrototype1, reachabilityDataPojoPrototype2, reachabilityDataPojoPrototype3));

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK_WITH_SQL_DETAILS)
				.variable("projectId", PROJECT_ID_1.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_lowerBoundModuleName_name", "ASC"))
				.execute();

		/* match the expected values from test-data-module-sqldetails.sql */
		response.path("reachabilityData.content[0].sqlDetails[0].tableName")
				.entity(String.class)
				.isEqualTo("table7");
		response.path("reachabilityData.content[1].sqlDetails[0].tableName")
				.entity(String.class)
				.isEqualTo("table8");
		response.path("reachabilityData.content[2].sqlDetails[0].tableName")
				.entity(String.class)
				.isEqualTo("table9");

		response.path("reachabilityData.content[0].sqlDetails[0].queryType")
				.entity(String.class)
				.isEqualTo("SELECT");
		response.path("reachabilityData.content[1].sqlDetails[0].queryType")
				.entity(String.class)
				.isEqualTo("SELECT");
		response.path("reachabilityData.content[2].sqlDetails[0].queryType")
				.entity(String.class)
				.isEqualTo("UPDATE");

		response.path("reachabilityData.content[0].sqlDetails[0].nonconditional")
				.entityList(String.class)
				.containsExactly("column14", "column16");
		response.path("reachabilityData.content[1].sqlDetails[0].nonconditional")
				.entityList(String.class)
				.containsExactly("column15");
		response.path("reachabilityData.content[2].sqlDetails[0].nonconditional")
				.entityList(String.class)
				.containsExactly("column18", "column19");

		response.path("reachabilityData.content[0].sqlDetails[0].conditional")
				.entityList(String.class)
				.containsExactly("column14");
		response.path("reachabilityData.content[1].sqlDetails[0].conditional")
				.entityList(String.class)
				.hasSize(0);
		response.path("reachabilityData.content[2].sqlDetails[0].conditional")
				.entityList(String.class)
				.hasSize(0);

		response.path("reachabilityData.content[0].sqlDetails[0].query")
				.entity(String.class)
				.isEqualTo("SELECT table7.column14 FROM table7, table8 WHERE table7.column14 = table8.column15 AND table7.column16 > 50");
		response.path("reachabilityData.content[1].sqlDetails[0].query")
				.entity(String.class)
				.isEqualTo("SELECT table7.column14 FROM table7, table8 WHERE table7.column14 = table8.column15 AND table7.column16 > 50");
		response.path("reachabilityData.content[2].sqlDetails[0].query")
				.entity(String.class)
				.isEqualTo("UPDATE table9 SET column17 = 'new_value' WHERE column18 = 'old_value' AND column19 = 'another_value'");
	}

	@Order(32)
	@DisplayName("Find functional groups with wildcard search by deepName.")
	@Test
	void findFunctionalBlocksWildCardSearchByNameWithDeepName() {
		final var childBlock1 = createFunctionalBlockPojoPrototype("Child2", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var childBlockUid1 = blockService.create(childBlock1);

		final var functionalBlock1 = createFunctionalBlockPojoPrototype("Functional_block1", "Functional_block_description1",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock2 = createFunctionalBlockPojoPrototype("Functional_block2", "Functional_block_description2",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var childBlock = createFunctionalBlockPojoPrototype("FB Child", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);

		final var functionalBlockUid1 = blockService.create(functionalBlock1);
		final var functionalBlockUid2 = blockService.create(functionalBlock2);
		final var childBlockUid = blockService.create(childBlock);

		final var functionalBlock3 = createFunctionalBlockPojoPrototype("Example", "Functional_block_description3",
				new ModuleLocation(100, 250), Collections.singletonList(childBlockUid), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock4 = createFunctionalBlockPojoPrototype("Functional_Unit1", "Functional_block_description4",
				new ModuleLocation(10, 25), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), projectId5);

		final var functionalBlockUid3 = blockService.create(functionalBlock3);
		final var functionalBlockUid4 = blockService.create(functionalBlock4);
		blockService.setChildrenDeep(functionalBlockUid3, List.of(childBlockUid));
		blockService.setChildrenDeep(functionalBlockUid1, List.of(childBlockUid1));
		blockService.setChildrenDeep(functionalBlockUid2, List.of(childBlockUid1));

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FUNCTIONAL_BLOCKS_WITH_FILTER)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_deepName: {in: [\"F*\"]}}", Object.class))
				.execute();

		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(3L);
		final List<String> names = new ArrayList<>();
		names.add(response.path("functionalBlocks.content[0].name").entity(String.class).get());
		names.add(response.path("functionalBlocks.content[1].name").entity(String.class).get());
		names.add(response.path("functionalBlocks.content[2].name").entity(String.class).get());
		assertTrue(names.contains("Functional_block1"));
		assertTrue(names.contains("Functional_block2"));
		assertTrue(names.contains("Example"));
		blockService.delete(functionalBlockUid1);
		blockService.delete(functionalBlockUid2);
		blockService.delete(functionalBlockUid3);
		blockService.delete(functionalBlockUid4);
		blockService.delete(childBlockUid);
		blockService.delete(childBlockUid1);
	}

	@Order(33)
	@DisplayName("Find functional groups filtered by data dictionaries.")
	@Test
	void findFunctionalBlocksWithDataDictionaryFilter() {

		final var childBlock1 = createFunctionalBlockPojoPrototype("childBlock1", "Child Block 1",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var childBlockUid1 = blockService.create(childBlock1);

		final var functionalBlock1 = createFunctionalBlockPojoPrototype("Functional_block1", "Functional_block_description1",
				new ModuleLocation(100, 250), Collections.singletonList(childBlockUid1),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock2 = createFunctionalBlockPojoPrototype("Functional_block2", "Functional_block_description2",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock3 = createFunctionalBlockPojoPrototype("FB Child", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock4 = createFunctionalBlockPojoPrototype("Example", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock5 = createFunctionalBlockPojoPrototype("Functional_Unit1", "Functional_block_description4",
				new ModuleLocation(10, 25), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), projectId5);

		final var functionalBlockUid1 = blockService.create(functionalBlock1);
		final var functionalBlockUid2 = blockService.create(functionalBlock2);
		final var functionalBlockUid3 = blockService.create(functionalBlock3);
		final var functionalBlockUid4 = blockService.create(functionalBlock4);
		final var functionalBlockUid5 = blockService.create(functionalBlock5);

		final var module1 = createTestModule("Group", "src/Group.cbl", PROJECT_ID_3, null, null);
		final var dataDictionaryEntry = createDataDictionaryEntry(module1.identity(), "DataDictionaryEntry1", false,
				new ModuleLocation(3000, 100));
		final var dataDictionaryEntry2 = createDataDictionaryEntry(module1.identity(), "DataDictionaryEntry2", false,
				new ModuleLocation(2000, 100));

		blockService.setReferencedDataDictionaries(functionalBlockUid1, Arrays.asList(dataDictionaryEntry.getUid(), dataDictionaryEntry2.getUid()));
		blockService.setReferencedDataDictionaries(functionalBlockUid2, Arrays.asList(dataDictionaryEntry.getUid(), dataDictionaryEntry2.getUid()));

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FUNCTIONAL_BLOCKS_WITH_FILTER)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_referencedDataDictionaries: {in: [" +
						dataDictionaryEntry.getUid() + "," + dataDictionaryEntry2.getUid() +"]}}", Object.class))
				.execute();

		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(2L);
		final List<String> names = new ArrayList<>();
		names.add(response.path("functionalBlocks.content[0].name").entity(String.class).get());
		names.add(response.path("functionalBlocks.content[1].name").entity(String.class).get());
		assertTrue(names.contains("Functional_block1"));
		assertTrue(names.contains("Functional_block2"));
		blockService.delete(childBlockUid1);
		blockService.delete(functionalBlockUid1);
		blockService.delete(functionalBlockUid2);
		blockService.delete(functionalBlockUid3);
		blockService.delete(functionalBlockUid4);
		blockService.delete(functionalBlockUid5);
	}

	@ParameterizedTest
	@Order(34)
	@DisplayName("Find functional groups search by deepName When a root block is an empty block")
	@MethodSource("provideFilterObjectsAndExpectedResults")
	void searchFbByNameWhenEmptyRootBlockPresent(final String filterObject, final List<String> expectedNames) {
		final var childBlock1 = createFunctionalBlockPojoPrototype("Child", "Child_Description",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), projectId5);
		final var childBlockUid1 = blockService.create(childBlock1);

		final var functionalBlock1 = createFunctionalBlockPojoPrototype("Functional_block1", "Functional_block_description1",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlockUid1 = blockService.create(functionalBlock1);

		blockService.setChildrenDeep(functionalBlockUid1, List.of(childBlockUid1));
		
		final var emptyRootBlock = createFunctionalBlockPojoPrototype("Functional_Block_Empty", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);

		final var emptyRootBlockUid = blockService.create(emptyRootBlock);

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FUNCTIONAL_BLOCKS_WITH_FILTER)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson(filterObject, Object.class))
				.execute();

		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(Long.valueOf(expectedNames.size()));
		final List<String> actualNames = new ArrayList<>();
		for (int i = 0; i < expectedNames.size(); i++) {
			actualNames.add(response.path("functionalBlocks.content[" + i + "].name").entity(String.class).get());
		}
		assertThat(actualNames, containsInAnyOrder(expectedNames.toArray()));
		blockService.delete(functionalBlockUid1);
		blockService.delete(emptyRootBlockUid);
		blockService.delete(childBlockUid1);
	}

	@ParameterizedTest
	@Order(35)
	@DisplayName("Find functional groups search by deepName When a root node doesn't have an empty block, but its child node does.")
	@MethodSource("provideFilterObjectsAndExpectedResultsForEmptyChildBlock")
	void searchFbByNameWhenRootNodeChildHasEmptyBlock(final String filterObject, final List<String> expectedNames) {
		final var childBlock1 = createFunctionalBlockPojoPrototype("Child2", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), projectId5);
		final var childBlockUid1 = blockService.create(childBlock1);

		final var functionalBlock1 = createFunctionalBlockPojoPrototype("Functional_block1", "Functional_block_description1",
				new ModuleLocation(100, 250), Collections.singletonList(childBlockUid1), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlockUid1 = blockService.create(functionalBlock1);
		
		final var emptyChildBlock = createFunctionalBlockPojoPrototype("Fb_Root_Blocks_Child", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var emptyChildBlockUid = blockService.create(emptyChildBlock);
		
		final var nonEmptyChild = createFunctionalBlockPojoPrototype("Child2OfRootBlock", "Functional_block_description3",
				new ModuleLocation(100, 250), Collections.singletonList(emptyChildBlockUid), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var nonEmptyChildUid = blockService.create(nonEmptyChild);
		
		final var rootBlockWithChildHavingEmptyBlock = createFunctionalBlockPojoPrototype("RootBlockWithChildMatchingSearch", "Functional_block_description3",
				new ModuleLocation(100, 250), Arrays.asList(nonEmptyChildUid), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		
		final var rootBlockWithChildHavingEmptyBlockUid = blockService.create(rootBlockWithChildHavingEmptyBlock);
		blockService.setChildrenDeep(rootBlockWithChildHavingEmptyBlockUid, List.of(nonEmptyChildUid));
		blockService.setChildrenDeep(rootBlockWithChildHavingEmptyBlockUid, List.of(emptyChildBlockUid));
		blockService.setChildrenDeep(functionalBlockUid1, List.of(childBlockUid1));

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FUNCTIONAL_BLOCKS_WITH_FILTER)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson(filterObject, Object.class))
				.execute();

		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(Long.valueOf(expectedNames.size()));
		final List<String> actualNames = new ArrayList<>();
		for (int i = 0; i < expectedNames.size(); i++) {
			actualNames.add(response.path("functionalBlocks.content[" + i + "].name").entity(String.class).get());
		}
		assertThat(actualNames, containsInAnyOrder(expectedNames.toArray()));
		blockService.delete(functionalBlockUid1);
		blockService.delete(rootBlockWithChildHavingEmptyBlockUid);
		blockService.delete(emptyChildBlockUid);
		blockService.delete(childBlockUid1);
		blockService.delete(nonEmptyChildUid);
	}

	@Test
	@Order(36)
	void testFilterFunctionalBlocksByLowerBoundAccessTypes() {
		final UUID lbFunctionalBlockPojoUUID1 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block1",
				"Functional_block_description1", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND),
						FunctionalBlockFlag.RA_ACCESS_TYPE.name(), List.of("READ")), projectId5));
		final UUID lbFunctionalBlockPojoUUID2 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block2",
				"Functional_block_description2", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND),
						FunctionalBlockFlag.RA_ACCESS_TYPE.name(), List.of("WRITE")), projectId5));
		final UUID lbFunctionalBlockPojoUUID3 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block3",
				"Functional_block_description3", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND),
						FunctionalBlockFlag.RA_ACCESS_TYPE.name(), List.of("READ", "WRITE")), projectId5));
		final UUID lbFunctionalBlockPojoUUID4 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block4",
				"Functional_block_description4", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND)), projectId5));
		final UUID lbFunctionalBlockPojoUUID5 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block5",
				"Functional_block_description5", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND),
						FunctionalBlockFlag.RA_ACCESS_TYPE.name(), List.of("READ")), projectId5));

		final UUID functionalBlockPojoUUID1 = blockService.create(createFunctionalBlockPojoPrototype("Tp_Functional_block1",
				"Functional_block_description_Top_Down1", new ModuleLocation(100, 250), Collections.singletonList(lbFunctionalBlockPojoUUID1),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)), projectId5));
		final UUID functionalBlockPojoUUID2 = blockService.create(createFunctionalBlockPojoPrototype("Tp_Functional_block2",
				"Functional_block_description_Top_Down2", new ModuleLocation(100, 250), Collections.singletonList(lbFunctionalBlockPojoUUID2),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)), projectId5));
		final UUID functionalBlockPojoUUID3 = blockService.create(createFunctionalBlockPojoPrototype("Tp_Functional_block3",
				"Functional_block_description_Top_Down3", new ModuleLocation(100, 250), Collections.singletonList(lbFunctionalBlockPojoUUID3),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)), projectId5));
		final UUID functionalBlockPojoUUID4 = blockService.create(createFunctionalBlockPojoPrototype("Tp_Functional_block4",
				"Functional_block_description_Top_Down4", new ModuleLocation(100, 250), List.of(lbFunctionalBlockPojoUUID5),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN)), projectId5));
		final UUID mergeBlockPojoUUID1 = blockService.create(createFunctionalBlockPojoPrototype("Merge_Functional_block1",
				"Functional_block_description_Merge1", new ModuleLocation(100, 250), List.of(functionalBlockPojoUUID4),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY)), projectId5));

		final Response response1 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"REACHABILITY\"}, content_children: {eq: {" +
						" content_type: {eq: \"RA_LOWER_BOUND\"}," +
						" content_lowerBoundAccessTypes: {eq: \"READ\"}}}}", Object.class))
				.execute();

		final FunctionalBlockPojo[] functionalBlockList1 = getFunctionalBlocksFromJson(response1, 3);
		assertEquals(Set.of(functionalBlockPojoUUID1, functionalBlockPojoUUID3, mergeBlockPojoUUID1),
				Set.of(functionalBlockList1[0].getUid(), functionalBlockList1[1].getUid(), functionalBlockList1[2].getUid()));
		assertTrue(response1.path("functionalBlocks.content[0].lowerBoundAccessTypes").entityList(String.class).get().contains("READ"));
		assertTrue(response1.path("functionalBlocks.content[1].lowerBoundAccessTypes").entityList(String.class).get().contains("READ"));
		assertTrue(response1.path("functionalBlocks.content[2].lowerBoundAccessTypes").entityList(String.class).get().contains("READ"));

		final Response response2 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"REACHABILITY\"}, content_children: {eq: {" +
						" content_type: {eq: \"RA_LOWER_BOUND\"}," +
						" content_lowerBoundAccessTypes: {eq: \"WRITE\"}}}}", Object.class))
				.execute();
		final FunctionalBlockPojo[] functionalBlockList2 = getFunctionalBlocksFromJson(response2, 2);
		assertEquals(Set.of(functionalBlockPojoUUID2, functionalBlockPojoUUID3), Set.of(functionalBlockList2[0].getUid(), functionalBlockList2[1].getUid()));
		assertTrue(response2.path("functionalBlocks.content[0].lowerBoundAccessTypes").entityList(String.class).get().contains("WRITE"));
		assertTrue(response2.path("functionalBlocks.content[1].lowerBoundAccessTypes").entityList(String.class).get().contains("WRITE"));

		final Response response3 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"REACHABILITY\"}, content_children: {eq: {" +
						" content_type: {eq: \"RA_LOWER_BOUND\"}," +
						" content_lowerBoundAccessTypes: {in: [\"READ\", \"WRITE\"]}}}}", Object.class))
				.execute();

		final FunctionalBlockPojo[] functionalBlockList3 = getFunctionalBlocksFromJson(response3, 4);
		assertEquals(Set.of(functionalBlockPojoUUID1, functionalBlockPojoUUID2, functionalBlockPojoUUID3, mergeBlockPojoUUID1),
				Set.of(functionalBlockList3[0].getUid(), functionalBlockList3[1].getUid(), functionalBlockList3[2].getUid(), functionalBlockList3[3].getUid()));
		assertTrue(List.of("READ", "WRITE").containsAll(response3.path("functionalBlocks.content[0].lowerBoundAccessTypes").entityList(String.class).get()));
		assertTrue(List.of("READ", "WRITE").containsAll(response3.path("functionalBlocks.content[1].lowerBoundAccessTypes").entityList(String.class).get()));
		assertTrue(List.of("READ", "WRITE").containsAll(response3.path("functionalBlocks.content[2].lowerBoundAccessTypes").entityList(String.class).get()));
		assertTrue(List.of("READ", "WRITE").containsAll(response3.path("functionalBlocks.content[3].lowerBoundAccessTypes").entityList(String.class).get()));

		blockService.delete(functionalBlockPojoUUID1);
		blockService.delete(functionalBlockPojoUUID2);
		blockService.delete(functionalBlockPojoUUID3);
		blockService.delete(functionalBlockPojoUUID4);
		blockService.delete(lbFunctionalBlockPojoUUID1);
		blockService.delete(lbFunctionalBlockPojoUUID2);
		blockService.delete(lbFunctionalBlockPojoUUID3);
		blockService.delete(lbFunctionalBlockPojoUUID4);
		blockService.delete(lbFunctionalBlockPojoUUID5);
		blockService.delete(mergeBlockPojoUUID1);
	}

	@Test
	@Order(37)
	void testFilterFunctionalBlocksByLowerBoundTechnologyTypes() {
		final UUID lbFunctionalBlockPojoUUID1 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block1",
				"Functional_block_description1", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND)), projectId5));
		final UUID lbFunctionalBlockPojoUUID2 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block2",
				"Functional_block_description2", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND)), projectId5));
		final UUID lbFunctionalBlockPojoUUID3 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block3",
				"Functional_block_description3", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND)), projectId5));
		final UUID lbFunctionalBlockPojoUUID4 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block4",
				"Functional_block_description4", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND)), projectId5));
		final UUID lbFunctionalBlockPojoUUID5 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block5",
				"Functional_block_description5", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND)), projectId5));

		final var module1 = createTestModule("module1", "lbModule1.cbl", projectId5, Technology.COBOL, Type.JOB);
		final var module2 = createTestModule("module2", "lbModule2.cbl", projectId5, Technology.JCL, Type.EXEC_PGM);
		final var module3 = createTestModule("module3", "lbModule3.cbl", projectId5, Technology.JCL, Type.EXEC);
		final var module4 = createTestModule("module4", "lbModule4.cbl", projectId5, Technology.JCL, Type.EXEC);
		final var module5 = createTestModule("module5", "lbModule5.cbl", projectId5, Technology.JCL, Type.FILE);

		final var resolvedModulePart1 = new ResolvedModulePart(EntityId.of(module1.getId()), new ModuleLocation(100, 250));
		final var resolvedModulePart2 = new ResolvedModulePart(EntityId.of(module2.getId()), new ModuleLocation(100, 250));
		final var resolvedModulePart3 = new ResolvedModulePart(EntityId.of(module3.getId()), new ModuleLocation(100, 250));
		final var resolvedModulePart4 = new ResolvedModulePart(EntityId.of(module4.getId()), new ModuleLocation(100, 250));
		final var resolvedModulePart5 = new ResolvedModulePart(EntityId.of(module5.getId()), new ModuleLocation(100, 250));

		blockService.setResolvedModuleParts(lbFunctionalBlockPojoUUID1, List.of(resolvedModulePart1));
		blockService.setResolvedModuleParts(lbFunctionalBlockPojoUUID2, List.of(resolvedModulePart2));
		blockService.setResolvedModuleParts(lbFunctionalBlockPojoUUID3, List.of(resolvedModulePart3));
		blockService.setResolvedModuleParts(lbFunctionalBlockPojoUUID4, List.of(resolvedModulePart4));
		blockService.setResolvedModuleParts(lbFunctionalBlockPojoUUID5, List.of(resolvedModulePart5));

		final UUID functionalBlockPojoUUID1 = blockService.create(createFunctionalBlockPojoPrototype("Tp_Functional_block1",
				"Functional_block_description_Top_Down1", new ModuleLocation(100, 250), Collections.singletonList(lbFunctionalBlockPojoUUID1),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)), projectId5));
		final UUID functionalBlockPojoUUID2 = blockService.create(createFunctionalBlockPojoPrototype("Tp_Functional_block2",
				"Functional_block_description_Top_Down2", new ModuleLocation(100, 250), Collections.singletonList(lbFunctionalBlockPojoUUID2),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)), projectId5));
		final UUID functionalBlockPojoUUID3 = blockService.create(createFunctionalBlockPojoPrototype("Tp_Functional_block3",
				"Functional_block_description_Top_Down3", new ModuleLocation(100, 250), Collections.singletonList(lbFunctionalBlockPojoUUID3),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)), projectId5));
		final UUID functionalBlockPojoUUID4 = blockService.create(createFunctionalBlockPojoPrototype("Tp_Functional_block4",
				"Functional_block_description_Top_Down4", new ModuleLocation(100, 250), List.of(lbFunctionalBlockPojoUUID4),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)), projectId5));
		final UUID functionalBlockPojoUUID5 = blockService.create(createFunctionalBlockPojoPrototype("Tp_Functional_block5",
				"Functional_block_description_Top_Down5", new ModuleLocation(100, 250), List.of(lbFunctionalBlockPojoUUID5),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN)), projectId5));
		final UUID mergeBlockPojoUUID1 = blockService.create(createFunctionalBlockPojoPrototype("Merge_Functional_block1",
				"Functional_block_description_Merge1", new ModuleLocation(100, 250), List.of(functionalBlockPojoUUID5),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY)), projectId5));

		final Response response1 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"REACHABILITY\"}, content_children: {eq: {" +
						" content_type: {eq: \"RA_LOWER_BOUND\"}," +
						" content_resolvedModuleParts_module_type: {eq: \"EXEC\"}," +
						" content_resolvedModuleParts_module_technology: {eq : \"JCL\"} }}}", Object.class))
				.execute();

		final FunctionalBlockPojo[] functionalBlockList1 = getFunctionalBlocksFromJson(response1, 2);
		assertEquals(Set.of(functionalBlockPojoUUID3, functionalBlockPojoUUID4), Set.of(functionalBlockList1[0].getUid(), functionalBlockList1[1].getUid()));

		final Response response2 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"REACHABILITY\"}, content_children: {eq: {" +
						" content_type: {eq: \"RA_LOWER_BOUND\"}," +
						" content_resolvedModuleParts_module_type: {in: [\"EXEC\", \"EXEC_PGM\"]}," +
						" content_resolvedModuleParts_module_technology: {eq : \"JCL\"} }}}", Object.class))
				.execute();

		final FunctionalBlockPojo[] functionalBlockList2 = getFunctionalBlocksFromJson(response2, 3);
		assertEquals(Set.of(functionalBlockPojoUUID2, functionalBlockPojoUUID3, functionalBlockPojoUUID4),
				Set.of(functionalBlockList2[0].getUid(), functionalBlockList2[1].getUid(), functionalBlockList2[2].getUid()));

		final Response response3 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"REACHABILITY\"}, content_children: {eq: {" +
						" content_type: {eq: \"RA_LOWER_BOUND\"}," +
						" content_resolvedModuleParts_module_type: {in: [\"EXEC\", \"EXEC_PGM\", \"JOB\"]}," +
						" content_resolvedModuleParts_module_technology: {in : [\"JCL\", \"COBOL\"]} }}}", Object.class))
				.execute();

		final FunctionalBlockPojo[] functionalBlockList3 = getFunctionalBlocksFromJson(response3, 4);
		assertEquals(Set.of(functionalBlockPojoUUID1, functionalBlockPojoUUID2, functionalBlockPojoUUID3, functionalBlockPojoUUID4),
				Set.of(functionalBlockList3[0].getUid(), functionalBlockList3[1].getUid(), functionalBlockList3[2].getUid(), functionalBlockList3[3].getUid()));

		// Verify if the 'content_children' filter is checking up to the second level or not
		final Response response4 = assertNotNull(tester).document(QUERY_TEMPLATE_WITH_MODULE_PART)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"REACHABILITY\"}, content_children: {eq: {" +
						" content_type: {eq: \"RA_LOWER_BOUND\"}," +
						" content_resolvedModuleParts_module_type: {in: [\"EXEC_PGM\", \"FILE\"]}," +
						" content_resolvedModuleParts_module_technology: {eq : \"JCL\"} }}}", Object.class))
				.execute();
		final FunctionalBlockPojo[] functionalBlockList4 = getFunctionalBlocksFromJson(response4, 2);
		assertEquals(Set.of(functionalBlockPojoUUID2, mergeBlockPojoUUID1), Set.of(functionalBlockList4[0].getUid(), functionalBlockList4[1].getUid()));

		blockService.delete(functionalBlockPojoUUID1);
		blockService.delete(functionalBlockPojoUUID2);
		blockService.delete(functionalBlockPojoUUID3);
		blockService.delete(functionalBlockPojoUUID4);
		blockService.delete(functionalBlockPojoUUID5);
		blockService.delete(lbFunctionalBlockPojoUUID1);
		blockService.delete(lbFunctionalBlockPojoUUID2);
		blockService.delete(lbFunctionalBlockPojoUUID3);
		blockService.delete(lbFunctionalBlockPojoUUID4);
		blockService.delete(lbFunctionalBlockPojoUUID5);
		blockService.delete(mergeBlockPojoUUID1);
	}

	@Test
	void testReachabilityDataTaxonomyFiltering() {
		final ModulePojo upperBoundModule1 = createTestModule("UpperModuleTaxonomy1", "UpperModuleTaxonomy1.cbl", projectId5, Technology.COBOL, Type.JOB);
		final ModulePojo upperBoundModule2 = createTestModule("UpperModuleTaxonomy2", "UpperModuleTaxonomy2.cbl", projectId5, Technology.JCL, Type.EXEC_PGM);
		final ModulePojo upperBoundModule3 = createTestModule("UpperModuleTaxonomy3", "UpperModuleTaxonomy3.cbl", projectId5, Technology.JCL, Type.EXEC);
		final ModulePojo upperBoundModule4 = createTestModule("UpperModuleTaxonomy4", "UpperModuleTaxonomy4.cbl", projectId5, Technology.JCL, Type.EXEC);
		final ModulePojo lowerBoundModule1 = createTestModule("LowerModuleTaxonomy1", "LowerModuleTaxonomy1.cbl", projectId5, Technology.COBOL, Type.JOB);
		final ModulePojo lowerBoundModule2 = createTestModule("LowerModuleTaxonomy2", "LowerModuleTaxonomy2.cbl", projectId5, Technology.JCL, Type.EXEC_PGM);
		final ModulePojo lowerBoundModule3 = createTestModule("LowerModuleTaxonomy3", "LowerModuleTaxonomy3.cbl", projectId5, Technology.JCL, Type.EXEC);
		final ModulePojo lowerBoundModule4 = createTestModule("LowerModuleTaxonomy4", "LowerModuleTaxonomy4.cbl", projectId5, Technology.JCL, Type.EXEC);
		final ModulePojo accessBoundModule1 = createTestModule("AccessModuleTaxonomy1", "AccessModuleTaxonomy1.cbl", projectId5, Technology.COBOL, Type.JOB);
		final ModulePojo accessBoundModule2 = createTestModule("AccessModuleTaxonomy2", "AccessModuleTaxonomy2.cbl", projectId5, Technology.JCL, Type.EXEC);
		final ModulePojo accessBoundModule3 = createTestModule("AccessModuleTaxonomy3", "AccessModuleTaxonomy3.cbl", projectId5, Technology.JCL, Type.EXEC);
		final ModulePojo accessBoundModule4 = createTestModule("AccessModuleTaxonomy4", "AccessModuleTaxonomy4.cbl", projectId5, Technology.JCL, Type.EXEC);
		final ModulePojo intermediateModule1 = createTestModule("IntermediateTaxonomy1", "IntermediateTaxonomy1.cbl", projectId5, Technology.JCL, Type.EXEC);
		final ModulePojo intermediateModule2 = createTestModule("IntermediateTaxonomy2", "IntermediateTaxonomy2.cbl", projectId5, Technology.JCL, Type.EXEC);


		final UUID functionalBlockPojoUUID = blockService.create(createFunctionalBlockPojoPrototype("functionalBlock",
				"FunctionalBlock", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND)), projectId5));

		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype()
				.setName("taxonomy1")
				.setProject(PROJECT_ID_1)
				.setCategoryId(taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("UpperBound1").setProject(PROJECT_ID_1))));

		final var taxonomy1 = taxonomyService.get(taxonomyService.create(new TaxonomyPojoPrototype()
				.setName("TaxonomyA")
				.setProject(PROJECT_ID_1)
				.setType(type)));

		final var taxonomy2 = taxonomyService.get(taxonomyService.create(new TaxonomyPojoPrototype()
				.setName("TaxonomyB")
				.setProject(PROJECT_ID_1)
				.setType(type)));

		final var taxonomy3 = taxonomyService.get(taxonomyService.create(new TaxonomyPojoPrototype()
				.setName("TaxonomyC")
				.setProject(PROJECT_ID_1)
				.setType(type)));

		taxonomyService.createModuleLink(upperBoundModule1.getUid(), taxonomy1.identity());
		taxonomyService.createModuleLink(upperBoundModule2.getUid(), taxonomy1.identity());
		taxonomyService.createModuleLink(upperBoundModule3.getUid(), taxonomy2.identity());
		taxonomyService.createModuleLink(upperBoundModule4.getUid(), taxonomy3.identity());
		taxonomyService.createModuleLink(lowerBoundModule1.getUid(), taxonomy1.identity());
		taxonomyService.createModuleLink(lowerBoundModule2.getUid(), taxonomy2.identity());
		taxonomyService.createModuleLink(lowerBoundModule3.getUid(), taxonomy2.identity());
		taxonomyService.createModuleLink(lowerBoundModule4.getUid(), taxonomy3.identity());
		taxonomyService.createModuleLink(accessBoundModule1.getUid(), taxonomy1.identity());
		taxonomyService.createModuleLink(accessBoundModule2.getUid(), taxonomy3.identity());
		taxonomyService.createModuleLink(intermediateModule1.getUid(), taxonomy2.identity());
		taxonomyService.createModuleLink(intermediateModule2.getUid(), taxonomy3.identity());

		final ReachabilityDataPojoPrototype reachabilityDataPojoPrototype1 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperBoundModule1.identity())
				.setLowerBoundModuleId(lowerBoundModule1.identity())
				.setAccessModuleId(accessBoundModule1.identity());
		final ReachabilityDataPojoPrototype reachabilityDataPojoPrototype2 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperBoundModule2.identity())
				.setLowerBoundModuleId(lowerBoundModule2.identity())
				.setAccessModuleId(accessBoundModule2.identity());
		final ReachabilityDataPojoPrototype reachabilityDataPojoPrototype3 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperBoundModule3.identity())
				.setLowerBoundModuleId(lowerBoundModule3.identity())
				.setAccessModuleId(accessBoundModule3.identity())
				.setIntermediateModules(List.of(intermediateModule1.identity()));
		final ReachabilityDataPojoPrototype reachabilityDataPojoPrototype4 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperBoundModule4.identity())
				.setLowerBoundModuleId(lowerBoundModule4.identity())
				.setAccessModuleId(accessBoundModule4.identity())
				.setIntermediateModules(List.of(intermediateModule2.identity()));

		blockService.setReachabilityData(functionalBlockPojoUUID,
				List.of(reachabilityDataPojoPrototype1, reachabilityDataPojoPrototype2, reachabilityDataPojoPrototype3, reachabilityDataPojoPrototype4));

		final Response response1 = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", projectId5.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_upperBoundModuleName_name", "DESC"))
				.variable("filterObject", gson.fromJson("{content_upperBoundTaxonomies: {eq:\"" + taxonomy1.getUid() + "\"}}", Object.class))
				.execute();

		response1.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(2L);
		response1.path("reachabilityData.content[0].upperBoundModules.name").entity(String.class).isEqualTo(upperBoundModule2.getName());
		response1.path("reachabilityData.content[1].upperBoundModules.name").entity(String.class).isEqualTo(upperBoundModule1.getName());

		final Response response2 = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", projectId5.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_upperBoundModuleName_name", "DESC"))
				.variable("filterObject", gson.fromJson("{content_upperBoundTaxonomies:"
						+ "{in:[\"" + taxonomy2.getId() + "\",\"" + taxonomy3.getId() + "\"]}}", Object.class))
				.execute();

		response2.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(2L);
		response2.path("reachabilityData.content[0].upperBoundModules.name").entity(String.class).isEqualTo(upperBoundModule4.getName());
		response2.path("reachabilityData.content[1].upperBoundModules.name").entity(String.class).isEqualTo(upperBoundModule3.getName());

		final Response response3 = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", projectId5.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_lowerBoundModuleName_name", "DESC"))
				.variable("filterObject", gson.fromJson("{content_lowerBoundTaxonomies: {eq:\"" + taxonomy2.getUid() + "\"}}", Object.class))
				.execute();

		response3.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(2L);
		response3.path("reachabilityData.content[0].lowerBoundModules.name").entity(String.class).isEqualTo(lowerBoundModule3.getName());
		response3.path("reachabilityData.content[1].lowerBoundModules.name").entity(String.class).isEqualTo(lowerBoundModule2.getName());

		final Response response4 = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", projectId5.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_lowerBoundModuleName_name", "DESC"))
				.variable("filterObject", gson.fromJson("{content_lowerBoundTaxonomies:"
						+ "{in:[\"" + taxonomy1.getId() + "\",\"" + taxonomy3.getId() + "\"]}}", Object.class))
				.execute();

		response4.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(2L);
		response4.path("reachabilityData.content[0].lowerBoundModules.name").entity(String.class).isEqualTo(lowerBoundModule4.getName());
		response4.path("reachabilityData.content[1].lowerBoundModules.name").entity(String.class).isEqualTo(lowerBoundModule1.getName());

		final Response response5 = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", projectId5.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_upperBoundModuleName_name", "DESC"))
				.variable("filterObject", gson.fromJson("{content_pathTaxonomies: {eq:\"" + taxonomy3.getUid() + "\"}}", Object.class))
				.execute();
		response5.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(2L);
		response5.path("reachabilityData.content[0].upperBoundModules.name").entity(String.class).isEqualTo(upperBoundModule4.getName());
		response5.path("reachabilityData.content[1].upperBoundModules.name").entity(String.class).isEqualTo(upperBoundModule2.getName());

		final Response response6 = assertNotNull(tester).document(QUERY_TEMPLATE_FOR_REACHABILITY_BLOCK)
				.variable("projectId", projectId5.getNid())
				.variable("functionalBlocks", Collections.singletonList(functionalBlockPojoUUID))
				.variable("sortObject", Map.of("content_upperBoundModuleName_name", "DESC"))
				.variable("filterObject", gson.fromJson("{content_pathTaxonomies:"
						+ "{in:[\"" + taxonomy1.getId() + "\",\"" + taxonomy2.getId() + "\"]}}", Object.class))
				.execute();

		response6.path("reachabilityData.totalElements").entity(Long.class).isEqualTo(2L);
		response6.path("reachabilityData.content[0].lowerBoundModules.name").entity(String.class).isEqualTo(lowerBoundModule3.getName());
		response6.path("reachabilityData.content[1].lowerBoundModules.name").entity(String.class).isEqualTo(lowerBoundModule1.getName());
		blockService.delete(functionalBlockPojoUUID);
	}

	@Order(38)
	@DisplayName("Find functional groups filtered by data dictionaries names.")
	@Test
	void findFunctionalBlocksWithDataDictionaryNamesFilter() {

		final var firstChildBlockUid1 = blockService.create(createFunctionalBlockPojoPrototype("FirstChildBlock1", "Child Block 1",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5));

		final var functionalBlockUid1 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block1", "Functional_block_description1",
				new ModuleLocation(100, 250), Collections.singletonList(firstChildBlockUid1), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5));
		final var functionalBlockUid2 = blockService.create(createFunctionalBlockPojoPrototype("Functional_block2", "Functional_block_description2",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5));
		final var functionalBlockUid3 = blockService.create(createFunctionalBlockPojoPrototype("FB Child", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5));
		final var functionalBlockUid4 = blockService.create(createFunctionalBlockPojoPrototype("Example", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5));
		final var functionalBlockUid5 = blockService.create(createFunctionalBlockPojoPrototype("Functional_Unit1", "Functional_block_description4",
				new ModuleLocation(10, 25), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), projectId5));

		final var module1 = createTestModule("MMRSGroup", "src/MMRSGroup.cbl", PROJECT_ID_3, null, null);
		final var dataDictionaryEntry = createDataDictionaryEntry(module1.identity(), "DataDictionaryEntry1", false,
				new ModuleLocation(3000, 100));
		final var dataDictionaryEntry2 = createDataDictionaryEntry(module1.identity(), "DataDictionaryEntry2", false,
				new ModuleLocation(2000, 100));

		blockService.setReferencedDataDictionaries(functionalBlockUid1, Arrays.asList(dataDictionaryEntry.getUid(), dataDictionaryEntry2.getUid()));
		blockService.setReferencedDataDictionaries(functionalBlockUid2, Arrays.asList(dataDictionaryEntry.getUid(), dataDictionaryEntry2.getUid()));

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FUNCTIONAL_BLOCKS_WITH_FILTER)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_referencedDataDictionaryNames: {in: [" +
						dataDictionaryEntry.getName() + "," + dataDictionaryEntry2.getName() +"]}}", Object.class))
				.execute();

		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(2L);
		final List<String> names = new ArrayList<>();
		names.add(response.path("functionalBlocks.content[0].name").entity(String.class).get());
		names.add(response.path("functionalBlocks.content[1].name").entity(String.class).get());
		assertTrue(names.contains("Functional_block1"));
		assertTrue(names.contains("Functional_block2"));

		blockService.delete(firstChildBlockUid1);
		blockService.delete(functionalBlockUid1);
		blockService.delete(functionalBlockUid2);
		blockService.delete(functionalBlockUid3);
		blockService.delete(functionalBlockUid4);
		blockService.delete(functionalBlockUid5);
	}

	@Order(39)
	@DisplayName("Find functional groups with wildcard search by deepName list.")
	@Test
	void findFunctionalBlocksWildCardSearchByNames() {
		final var childBlock1 = createFunctionalBlockPojoPrototype("Level 2", "Level2_description",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var childBlockUid1 = blockService.create(childBlock1);

		final var functionalBlock1 = createFunctionalBlockPojoPrototype("Functional_block1", "Functional_block_description1",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock2 = createFunctionalBlockPojoPrototype("Functional_block2", "Functional_block_description2",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock3 = createFunctionalBlockPojoPrototype("Functional_block3", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var childBlock = createFunctionalBlockPojoPrototype("FB Child", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);

		final var functionalBlockUid1 = blockService.create(functionalBlock1);
		final var functionalBlockUid2 = blockService.create(functionalBlock2);
		final var functionalBlockUid3 = blockService.create(functionalBlock3);
		final var childBlockUid = blockService.create(childBlock);

		final var functionalBlock4 = createFunctionalBlockPojoPrototype("Example", "Functional_block_description3",
				new ModuleLocation(100, 250), Collections.singletonList(childBlockUid), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock5 = createFunctionalBlockPojoPrototype("Functional_Unit1", "Functional_block_description4",
				new ModuleLocation(10, 25), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), projectId5);

		final var functionalBlockUid4 = blockService.create(functionalBlock4);
		final var functionalBlockUid5 = blockService.create(functionalBlock5);
		blockService.setChildrenDeep(functionalBlockUid4, List.of(childBlockUid));
		blockService.setChildrenDeep(functionalBlockUid1, List.of(childBlockUid1));
		blockService.setChildrenDeep(functionalBlockUid2, List.of(childBlockUid1));

		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FUNCTIONAL_BLOCKS_WITH_FILTER)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("filterObject", gson.fromJson("{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_deepName: {in: [\"Example\", \"Level*\"]}}",
						Object.class))
				.execute();

		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(3L);
		final List<String> names = new ArrayList<>();
		names.add(response.path("functionalBlocks.content[0].name").entity(String.class).get());
		names.add(response.path("functionalBlocks.content[1].name").entity(String.class).get());
		names.add(response.path("functionalBlocks.content[2].name").entity(String.class).get());
		assertTrue(names.contains("Functional_block1"));
		assertTrue(names.contains("Functional_block2"));
		assertTrue(names.contains("Example"));
		blockService.delete(functionalBlockUid1);
		blockService.delete(functionalBlockUid2);
		blockService.delete(functionalBlockUid3);
		blockService.delete(functionalBlockUid4);
		blockService.delete(functionalBlockUid5);
		blockService.delete(childBlockUid);
		blockService.delete(childBlockUid1);
	}

		@Order(40)
		@DisplayName("Find functional groups sort by last modified in ascending order.")
		@Test
		void findFunctionalBlocksSortByLasModifiedInAscOrder() {
			final var childBlock1 = createFunctionalBlockPojoPrototype("Child Fg1", "Level2_description",
					new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
			final var childBlockUid1 = blockService.create(childBlock1);
			final var childBlock2 = createFunctionalBlockPojoPrototype("Child Fg2", "Functional_block_description3",
					new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
			final var childBlockUid2 = blockService.create(childBlock2);

			final var functionalBlock1 = createFunctionalBlockPojoPrototype("Fg1", "Functional_block_description1",
					new ModuleLocation(100, 250), Collections.singletonList(childBlockUid1), Map.of(FunctionalBlockFlag.TYPE.name(),
							List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
			final var functionalBlock2 = createFunctionalBlockPojoPrototype("Fg2", "Functional_block_description2",
					new ModuleLocation(100, 250), Collections.singletonList(childBlockUid1),
					Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
			final var functionalBlock3 = createFunctionalBlockPojoPrototype("Fg3", "Functional_block_description3",
					new ModuleLocation(100, 250), Collections.singletonList(childBlockUid2),
					Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);

			final var functionalBlockUid1 = blockService.create(functionalBlock1);
			final var functionalBlockUid2 = blockService.create(functionalBlock2);
			final var functionalBlockUid3 = blockService.create(functionalBlock3);

			blockService.setChildrenDeep(functionalBlockUid1, List.of(childBlockUid1));
			blockService.setChildrenDeep(functionalBlockUid2, List.of(childBlockUid1));

			final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FUNCTIONAL_BLOCKS_WITH_FILTER)
					.variable("projectId", projectId5.getNid())
					.variable("page", 0)
					.variable("size", 30)
					.variable("sortObject", Map.of("content_updated", "ASC"))
					.variable("filterObject", gson.fromJson(FILTER_OBJECT_FOR_FUNCTIONAL_GROUPS_WHICH_ARE_NOT_PART_OF_ANY_OTHER_GROUP, Object.class))
					.execute();

			response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(3L);
			assertEquals("Fg1", response.path("functionalBlocks.content[0].name").entity(String.class).get());
			assertEquals("Fg2", response.path("functionalBlocks.content[1].name").entity(String.class).get());
			assertEquals("Fg3", response.path("functionalBlocks.content[2].name").entity(String.class).get());
			blockService.delete(List.of(functionalBlockUid1, functionalBlockUid2, functionalBlockUid3, childBlockUid1, childBlockUid2));
		}
		
		@Order(41)
		@DisplayName("Find functional groups sort by last modified in descending order.")
		@Test
		void findFunctionalBlocksSortByLasModifiedInDescOrder() {
			final var childBlock1 = createFunctionalBlockPojoPrototype("Child group1", "Level2_description",
					new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
			final var childBlockUid1 = blockService.create(childBlock1);
			final var childBlock2 = createFunctionalBlockPojoPrototype("Child group2", "Functional_block_description3",
					new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
			final var childBlockUid2 = blockService.create(childBlock2);

			final var functionalBlock2 = createFunctionalBlockPojoPrototype("Group2", "Functional_block_description2",
					new ModuleLocation(100, 250), Collections.singletonList(childBlockUid1), Map.of(FunctionalBlockFlag.TYPE.name(),
							List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
			final var functionalBlock3 = createFunctionalBlockPojoPrototype("Group3", "Functional_block_description3",
					new ModuleLocation(100, 250), Collections.singletonList(childBlockUid2), Map.of(FunctionalBlockFlag.TYPE.name(),
							List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
			final var functionalBlock1 = createFunctionalBlockPojoPrototype("Group1", "Functional_block_description1",
					new ModuleLocation(100, 250), Collections.singletonList(childBlockUid1), Map.of(FunctionalBlockFlag.TYPE.name(),
							List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);

			final var functionalBlockUid2 = blockService.create(functionalBlock2);
			final var functionalBlockUid3 = blockService.create(functionalBlock3);
			final var functionalBlockUid1 = blockService.create(functionalBlock1);

			final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FUNCTIONAL_BLOCKS_WITH_FILTER)
					.variable("projectId", projectId5.getNid())
					.variable("page", 0)
					.variable("size", 30)
					.variable("sortObject", Map.of("content_updated", "DESC"))
					.variable("filterObject", gson.fromJson(FILTER_OBJECT_FOR_FUNCTIONAL_GROUPS_WHICH_ARE_NOT_PART_OF_ANY_OTHER_GROUP, Object.class))
					.execute();

			response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(3L);
			assertEquals("Group1", response.path("functionalBlocks.content[0].name").entity(String.class).get());
			assertEquals("Group3", response.path("functionalBlocks.content[1].name").entity(String.class).get());
			assertEquals("Group2", response.path("functionalBlocks.content[2].name").entity(String.class).get());
			blockService.delete(List.of(functionalBlockUid1, functionalBlockUid2, functionalBlockUid3, childBlockUid1, childBlockUid2));
		}

	@ParameterizedTest
	@Order(42)
	@MethodSource("provideSortArgumentsAndExpectedResult")
	void findFunctionalBlocksSortByName(final String sortDirection, final String[] expectedOrder) {
		blockService.deleteAllOfProject(projectId5);
		final var childBlock1 = createFunctionalBlockPojoPrototype("Child Fg1", "Level2_description",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var childBlockUid1 = blockService.create(childBlock1);
		final var childBlock2 = createFunctionalBlockPojoPrototype("Child Fg2", "Functional_block_description3",
				new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var childBlockUid2 = blockService.create(childBlock2);

		final var functionalBlock1 = createFunctionalBlockPojoPrototype("Fg1", "Functional_block_description1",
				new ModuleLocation(100, 250), Collections.singletonList(childBlockUid1), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock2 = createFunctionalBlockPojoPrototype("Fg2", "Functional_block_description2",
				new ModuleLocation(100, 250), Collections.singletonList(childBlockUid1), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);
		final var functionalBlock3 = createFunctionalBlockPojoPrototype("Fg3", "Functional_block_description3",
				new ModuleLocation(100, 250), Collections.singletonList(childBlockUid2), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)), projectId5);

		final var functionalBlockUid1 = blockService.create(functionalBlock1);
		final var functionalBlockUid2 = blockService.create(functionalBlock2);
		final var functionalBlockUid3 = blockService.create(functionalBlock3);

		blockService.setChildrenDeep(functionalBlockUid1, List.of(childBlockUid1));
		blockService.setChildrenDeep(functionalBlockUid2, List.of(childBlockUid1));
		final Response response = assertNotNull(tester).document(QUERY_TEMPLATE_FUNCTIONAL_BLOCKS_WITH_FILTER)
				.variable("projectId", projectId5.getNid())
				.variable("page", 0)
				.variable("size", 30)
				.variable("sortObject", Map.of("content_name", sortDirection))
				.variable("filterObject", gson.fromJson(FILTER_OBJECT_FOR_FUNCTIONAL_GROUPS_WHICH_ARE_NOT_PART_OF_ANY_OTHER_GROUP, Object.class))
				.execute();
		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(3L);
		for (int i = 0; i < expectedOrder.length; i++) {
			assertEquals(expectedOrder[i], response.path(String.format("functionalBlocks.content[%d].name", i)).entity(String.class).get());
		}
		blockService.delete(List.of(functionalBlockUid1, functionalBlockUid2, functionalBlockUid3, childBlockUid1, childBlockUid2));
	}

	private static Stream<Arguments> provideSortArgumentsAndExpectedResult() {
		return Stream.of(
				Arguments.of("ASC", new String[]{"Fg1", "Fg2", "Fg3"}),
				Arguments.of("DESC", new String[]{"Fg3", "Fg2", "Fg1"})
		);
	}

	private static Stream<Arguments> provideFilterObjectsAndExpectedResults() {
		return Stream.of(
				Arguments.of("{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_deepName: {in: [\"F*\"]}}", List.of("Functional_block1",
						"Functional_Block_Empty")),
				Arguments.of("{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_deepName: {in: [\"Functional_Block_Empty\"]}}",
						List.of("Functional_Block_Empty"))
		);
	}
	
	private static Stream<Arguments> provideFilterObjectsAndExpectedResultsForEmptyChildBlock() {
		return Stream.of(
				Arguments.of("{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_deepName: {in: [\"F*\"]}}", List.of("Functional_block1",
						"RootBlockWithChildMatchingSearch")),
				Arguments.of("{content_type: {eq: \"FUNCTIONAL_GROUP\"}, content_deepName: {in: [\"Fb_Root_Blocks_Child\"]}}",
						List.of("RootBlockWithChildMatchingSearch"))
		);
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

	private FunctionalBlockPojo getFunctionalBlockFromJson(final Response response) {
		final FunctionalBlockPojo respModule = response.path("functionalBlock").entity(FunctionalBlockPojo.class).get();
		return respModule;
	}

	private ProjectPojo createProject(final Long clientId) {
		return projectService.create(new ProjectPojoPrototype()
				.setName("Test Project"+ clientId)
				.setClient(EntityId.of(2l))
				.setNatures(Collections.singleton(ProjectNature.MINING)));
	}

	private FunctionalBlockPojo[] getFunctionalBlocksFromJson(final Response response, final int numberOfElement) {
		response.path("functionalBlocks.totalElements").entity(Long.class).isEqualTo(Long.valueOf(numberOfElement));
		final FunctionalBlockPojo[] result = (FunctionalBlockPojo[]) Array.newInstance(FunctionalBlockPojo.class, numberOfElement);
		for (int i = 0; i < numberOfElement; i++) {
			final String path = String.format("functionalBlocks.content[%d]", Integer.valueOf(i));
			final FunctionalBlockPojo respModule = response.path(path).entity(FunctionalBlockPojo.class).get();
			result[i] = respModule;
		}
		return result;
	}

	private ModulePojo createTestModule(final String name, @Nullable final String path, final EntityId projectId,
			@Nullable final Technology technology, @Nullable final Type type) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setName(name);
		module.setTechnology(technology != null ? technology : Technology.COBOL);
		module.setType(type != null ? type : Type.PROGRAM);
		module.setOrigin(Origin.CUSTOM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setCreator(Creator.DISCOVERY);
		module.setPath(path);
		return moduleService.getModule(moduleService.create(module));
	}

	private AnnotationPojo createAnnotation(final String name, final AnnotationType annotationType, final WorkingState workingState,
			final String sourceAttachment, final EntityId moduleId, final String createdByUserId, final Integer offset,
			final Integer length) {
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.setName(name);
		annotation.setType(annotationType);
		annotation.setState(workingState);
		annotation.setSourceAttachment(sourceAttachment);
		annotation.setModule(moduleId);
		annotation.setLocation(new ModuleLocation(offset, length));
		annotation.setCreatedByUserId(createdByUserId);

		return annotationService.get(annotationService.create(annotation));
	}

	private DataDictionaryPojo createDataDictionaryEntry(final EntityId moduleId, final String dataElementName, final boolean isCandidate,
			final ModuleLocation moduleLocation) {
		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype();
		dataDictionaryEntry.setName(dataElementName);
		dataDictionaryEntry.setDescription("MY description");
		dataDictionaryEntry.setFormat("PICX");
		dataDictionaryEntry.setIsCandidate(isCandidate);
		dataDictionaryEntry.setCreatedByUserId("Id");
		dataDictionaryEntry.setCreatedByUserId("name");
		dataDictionaryEntry.setModule(moduleId);
		dataDictionaryEntry.setLocation(moduleLocation);
		dataDictionaryEntry.setDefinedLocation(DefinedLocation.PROGRAM);
		dataDictionaryEntry.setState(WorkingState.CANDIDATE);

		return dataDictionaryService.create(dataDictionaryEntry);
	}
}
