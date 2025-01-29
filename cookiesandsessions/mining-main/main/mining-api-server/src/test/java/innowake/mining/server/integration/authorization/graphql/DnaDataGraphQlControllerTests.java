/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.authorization.graphql;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.BDDMockito.given;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.graphql.test.tester.GraphQlTester;
import org.springframework.graphql.test.tester.GraphQlTester.Response;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.graphql.test.tester.WebGraphQlTester;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.test.web.servlet.client.MockMvcWebTestClient;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.context.WebApplicationContext;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.config.security.MiningRole;
import innowake.mining.server.graphql.controller.ControllerLocalContext;
import innowake.mining.server.graphql.controller.DNADataGraphQlController;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.dna.DnaClusterAlgorithm;
import innowake.mining.shared.entities.dna.DnaCommunityPojoPrototype;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSnapshotPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import innowake.mining.shared.service.UserRoleService;

/**
 * Tests for the schema mapping methods of the {@link DNADataGraphQlController}.
 */
class DnaDataGraphQlControllerTests extends DatabaseRelatedTest {
	private final static Long ONE = Long.valueOf(1);
	private static Long projectId = ONE;

	private static String DNA_DATA_QUERY_TEMPLATE = "query ($projectId: EntityId!) {" +
													  "dnaData(projectId: $projectId) {" +
													    "moduleCount," +
													    "clusterings {" +
													      "algorithm {" +
													        "key," +
													        "value" +
													      "}," +
													      "options {" +
													        "name," +
													        "title," +
													        "value" +
													      "}," +
													      "clusters {" +
													        "clusterIndex," +
													        "moduleCount," +
													        "clusterDescription," +
													        "modules { content {" +
													          "clusterIndex," +
													          "module {" +
													            "id," +
													            "name" +
													          "} }" +
													        "}" +
													      "}" +
													    "}" +
													  "}" +
													"}";

	private static String DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE = "query ($projectId: EntityId!, $algorithm: String!, $index: Int, $page: Int, $size: Int, "
																			+ "$sortObject: [SortObject_dnaModulesInCluster], $updatedTime: String) "
																	+ "{"
																	    + "dnaModulesInCluster(projectId: $projectId, algorithm: $algorithm, page: $page, size: $size"
																	    + ", sortObject: $sortObject, clusterIndex: $index, updatedTime: $updatedTime) "
																	    + "{"
																	    	+ "content { "
																	    		+ "module { "
																		    		+ "id "
																		    		+ "name "
																		    		+ "path "
																	    		+ "} "
																	    		+ "clusterIndex "
																    		+ "} "
																    		+ "totalElements"
																	    + "}"
																    + "}";
	
	private static String DNA_MODULE_QUERY_TEMPLATE  = "query ($projectId: EntityId!) {" +
			  "dnaData(projectId: $projectId) {" +
			    "moduleCount," +
			    "clusterings {" +
			      "algorithm {" +
			        "key," +
			        "value" +
			      "}," +
			      "options {" +
			        "name," +
			        "title," +
			        "value" +
			      "}," +
			      "clusters {" +
			        "clusterIndex," +
			        "moduleCount," +
			        "clusterDescription," +
			        "clusterTitle," +
			        "modules { content {" +
			          "clusterIndex," +
			          "module {" +
			            "id," +
			            "name" +
			          "} }" +
			        "}" +
			      "}" +
			    "}" +
			  "}" +
			"}";

	@MockBean
	@Nullable
	RestTemplate oauthRestTemplate;
	@Nullable
	@MockBean
	private UserRoleService userRoleService;
	@Nullable
	private WebGraphQlTester tester;

	@Autowired
	private DnaDataService dnaData;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private WebApplicationContext webAppContext;


	@BeforeAll
	void init() {
		final WebTestClient client = MockMvcWebTestClient.bindToApplicationContext(webAppContext)
				.configureClient()
				.baseUrl(GraphQlAuthorizationTests.GRAPHQL_ENDPOINT)
				.build();

		tester = HttpGraphQlTester.create(client);
		loadTestData();
	}

	/**
	 * Tests that the GraphQL query returns expected dnaData JSON.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test dnaData query")
	@Test
	void testDnaDataQuery() {
		final Locale locale = Locale.getDefault();
		try {
			Locale.setDefault(Locale.ENGLISH);

			/* Set full user accesses to project */
			setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);
	
			/* Set up variables and create expected JSON */
			final String expectedJson = 
					"{\"dnaData\":{\"moduleCount\":12,\"clusterings\":[{\"algorithm\":[{\"key\":\"Sequencer\",\"value\":\"COBOL Methods\"},"
					+ "{\"key\":\"Similarity\",\"value\":\"Weighted Levenshtein\"},{\"key\":\"Clustering\",\"value\":\"Louvain\"}],"
					+ "\"options\":[{\"name\":\"maxLevels\",\"title\":\"Maximum Levels\",\"value\":\"5\"},"
					+ "{\"name\":\"maxIterations\",\"title\":\"Maximum Iterations\",\"value\":\"10\"},"
					+ "{\"name\":\"defaultTolerance\",\"title\":\"Default Tolerance\",\"value\":\"0.000100\"},"
					+ "{\"name\":\"minDNALength\",\"title\":\"Minimum DNA Length\",\"value\":\"20\"},"
					+ "{\"name\":\"similarity threshold\",\"title\":\"Similarity Threshold\",\"value\":\"0.85\"}],"
					+ "\"clusters\":[{\"clusterIndex\":-1,\"moduleCount\":2,\"clusterDescription\":\"\","
					+ "\"modules\":{\"content\":[{\"clusterIndex\":-1,\"module\":{\"id\":2008,\"name\":\"MMRS71E1\"}},"
					+ "{\"clusterIndex\":-1,\"module\":{\"id\":2009,\"name\":\"MMRS71F1\"}}]}},"
					+ "{\"clusterIndex\":1,\"moduleCount\":4,\"clusterDescription\":\"\","
					+ "\"modules\":{\"content\":[{\"clusterIndex\":1,\"module\":{\"id\":2000,\"name\":\"MMRS7101\"}},"
					+ "{\"clusterIndex\":1,\"module\":{\"id\":2001,\"name\":\"MMRS7112\"}},"
					+ "{\"clusterIndex\":1,\"module\":{\"id\":2002,\"name\":\"MMRS71D1\"}},"
					+ "{\"clusterIndex\":1,\"module\":{\"id\":2003,\"name\":\"MMRS71Z1\"}}]}},"
					+ "{\"clusterIndex\":2,\"moduleCount\":4,\"clusterDescription\":\"\","
					+ "\"modules\":{\"content\":[{\"clusterIndex\":2,\"module\":{\"id\":2004,\"name\":\"MMRS7102\"}},"
					+ "{\"clusterIndex\":2,\"module\":{\"id\":2005,\"name\":\"MMRS7111\"}},"
					+ "{\"clusterIndex\":2,\"module\":{\"id\":2006,\"name\":\"MMRS71B1\"}},"
					+ "{\"clusterIndex\":2,\"module\":{\"id\":2007,\"name\":\"MMRS71C1\"}}]}}]}]}}";
			final Map<String, Object> variables = new HashMap<>();
			variables.put("projectId", projectId);
	
			/* Execute GraphQL request and verify returned JSON */
			verifyReceivedDnaData(executeQuery(DNA_DATA_QUERY_TEMPLATE, variables), expectedJson);
		} finally {
			Locale.setDefault(locale);
		}
	}

	/**
	 * Tests that the GraphQL query returns all expected modules in cluster for METHOD RULE algorithm in all clusters.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test dnaModulesInCluster query with algorithm")
	@Test
	void testModulesInClusterQueryWithAlgorithm() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);

		/* Set up variables and create expected JSON */
		final String expectedJson = "{\"dnaModulesInCluster\":{\"content\":["
				+ "{\"module\":{\"id\":2000,\"name\":\"MMRS7101\",\"path\":\"src/cobol/programs/MMRS7101.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2001,\"name\":\"MMRS7112\",\"path\":\"src/cobol/programs/MMRS7112.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2002,\"name\":\"MMRS71D1\",\"path\":\"src/cobol/programs/MMRS71D1.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2003,\"name\":\"MMRS71Z1\",\"path\":\"src/cobol/programs/MMRS71Z1.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2004,\"name\":\"MMRS7102\",\"path\":\"src/cobol/programs/MMRS7102.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2005,\"name\":\"MMRS7111\",\"path\":\"src/cobol/programs/MMRS7111.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2006,\"name\":\"MMRS71B1\",\"path\":\"src/cobol/programs/MMRS71B1.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2007,\"name\":\"MMRS71C1\",\"path\":\"src/cobol/programs/MMRS71C1.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2008,\"name\":\"MMRS71E1\",\"path\":\"src/cobol/programs/MMRS71E1.cbl\"},\"clusterIndex\":-1},"
				+ "{\"module\":{\"id\":2009,\"name\":\"MMRS71F1\",\"path\":\"src/cobol/programs/MMRS71F1.cbl\"},\"clusterIndex\":-1}]}}";

		final Map<String, Object> variables = new HashMap<>();
		variables.put("projectId", projectId);
		variables.put("algorithm", "COBOL Methods");
		variables.put("page", 0);
		variables.put("size", 20);
		variables.put("sortObject", Map.of("content_module_id", "ASC"));

		/* Execute GraphQL request and verify response data */
		verifyReceivedDnaData(executeQuery(DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE, variables), expectedJson);
	}

	/**
	 * Tests that the GraphQL query returns all expected modules in cluster for a particular snapshot
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test dnaModulesInCluster query with updatedTime")
	@Test
	void testModulesInClusterQueryWithUpdatedTime() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);

		/* Set up variables and create expected JSON */
		final String expectedJson = "{\"dnaModulesInCluster\":{\"content\":["
				+ "{\"module\":{\"id\":2000,\"name\":\"MMRS7101\",\"path\":\"src/cobol/programs/MMRS7101.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2001,\"name\":\"MMRS7112\",\"path\":\"src/cobol/programs/MMRS7112.cbl\"},\"clusterIndex\":1}]}}";

		final List<String> snapshots = dnaData.findSnapshots(q -> q.ofProject(EntityId.of(projectId))
							.sortUpdated(SortDirection.ASCENDING)).stream()
			.map(s -> String.valueOf(s.getUpdated().toEpochMilli()))
			.collect(Collectors.toList());
		assertEquals(2, snapshots.size());

		final Map<String, Object> variables = new HashMap<>();
		variables.put("projectId", projectId);
		variables.put("algorithm", "COBOL Methods");
		variables.put("page", 0);
		variables.put("size", 20);
		variables.put("sortBy", Arrays.asList("id;ASC"));
		variables.put("updatedTime", snapshots.get(0));

		/* Execute GraphQL request and verify response data */
		verifyReceivedDnaData(executeQuery(DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE, variables), expectedJson);
	}

	/**
	 * Tests that the GraphQL query returns expected dnaData JSON with clusterName and clusterDescription.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test dnaDataModule query")
	@Test
	void testDnaDataModuleQuery() {
		final Locale locale = Locale.getDefault();
		try {
			Locale.setDefault(Locale.ENGLISH);

			/* Set full user accesses to project */
			setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);
	
			/* Set up variables and create expected JSON */
			final String expectedJson = 
					"{\"dnaData\":{\"moduleCount\":12,\"clusterings\":[{\"algorithm\":[{\"key\":\"Sequencer\",\"value\":\"COBOL Methods\"},"
					+ "{\"key\":\"Similarity\",\"value\":\"Weighted Levenshtein\"},{\"key\":\"Clustering\",\"value\":\"Louvain\"}],"
					+ "\"options\":[{\"name\":\"maxLevels\",\"title\":\"Maximum Levels\",\"value\":\"5\"},"
					+ "{\"name\":\"maxIterations\",\"title\":\"Maximum Iterations\",\"value\":\"10\"},"
					+ "{\"name\":\"defaultTolerance\",\"title\":\"Default Tolerance\",\"value\":\"0.000100\"},"
					+ "{\"name\":\"minDNALength\",\"title\":\"Minimum DNA Length\",\"value\":\"20\"},"
					+ "{\"name\":\"similarity threshold\",\"title\":\"Similarity Threshold\",\"value\":\"0.85\"}],"
					+ "\"clusters\":[{\"clusterIndex\":-1,\"moduleCount\":2,\"clusterDescription\":\"\",\"clusterTitle\":\"Unassigned Modules\","
					+ "\"modules\":{\"content\":[{\"clusterIndex\":-1,\"module\":{\"id\":2008,\"name\":\"MMRS71E1\"}},"
					+ "{\"clusterIndex\":-1,\"module\":{\"id\":2009,\"name\":\"MMRS71F1\"}}]}},"
					+ "{\"clusterIndex\":1,\"moduleCount\":4,\"clusterDescription\":\"\",\"clusterTitle\":\"\","
					+ "\"modules\":{\"content\":[{\"clusterIndex\":1,\"module\":{\"id\":2000,\"name\":\"MMRS7101\"}},"
					+ "{\"clusterIndex\":1,\"module\":{\"id\":2001,\"name\":\"MMRS7112\"}},"
					+ "{\"clusterIndex\":1,\"module\":{\"id\":2002,\"name\":\"MMRS71D1\"}},"
					+ "{\"clusterIndex\":1,\"module\":{\"id\":2003,\"name\":\"MMRS71Z1\"}}]}},"
					+ "{\"clusterIndex\":2,\"moduleCount\":4,\"clusterDescription\":\"\",\"clusterTitle\":\"\","
					+ "\"modules\":{\"content\":[{\"clusterIndex\":2,\"module\":{\"id\":2004,\"name\":\"MMRS7102\"}},"
					+ "{\"clusterIndex\":2,\"module\":{\"id\":2005,\"name\":\"MMRS7111\"}},"
					+ "{\"clusterIndex\":2,\"module\":{\"id\":2006,\"name\":\"MMRS71B1\"}},"
					+ "{\"clusterIndex\":2,\"module\":{\"id\":2007,\"name\":\"MMRS71C1\"}}]}}]}]}}";
			final Map<String, Object> variables = new HashMap<>();
			variables.put("projectId", projectId);
	
			/* Execute GraphQL request and verify returned JSON */
			verifyReceivedDnaData(executeQuery(DNA_MODULE_QUERY_TEMPLATE, variables), expectedJson);
		} finally {
			Locale.setDefault(locale);
		}
	}

	/**
	 * Tests that the GraphQL query returns all expected modules in cluster 1.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test dnaModulesInCluster query with algorithm and cluster index 1")
	@Test
	void testModulesInClusterQueryWithAlgorithmAndClusterIndex() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);

		/* Set up variables and create expected JSON */
		final String expectedJson = "{\"dnaModulesInCluster\":{\"content\":["
				+ "{\"module\":{\"id\":2000,\"name\":\"MMRS7101\",\"path\":\"src/cobol/programs/MMRS7101.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2001,\"name\":\"MMRS7112\",\"path\":\"src/cobol/programs/MMRS7112.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2002,\"name\":\"MMRS71D1\",\"path\":\"src/cobol/programs/MMRS71D1.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2003,\"name\":\"MMRS71Z1\",\"path\":\"src/cobol/programs/MMRS71Z1.cbl\"},\"clusterIndex\":1}]}}";
		final Map<String, Object> variables = new HashMap<>();
		variables.put("projectId", projectId);
		variables.put("algorithm", "COBOL Methods");
		variables.put("index", Integer.valueOf(1));
		variables.put("page", 0);
		variables.put("size", 20);
		variables.put("sortObject", Map.of("content_module_id", "ASC"));

		/* Execute GraphQL request and verify response data */
		verifyReceivedDnaData(executeQuery(DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE, variables), expectedJson);
	}

	/**
	 * Tests that the GraphQL query returns all expected modules in cluster 2.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test dnaModulesInCluster query with algorithm and cluster index 2")
	@Test
	void testModulesInClusterQueryWithAlgorithmAndDifferentClusterIndex() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);

		/* Set up variables and create expected JSON */
		final String expectedJson = "{\"dnaModulesInCluster\":{\"content\":["
				+ "{\"module\":{\"id\":2004,\"name\":\"MMRS7102\",\"path\":\"src/cobol/programs/MMRS7102.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2005,\"name\":\"MMRS7111\",\"path\":\"src/cobol/programs/MMRS7111.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2006,\"name\":\"MMRS71B1\",\"path\":\"src/cobol/programs/MMRS71B1.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2007,\"name\":\"MMRS71C1\",\"path\":\"src/cobol/programs/MMRS71C1.cbl\"},\"clusterIndex\":2}]}}";
		final Map<String, Object> variables = new HashMap<>();
		variables.put("projectId", projectId);
		variables.put("algorithm", "COBOL Methods");
		variables.put("index", Integer.valueOf(2));
		variables.put("page", 0);
		variables.put("size", 20);
		variables.put("sortObject", Map.of("content_module_id", "ASC"));

		/* Execute GraphQL request and verify response data */
		verifyReceivedDnaData(executeQuery(DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE, variables), expectedJson);
	}

	/**
	 * Tests that the GraphQL query returns all expected modules in cluster -1.
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test dnaModulesInCluster query with algorithm and cluster index -1")
	@Test
	void testModulesInClusterQueryWithAlgorithmAndDifferentClusterIndexForUnassignedModules() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);
		
		/* Set up variables and create expected JSON */
		final String expectedJson = "{\"dnaModulesInCluster\":{\"content\":["
				+ "{\"module\":{\"id\":2008,\"name\":\"MMRS71E1\",\"path\":\"src/cobol/programs/MMRS71E1.cbl\"},\"clusterIndex\":-1},"
				+ "{\"module\":{\"id\":2009,\"name\":\"MMRS71F1\",\"path\":\"src/cobol/programs/MMRS71F1.cbl\"},\"clusterIndex\":-1}]}}";
		final Map<String, Object> variables = new HashMap<>();
		variables.put("projectId", projectId);
		variables.put("algorithm", "COBOL Methods");
		variables.put("index", Integer.valueOf(-1));
		variables.put("page", 0);
		variables.put("size", 20);
		variables.put("sortObject", Map.of("content_module_id", "ASC"));
		
		/* Execute GraphQL request and verify response data */
		verifyReceivedDnaData(executeQuery(DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE, variables), expectedJson);
	}
	
	@DisplayName("Test dnaModulesInCluster query with default page, default size and sort")
	@Test
	void testModulesInClusterQueryWithDefaultPageAndDefaultSizeAndSort() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);

		/* Set up variables and create expected JSON with default page and size */
		final String expectedJson = "{\"dnaModulesInCluster\":{\"content\":["
				+ "{\"module\":{\"id\":2000,\"name\":\"MMRS7101\",\"path\":\"src/cobol/programs/MMRS7101.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2004,\"name\":\"MMRS7102\",\"path\":\"src/cobol/programs/MMRS7102.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2005,\"name\":\"MMRS7111\",\"path\":\"src/cobol/programs/MMRS7111.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2001,\"name\":\"MMRS7112\",\"path\":\"src/cobol/programs/MMRS7112.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2006,\"name\":\"MMRS71B1\",\"path\":\"src/cobol/programs/MMRS71B1.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2007,\"name\":\"MMRS71C1\",\"path\":\"src/cobol/programs/MMRS71C1.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2002,\"name\":\"MMRS71D1\",\"path\":\"src/cobol/programs/MMRS71D1.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2008,\"name\":\"MMRS71E1\",\"path\":\"src/cobol/programs/MMRS71E1.cbl\"},\"clusterIndex\":-1},"
				+ "{\"module\":{\"id\":2009,\"name\":\"MMRS71F1\",\"path\":\"src/cobol/programs/MMRS71F1.cbl\"},\"clusterIndex\":-1},"
				+ "{\"module\":{\"id\":2003,\"name\":\"MMRS71Z1\",\"path\":\"src/cobol/programs/MMRS71Z1.cbl\"},\"clusterIndex\":1}],\"totalElements\":10}}";
		final Map<String, Object> variables = new HashMap<>();
		variables.put("projectId", projectId);
		variables.put("algorithm", "COBOL Methods");
		variables.put("page", 0);
		variables.put("size", 0);
		variables.put("sortObject", Map.of("content_module_name", "ASC"));

		/* Execute GraphQL request and verify response data */
		executeQuery(DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE, variables).path("").matchesJsonStrictly(expectedJson);
	}

	@DisplayName("Test dnaModulesInCluster query with default page, max size and sort")
	@Test
	void testModulesInClusterQueryWithDefaultPageAndMaxSizeAndSort() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);

		/* Set up variables and create expected JSON with default page and size */
		final String expectedJson = "{\"dnaModulesInCluster\":{\"content\":["
				+ "{\"module\":{\"id\":2000,\"name\":\"MMRS7101\",\"path\":\"src/cobol/programs/MMRS7101.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2004,\"name\":\"MMRS7102\",\"path\":\"src/cobol/programs/MMRS7102.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2005,\"name\":\"MMRS7111\",\"path\":\"src/cobol/programs/MMRS7111.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2001,\"name\":\"MMRS7112\",\"path\":\"src/cobol/programs/MMRS7112.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2006,\"name\":\"MMRS71B1\",\"path\":\"src/cobol/programs/MMRS71B1.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2007,\"name\":\"MMRS71C1\",\"path\":\"src/cobol/programs/MMRS71C1.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2002,\"name\":\"MMRS71D1\",\"path\":\"src/cobol/programs/MMRS71D1.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2008,\"name\":\"MMRS71E1\",\"path\":\"src/cobol/programs/MMRS71E1.cbl\"},\"clusterIndex\":-1},"
				+ "{\"module\":{\"id\":2009,\"name\":\"MMRS71F1\",\"path\":\"src/cobol/programs/MMRS71F1.cbl\"},\"clusterIndex\":-1},"
				+ "{\"module\":{\"id\":2003,\"name\":\"MMRS71Z1\",\"path\":\"src/cobol/programs/MMRS71Z1.cbl\"},\"clusterIndex\":1}],\"totalElements\":10}}";
		final Map<String, Object> variables = new HashMap<>();
		variables.put("projectId", projectId);
		variables.put("algorithm", "COBOL Methods");
		variables.put("page", 0);
		variables.put("size", 20);
		variables.put("sortObject", Map.of("content_module_name", "ASC"));

		/* Execute GraphQL request and verify response data */
		executeQuery(DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE, variables).path("").matchesJsonStrictly(expectedJson);
	}

	@DisplayName("Test dnaModulesInCluster query with one page, size and sort by name ASC")
	@Test
	void testModulesInClusterQueryWithOnePageSizeAndSortByNameASC() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);

		/* Set up variables and create expected JSON with default page and size */
		final String expectedJson = "{\"dnaModulesInCluster\":{\"content\":["
				+ "{\"module\":{\"id\":2007,\"name\":\"MMRS71C1\",\"path\":\"src/cobol/programs/MMRS71C1.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2002,\"name\":\"MMRS71D1\",\"path\":\"src/cobol/programs/MMRS71D1.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2008,\"name\":\"MMRS71E1\",\"path\":\"src/cobol/programs/MMRS71E1.cbl\"},\"clusterIndex\":-1},"
				+ "{\"module\":{\"id\":2009,\"name\":\"MMRS71F1\",\"path\":\"src/cobol/programs/MMRS71F1.cbl\"},\"clusterIndex\":-1},"
				+ "{\"module\":{\"id\":2003,\"name\":\"MMRS71Z1\",\"path\":\"src/cobol/programs/MMRS71Z1.cbl\"},\"clusterIndex\":1}],\"totalElements\":10}}";
		final Map<String, Object> variables = new HashMap<>();
		variables.put("projectId", projectId);
		variables.put("algorithm", "COBOL Methods");
		variables.put("page", 1);
		variables.put("size", 5);
		variables.put("sortObject", Map.of("content_module_name", "ASC"));

		/* Execute GraphQL request and verify response data */
		executeQuery(DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE, variables).path("").matchesJsonStrictly(expectedJson);
	}

	@DisplayName("Test dnaModulesInCluster query with one page, size and sort by name DESC")
	@Test
	void testModulesInClusterQueryWithOnePageSizeAndSortByNameDESC() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);

		/* Set up variables and create expected JSON with default page and size */
		final String expectedJson = "{\"dnaModulesInCluster\":{\"content\":["
				+ "{\"module\":{\"id\":2006,\"name\":\"MMRS71B1\",\"path\":\"src/cobol/programs/MMRS71B1.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2001,\"name\":\"MMRS7112\",\"path\":\"src/cobol/programs/MMRS7112.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2005,\"name\":\"MMRS7111\",\"path\":\"src/cobol/programs/MMRS7111.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2004,\"name\":\"MMRS7102\",\"path\":\"src/cobol/programs/MMRS7102.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2000,\"name\":\"MMRS7101\",\"path\":\"src/cobol/programs/MMRS7101.cbl\"},\"clusterIndex\":1}],\"totalElements\":10}}";
		final Map<String, Object> variables = new HashMap<>();
		variables.put("projectId", projectId);
		variables.put("algorithm", "COBOL Methods");
		variables.put("page", 1);
		variables.put("size", 5);
		variables.put("sortObject", Map.of("content_module_name", "DESC"));

		/* Execute GraphQL request and verify response data */
		executeQuery(DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE, variables).path("").matchesJsonStrictly(expectedJson);
	}

	@DisplayName("Test dnaModulesInCluster query with two page, size and sort by name DESC")
	@Test
	void testModulesInClusterQueryWithTwoPageSizeAndSortByNameDESC() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);

		/* Set up variables and create expected JSON with default page and size */
		final String expectedJson = "{\"dnaModulesInCluster\":{\"content\":[],\"totalElements\":10}}";
		final Map<String, Object> variables = new HashMap<>();
		variables.put("projectId", projectId);
		variables.put("algorithm", "COBOL Methods");
		variables.put("page", 2);
		variables.put("size", 5);
		variables.put("sortObject", Map.of("content_module_name", "DESC"));

		/* Execute GraphQL request and verify response data */
		executeQuery(DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE, variables).path("").matchesJsonStrictly(expectedJson);
	}

	@DisplayName("Test dnaModulesInCluster query with different variables for negative scenario")
	@Test
	void testModulesInClusterQueryWithNegativeScenario() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);

		/* Set up variables and create expected JSON with default page and size */
		final String expectedJson = "{\"dnaModulesInCluster\":{\"content\":[],\"totalElements\":0}}";
		final Map<String, Object> variables = new HashMap<>();
		variables.put("projectId", Long.valueOf(7));
		variables.put("algorithm", "NATURAL Methods");
		variables.put("page", 0);
		variables.put("size", 0);
		variables.put("sortObject", Map.of("content_module_name", "ASC"));

		/* Execute GraphQL request and verify response data */
		executeQuery(DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE, variables).path("").matchesJsonStrictly(expectedJson);
	}

	@DisplayName("Test dnaModulesInCluster query with one page, size and default sort")
	@Test
	void testModulesInClusterQueryWithOnePageSizeAndDefaultSort() {
		/* Set full user accesses to project */
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId), RoleType.VIEWER, NatureType.DISCOVERY);

		/* Set up variables and create expected JSON with default page and size */
		final String expectedJson = "{\"dnaModulesInCluster\":{\"content\":["
				+ "{\"module\":{\"id\":2007,\"name\":\"MMRS71C1\",\"path\":\"src/cobol/programs/MMRS71C1.cbl\"},\"clusterIndex\":2},"
				+ "{\"module\":{\"id\":2002,\"name\":\"MMRS71D1\",\"path\":\"src/cobol/programs/MMRS71D1.cbl\"},\"clusterIndex\":1},"
				+ "{\"module\":{\"id\":2008,\"name\":\"MMRS71E1\",\"path\":\"src/cobol/programs/MMRS71E1.cbl\"},\"clusterIndex\":-1},"
				+ "{\"module\":{\"id\":2009,\"name\":\"MMRS71F1\",\"path\":\"src/cobol/programs/MMRS71F1.cbl\"},\"clusterIndex\":-1},"
				+ "{\"module\":{\"id\":2003,\"name\":\"MMRS71Z1\",\"path\":\"src/cobol/programs/MMRS71Z1.cbl\"},\"clusterIndex\":1}],\"totalElements\":10}}";
		final Map<String, Object> variables = new HashMap<>();
		variables.put("projectId", projectId);
		variables.put("algorithm", "COBOL Methods");
		variables.put("page", 1);
		variables.put("size", 5);
		variables.put("sortObject", null);

		/* Execute GraphQL request and verify response data */
		executeQuery(DNA_MODULES_IN_CLUSTER_QUERY_TEMPLATE, variables).path("").matchesJsonStrictly(expectedJson);
	}

	private static void verifyReceivedDnaData(final Response response, final String expectedJson) {
		response.path("").matchesJson(expectedJson);
	}

	/**
	 * Executes the GraphQL query
	 *
	 * @param queryTemplate The query template
	 * @param variables {@linkplain Map} of required variables
	 * @return {@link Response} from GraphQL with the query result
	 */
	private Response executeQuery(final String queryTemplate, final Map<String, Object> variables) {
		final GraphQlTester.Request<?> request = assertNotNull(tester).document(queryTemplate);
		for (Map.Entry<String, Object> entry : variables.entrySet()) {
			request.variable(entry.getKey(), entry.getValue());
		}
		return request.execute();
	}

	/**
	 * Sets the project accesses for the current user. The given {@code authProjectIds} are used for setting the user's {@link Authentication}. The given
	 * {@code userRoleProjectIds} are used for the {@link UserRoleService}. Both lists can differ.
	 *
	 * <p>Usually the associated project IDs in the UserRoleService should match the project authorities. If a user has no authorization for a project
	 * ({@link SimpleGrantedAuthority}), the server responses with access {@code FORBIDDEN}. For explicitly testing of {@link ControllerLocalContext}
	 * and {@link UserRoleService} both project ID lists can differ.</p>
	 *
	 * @param authProjectIds project IDs for the {@link Authentication}
	 * @param userRoleProjectIds project IDs for the {@link UserRoleService}
	 * @param role the required {@linkplain RoleType}
	 * @param nature the required {@linkplain NatureType}
	 */
	private void setupProjectAccesses(final List<Long> authProjectIds, final List<Long> userRoleProjectIds, final RoleType role, final NatureType nature) {
		final UserRoleService userRoleService = assertNotNull(this.userRoleService);
		given(userRoleService.getProjectIds()).willReturn(userRoleProjectIds);

		final List<GrantedAuthority> authorities = new ArrayList<>(authProjectIds.size() * 2);
		authProjectIds.forEach(projectId -> {
			authorities.add(new MiningRole(String.format("client-1-project-%d-%s", projectId, role.getValue())));
			authorities.add(new MiningRole(String.format("client-1-project-%d-%s", projectId, nature.getValue())));
		});

		final Authentication auth = new UsernamePasswordAuthenticationToken("", "", authorities);
		final SecurityContext context = SecurityContextHolder.createEmptyContext();
		context.setAuthentication(auth);
		SecurityContextHolder.setContext(context);
	}

	private void loadTestData() {
		final var project = projectService.create(new ProjectPojoPrototype()
				.setNatures(new HashSet<>(Arrays.asList(ProjectNature.DISCOVERY)))
				.setName("TEST PROJECT 1")
				.setClient(EntityId.of(ONE))
				.setMetricsDate(LocalDate.of(2020, 5, 5).atStartOfDay(ZoneId.systemDefault()).toInstant())
			);
		projectId = project.getId();
		final EntityId projectPgId = project.identity();
		
		final List<ModulePojo> modules = new ArrayList<>();
		final List<String> programNames = Arrays.asList("MMRS7101", "MMRS7112", "MMRS71D1", "MMRS71Z1", "MMRS7102", "MMRS7111", "MMRS71B1", "MMRS71C1", "MMRS71E1", "MMRS71F1");
		final String content = "       PROCEDURE DIVISION.\\n           DISPLAY '1'\\n           .\\n";
		for (final String name : programNames) {
			final ModulePojoPrototype module = new ModulePojoPrototype()
					.setName(name)
					.setPath("src/cobol/programs/" + name + ".cbl")
					.setProject(EntityId.of(projectId))
					.setTechnology(Technology.COBOL)
					.setType(Type.PROGRAM)
					.setStorage(Storage.FILE)
					.setIdentification(Identification.IDENTIFIED)
					.setOrigin(Origin.CUSTOM)
					.setCreator(Creator.DISCOVERY)
					.setDescription("Test program for DnaData")
					.setContent(content);
			modules.add(moduleService.getModule(moduleService.create(module)));
		}
		
		final UUID idOfSnapshot1 = createDnaSnapshot(projectPgId);
		/* creating community with cluster index 1 */
		createDnaCommunity(Integer.valueOf(1), idOfSnapshot1, modules.subList(0, 2),  StringUtils.EMPTY);
		
		final UUID idOfSnapshot2 = createDnaSnapshot(projectPgId);
		/* creating community with cluster index 1 */
		createDnaCommunity(Integer.valueOf(1), idOfSnapshot2, modules.subList(0, 4), StringUtils.EMPTY);
		/* creating community with cluster index 2 */
		createDnaCommunity(Integer.valueOf(2), idOfSnapshot2, modules.subList(4, 8), StringUtils.EMPTY);
		/* creating community with cluster index -1 */
		createDnaCommunity(Integer.valueOf(-1), idOfSnapshot2, modules.subList(8, modules.size()), "Unassigned Modules");
	}

	private void createDnaCommunity(final Integer clusterIndex, final UUID snapshotId, final List<ModulePojo> modules, final String title) {
		final DnaCommunityPojoPrototype dnaCommunity =  new DnaCommunityPojoPrototype();
		dnaCommunity.setSnapshot(snapshotId);
		dnaCommunity.setSequencerId(DnaSequencer.COBOL_METHOD_RULE);
		dnaCommunity.setSimilarityId(DnaSimilarityAlgorithm.WEIGHTED_LEVENSHTEIN);
		dnaCommunity.setClusterAlgorithmId(DnaClusterAlgorithm.LOUVAIN);
		dnaCommunity.setTitle(title);
		dnaCommunity.setClusterIndex(clusterIndex);

		final UUID communityUuid = dnaData.createCommunity(dnaCommunity, true);

		final List<UUID> moduleUuids = modules.stream().map(m -> m.getUid()).collect(Collectors.toList());
		dnaData.putCommunityModules(communityUuid, moduleUuids);
	}
	
	private UUID createDnaSnapshot(final EntityId projectId) {
		final Map<String, Object> dnaConfig = new HashMap<>();
		dnaConfig.put("maxLevels", Integer.valueOf(5));
		dnaConfig.put("minDNALength", Integer.valueOf(20));
		dnaConfig.put("maxIterations", Integer.valueOf(10));
		dnaConfig.put("defaultTolerance", Double.valueOf(0.0001));
		dnaConfig.put("similarityThreshold", Double.valueOf(0.85));

		final DnaSnapshotPojoPrototype dnaSnapshot = new DnaSnapshotPojoPrototype()
							.setTotalModuleCount(Integer.valueOf(12))
							.setProjectId(projectId)
							.setDnaConfig(dnaConfig);

		return dnaData.createSnapshot(dnaSnapshot);
	}
}
