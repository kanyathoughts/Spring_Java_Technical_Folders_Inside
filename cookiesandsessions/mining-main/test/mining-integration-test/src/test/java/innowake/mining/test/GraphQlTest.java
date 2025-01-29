/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.springframework.graphql.test.tester.GraphQlTester;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.test.web.reactive.server.WebTestClient;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.client.service.project.ProjectServiceProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.test.util.RestTemplateUtil;
import software.amazon.awssdk.utils.StringUtils;

/**
 * Test submitting queries to GraphQL endpoint
 */
class GraphQlTest extends IntegrationTest {

	private static final String GRAPHQL_ENDPOINT = "/v2/graphql";

	private final WebTestClient client;

	public GraphQlTest() {
		final ConnectionInfo info = getConnectionInfo();
		this.client = WebTestClient.bindToServer()
				.baseUrl(info.getUrl() + RouteConfiguration.API_BASE + GRAPHQL_ENDPOINT)
				.defaultHeaders((h) -> h.addAll(RestTemplateUtil.getHttpHeaders(info)))
				.build();
	}

	private GraphQlTester.Response query(final String q) {
		return HttpGraphQlTester.create(client).document(q).execute();
	}

	@Test
	void testQueryProject() {
		query("{ project(projectId: 1) { name client { name } } }").path("")
			.matchesJson("{\"project\":{\"name\":\"Demo Project A\",\"client\":{\"name\":\"Demo Client 1\"}}}");
	}
	
	@Test
	void testQueryDateTimeInstant() throws IOException {
		ProjectServiceProvider projectService = MiningApiClient.projectService(getConnectionInfo());
		projectService.updateProject().setProject(new ProjectPojoPrototype()
				.withId(EntityId.of(1L))
				.setMetricsDate(Instant.ofEpochMilli(1))).execute();
		query("{ project(projectId: 1) { metricsDate } }").path("")
			.matchesJson("{\"project\":{\"metricsDate\":\"1970-01-01T00:00:00.001Z\"}}");
	}

	@Test
	void testQueryDateTimeNull() {
		query("{ project(projectId: 2) { metricsDate, metricsDateTimestamp } }").path("")
			.matchesJson("{\"project\":{\"metricsDate\":null,\"metricsDateTimestamp\":null}}");
	}

	@Test
	void testQueryDateTime() {
		@SuppressWarnings("unchecked")
		final List<Map<String, Object>> modules =
			query("{ modules(projectId: 1) { content { modifiedDate, modifiedDateTimestamp } } }").path("data.modules.content").entity(List.class).get();
		assertEquals(43, modules.size());
		for (final Map<String, Object> module : modules){
			assertEquals(
				((Long)module.get("modifiedDateTimestamp")).longValue(),
				Instant.parse((String)module.get("modifiedDate")).toEpochMilli()
			);
		}
	}

	@Test
	void testQueryClient() {
		query("{ client(clientId: 1) { name } }").path("")
			.matchesJson("{client:{name:\"Demo Client 1\"}}");
	}

	@Test
	void testQueryModules() {
		query("{ modules(projectId: 1) { totalElements content { name technology type annotationCount } } }").path("")
			.matchesJson("{\"modules\":{\"totalElements\":43,\"content\":["
					+ "{\"name\":\"QBGPSLP1MMRS710A.STEP01.MMRS7102\",\"technology\":\"JCL\",\"type\":\"EXEC_PGM\",\"annotationCount\":1},"
					+ "{\"name\":\"DPGM1\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"BASICOFUNC\",\"technology\":\"BASIC\",\"type\":\"FUNCTION\",\"annotationCount\":0},"
					+ "{\"name\":\"MMRS7101\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":1},"
					+ "{\"name\":\"DPGM2\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"BASICOPROG\",\"technology\":\"BASIC\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"DPGM3\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"V2_API_TEST\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"DPGM4\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"V2_API_UPDATE_TEST\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"PRGA\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"DPGM5\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"PRGTEST\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"PRGB\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"DPGM6\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"CC1\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"DPGM7\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"CC2\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"DPGM8\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"PRGC\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"DPGM9\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"PRGD\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"DPGM10\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"NATPGRA\",\"technology\":\"NATURAL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"IOSCOPE\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"DPGM11\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"NATCCA\",\"technology\":\"NATURAL\",\"type\":\"COPYCODE\",\"annotationCount\":0},"
					+ "{\"name\":\"UISCOPE\",\"technology\":\"CICS\",\"type\":\"BMS_MAPSET\",\"annotationCount\":0},"
					+ "{\"name\":\"DPGM12\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"DBSCOPE\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"DPGM13\",\"technology\":\"COBOL\",\"type\":\"COPYBOOK\",\"annotationCount\":0},"
					+ "{\"name\":\"PARAM\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"EXECSQL\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":3},"
					+ "{\"name\":\"IO001A\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"PRG1\",\"technology\":\"NATURAL\",\"type\":\"PROGRAM\",\"annotationCount\":4},"
					+ "{\"name\":\"PRG2\",\"technology\":\"NATURAL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"PRG3\",\"technology\":\"NATURAL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"PRG4\",\"technology\":\"NATURAL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"PRGx\",\"technology\":\"NATURAL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"DISPLAYPGM\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":0},"
					+ "{\"name\":\"BASICO\",\"technology\":\"BASIC\",\"type\":\"OBJECT\",\"annotationCount\":0},"
					+ "{\"name\":\"IDCAMS\",\"technology\":\"UNKNOWN\",\"type\":\"UTILITY\",\"annotationCount\":0},"
					+ "{\"name\":\"ABEND\",\"technology\":\"UNKNOWN\",\"type\":\"UTILITY\",\"annotationCount\":0}]}}");
	}

	@Test
	void testQueryStatements() {
		query("query { statements(projectId: 1) { content { text } } }").path("")
			.matchesJson("{\"statements\":{\"content\":["
					+ "{\"text\":\"TestStatementC\"},"
					+ "{\"text\":\"TestStatementD\"}]}}");
	}

	@Test
	void testQuerySqlStatements() {
		query("query { statements(projectId: 1) { content { text sqlLength tables  } } }").path("")
			.matchesJson("{\"statements\":{\"content\":["
					+ "{\"text\":\"TestStatementC\",\"sqlLength\":84,\"tables\":15},"
					+ "{\"text\":\"TestStatementD\",\"sqlLength\":8,\"tables\":10}]}}");
	}

	@Test
	void testQuerySqlStatementsWithMultiExpect() {
		query("{ "
				+ "  statements(projectId: 1) { "
				+ "    content { "
				+ "      type "
				+ "      text "
				+ "      sqlLength "
				+ "      tables "
				+ "    } "
				+ "  } "
				+ "}").path("")
		.matchesJson("{\"statements\":{\"content\":["
				+ "{\"type\":\"SELECT\",\"text\":\"TestStatementC\",\"sqlLength\":84,\"tables\":15},"
				+ "{\"type\":\"SELECT\",\"text\":\"TestStatementD\",\"sqlLength\":8,\"tables\":10}]}}");
	}

	@Test
	void testQueryStatementsByFilterWithDiffValues() {
		query("{\n"
				+ "  statements(projectId: 3, filterObject: {content_module_id: {eq: 2044}}) {\n"
				+ "    content {\n"
				+ "      text\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
		.matchesJson("{\"statements\":{\"content\":["
				+ "{\"text\":\"TestStatementA\"},"
				+ "{\"text\":\"TestStatementB\"}]}}");
		query("{\n"
				+ "  statements(projectId: 1, filterObject: {content_module_id: {eq: 2000}}) {\n"
				+ "    content {\n"
				+ "      text\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
		.matchesJson("{\"statements\":{\"content\":["
				+ "{\"text\":\"TestStatementC\"}]}}");
		query("{\n"
				+ "  statements(projectId: 3, filterObject: {content_text: {eq: \"TestStatementB\"}}) {\n"
				+ "    content {\n"
				+ "      text\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
		.matchesJson("{\"statements\":{\"content\":["
				+ "{\"text\":\"TestStatementB\"}]}}");
		query("{\n"
				+ "  statements(\n"
				+ "    projectId: 3\n"
				+ "    filterObject: {content_module_path: {eq: \"src/basic/programs/BasicModule.bas\"}}\n"
				+ "  ) {\n"
				+ "    content {\n"
				+ "      text\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
		.matchesJson("{\"statements\":{\"content\":["
				+ "{\"text\":\"TestStatementA\"},"
				+ "{\"text\":\"TestStatementB\"}]}}");
		query("{\n"
				+ "  statements(\n"
				+ "    projectId: 3\n"
				+ "    filterObject: {content_module_name: {eq: \"BasicModule\"}}\n"
				+ "  ) {\n"
				+ "    content {\n"
				+ "      text\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
		.matchesJson("{\"statements\":{\"content\":["
				+ "{\"text\":\"TestStatementA\"},"
				+ "{\"text\":\"TestStatementB\"}]}}");
	}

	@Test
	void testQuerySqlStatementsWithoutAnyFilter() {
		query("{\n"
				+ "  statements(projectId: 1, filterObject: {}) {\n"
				+ "    content {\n"
				+ "      text\n"
				+ "      sqlLength\n"
				+ "      tables\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
		.matchesJson("{\"statements\":{\"content\":["
				+ "{\"text\":\"TestStatementC\",\"sqlLength\":84,\"tables\":15},"
				+ "{\"text\":\"TestStatementD\",\"sqlLength\":8,\"tables\":10}]}}");
	}

	@Test
	void testQueryStatementsWithoutAnyFilter() {
		query("{\n"
				+ "  statements(projectId: 3, filterObject: {}) {\n"
				+ "    content {\n"
				+ "      text\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
		.matchesJson("{\"statements\":{\"content\":["
				+ "{\"text\":\"TestStatementA\"},"
				+ "{\"text\":\"TestStatementB\"}]}}");
	}

	@Test
	void testQueryModuleSqlStatementsByFilter() {
		query("{\n"
				+ "  statements(projectId: 3, filterObject: {content_module_id: {eq: 2044}}) {\n"
				+ "    content {\n"
				+ "      text\n"
				+ "      sqlLength\n"
				+ "      tables\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
		.matchesJson("{\"statements\":{\"content\":["
				+ "{\"text\":\"TestStatementA\",\"sqlLength\":84,\"tables\":15},"
				+ "{\"text\":\"TestStatementB\",\"sqlLength\":8,\"tables\":10}]}}");
		query("{\n"
				+ "  statements(\n"
				+ "    projectId: 3\n"
				+ "    filterObject: {content_module_name: {eq: \"BasicModule\"}}\n"
				+ "  ) {\n"
				+ "    content {\n"
				+ "      text\n"
				+ "      sqlLength\n"
				+ "      tables\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
		.matchesJson("{\"statements\":{\"content\":["
				+ "{\"text\":\"TestStatementA\",\"sqlLength\":84,\"tables\":15},"
				+ "{\"text\":\"TestStatementB\",\"sqlLength\":8,\"tables\":10}]}}");
		query("{\n"
				+ "  statements(\n"
				+ "    projectId: 3\n"
				+ "    filterObject: {content_module_path: {eq: \"src/basic/programs/BasicModule.bas\"}}\n"
				+ "  ) {\n"
				+ "    content {\n"
				+ "      text\n"
				+ "      sqlLength\n"
				+ "      tables\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
		.matchesJson("{\"statements\":{\"content\":["
				+ "{\"text\":\"TestStatementA\",\"sqlLength\":84,\"tables\":15},"
				+ "{\"text\":\"TestStatementB\",\"sqlLength\":8,\"tables\":10}]}}");
	}

	@Test
	void testQueryAnnotationWithIdDescriptionTypeStateAndCategory() {
		query("{\n"
				+ "  annotations(projectId: 1) {\n"
				+ "    totalElements\n"
				+ "    content {\n"
				+ "      id\n"
				+ "      name\n"
				+ "      type\n"
				+ "      state\n"
				+ "      categoryName\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
			.matchesJson("{\"annotations\":{\"totalElements\":9,\"content\":["
					+ "{\"id\":2,\"name\":\"Annotation 2\",\"type\":\"RULE\",\"state\":\"CANDIDATE\",\"categoryName\":\"Annotation Category A\"},"
					+ "{\"id\":3,\"name\":\"Annotation 3\",\"type\":\"RULE\",\"state\":\"CANDIDATE\",\"categoryName\":\"Annotation Category A\"},"
					+ "{\"id\":4,\"name\":\"Annotation 4\",\"type\":\"RULE\",\"state\":\"CANDIDATE\",\"categoryName\":\"Annotation Category A\"},"
					+ "{\"id\":5,\"name\":\"Annotation 5\",\"type\":\"RULE\",\"state\":\"IN_ANALYSIS\",\"categoryName\":\"Annotation Category B\"},"
					+ "{\"id\":6,\"name\":\"Annotation 6\",\"type\":\"RULE\",\"state\":\"REJECTED\",\"categoryName\":\"Annotation Category C\"},"
					+ "{\"id\":7,\"name\":\"Database Annotation 1\",\"type\":\"DATABASE\",\"state\":\"CANDIDATE\",\"categoryName\":\"Annotation Category B\"},"
					+ "{\"id\":8,\"name\":\"Database Annotation 2\",\"type\":\"DATABASE\",\"state\":\"CANDIDATE\",\"categoryName\":\"Annotation Category B\"},"
					+ "{\"id\":1,\"name\":\"Annotation 1\",\"type\":\"RULE\",\"state\":\"CANDIDATE\",\"categoryName\":\"Annotation Category A\"},"
					+ "{\"id\":9,\"name\":\"DeleteAnnotation\",\"type\":\"DATABASE\",\"state\":\"CANDIDATE\",\"categoryName\":\"Annotation Category B\"}]}}");
	}

	@Test
	void testQueryAnnotationWithDescriptionSourceCodeCreatedByAndModifiedBy() {
		query("{ annotations(projectId: 1) { totalElements content { name sourceAttachment createdByUserId updatedByUserId } } }")
			.path("")
			.matchesJson("{\"annotations\":{\"totalElements\":9,\"content\":["
					+ "{\"name\":\"Database Annotation 2\",\"sourceAttachment\": \"GOBACK.\",\"createdByUserId\":\"system_user\","
					+ "\"updatedByUserId\":\"system_user\"},"
					+ "{\"name\":\"Annotation 1\",\"sourceAttachment\": \"abcd\",\"createdByUserId\":\"admin\",\"updatedByUserId\":\"admin\"},"
					+ "{\"name\":\"DeleteAnnotation\",\"sourceAttachment\": \"test\",\"createdByUserId\":\"admin\",\"updatedByUserId\":null},"
					+ "{\"name\":\"Annotation 2\",\"sourceAttachment\": \"1234\",\"createdByUserId\":\"admin\",\"updatedByUserId\":null},"
					+ "{\"name\":\"Annotation 3\",\"sourceAttachment\": \"efgh\",\"createdByUserId\":\"admin\",\"updatedByUserId\":null},"
					+ "{\"name\":\"Annotation 4\",\"sourceAttachment\": \"5678\",\"createdByUserId\":\"admin\",\"updatedByUserId\":null},"
					+ "{\"name\":\"Annotation 5\",\"sourceAttachment\": \"5678\",\"createdByUserId\":\"admin\",\"updatedByUserId\":null},"
					+ "{\"name\":\"Annotation 6\",\"sourceAttachment\": \"5678\",\"createdByUserId\":\"admin\",\"updatedByUserId\":null},"
					+ "{\"name\":\"Database Annotation 1\",\"sourceAttachment\": \"IDENTIFICATION\",\"createdByUserId\":\"system_user\","
					+ "\"updatedByUserId\":\"admin\"}]}}");
	}

	@Test
	void testQueryAnnotationModuleDataPoints() {
		query("{\n"
				+ "  annotations(projectId: 1) {\n"
				+ "    totalElements\n"
				+ "    content {\n"
				+ "      module {\n"
				+ "        name\n"
				+ "        path\n"
				+ "        technology\n"
				+ "        type\n"
				+ "      }\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
			.matchesJson("{\"annotations\":{\"totalElements\":9,\"content\":["
					+ "{\"module\":{\"name\":\"MMRS7101\",\"path\":\"src/cobol/programs/MMRS7101.cbl\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\"}},"
					+ "{\"module\":{\"name\":\"EXECSQL\",\"path\":\"src/cobol/programs/EXECSQL.cbl\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\"}},"
					+ "{\"module\":{\"name\":\"EXECSQL\",\"path\":\"src/cobol/programs/EXECSQL.cbl\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\"}},"
					+ "{\"module\":{\"name\":\"PRG1\",\"path\":\"src-natural/LibA/PRG1.nsp\",\"technology\":\"NATURAL\",\"type\":\"PROGRAM\"}},"
					+ "{\"module\":{\"name\":\"EXECSQL\",\"path\":\"src/cobol/programs/EXECSQL.cbl\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\"}},"
					+ "{\"module\":{\"name\":\"PRG1\",\"path\":\"src-natural/LibA/PRG1.nsp\",\"technology\":\"NATURAL\",\"type\":\"PROGRAM\"}},"
					+ "{\"module\":{\"name\":\"PRG1\",\"path\":\"src-natural/LibA/PRG1.nsp\",\"technology\":\"NATURAL\",\"type\":\"PROGRAM\"}},"
					+ "{\"module\":{\"name\":\"PRG1\",\"path\":\"src-natural/LibA/PRG1.nsp\",\"technology\":\"NATURAL\",\"type\":\"PROGRAM\"}},"
					+ "{\"module\":{\"name\":\"QBGPSLP1MMRS710A.STEP01.MMRS7102\",\"path\":null,\"technology\":\"JCL\",\"type\":\"EXEC_PGM\"}}]}}");
	}

	@Test
	void testQueryEdgeProperties() {
		final GraphQlTester.Response response = query("{\n"
				+ "  modules(projectId: 1, filterObject: {content_id:{eq: 2000}}) {\n"
				+ "    content {\n"
				+ "      name\n"
				+ "      customProperties\n"
				+ "      dependencies {\n"
				+ "        properties\n"
				+ "      }\n"
				+ "    }\n"
				+ "  }\n"
				+ "}");
		final ModuleRelationshipPojo r1 = response.path("modules.content[0].dependencies[0]").entity(ModuleRelationshipPojo.class).get();
		final ModuleRelationshipPojo r2 = response.path("modules.content[0].dependencies[1]").entity(ModuleRelationshipPojo.class).get();
		final ModuleRelationshipPojo r3 = response.path("modules.content[0].dependencies[2]").entity(ModuleRelationshipPojo.class).get();

		var edgeList = Arrays.asList(r1, r2, r3);

		assertEquals(3, response.path("modules.content[0].dependencies").entityList(Object.class).get().size());
		assertTrue("One of the edges should  have the property { \"TEST_PROPERTY\": \"Hello World\" }",
				edgeList.stream().anyMatch(ref -> {
					final Optional<Map<String, Object>> properties = ref.getProperties();
					if (properties.isEmpty() || properties.get().isEmpty()) {
						return false;
					}
					return "Hello World".equals(properties.get().get("TEST_PROPERTY"));
				}));
	}

	@Test
	void testQueryAnnotationsByModuleNameWithWildcard() {
		final List<AnnotationPojo> annotationList = query("{ annotations(projectId: 1, "
				+ "filterObject: {content_module_name: {eq: \"prg%\"}}) "
				+ "{ content { name moduleName } } } ")
			.path("annotations.content")
			.entityList(AnnotationPojo.class)
			.get();

		/* the program "PRG1", which should be found by the wildcard, has 4 annotations (there are 13 annotations in total in the test data) */
		assertEquals(4, annotationList.size());
		/* check that annotations for the correct module were found */
		assertTrue("The Module name of all found Annotations should start with 'prg'", annotationList.stream().allMatch(annotation ->
			StringUtils.startsWithIgnoreCase(annotation.getModuleName(), "prg")
		));
	}

	@Test
	void testQueryModulesWithFilterObjectAndOperation() {
		query("{ modules(projectId: 1, filterObject : {content_name : {eq:\"MMRS%\"},"
				+ "content_technology: {eq : COBOL}}) { "
				+ "totalElements content { name technology type annotationCount } } }").path("")
				.matchesJsonStrictly("{\"modules\":{\"totalElements\":1,\"content\":[{\"name\":\"MMRS7101\",\"technology\":\"COBOL\",\"type\":\"PROGRAM\",\"annotationCount\":1}]}}");
	}

	@Test
	void testQueryAnnotationWithFilterObject() {
		query("{\n"
				+ "  annotations(projectId: 1, filterObject: {\n"
				+ "    content_type: {eq: DATABASE}}) {\n"
				+ "    totalElements\n"
				+ "    content {\n"
				+ "      id\n"
				+ "      name\n"
				+ "      type\n"
				+ "      state\n"
				+ "      categoryName\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
		.matchesJson("{\"annotations\":{\"totalElements\":3,\"content\":["
				+ "{\"id\":9,\"name\":\"DeleteAnnotation\",\"type\":\"DATABASE\",\"state\":\"CANDIDATE\",\"categoryName\":\"Annotation Category B\"},"
				+ "{\"id\":7,\"name\":\"Database Annotation 1\",\"type\":\"DATABASE\",\"state\":\"CANDIDATE\",\"categoryName\":\"Annotation Category B\"},"
				+ "{\"id\":8,\"name\":\"Database Annotation 2\",\"type\":\"DATABASE\",\"state\":\"CANDIDATE\",\"categoryName\":\"Annotation Category B\"}]}}");
	}

	@Test
	void testQueryDataDictionaryWithFilterObject() {
		query("{\n"
				+ "  dataDictionaries(\n"
				+ "    projectId: 1\n"
				+ "    filterObject: {content_module_name: {eq: \"MMRS\"}}\n"
				+ "  ) {\n"
				+ "    totalElements\n"
				+ "    content {\n"
				+ "      id\n"
				+ "      name\n"
				+ "      createdByUserId\n"
				+ "      module { name }\n"
				+ "    }\n"
				+ "  }\n"
				+ "}").path("")
				.matchesJson("{\n"
						+ "    \"dataDictionaries\": {\n"
						+ "      \"totalElements\": 3,\n"
						+ "      \"content\": [\n"
						+ "        {\n"
						+ "          \"id\": 2,\n"
						+ "          \"name\": \"MY-BIN-FIELDS\",\n"
						+ "          \"createdByUserId\": \"admin\",\n"
						+ "          \"module\": { \"name\": \"MMRS7101\" }\n"
						+ "        },\n"
						+ "        {\n"
						+ "          \"id\": 3,\n"
						+ "          \"name\": \"MY-HEX-ORIGIN-LEN\",\n"
						+ "          \"createdByUserId\": \"admin\",\n"
						+ "          \"module\": { \"name\": \"MMRS7101\" }\n"
						+ "        },\n"
						+ "        {\n"
						+ "          \"id\": 1,\n"
						+ "          \"name\": \"MY-PROGRAM-NAME\",\n"
						+ "          \"createdByUserId\": \"admin\",\n"
						+ "          \"module\": { \"name\": \"MMRS7101\" }\n"
						+ "        }\n"
						+ "      ]\n"
						+ "    }\n"
						+ "  }");
	}

	@Test
	void testQuerySqlStatementsForNameNotLikeAndIn() {
		query("{ "
					+ "statements( projectId: 2 "
						+ "filterObject: { "
							+ "content_type: {"
								+ "in: [\"INSERT\", \"UPDATE\", \"DELETE\", \"MERGE\", \"SELECT\", \"ALTER_CHECK_CONSTRAINT\"] "
								+ "notIn: [\"ALTER_VIEW\", \"ALTER_TABLE\", \"ALTER_CHECK_CONSTRAINT\", \"CREATE_VIEW\", \"CREATE_TABLE DROP_VIEW\", \"DROP_TABLE\", \"GRANT\"]"
							+ "} "
						+ "} "
					+ ") "
					+ "{ totalElements "
						+ "content { text type } "
					+ "} "
				+ "}")
			.path("")
			.matchesJson("{\"statements\":{\"totalElements\":1,\"content\":[{\"text\":\"TestStatementG\",\"type\":\"SELECT\"}]}}");
	}
	
	@Test
	void testQuerySqlStatementsForNameNotLikeAndNotIn() {
		query("{\n"
				+ "  statements(\n"
				+ "    projectId: 2\n"
				+ "    filterObject: {content_type: {notIn: [\"ALTER_CHECK_CONSTRAINT\", \"SELECT\"], \n"
				+ "      in: [\"SELECT\", \"ALTER_CHECK_CONSTRAINT\", \"DECLARE_TABLE\"]}}\n"
				+ "  ) {\n"
				+ "    totalElements\n"
				+ "    content {\n"
				+ "      text\n"
				+ "      type\n"
				+ "    }\n"
				+ "  }\n"
				+ "}")
						.path("")
						.matchesJson("{\"statements\":{\"totalElements\":1,\"content\":[{\"text\":\"TestStatementE\","
								+ "\"type\":\"DECLARE_TABLE\"}]}}");
	}
	
	@Test
	void testQuerySqlStatementsForNameIn() {
		query("{\n"
				+ "  statements(\n"
				+ "    projectId: 2\n"
				+ "    filterObject: {content_type: {in: [\"LOCK_TABLE\", \"OPEN\", \"CLOSE\", \"FETCH\", \"COMMIT\", \"EXECUTE\", \"ROLLBACK\", \"ALLOCATE_CURSOR\", \"SET\", \"ASSOCIATE_LOCATOR\", \"PREPARE\", \"ENTRY\", \"WHENEVER\", \"UNKNOWN\", \"DECLARE_TABLE\", \"DECLARE_SCHEMA\", \"DECLARE_TEMP_TABLE\", \"CALL\"]}}\n"
				+ "  ) {\n"
				+ "    totalElements\n"
				+ "    content {\n"
				+ "      text\n"
				+ "      type\n"
				+ "    }\n"
				+ "  }\n"
				+ "}")
						.path("")
						.matchesJson("{\"statements\":{\"totalElements\":1,\"content\":[{\"text\":\"TestStatementE\","
								+ "\"type\":\"DECLARE_TABLE\"}]}}");
	}
	
	@Test
	void testQuerySqlStatementsForNameNotIn() {
		query("{\n"
				+ "  statements(\n"
				+ "    projectId: 1\n"
				+ "    filterObject: {content_type: {notIn: [\"LOCK_TABLE\", \"OPEN\", \"CLOSE\", \"FETCH\", \"COMMIT\", \"EXECUTE\", \"ROLLBACK\", \"ALLOCATE_CURSOR\", \"SET\", \"ASSOCIATE_LOCATOR\", \"PREPARE\", \"ENTRY\", \"WHENEVER\", \"UNKNOWN\", \"DECLARE_TABLE\", \"DECLARE_SCHEMA\", \"DECLARE_TEMP_TABLE\", \"CALL\"]}}\n"
				+ "  ) {\n"
				+ "    totalElements\n"
				+ "    content {\n"
				+ "      text\n"
				+ "      type\n"
				+ "    }\n"
				+ "  }\n"
				+ "}")
						.path("")
						.matchesJson("{\"statements\":{\"totalElements\":2,\"content\":["
								+ "{\"text\":\"TestStatementD\",\"type\":\"SELECT\"},"
								+ "{\"text\":\"TestStatementC\",\"type\":\"SELECT\"}]}}");
	}
}
