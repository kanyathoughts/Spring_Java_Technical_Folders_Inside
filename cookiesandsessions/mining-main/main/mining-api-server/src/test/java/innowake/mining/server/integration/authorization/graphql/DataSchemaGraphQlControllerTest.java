/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.authorization.graphql;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.apache.commons.lang.math.NumberUtils.LONG_ONE;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.test.tester.GraphQlTester.Response;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.test.web.servlet.client.MockMvcWebTestClient;
import org.springframework.web.context.WebApplicationContext;

import innowake.mining.server.graphql.controller.DataSchemaGraphQlController;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.FieldInfoPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for the schema mapping methods of the {@link DataSchemaGraphQlController}.
 * <p>Tests that GraphQL queries can reach other projects only through the GraphQL API if the user has access to them.</p>
 */
class DataSchemaGraphQlControllerTest extends AbstractGraphQlControllerTests {
	
	private static EntityId projectId = EntityId.VOID;
	private static EntityId moduleId = EntityId.VOID;

	private static final String QUERY_TEMPLATE = "query ($projectId: EntityId, $moduleId: EntityId) {" +
			"fieldInfos(projectId: $projectId, moduleId: $moduleId) {" +
				"ordinal, " +	
				"name, " +
				"dataType, " + 
				"size, "+
				"reference, " +
				"primaryKey, " +
				"autoIncrement, " +
				"comment" +
			"}" + 
		"}";
	
	@Autowired
	private FieldInfoService fieldInfoService;
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
		
		final ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setName("Project_1");
		project.setClient(EntityId.of(LONG_ONE));
		project.setNatures(Collections.emptySet());
		projectId = projectService.create(project).identity();
		
		moduleId = createTestModule("ADMIN_B_MESSAGES", projectId);

		final Map<String, Object> properties1 = new HashMap<>();
		properties1.put("type", "INTEGER");
		properties1.put("size", String.valueOf(4));
		final FieldInfoPojoPrototype fieldInfo1 = new FieldInfoPojoPrototype()
				.setOrdinal(1)
				.setName("MVERSION")
				.setModule(moduleId)
				.setProperties(properties1);
		fieldInfoService.create(fieldInfo1);

		final Map<String, Object> properties2 = new HashMap<>();
		properties2.put("type", "CHAR");
		properties2.put("size", String.valueOf(12));
		final FieldInfoPojoPrototype fieldInfo2 = new FieldInfoPojoPrototype()
				.setOrdinal(1)
				.setName("PERSISTENT")
				.setModule(moduleId)
				.setProperties(properties2);
		fieldInfoService.create(fieldInfo2);
	}
	
	@Test
	void testSingleProjectFieldInfoSearch() {
		setupProjectAccesses(Arrays.asList(projectId), Arrays.asList(projectId));
		
		final String expectedJson = "{\"fieldInfos\"=[{\"ordinal\"=1, \"name\"=\"MVERSION\", \"dataType\"=\"INTEGER\", \"size\"=4, \"reference\"=null," +
		"\"primaryKey\"=null, \"autoIncrement\"=false, \"comment\"=null}, {\"ordinal\"=1, \"name\"=\"PERSISTENT\", \"dataType\"=\"CHAR\", \"size\"=12," +
		"\"reference\"=null, \"primaryKey\"=null, \"autoIncrement\"=false, \"comment\"=null}]}";
		
		executeQuery(QUERY_TEMPLATE).path("").matchesJsonStrictly(expectedJson);
	}
	
	/**
	 * Executes the given GraphQL query.
	 * 
	 * @param queryTemplate the query template to run
	 * @return {@link Response} from GraphQL with the query result
	 */
	private Response executeQuery(final String queryTemplate) {
		final Response response = assertNotNull(tester)
				.document(queryTemplate)
				.variable("projectId", projectId.getNid())
				.variable("moduleId", moduleId.getNid())
				.execute();
		return response;
	}

	private EntityId createTestModule(final String name, final EntityId project) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(project);
		module.setName(name);
		module.setOrigin(Origin.CUSTOM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setPath("path");
		module.setTechnology(Technology.SQL);
		module.setType(Type.TABLE);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
}
