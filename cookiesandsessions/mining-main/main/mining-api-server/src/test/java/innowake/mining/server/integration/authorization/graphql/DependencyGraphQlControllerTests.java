/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.integration.authorization.graphql;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.graphql.controller.DependencyGraphQlController;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.test.tester.GraphQlTester;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.test.web.servlet.client.MockMvcWebTestClient;
import org.springframework.web.context.WebApplicationContext;

import java.util.Collections;
import java.util.List;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.apache.commons.lang.math.NumberUtils.LONG_ONE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

/**
 * Tests for the schema mapping methods of the {@link DependencyGraphQlController}.
 * <p>Tests that GraphQL queries can reach other projects only through the GraphQL API if the user has access to them.</p>
 */
class DependencyGraphQlControllerTests extends AbstractGraphQlControllerTests {

	private static EntityId projectId = EntityId.VOID;

	@Autowired
	private WebApplicationContext webAppContext;
	@Autowired
	private ModuleService moduleService;

	private static final String QUERY_TEMPLATE = "query($projectId: Long!, $filter: FilterObject_moduleDependencies, $moduleId: Long!) { "
			+ "moduleDependencies(projectId: $projectId, page: 0, size: 30, filterObject: $filter, moduleId: $moduleId) { "
				+ "content { "
					+ "targetName "
					+ "targetId "
					+ "errorCount "
					+ "relationship "
					+ "direction "
					+ "identification "
				+ "} "
				+ "totalElements size "
			+ "} "
		+ "}";

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
	}

	@AfterAll
	void tearDown() {
		projectService.deleteProjectCascading(LONG_ONE, projectId.getNid());
	}

	@DisplayName("Test module dependencies identification and error count")
	@Test
	void testModuleDependenciesIdentificationAndErrorCount() {
		/* Create a Module */
		final var module1 = createTestModule(projectId, "Module1", "programs/Module1.cbl", Identification.IDENTIFIED);

		/* Create Module2 with error markers */
		final var module2 = createTestModule(projectId, "Module2", "programs/Module2.cbl", Identification.IDENTIFIED);
		final ErrorMarkerPojoPrototype error1 = new ErrorMarkerPojoPrototype()
				.setProject(projectId)
				.setModule(module2)
				.setCause("Error1");
		final ErrorMarkerPojoPrototype error2 = new ErrorMarkerPojoPrototype()
				.setProject(projectId)
				.setModule(module2)
				.setCause("Error2");
		moduleService.createErrorMarkers(List.of(error1, error2));

		/* Create Module3 with MISSING identification */
		final var module3 = createTestModule(projectId, "Module3", "programs/Module3.cbl", Identification.MISSING);

		createModuleReference(module3, module1, 10, 12);
		createModuleReference(module1, module2, 3, 15);
		createModuleReference(module1, module2, 14, 5);

		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final GraphQlTester.Response response = executeQueryBasedOnTemplate(projectId, module1.getNid());
		response.path("moduleDependencies.totalElements").entity(Long.class).isEqualTo(2L);
		final String moduleName = response.path("moduleDependencies.content[0].targetName").entity(String.class).get();
		if (moduleName.equals("Module2")) {
			assertModule(response, "IDENTIFIED", 2, 0);
			assertModule(response, "MISSING", 0, 1);
		} else {
			assertModule(response, "MISSING", 0, 0);
			assertModule(response, "IDENTIFIED", 2, 1);
		}
	}

	private void assertModule(final GraphQlTester.Response response, String identified, int errorCount, int index) {
		assertEquals(identified, response.path("moduleDependencies.content["+index+"].identification").entity(String.class).get());
		assertEquals(errorCount, response.path("moduleDependencies.content["+index+"].errorCount").entity(Integer.class).get());
	}

	private void createModuleReference(final EntityId module1, final EntityId module2, final Integer offset, final Integer length) {
		moduleService.createRelationship(
				new ModuleRelationshipPojoPrototype()
						.setSrcModule(module1)
						.setRelationship(RelationshipType.CALLS)
						.setDstModule(module2)
						.setSrcLocation(new ModuleLocation(offset, length)));
	}

	private EntityId createTestModule(final EntityId projectId, final String name, @Nullable final String path, final Identification identification) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(name);
		module.setProject(projectId);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(identification);
		module.setOrigin(Origin.CUSTOM);
		module.setDescription("Creating Module " + name);
		module.setCreator(Creator.DISCOVERY);
		module.setPath(path);
		return moduleService.create(module);
	}

	private GraphQlTester.Response executeQueryBasedOnTemplate(final EntityId projectId, final Long moduleId) {
		return assertNotNull(tester)
				.document(DependencyGraphQlControllerTests.QUERY_TEMPLATE)
				.variable("projectId", projectId.getNid())
				.variable("moduleId", moduleId)
				.variable("filterObject", null)
				.execute();
	}

}
