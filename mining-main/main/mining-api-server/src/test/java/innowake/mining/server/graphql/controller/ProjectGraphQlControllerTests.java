/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import static innowake.lib.core.lang.Assert.assertNotNull;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.test.tester.GraphQlTester.Response;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.graphql.test.tester.WebGraphQlTester;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.test.web.servlet.client.MockMvcWebTestClient;
import org.springframework.web.context.WebApplicationContext;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.integration.authorization.graphql.GraphQlAuthorizationTests;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.tags.IntegrationTest;

/**
 * Integration tests for {@link ProjectGraphQlController}.
 */
@IntegrationTest
class ProjectGraphQlControllerTests extends DatabaseResettingTest {

	@Autowired
	private WebApplicationContext webAppContext;

	@Nullable
	private WebGraphQlTester tester;

	@BeforeAll
	void init() {
		final WebTestClient client = MockMvcWebTestClient.bindToApplicationContext(webAppContext)
				.configureClient()
				.baseUrl(GraphQlAuthorizationTests.GRAPHQL_ENDPOINT)
				.build();

		tester = HttpGraphQlTester.create(client);
	}

	@Test
	void testQueryNid() {
		final ProjectPojo projectPojo = projectService.get(EntityId.of(1L));

		final String query = "query ($projectId: EntityId!) { project(projectId: $projectId) { uid } }";
		final Response response = assertNotNull(tester)
				.document(query)
				.variable("projectId", 1L)
				.execute();

		response.path("project.uid").matchesJsonStrictly("\"" + projectPojo.getUid().toString() + "\"");
	}
	@Test
	void testQueryUUID() {
		final ProjectPojo projectPojo = projectService.get(EntityId.of(1L));

		final String query = "query ($projectId: EntityId!) { project(projectId: $projectId) { uid } }";
		final Response response = assertNotNull(tester)
				.document(query)
				.variable("projectId", projectPojo.getUid())
				.execute();

		response.path("project.uid").matchesJsonStrictly("\"" + projectPojo.getUid().toString() + "\"");
	}
}