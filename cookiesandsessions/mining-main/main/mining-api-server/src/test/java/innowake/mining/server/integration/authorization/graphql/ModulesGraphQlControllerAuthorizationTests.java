/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.authorization.graphql;

import org.junit.jupiter.api.Test;
import org.springframework.graphql.execution.ErrorType;

import innowake.mining.server.graphql.controller.ModulesGraphQlController;

/**
 * Authorization tests for {@link ModulesGraphQlController}.
 */
class ModulesGraphQlControllerAuthorizationTests extends GraphQlAuthorizationTests {

	/* default query to use for testing - as this test suite only checks whether the query is permitted, the actual selection of fields is not relevant */
	private static String QUERY = "query ($projectId: Long!) { modules(projectId: $projectId) { totalElements } }";

	@Test void test0001() { test(QUERY, variables("projectId", 1), roles(admin()), expectSuccess()); }
	@Test void test0002() { test(QUERY, variables("projectId", 5), roles(admin()), expectSuccess()); }
	@Test void test0003() { test(QUERY, variables("projectId", 1), roles(miningViewer()), expectSuccess()); }
	@Test void test0004() { test(QUERY, variables("projectId", 5), roles(miningViewer()), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0005() { test(QUERY, variables("projectId", 1), roles(clientAdmin()), expectSuccess()); }
	@Test void test0006() { test(QUERY, variables("projectId", 1), roles(clientAdmin(2)), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0007() { test(QUERY, variables("projectId", 1), roles(miningManager()), expectSuccess()); }
	@Test void test0008() { test(QUERY, variables("projectId", 1), roles(miningManager(1, 2)), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0009() { test(QUERY, variables("projectId", 1), roles(miningEditor()), expectSuccess()); }
	@Test void test0010() { test(QUERY, variables("projectId", 1), roles(miningEditor(1, 2)), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0011() { test(QUERY, variables("projectId", 1), roles(miningViewer(1, 2)), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0012() { test(QUERY, variables("projectId", 5), roles(miningViewer(1, 5)), expectSuccess()); }
	@Test void test0013() { test(QUERY, variables("projectId", 5), roles(miningViewer(1, 4)), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0014() { test(QUERY, variables("projectId", 1), roles(discoveryManager()), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0015() { test(QUERY, variables("projectId", 1), roles(discoveryEditor()), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0016() { test(QUERY, variables("projectId", 1), roles(discoveryViewer()), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0017() { test(QUERY, variables("projectId", 1), roles(discoveryLightManager()), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0018() { test(QUERY, variables("projectId", 1), roles(discoveryLightEditor()), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0019() { test(QUERY, variables("projectId", 1), roles(discoveryLightViewer()), expectError(ErrorType.FORBIDDEN)); }

}
