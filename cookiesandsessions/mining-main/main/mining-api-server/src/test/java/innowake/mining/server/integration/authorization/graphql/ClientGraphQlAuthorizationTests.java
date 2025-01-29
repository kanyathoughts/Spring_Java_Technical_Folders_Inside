/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization.graphql;

import org.junit.jupiter.api.Test;
import org.springframework.graphql.execution.ErrorType;

import innowake.mining.server.graphql.controller.ClientGraphQlController;

/**
 * Authorization tests for {@link ClientGraphQlController}.
 */
class ClientGraphQlAuthorizationTests extends GraphQlAuthorizationTests {

	/* default query to use for testing - as this test suite only checks whether the query is permitted, the actual selection of fields is not relevant */
	private static String QUERY = "query ($clientId: Long!)  { client(clientId: $clientId) { name } }";
	
	@Test void test0001() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(admin()), expectSuccess()); }
	@Test void test0002() { test(QUERY, variables("clientId", Long.valueOf(5)), roles(admin()), expectError(ErrorType.NOT_FOUND)); }
	@Test void test0003() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(clientAdmin()), expectSuccess()); }
	@Test void test0004() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(clientAdmin(2)), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0005() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(miningManager()), expectSuccess()); }
	@Test void test0006() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(miningManager(2, 2)), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0007() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(miningEditor()), expectSuccess()); }
	@Test void test0008() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(miningEditor(2, 2)), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0009() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(miningViewer()), expectSuccess()); }
	@Test void test0010() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(miningViewer(1, 2)), expectSuccess()); }
	@Test void test0011() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(miningViewer(1, 5)), expectSuccess()); }
	@Test void test0012() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(miningViewer(1, 4)), expectSuccess()); }
	@Test void test0013() { test(QUERY, variables("clientId", Long.valueOf(5)), roles(miningViewer(1, 4)), expectError(ErrorType.FORBIDDEN)); }
	@Test void test0014() { test(QUERY, variables("clientId", Long.valueOf(5)), roles(miningViewer(5, 8)), expectError(ErrorType.NOT_FOUND)); }
	@Test void test0015() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(discoveryManager()), expectSuccess()); }
	@Test void test0016() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(discoveryEditor()), expectSuccess()); }
	@Test void test0017() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(discoveryViewer()), expectSuccess()); }
	@Test void test0018() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(discoveryLightManager()), expectSuccess()); }
	@Test void test0019() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(discoveryLightEditor()), expectSuccess()); }
	@Test void test0020() { test(QUERY, variables("clientId", Long.valueOf(1)), roles(discoveryLightViewer()), expectSuccess()); }

}
