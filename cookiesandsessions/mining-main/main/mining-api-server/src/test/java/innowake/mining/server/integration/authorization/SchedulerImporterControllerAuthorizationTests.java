/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.integration.authorization;

import innowake.mining.server.controller.SchedulerImporterController;
import org.junit.jupiter.api.Test;

import static innowake.mining.server.controller.SchedulerImporterController.CREATE_SCHEDULER_IMPORT;
import static innowake.mining.server.controller.SchedulerImporterController.GET_SUPPORTED_IMPORTERS;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

/**
 * Tests the authorization of the {@link SchedulerImporterController}
 */
class SchedulerImporterControllerAuthorizationTests extends RestAuthorizationTests {

	/* POST a new scheduler import */
	/* Expecting BAD_REQUEST in case of successful authorization due to scheduler type not being present */
	@Test void test1001() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(admin()), BAD_REQUEST); }
	@Test void test1002() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("3"), noParams(), OBJECT, roles(admin()), BAD_REQUEST); }
	@Test void test1003() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test1004() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1005() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(miningManager()), BAD_REQUEST); }
	@Test void test1006() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1007() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test1008() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1009() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test1010() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1011() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test1012() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1013() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1014() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1015() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1016() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1017() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1018() throws Exception { test(POST, url(CREATE_SCHEDULER_IMPORT), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }


	/* GET all importers */
	/* Expecting OK in case of successful authorization */
	@Test void test1100() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test1101() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1102() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test1103() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1104() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test1105() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(miningManager(2, 2)), FORBIDDEN); }
	@Test void test1106() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test1107() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(miningEditor(2, 2)), FORBIDDEN); }
	@Test void test1108() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test1109() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("2"), noParams(), OBJECT, roles(miningViewer(1, 2)), OK); }
	@Test void test1110() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("2"), noParams(), OBJECT, roles(miningViewer(2, 4)), FORBIDDEN); }
	@Test void test1111() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1112() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1113() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1114() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1115() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()),
			FORBIDDEN); }
	@Test void test1116() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()),
			FORBIDDEN); }
	@Test void test1117() throws Exception { test(GET, url(GET_SUPPORTED_IMPORTERS), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()),
			FORBIDDEN); }
}
