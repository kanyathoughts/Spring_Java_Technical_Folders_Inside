/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.DataPointController.DATA_POINTS_FOR_TYPE_URL;
import static innowake.mining.server.controller.DataPointController.DATA_POINTS_TYPES_URL;
import static innowake.mining.server.controller.DataPointController.DATA_POINTS_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.DataPointController;

/**
 * Authorization tests for the {@link DataPointController}.
 */
class DataPointControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Retrieve data points by project  */
	/* Expecting OK in case of successful authorization */
	@Test void test0001() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0004() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0005() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0006() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0007() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0008() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0009() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0010() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0011() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0012() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0013() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0014() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0015() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0016() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), OK); }
	@Test void test0017() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), OK); }
	@Test void test0018() throws Exception { test(GET, url(DATA_POINTS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), OK); }
	
	/* Retrieve data point types by project  */
	/* Expecting OK in case of successful authorization */
	@Test void test0100() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0101() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0103() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0104() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0105() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0106() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0107() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0108() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0109() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0110() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0111() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0112() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0113() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0114() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0115() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), OK); }
	@Test void test0116() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), OK); }
	@Test void test0117() throws Exception { test(GET, url(DATA_POINTS_TYPES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), OK); }
	
	/* Retrieve data point for types by project  */
	/* Expecting NOT_FOUND in case of successful authorization since "type" is not a valid typeName */
	@Test void test0200() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0201() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("5", "type"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0202() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0203() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0204() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0205() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0206() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0207() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0208() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0209() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0210() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("5", "type"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0211() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("5", "type"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0212() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(discoveryManager()), NOT_FOUND); }
	@Test void test0213() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(discoveryEditor()), NOT_FOUND); }
	@Test void test0214() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(discoveryViewer()), NOT_FOUND); }
	@Test void test0215() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(discoveryLightManager()), NOT_FOUND); }
	@Test void test0216() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(discoveryLightEditor()), NOT_FOUND); }
	@Test void test0217() throws Exception { test(GET, url(DATA_POINTS_FOR_TYPE_URL), uriVars("1", "type"), noParams(), OBJECT, roles(discoveryLightViewer()), NOT_FOUND); }
	
}
