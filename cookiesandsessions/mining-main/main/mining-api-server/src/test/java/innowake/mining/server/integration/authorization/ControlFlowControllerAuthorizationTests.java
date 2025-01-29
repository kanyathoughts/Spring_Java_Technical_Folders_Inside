/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.cfg.ControlFlowController.CONTROL_FLOW_MULTIPLE_URL;
import static innowake.mining.server.controller.cfg.ControlFlowController.CONTROL_FLOW_SINGLE_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpStatus.ACCEPTED;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.cfg.ControlFlowController;

/**
 * Authorization tests for the {@link ControlFlowController}.
 */
class ControlFlowControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Retrieve control flow graph */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test0001() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0002() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0004() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0005() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0006() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0007() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0008() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0009() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0010() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0011() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0012() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0013() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0014() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0015() throws Exception { test(GET, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Calculate control flow graph */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test0100() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0101() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0103() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0104() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0105() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0106() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0107() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0108() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0109() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0110() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0111() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0112() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0113() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0114() throws Exception { test(POST, url(CONTROL_FLOW_SINGLE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Calculate control flow graph with module paths */
	/* Expecting ACCEPTED in case of successful authorization as the job is submitted but with an empty list of paths */
	@Test void test0200() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), ACCEPTED); }
	@Test void test0201() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), ACCEPTED); }
	@Test void test0202() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0203() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), ACCEPTED); }
	@Test void test0204() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0205() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), ACCEPTED); }
	@Test void test0206() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0207() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0208() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0209() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0210() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0211() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0212() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0213() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0214() throws Exception { test(POST, url(CONTROL_FLOW_MULTIPLE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
