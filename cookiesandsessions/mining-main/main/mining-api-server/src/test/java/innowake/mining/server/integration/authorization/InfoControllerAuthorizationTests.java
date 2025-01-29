/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.InfoController.API_INFO_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.InfoController;

/**
 * Authorization tests for the {@link InfoController}.
 */
class InfoControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Retrieve API Info */
	/* Expecting OK in case of successful authorization */
	@Test void test0001() throws Exception { test(GET, url(API_INFO_URL), noUriVars(), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(API_INFO_URL), noUriVars(), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0003() throws Exception { test(GET, url(API_INFO_URL), noUriVars(), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0004() throws Exception { test(GET, url(API_INFO_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0005() throws Exception { test(GET, url(API_INFO_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0006() throws Exception { test(GET, url(API_INFO_URL), noUriVars(), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0007() throws Exception { test(GET, url(API_INFO_URL), noUriVars(), noParams(), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0008() throws Exception { test(GET, url(API_INFO_URL), noUriVars(), noParams(), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0009() throws Exception { test(GET, url(API_INFO_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightManager()), OK); }
	@Test void test0010() throws Exception { test(GET, url(API_INFO_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightEditor()), OK); }
	@Test void test0011() throws Exception { test(GET, url(API_INFO_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightViewer()), OK); }
	
}
