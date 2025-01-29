/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.LicenseController.LICENSE_EXPIRY_INFO;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import innowake.mining.server.controller.LicenseController;

/**
 * Authorization tests for the {@link LicenseController}.
 */
class LicenseControllerV2AuthorizationTests extends RestAuthorizationTests {
	
	/* Expecting OK for all requests as this requires authentication only*/
	@Test void test0001() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(new SimpleGrantedAuthority("Irrelevant-role")), OK); }
	@Test void test0002() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0003() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0004() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(clientAdmin(2)), OK); }
	@Test void test0005() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0006() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(miningManager(1, 2)), OK); }
	@Test void test0007() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0008() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(miningEditor(1, 2)), OK); }
	@Test void test0009() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0010() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 2)), OK); }
	@Test void test0011() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 5)), OK); }
	@Test void test0012() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 4)), OK); }
	@Test void test0013() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0014() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0015() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0016() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(discoveryLightManager()), OK); }
	@Test void test0017() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(discoveryLightEditor()), OK); }
	@Test void test0018() throws Exception { test(GET, url(LICENSE_EXPIRY_INFO), noUriVars(), noParams(), OBJECT, roles(discoveryLightViewer()), OK); }
	
}
