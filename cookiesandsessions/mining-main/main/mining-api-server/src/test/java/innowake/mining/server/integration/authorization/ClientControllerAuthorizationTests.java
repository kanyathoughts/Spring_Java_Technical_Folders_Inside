/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.ClientController.CLIENT_BY_ID_URL;
import static innowake.mining.server.controller.ClientController.CLIENT_COLLECTION_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.METHOD_NOT_ALLOWED;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import innowake.mining.server.controller.ClientController;

/**
 * Authorization tests for the {@link ClientController}.
 */
class ClientControllerAuthorizationTests extends RestAuthorizationTests {

	/* Get all clients */		
	/* Expecting OK for all requests as this requires authentication only and the underlying query filters the list of clients */
	@Test void test0001() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(new SimpleGrantedAuthority("totally-not-a-relevant-role")), OK); }
	@Test void test0002() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0003() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0004() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(clientAdmin(2)), OK); }
	@Test void test0005() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0006() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningManager(1, 2)), OK); }
	@Test void test0007() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0008() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor(1, 2)), OK); }
	@Test void test0009() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0010() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 2)), OK); }
	@Test void test0011() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 5)), OK); }
	@Test void test0012() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 4)), OK); }
	@Test void test0013() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0014() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0015() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0016() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightManager()), OK); }
	@Test void test0017() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightEditor()), OK) ; }
	@Test void test0018() throws Exception { test(GET, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightViewer()), OK); }
	
	/* Get client by ID */
	/* Expecting OK in case of successful authorization. */
	@Test void test0100() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0101() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0103() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0104() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0105() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(2, 2)), FORBIDDEN); }
	@Test void test0106() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0107() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(2, 2)), FORBIDDEN); }
	@Test void test0108() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0109() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), OK); }
	@Test void test0110() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 5)), OK); }
	@Test void test0111() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 4)), OK); }
	@Test void test0112() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0113() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(5, 8)), NOT_FOUND); }
	@Test void test0114() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0115() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0116() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0117() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), OK); }
	@Test void test0118() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), OK); }
	@Test void test0119() throws Exception { test(GET, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), OK); }
	
	/* Create new client */
	/* Expecting METHOD_NOT_ALLOWED in case of successful authorization */
	@Test void test0200() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(admin()), METHOD_NOT_ALLOWED); }
	@Test void test0201() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(clientAdmin()), FORBIDDEN); }
	@Test void test0202() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0203() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0204() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0205() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0206() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0207() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0208() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0209() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0210() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0211() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0212() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0213() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0214() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0215() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0216() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Update client */
	/* Expecting BAD_REQUEST in case of successful authorization as the empty JSON object does not validate */
	@Test void test0300() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), BAD_REQUEST); }
	@Test void test0301() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0302() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), FORBIDDEN); }
	@Test void test0303() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0304() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0305() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0306() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0307() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0308() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0309() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0310() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0311() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0312() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0313() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0314() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0315() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0316() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0317() throws Exception { test(PUT, url(CLIENT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	

}
