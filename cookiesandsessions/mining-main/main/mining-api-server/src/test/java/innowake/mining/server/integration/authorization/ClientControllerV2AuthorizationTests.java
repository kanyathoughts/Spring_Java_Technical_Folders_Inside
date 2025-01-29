/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.ClientControllerV2.CLIENT_BY_ID_URL;
import static innowake.mining.server.controller.ClientControllerV2.CLIENT_COLLECTION_URL;
import static innowake.mining.server.controller.ClientControllerV2.CLIENT_LOGO_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.CREATED;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.HttpStatus.OK;

import org.apache.http.entity.ContentType;
import org.junit.jupiter.api.Test;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import innowake.mining.server.controller.ClientControllerV2;

/**
 * Authorization tests for the {@link ClientControllerV2}.
 */
class ClientControllerV2AuthorizationTests extends RestAuthorizationTests {
	
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
	
	/* Create new client */
	/* Expecting BAD_REQUEST in case of successful authorization as the empty JSON object does not validate */
	@Test void test0100() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(admin()), BAD_REQUEST); }
	@Test void test0101() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(clientAdmin()), FORBIDDEN); }
	@Test void test0102() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0103() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0104() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0105() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0106() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0107() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0108() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0109() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0110() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0111() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0112() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0113() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0114() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0115() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0116() throws Exception { test(POST, url(CLIENT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Delete client */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test0200() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0201() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(clientAdmin()), FORBIDDEN); }
	@Test void test0202() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0203() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0204() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0205() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0206() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0207() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0208() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0209() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0210() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0211() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0212() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0213() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0214() throws Exception { test(DELETE, url(CLIENT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Get logo */
	/* Expecting NOT_FOUND in case of successful authorization as there is no logo associated with the client with ID 1 */
	@Test void test0300() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0301() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0302() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0303() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0304() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(2, 3)), FORBIDDEN); }
	@Test void test0305() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0306() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(2, 3)), FORBIDDEN); }
	@Test void test0307() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0308() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(2, 3)), FORBIDDEN); }
	@Test void test0309() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), NOT_FOUND); }
	@Test void test0310() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), NOT_FOUND); }
	@Test void test0311() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), NOT_FOUND); }
	@Test void test0312() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), NOT_FOUND); }
	@Test void test0313() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), NOT_FOUND) ; }
	@Test void test0314() throws Exception { test(GET, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), NOT_FOUND); }
	
	/* Delete logo */
	/* NOT_FOUND will be thrown if the Client for which the Logo is to be deleted does not exist,
	 * but deleting the Logo of an existing Client that does not currently have a Logo is not an error. */
	@Test void test0400() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0401() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0402() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), FORBIDDEN); }
	@Test void test0403() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0404() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0405() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(2, 3)), FORBIDDEN); }
	@Test void test0406() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0407() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(2, 3)), FORBIDDEN); }
	@Test void test0408() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0409() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(2, 3)), FORBIDDEN); }
	@Test void test0410() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0411() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0412() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0413() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0414() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN) ; }
	@Test void test0415() throws Exception { test(DELETE, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Create logo */
	/* Expecting NOT_FOUND in case of successful authorization as there is no logo associated with the client with ID 1 */
	@Test void test0500() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(admin()), CREATED, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0501() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(clientAdmin()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0502() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(clientAdmin(2)), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0503() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(miningManager()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0504() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(miningManager(2, 3)), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0505() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(miningEditor()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0506() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(miningEditor(2, 3)), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0507() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(miningViewer()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0508() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(miningViewer(2, 3)), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0509() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(discoveryManager()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0510() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(discoveryEditor()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0511() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(discoveryViewer()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0512() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(discoveryLightManager()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0513() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(discoveryLightEditor()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0514() throws Exception { multiPartTests(POST, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(discoveryLightViewer()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	
	/* Update logo */
	/* Expecting NOT_FOUND in case of successful authorization as there is no logo associated with the client with ID 1 */
	@Test void test0600() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(admin()), NO_CONTENT, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0601() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(clientAdmin()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0602() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(clientAdmin(2)), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0603() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(miningManager()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0604() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(miningManager(2, 3)), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0605() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(miningEditor()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0606() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(miningEditor(2, 3)), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0607() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(miningViewer()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0608() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(miningViewer(2, 3)), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0609() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(discoveryManager()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0610() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(discoveryEditor()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0611() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(discoveryViewer()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0612() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(discoveryLightManager()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	@Test void test0613() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(discoveryLightEditor()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG) ; }
	@Test void test0614() throws Exception { multiPartTests(PUT, url(CLIENT_LOGO_URL), uriVars("1"), noParams(), roles(discoveryLightViewer()), FORBIDDEN, "file", "foo.jpg", ContentType.IMAGE_JPEG); }
	
}
