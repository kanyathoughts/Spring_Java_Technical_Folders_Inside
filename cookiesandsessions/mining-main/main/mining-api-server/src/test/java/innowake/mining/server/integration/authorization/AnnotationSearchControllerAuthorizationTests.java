/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.AnnotationSearchController.ANNOTATION_BY_ID_URL;
import static innowake.mining.server.controller.AnnotationSearchController.ANNOTATION_SEARCH_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.AnnotationSearchController;

/**
 * Authorization tests for the {@link AnnotationSearchController}.
 */
class AnnotationSearchControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Search for annotations  */
	/* Expecting OK in case of successful authorization */
	@Test void test0001() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0004() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0005() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0006() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0007() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0008() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0009() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0010() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0011() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0012() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0013() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0014() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0015() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0016() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0017() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0018() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Update annotation */
	/* Expecting BAD_REQUEST in case of successful authorization as the empty JSON object does not validate */
	@Test void test0100() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), BAD_REQUEST); }
	@Test void test0101() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0103() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0104() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), BAD_REQUEST); }
	@Test void test0105() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0106() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), BAD_REQUEST); }
	@Test void test0107() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0108() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0109() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0110() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0111() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0112() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0113() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0114() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0115() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0116() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0117() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
