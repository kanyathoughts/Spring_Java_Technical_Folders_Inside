/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.AnnotationReportingController.ANNOTATION_REPORTING_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.AnnotationReportingController;

/**
 * Authorization tests for the {@link AnnotationReportingController}.
 */
class AnnotationReportingControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Retrieve annotation reporting  */
	/* Expecting OK in case of successful authorization */
	@Test void test0001() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0004() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0005() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0006() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0007() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0008() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0009() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0010() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0011() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0012() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0013() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0014() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0015() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0016() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0017() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0018() throws Exception { test(POST, url(ANNOTATION_REPORTING_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
