/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.CandidateIdentificationController.CANDIDATE_IDENTIFICATION_URL;
import static innowake.mining.server.controller.CandidateIdentificationController.DEAD_CODE_IDENTIFICATION_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpStatus.ACCEPTED;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.CandidateIdentificationController;

/**
 * Authorization tests for the {@link CandidateIdentificationController}.
 */
class CandidateIdentificationControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Identify candidates  */
	/* Expecting ACCEPTED in case of successful authorization */
	@Test void test0001() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), ACCEPTED); }
	@Test void test0002() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), ACCEPTED); }
	@Test void test0004() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0005() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), ACCEPTED); }
	@Test void test0006() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0007() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0008() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0009() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0010() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0011() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0012() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0013() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0014() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0015() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0016() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0017() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0018() throws Exception { test(POST, url(CANDIDATE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	

				/* Identify dead code  */
				/* Expecting ACCEPTED in case of successful authorization */
	@Test void test0101() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), ACCEPTED); }
	@Test void test0102() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0103() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), ACCEPTED); }
	@Test void test0104() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0105() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), ACCEPTED); }
	@Test void test0106() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0107() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0108() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0109() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0110() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0111() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0112() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0113() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0114() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0115() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0116() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0117() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0118() throws Exception { test(POST, url(DEAD_CODE_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
}
