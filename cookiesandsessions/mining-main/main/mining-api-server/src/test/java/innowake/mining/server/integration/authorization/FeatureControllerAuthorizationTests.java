/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.util.tuple.Tuple2.of;
import static innowake.mining.server.controller.FeatureController.FEATURE_BY_ID;
import static innowake.mining.server.controller.FeatureController.STATE;
import static innowake.mining.server.controller.FeatureController.TOGGLE_FEATURE_BY_ID;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static java.util.Arrays.asList;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.FeatureController;

/**
 * Authorization Tests for {@link FeatureController}.
 */
class FeatureControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Retrieve Feature identified by its ID. */
	/* Expecting NOT_FOUND in case of successful authorization as the Feature ID is invalid. */
	@Test void test0001() throws Exception { test(GET, url(FEATURE_BY_ID), uriVars("feature-id"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0002() throws Exception { test(GET, url(FEATURE_BY_ID), uriVars("feature-id"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(GET, url(FEATURE_BY_ID), uriVars("feature-id"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0004() throws Exception { test(GET, url(FEATURE_BY_ID), uriVars("feature-id"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0005() throws Exception { test(GET, url(FEATURE_BY_ID), uriVars("feature-id"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0006() throws Exception { test(GET, url(FEATURE_BY_ID), uriVars("feature-id"), noParams(), OBJECT, roles(discoveryManager()), NOT_FOUND); }
	@Test void test0007() throws Exception { test(GET, url(FEATURE_BY_ID), uriVars("feature-id"), noParams(), OBJECT, roles(discoveryEditor()), NOT_FOUND); }
	@Test void test0008() throws Exception { test(GET, url(FEATURE_BY_ID), uriVars("feature-id"), noParams(), OBJECT, roles(discoveryViewer()), NOT_FOUND); }
	@Test void test0009() throws Exception { test(GET, url(FEATURE_BY_ID), uriVars("feature-id"), noParams(), OBJECT, roles(discoveryLightManager()), NOT_FOUND); }
	@Test void test0010() throws Exception { test(GET, url(FEATURE_BY_ID), uriVars("feature-id"), noParams(), OBJECT, roles(discoveryLightEditor()), NOT_FOUND); }
	@Test void test0011() throws Exception { test(GET, url(FEATURE_BY_ID), uriVars("feature-id"), noParams(), OBJECT, roles(discoveryLightViewer()), NOT_FOUND); }
	
	/* Toggle the state for a given feature */
	/* Expecting NOT_FOUND in case of successful authorization as the Feature ID is invalid. */
	@Test void test0100() throws Exception { test(POST, url(TOGGLE_FEATURE_BY_ID), uriVars("feature-id"), params(asList(of(STATE, "true"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0101() throws Exception { test(POST, url(TOGGLE_FEATURE_BY_ID), uriVars("feature-id"), params(asList(of(STATE, "true"))), OBJECT, roles(clientAdmin()), FORBIDDEN); }
	@Test void test0102() throws Exception { test(POST, url(TOGGLE_FEATURE_BY_ID), uriVars("feature-id"), params(asList(of(STATE, "true"))), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0103() throws Exception { test(POST, url(TOGGLE_FEATURE_BY_ID), uriVars("feature-id"), params(asList(of(STATE, "true"))), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0104() throws Exception { test(POST, url(TOGGLE_FEATURE_BY_ID), uriVars("feature-id"), params(asList(of(STATE, "true"))), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0105() throws Exception { test(POST, url(TOGGLE_FEATURE_BY_ID), uriVars("feature-id"), params(asList(of(STATE, "true"))), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0106() throws Exception { test(POST, url(TOGGLE_FEATURE_BY_ID), uriVars("feature-id"), params(asList(of(STATE, "true"))), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0107() throws Exception { test(POST, url(TOGGLE_FEATURE_BY_ID), uriVars("feature-id"), params(asList(of(STATE, "true"))), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0108() throws Exception { test(POST, url(TOGGLE_FEATURE_BY_ID), uriVars("feature-id"), params(asList(of(STATE, "true"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0109() throws Exception { test(POST, url(TOGGLE_FEATURE_BY_ID), uriVars("feature-id"), params(asList(of(STATE, "true"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0110() throws Exception { test(POST, url(TOGGLE_FEATURE_BY_ID), uriVars("feature-id"), params(asList(of(STATE, "true"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
