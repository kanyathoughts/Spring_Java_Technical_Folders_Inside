/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.HotSpotController.HOTSPOT_COLLECTION_URL;
import static innowake.mining.server.controller.HotSpotController.HOTSPOT_LIMITED_COLLECTION_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.HotSpotController;

/**
 * Authorization Tests for the {@link HotSpotController}.
 */
class HotSpotControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Retrieve HotSpots for a given Project. */
	/* Expecting OK in case of successful authorization. */
	@Test void test0001() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("5", "CALLS"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0004() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0005() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0006() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0007() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0008() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0009() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0010() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0011() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("5", "CALLS"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0012() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("5", "CALLS"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0013() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0014() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0015() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0016() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0017() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0018() throws Exception { test(GET, url(HOTSPOT_COLLECTION_URL), uriVars("1", "CALLS"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve limited HotSpots (limit of 5) for a given Project. */
	/* Expecting OK in case of successful authorization. */
	@Test void test0100() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0101() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("5", "CALLS", "5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0103() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0104() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0105() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0106() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0107() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0108() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0109() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0110() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("5", "CALLS", "5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0111() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("5", "CALLS", "5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0112() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0113() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0114() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0115() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0116() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0117() throws Exception { test(GET, url(HOTSPOT_LIMITED_COLLECTION_URL), uriVars("1", "CALLS", "5"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
