/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.util.tuple.Tuple2.of;
import static innowake.mining.server.controller.ReferenceController.MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL;
import static innowake.mining.server.controller.ReferenceController.MODULE_REFERENCES_URL;
import static innowake.mining.server.controller.ReferenceController.PROJECT_REFERENCES_URL;
import static innowake.mining.server.controller.ReferenceController.REFERENCE_BY_ID_URL;
import static innowake.mining.server.controller.ReferenceController.REQUEST_PARAM_RELATIONSHIP;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static java.util.Arrays.asList;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import java.util.UUID;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.ReferenceController;

/**
 * Authorization Tests for {@link ReferenceController}.
 */
class ReferenceControllerAuthorizationTests extends RestAuthorizationTests {
	
	private static final String DUMMY_UUID = new UUID(0, 0).toString(); 
	
	/* Retrieve references for a Module. */
	/* Expecting OK in case of successful authorization but since the module ID is invalid, it returns NOT_FOUND. */
	@Test void test0001() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0002() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("5", "1000"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0004() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0005() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0006() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0007() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0008() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0009() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0010() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0011() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0012() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0013() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0014() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0015() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0016() throws Exception { test(GET, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve references for a Project. */
	/* Expecting OK in case of successful authorization. */
	@Test void test0100() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0101() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0103() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0104() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0105() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0106() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0107() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0108() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0109() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0110() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0111() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0112() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0113() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0114() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0115() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0116() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0117() throws Exception { test(GET, url(PROJECT_REFERENCES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve single reference by ID. */
	/* Expecting NOT_FOUND in case of successful authorization as there is no reference with ID 3000 in module 1000. */
	@Test void test0200() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0201() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("5", "1000", DUMMY_UUID), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0202() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0203() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0204() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0205() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0206() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0207() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0208() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0209() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0210() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("5", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0211() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("5", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0212() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0213() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0214() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0215() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0216() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0217() throws Exception { test(GET, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Create new reference for a Module. */
	/* Expecting NOT_FOUND in case of successful authorization as the Module with ID 1000 does not belong to Project with ID 1. */
	@Test void test0300() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0301() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("5", "1000"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0302() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0303() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0304() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0305() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0306() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0307() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0308() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0309() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0310() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0311() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0312() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0313() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0314() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0315() throws Exception { test(POST, url(MODULE_REFERENCES_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Delete reference by ID for a Module. */
	/* Expecting NOT_FOUND in case of successful authorization as a reference with ID 3000 does not exist. */
	@Test void test0400() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0401() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("5", "1000", DUMMY_UUID), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0402() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0403() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0404() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0405() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0406() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0407() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0408() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0409() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0410() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0411() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0412() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0413() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0414() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0415() throws Exception { test(DELETE, url(REFERENCE_BY_ID_URL), uriVars("1", "1000", DUMMY_UUID), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Fetch references by module id's */
	/* Expecting NOT_FOUND in case of successful authorization as there are no modules with ID 1000 and ID 3000 in project 1 */
	@Test void test0500() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0501() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("5", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0502() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0503() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0504() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0505() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0506() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0507() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0508() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0509() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0510() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0511() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0512() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0513() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0514() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0515() throws Exception { test(GET, url(MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL), uriVars("1", "1000", "3000"), params(asList(of(REQUEST_PARAM_RELATIONSHIP, "CALLS"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
