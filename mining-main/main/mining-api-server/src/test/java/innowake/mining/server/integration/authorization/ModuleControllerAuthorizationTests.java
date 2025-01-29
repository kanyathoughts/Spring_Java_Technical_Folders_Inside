/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.util.tuple.Tuple2.of;
import static innowake.mining.server.controller.module.ModuleController.*;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.LIST;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static java.util.Arrays.asList;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.ACCEPTED;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.HttpStatus.OK;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.controller.module.ModuleController;
import innowake.mining.server.locking.ProjectLockService;


/**
 * Authorization tests for the {@link ModuleController}.
 */
class ModuleControllerAuthorizationTests extends RestAuthorizationTests {
	
	@MockBean
	@Nullable
	private ProjectLockService projectLockService;
	
	/* Retrieve modules */
	@Test void test0001() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0004() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0005() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0006() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0007() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0008() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0009() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0010() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0011() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0012() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0013() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0014() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0015() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0016() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0017() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0018() throws Exception { test(GET, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve Module by ID */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test0100() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0101() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0103() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0104() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0105() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0106() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0107() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0108() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0109() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0110() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0111() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0112() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0113() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0114() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0115() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0116() throws Exception { test(GET, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve Module by path */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with path 'foo' in project 1 */
	@Test void test0200() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0201() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0202() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0203() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0204() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0205() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0206() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0207() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0208() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0209() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("5"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0210() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("5"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0211() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0212() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0213() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0214() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0215() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0216() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_PATH, "foo"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve Modules by name */
	/* Expecting OK in case of successful authorization */
	@Test void test0300() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(admin()), OK); }
	@Test void test0301() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0302() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0303() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningManager()), OK); }
	@Test void test0304() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0305() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningEditor()), OK); }
	@Test void test0306() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0307() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningViewer()), OK); }
	@Test void test0308() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0309() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("5"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0310() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("5"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0311() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0312() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0313() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0314() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0315() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0316() throws Exception { test(GET, url(MODULE_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }

	/* Retrieve module count */
	@Test void test0400() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0401() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0402() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0403() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0404() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0405() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0406() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0407() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0408() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0409() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0410() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0411() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0412() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0413() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0414() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0415() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0416() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0417() throws Exception { test(GET, url(MODULE_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Create module */
	/* Expecting BAD_REQUEST in case of successful authorization as the empty JSON object does not validate */
	@Test void test0500() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), BAD_REQUEST); }
	@Test void test0501() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0502() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0503() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0504() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), BAD_REQUEST); }
	@Test void test0505() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0506() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0507() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0508() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0509() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0510() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0511() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0512() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0513() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0514() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0515() throws Exception { test(POST, url(MODULE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Update module */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test0600() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0601() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0602() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0603() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0604() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0605() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0606() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0607() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0608() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0609() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0610() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0611() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0612() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0613() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0614() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0615() throws Exception { test(PUT, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Delete module */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test0700() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0701() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0702() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0703() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0704() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0705() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0706() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0707() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0708() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0709() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0710() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0711() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0712() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0713() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0714() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0715() throws Exception { test(DELETE, url(MODULE_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Delete all modules */
	/* Expecting NO_CONTENT in case of successful authorization */
	@Test void test0800() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(admin()), NO_CONTENT); }
	@Test void test0801() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("5"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0802() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(clientAdmin()), NO_CONTENT); }
	@Test void test0803() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0804() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(miningManager()), NO_CONTENT); }
	@Test void test0805() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0806() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0807() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0808() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0809() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0810() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0811() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0812() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0813() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0814() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0815() throws Exception { test(DELETE, url(MODULE_COLLECTION_URL), uriVars("1"), params(asList(of(DELETE_PARAM_DELETE_SOURCE_OBJECTS, "false"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve annotations from a module */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test0900() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0901() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0902() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0903() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0904() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0905() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0906() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0907() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0908() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0909() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0910() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0911() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0912() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0913() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0914() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0915() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0916() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0917() throws Exception { test(GET, url(ANNOTATION_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve taxonomies from a module */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test1000() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1001() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1002() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1003() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1004() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test1005() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1006() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test1007() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1008() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test1009() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1010() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1011() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1012() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1013() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1014() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1015() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1016() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1017() throws Exception { test(GET, url(TAXONOMY_COLLECTION_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve dependencies from a module */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test1100() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1101() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("5", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1102() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1103() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1104() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test1105() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1106() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test1107() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1108() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test1109() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1110() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("5", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1111() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("5", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1112() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1113() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1114() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1115() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1116() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1117() throws Exception { test(GET, url(DEPENDENCIES_URL), uriVars("1", "1"), params(asList(of("maxDepth", "1"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Identify module description */
	/* Expecting ACCEPTED in case of successful authorization */
	@Test void test1200() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), ACCEPTED); }
	@Test void test1201() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1202() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), ACCEPTED); }
	@Test void test1203() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1204() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), ACCEPTED); }
	@Test void test1205() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1206() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test1207() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1208() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test1209() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1210() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1211() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1212() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1213() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1214() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1215() throws Exception { test(POST, url(DESCRIPTION_IDENTIFICATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Check AST on module */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test1300() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1301() throws Exception { test(GET, url(HAS_AST_URL), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1302() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1303() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1304() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test1305() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1306() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test1307() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1308() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test1309() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1310() throws Exception { test(GET, url(HAS_AST_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1311() throws Exception { test(GET, url(HAS_AST_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1312() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1313() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1314() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1315() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1316() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1317() throws Exception { test(GET, url(HAS_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Store AST on module */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test1400() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1401() throws Exception { test(POST, url(STORE_AST_URL), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1402() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1403() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1404() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test1405() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1406() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test1407() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1408() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test1409() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1410() throws Exception { test(POST, url(STORE_AST_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test1411() throws Exception { test(POST, url(STORE_AST_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1412() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1413() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1414() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1415() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1416() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1417() throws Exception { test(POST, url(STORE_AST_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Check error markers on module */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test1500() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1501() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1502() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1503() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1504() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test1505() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1506() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test1507() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1508() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test1509() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1510() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1511() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1512() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1513() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1514() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1515() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1516() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1517() throws Exception { test(GET, url(MODULE_ERROR_MARKER), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Check DataFlowGraph access for the CodeViewer Peek Data Flow*/
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test1600() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1601() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("5", "1", 1), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1602() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1603() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1604() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test1605() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1606() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test1607() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1608() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test1609() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1610() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("5", "1", 1), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1611() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("5", "1", 1), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1612() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1613() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1614() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1615() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1616() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1617() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_LINKS_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }

	/* Check DataFlowGraph access for the CodeViewer Show Data Flow*/
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test1700() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1701() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("5", "1", 1), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1702() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1703() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1704() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test1705() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), params(asList(of("assembled", "true"))), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test1706() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1707() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test1708() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1709() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test1710() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1711() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("5", "1", 1), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1712() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("5", "1", 1), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1713() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1714() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1715() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1716() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1717() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1718() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }

	/* Retrieve list of included module ids */
	@Test void test1800() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(admin()), NOT_FOUND); }
	@Test void test1801() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("5", "1"), noParams(), LIST, roles(admin()), NOT_FOUND); }
	@Test void test1802() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1803() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1804() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(miningManager()), NOT_FOUND); }
	@Test void test1805() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1806() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(miningEditor()), NOT_FOUND); }
	@Test void test1807() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1808() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(miningViewer()), NOT_FOUND); }
	@Test void test1809() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1810() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("5", "1"), noParams(), LIST, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1811() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("5", "1"), noParams(), LIST, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1812() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1813() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1814() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1815() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1816() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1817() throws Exception { test(GET, url(INCLUDED_MODULES_URL), uriVars("1", "1"), noParams(), LIST, roles(discoveryLightViewer()), FORBIDDEN); }

	/* Check DataFlowGraph access for the CodeViewer Show Data Flow */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test1900() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1901() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("5", "1", 1), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1902() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1903() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1904() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test1905() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), params(asList(of("assembled", "true"))), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test1906() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1907() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test1908() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1909() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test1910() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1911() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("5", "1", 1), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1912() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("5", "1", 1), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1913() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1914() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1915() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1916() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1917() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1918() throws Exception { test(GET, url(CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL), uriVars("1", "1", 1), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	

	@Override
	protected ResetScriptFile getScriptFile() {
		/* required for testRequiresReview() */
		return ResetScriptFile.COMPLETE;
	}
	
	@Test
	void testRequiresReview() throws Exception {
		resetTestData();
		mockMvc.perform(get("/api/v1/projects/1/modules/requiresReview")
						.with(miningUser(roles(miningViewer()))))
				.andExpect(status().isOk())
				.andExpect(content().string("2"));
		mockMvc.perform(delete("/api/v1/projects/1/modules/requiresReview")
						.with(miningUser(roles(miningViewer()))))
				.andExpect(status().isForbidden());
		mockMvc.perform(delete("/api/v1/projects/1/modules/requiresReview")
						.with(miningUser(roles(miningEditor()))))
				.andExpect(status().isOk());
		mockMvc.perform(get("/api/v1/projects/1/modules/requiresReview")
						.with(miningUser(roles(miningViewer()))))
				.andExpect(status().isOk())
				.andExpect(content().string("0"));
	}
}
