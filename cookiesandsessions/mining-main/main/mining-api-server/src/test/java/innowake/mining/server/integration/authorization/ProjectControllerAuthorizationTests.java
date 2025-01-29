/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.ProjectController.DELETE_AUTO_COMPLETION_VALUE;
import static innowake.mining.server.controller.ProjectController.FUNCTIONAL_BLOCK_REACHABILITY_CONFIG;
import static innowake.mining.server.controller.ProjectController.PROJECT_BY_ID_URL;
import static innowake.mining.server.controller.ProjectController.PROJECT_COLLECTIONS_FOR_CLIENT_URL;
import static innowake.mining.server.controller.ProjectController.PROJECT_COLLECTION_URL;
import static innowake.mining.server.controller.ProjectController.RENAME_AUTO_COMPLETION_VALUE;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.METHOD_NOT_ALLOWED;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.ProjectController;

/**
 * Authorization tests for the {@link ProjectController}.
 */
class ProjectControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* GET all projects */
	/* Expecting OK in case of successful authorization */
	@Test void test0001() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0003() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0004() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningManager(2, 2)), OK); }
	@Test void test0005() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0006() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor(2, 2)), OK); }
	@Test void test0007() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0008() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 2)), OK); }
	@Test void test0009() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0010() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0011() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0012() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightManager()), OK); }
	@Test void test0013() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightEditor()), OK); }
	@Test void test0014() throws Exception { test(GET, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightViewer()), OK); }
	
	/* GET project V1 from Project ID */
	/* Expecting OK in case of successful authorization */
	@Test void test0100() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0101() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0103() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0104() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0105() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(2, 2)), FORBIDDEN); }
	@Test void test0106() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0107() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(2, 2)), FORBIDDEN); }
	@Test void test0108() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0109() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer(1, 2)), OK); }
	@Test void test0110() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer(2, 4)), FORBIDDEN); }
	@Test void test0111() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0112() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0113() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0114() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0115() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), OK); }
	@Test void test0116() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), OK); }
	@Test void test0117() throws Exception { test(GET, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), OK); }
	
	/* Create new project */
	/* Expecting METHOD_NOT_ALLOWED in case of successful authorization */
	@Test void test0200() throws Exception { test(POST, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(admin()), METHOD_NOT_ALLOWED); }
	@Test void test0201() throws Exception { test(POST, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(clientAdmin()), METHOD_NOT_ALLOWED); }
	@Test void test0202() throws Exception { test(POST, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0203() throws Exception { test(POST, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0204() throws Exception { test(POST, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0205() throws Exception { test(POST, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0206() throws Exception { test(POST, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0207() throws Exception { test(POST, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0208() throws Exception { test(POST, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0209() throws Exception { test(POST, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0210() throws Exception { test(POST, url(PROJECT_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Update project */
	/* Expecting BAD_REQUEST in case of successful authorization as the empty JSON object does not validate */
	@Test void test0300() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), BAD_REQUEST); }
	@Test void test0301() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0302() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0303() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0304() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0305() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(2, 2)), FORBIDDEN); }
	@Test void test0306() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0307() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(2, 2)), FORBIDDEN); }
	@Test void test0308() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0309() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0310() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer(2, 4)), FORBIDDEN); }
	@Test void test0311() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0312() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0313() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0314() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0315() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0316() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0317() throws Exception { test(PUT, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Rename an auto completion list value to a new value. */
	/* Expecting NOT_FOUND in case of successful authorization as the empty JSON object does not validate */
	@Test void test0400() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0401() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("5", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(admin()),NOT_FOUND); }
	@Test void test0402() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0403() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0404() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0405() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(miningManager(2, 2)), FORBIDDEN); }
	@Test void test0406() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0407() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(miningEditor(2, 2)), FORBIDDEN); }
	@Test void test0408() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0409() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("2", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0410() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("2", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(miningViewer(2, 4)), FORBIDDEN); }
	@Test void test0411() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("5", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0412() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0413() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0414() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0415() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0416() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0417() throws Exception { test(PUT, url(RENAME_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A", "B"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Deletes the received value from auto-completion list for the given key. */
	/* Expecting BAD_REQUEST in case of successful authorization as the empty JSON object does not validate */
	@Test void test0500() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(admin()), BAD_REQUEST); }
	@Test void test0501() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("5", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0502() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0503() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(clientAdmin(2)),  FORBIDDEN); }
	@Test void test0504() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(miningManager()), BAD_REQUEST); }
	@Test void test0505() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(miningManager(2, 2)), FORBIDDEN); }
	@Test void test0506() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0507() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(miningEditor(2, 2)), FORBIDDEN); }
	@Test void test0508() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0509() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("2", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0510() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("2", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(miningViewer(2, 4)), FORBIDDEN); }
	@Test void test0511() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("5", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0512() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0513() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0514() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0515() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0516() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0517() throws Exception { test(DELETE, url(DELETE_AUTO_COMPLETION_VALUE), uriVars("1", "xAutoCompletionKey", "A"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieves Project for a Client. */
	/* Expecting OK in case of successful authorization. */
	@Test void test0600() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0601() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0602() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0603() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0604() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(miningManager(1, 5)), FORBIDDEN); }
	@Test void test0605() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(miningManager(5, 8)), NOT_FOUND); }
	@Test void test0606() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0607() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(miningEditor(1, 5)), FORBIDDEN); }
	@Test void test0608() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(miningEditor(5, 8)), NOT_FOUND); }
	@Test void test0609() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0610() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0611() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(5, 8)), NOT_FOUND); }
	@Test void test0612() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0613() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryManager(1, 5)), FORBIDDEN); }
	@Test void test0614() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryManager(5, 8)), NOT_FOUND); }
	@Test void test0615() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0616() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryEditor(1, 5)), FORBIDDEN); }
	@Test void test0617() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryEditor(5, 8)), NOT_FOUND); }
	@Test void test0618() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0619() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryViewer(1, 5)), FORBIDDEN); }
	@Test void test0620() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryViewer(5, 8)), NOT_FOUND); }
	@Test void test0621() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), OK); }
	@Test void test0622() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightManager(1, 5)), FORBIDDEN); }
	@Test void test0623() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightManager(5, 8)), NOT_FOUND); }
	@Test void test0624() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), OK); }
	@Test void test0625() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightEditor(1, 5)), FORBIDDEN); }
	@Test void test0626() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightEditor(5, 8)), NOT_FOUND); }
	@Test void test0627() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), OK); }
	@Test void test0628() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightViewer(1, 5)), FORBIDDEN); }
	@Test void test0629() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightViewer(5, 8)), NOT_FOUND); }

	/* Save Reachability analysis config */
	/* Expecting BAD_REQUEST in case of successful authorization as there is no uid found in project 1*/
	@Test void test1201() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(admin()), OK); }
	@Test void test1202() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("5"),
			noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1203() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test1204() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1205() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1206() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test1207() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1208() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test1209() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1210() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1211() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1212() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1213() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1214() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1215() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }

	/* Get Reachability analysis config */
	/* Expecting OK in case of successful authorization as there is no uid found in project 1*/
	@Test void test1301() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(admin()), OK); }
	@Test void test1303() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test1304() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1305() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1306() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test1307() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1308() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test1309() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1310() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1311() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1312() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1313() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1314() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1315() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG), uriVars("1"),
			noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
