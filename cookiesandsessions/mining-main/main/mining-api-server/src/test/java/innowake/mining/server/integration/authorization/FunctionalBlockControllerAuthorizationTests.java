/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.FunctionalBlockController.AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL;
import static innowake.mining.server.controller.FunctionalBlockController.COMPUTE_FUNCTIONAL_BLOCKS_URL;
import static innowake.mining.server.controller.FunctionalBlockController.FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ;
import static innowake.mining.server.controller.FunctionalBlockController.FUNCTIONAL_BLOCKS_BY_UID_URL;
import static innowake.mining.server.controller.FunctionalBlockController.FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL;
import static innowake.mining.server.controller.FunctionalBlockController.FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE;
import static innowake.mining.server.controller.FunctionalBlockController.FUNCTIONAL_BLOCK_MERGE;
import static innowake.mining.server.controller.FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE;
import static innowake.mining.server.controller.FunctionalBlockController.FUNCTIONAL_BLOCK_STATUS_UPDATE;
import static innowake.mining.server.controller.FunctionalBlockController.FUNCTIONAL_BLOCK_UNMERGE;
import static innowake.mining.server.controller.FunctionalBlockController.RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL;
import static innowake.mining.server.controller.FunctionalBlockController.REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL;
import static innowake.mining.server.controller.FunctionalBlockController.UNGROUP_FUNCTIONAL_BLOCKS_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.LIST;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.FunctionalBlockController;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockStatus;

/**
 * Authorization tests for the {@link FunctionalBlockController}.
 */
class FunctionalBlockControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Create new functional block */
	/* Expecting NOT_FOUND in case of successful authorization as there is no project found and CREATED if the project exist */
	@Test void test0001() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0002() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0003() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0004() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), BAD_REQUEST); }
	@Test void test0005() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0006() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), BAD_REQUEST); }
	@Test void test0007() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0008() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0009() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0010() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0011() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0012() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0013() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0014() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0015() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0016() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0017() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0018() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0019() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0020() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0021() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	
	/* Update functional block */
	/* Expecting BAD_REQUEST in case of successful authorization as there is no uid found in project 1*/
	@Test void test0100() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), LIST, roles(admin()), BAD_REQUEST); }
	@Test void test0101() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("5", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), LIST, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0103() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0104() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), LIST, roles(miningManager()), BAD_REQUEST); }
	@Test void test0105() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0106() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), LIST, roles(miningEditor()), BAD_REQUEST); }
	@Test void test0107() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0108() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0109() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0110() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0111() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0112() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0113() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0114() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0115() throws Exception { test(PUT, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Delete functional block */
	/* Expecting BAD_REQUEST in case of successful authorization as there is no uid found in project 1*/
	@Test void test0200() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0201() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("5", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0202() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0203() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0204() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0205() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0206() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0207() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0208() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0209() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0210() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0211() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0212() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0213() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0214() throws Exception { test(DELETE, url(FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Compute functional block */
	/* Expecting NOT_FOUND in case of successful authorization as there is no project found */
	@Test void test0300() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("5"), noParams(), LIST, roles(admin()), NOT_FOUND); }
	@Test void test0301() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(clientAdmin()), OK); }
	@Test void test0302() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0303() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0304() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(miningEditor()), OK); }
	@Test void test0305() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0306() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(miningViewer()), FORBIDDEN); }
	@Test void test0307() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0308() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0309() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0310() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0311() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0312() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0313() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0314() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0315() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0316() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0317() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0318() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0319() throws Exception { test(POST, url(COMPUTE_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), LIST, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	

	/* Merging functional block Operation */
	/* Expecting BAD_REQUEST in case of successful authorization as there is no project found */
	@Test void test0400() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0401() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0402() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0403() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(miningManager()), BAD_REQUEST); }
	@Test void test0404() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0405() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), BAD_REQUEST); }
	@Test void test0406() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0407() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0408() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0409() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0410() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0411() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0412() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0413() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0414() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0415() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0416() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0417() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0418() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0419() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0420() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_MERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
				
	/* UnMerging functional block Operation */
	/* Expecting BAD_REQUEST in case of successful authorization as there is no project found */
	@Test void test0501() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0502() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0503() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0504() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(miningManager()), BAD_REQUEST); }
	@Test void test0505() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0506() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), BAD_REQUEST); }
	@Test void test0507() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0508() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0509() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0510() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0511() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0512() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0513() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0514() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0515() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0516() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0517() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0518() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0519() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0520() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0521() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_UNMERGE), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }

	@Test void test0601() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("5", "550e8400-e29b-41d4-a716-446655440000"), noParams(),OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0602() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0603() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0604() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0605() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningEditor(1, 2)) , FORBIDDEN); }
	@Test void test0606() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("5", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0607() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("5", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0608() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0609() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0610() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0611() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0612() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0613() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }

	/* Status change of functional blocks */
	/* Expecting NOT_FOUND in case of successful authorization as there is no project found */
	@Test void test0701() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("5", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(admin()), NOT_FOUND); }
	@Test void test0702() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(clientAdmin()), OK); }
	@Test void test0703() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0704() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(miningManager()), OK); }
	@Test void test0705() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0706() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(miningEditor()), OK); }
	@Test void test0707() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0708() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(miningViewer()), FORBIDDEN); }
	@Test void test0709() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0710() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0711() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0712() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0713() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0714() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0715() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0716() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0717() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0718() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0719() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0720() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0721() throws Exception { test(PUT, url(FUNCTIONAL_BLOCK_STATUS_UPDATE), uriVars("1", FunctionalBlockStatus.ACTIVE), noParams(), LIST, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }

	/* Compute Reachability Analysis block */
	/* Expecting NOT_FOUND in case of successful authorization as there is no project found */
	@Test void test0800() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0801() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0802() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0803() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0804() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0805() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0806() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0807() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0808() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0809() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0810() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0811() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0812() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0813() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0814() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0815() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0816() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0817() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	@Test void test0818() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(1)), BAD_REQUEST); }
	@Test void test0819() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 1)), BAD_REQUEST); }
	@Test void test0820() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 1)), FORBIDDEN); }
	@Test void test0821() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS ), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 1)), FORBIDDEN); }

	/* Recalculates outdated functional blocks */
	/* Expecting BAD_REQUEST in case of successful authorization as there is no project found */
	@Test void test0901() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0902() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0903() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0904() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0905() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0906() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0907() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0908() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0909() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0910() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0911() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0912() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0913() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0914() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0915() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0916() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0917() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0918() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0919() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0920() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0921() throws Exception { test(PUT, url(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }

	/* Remove Reachability Blocks Without UpperBoundModule */
	/* Expecting an OK response if the authorization is successful and the project is found */
	@Test void test1001() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test1002() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1003() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test1004() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1005() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1006() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test1007() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1008() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test1009() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1010() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1011() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1012() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1013() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1014() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1015() throws Exception { test(DELETE, url(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }

	@Test void test1101() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("5", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(),OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1102() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1103() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test1104() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test1105() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(), OBJECT, roles(miningEditor(1, 2)) , FORBIDDEN); }
	@Test void test1106() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("5", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1107() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("5", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1108() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1109() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1110() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1111() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1112() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1113() throws Exception { test(POST, url(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"),
			noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }

	/* Delete automated functional block */
	/* Expecting BAD_REQUEST in case of successful authorization as there is no uid found in project 1*/
	@Test void test1114() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(admin()), OK); }
	@Test void test1115() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("5", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1116() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(clientAdmin()), OK); }
	@Test void test1117() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1118() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1119() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(miningEditor()), OK); }
	@Test void test1120() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams(),
			OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1121() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test1122() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1123() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1124() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1125() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1126() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1127() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1128() throws Exception { test(DELETE, url(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000"), noParams()
			, OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Delete functional block on ungroup*/
	/* Expecting BAD_REQUEST in case of successful authorization as there is no uid found in project 1*/
	@Test void test1129() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1130() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("5", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1131() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1132() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1133() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1134() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test1135() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1136() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test1137() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1138() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1139() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1140() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1141() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1142() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1143() throws Exception { test(DELETE, url(UNGROUP_FUNCTIONAL_BLOCKS_URL), uriVars("1", "550e8400-e29b-41d4-a716-446655440000",
			"550e8400-e29b-41d4-a716-446655440001"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }

}

