/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.TaxonomyController.ASSIGN_TAXONOMY_JOB_URL;
import static innowake.mining.server.controller.TaxonomyController.IDENTIFY_TAXONOMIES_URL;
import static innowake.mining.server.controller.TaxonomyController.SLOC_URL;
import static innowake.mining.server.controller.TaxonomyController.TAXONOMIES_COLLECTION_URL;
import static innowake.mining.server.controller.TaxonomyController.TAXONOMY_BY_ID_URL;
import static innowake.mining.server.controller.TaxonomyController.TAXONOMY_REPORTS_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.ACCEPTED;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.TaxonomyController;

/**
 * Authorization Tests for {@link TaxonomyController}.
 */
class TaxonomyControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Retrieve Taxonomies for a given Project. */
	/* Expecting OK in case of successful authorization. */
	@Test void test0001() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0004() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0005() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0006() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0007() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0008() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0009() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0010() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0011() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0012() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0013() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0014() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0015() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0016() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0017() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0018() throws Exception { test(GET, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Create Taxonomy for a given Project. */
	/* Expecting BAD_REQUEST in case of successful authorization as we're providing an empty object as Taxonomy. */
	@Test void test0100() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), BAD_REQUEST); }
	@Test void test0101() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0103() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0104() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), BAD_REQUEST); }
	@Test void test0105() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0106() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0107() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0108() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0109() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0110() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0111() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0112() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0113() throws Exception { test(POST, url(TAXONOMIES_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Update Taxonomy by ID. */
	/* Expecting NOT_FOUND in case of successful authorization as the Taxonomy ID of 1000 does not exist. */
	@Test void test0200() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0201() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("5", "1000"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0202() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0203() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0204() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0205() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0206() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0207() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0208() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0209() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0210() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0211() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0212() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0213() throws Exception { test(PUT, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* BulkAssign Taxonomies to Modules. */
	@Test void test0300() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0301() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0302() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0303() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(2, 2)), FORBIDDEN); }
	@Test void test0304() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0305() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(2, 2)), FORBIDDEN); }
	@Test void test0306() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0307() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0308() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0309() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0310() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0311() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0312() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0313() throws Exception { test(PUT, url(ASSIGN_TAXONOMY_JOB_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Delete Taxonomy by ID. */
	/* Expecting NOT_FOUND in case of successful authorization as the Taxonomy ID of 1000 does not exist. */
	@Test void test0400() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0401() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("5", "1000"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0402() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0403() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0404() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0405() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0406() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0407() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0408() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0409() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0410() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0411() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0412() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0413() throws Exception { test(DELETE, url(TAXONOMY_BY_ID_URL), uriVars("1", "1000"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve Taxonomy Reports for a given Project. */
	/* Expecting OK in case of successful authorization. */
	@Test void test0500() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0501() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0502() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0503() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0504() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0505() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0506() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0507() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0508() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0509() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0510() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0511() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0512() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0513() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0514() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0515() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0516() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0517() throws Exception { test(GET, url(TAXONOMY_REPORTS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Identify Technical Taxonomy for a given Project and list of Module paths. */
	/* Expecting ACCEPTED in case of successful authorization. */
	@Test void test0600() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), ACCEPTED); }
	@Test void test0601() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0602() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), ACCEPTED); }
	@Test void test0603() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0604() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), ACCEPTED); }
	@Test void test0605() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0606() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0607() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0608() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0609() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0610() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0611() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0612() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0613() throws Exception { test(POST, url(IDENTIFY_TAXONOMIES_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve map of Taxonomy type names and sum of SLOC of all Modules that have a Taxonomy of that type assigned for a given Project. */
	/* Expecting OK in case of successful authorization. */
	@Test void test0700() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0701() throws Exception { test(GET, url(SLOC_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0702() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0703() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0704() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0705() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0706() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0707() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0708() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0709() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0710() throws Exception { test(GET, url(SLOC_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0711() throws Exception { test(GET, url(SLOC_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0712() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0713() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0714() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0715() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0716() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0717() throws Exception { test(GET, url(SLOC_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
}
