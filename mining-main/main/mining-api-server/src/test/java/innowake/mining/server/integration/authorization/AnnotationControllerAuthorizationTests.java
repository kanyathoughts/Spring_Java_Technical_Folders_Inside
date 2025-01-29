/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.util.tuple.Tuple2.of;
import static innowake.mining.server.controller.AnnotationController.AI_ANNOTATION_TRANSLATION_URL;
import static innowake.mining.server.controller.AnnotationController.ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL;
import static innowake.mining.server.controller.AnnotationController.ANNOTATION_BY_ID_URL;
import static innowake.mining.server.controller.AnnotationController.ANNOTATION_COLLECTIONS_BASED_ON_OFFSET;
import static innowake.mining.server.controller.AnnotationController.ANNOTATION_COLLECTION_BY_MODULE_URL;
import static innowake.mining.server.controller.AnnotationController.ANNOTATION_COLLECTION_BY_PROJECT_URL;
import static innowake.mining.server.controller.AnnotationController.ANNOTATION_IMPORT_URL;
import static innowake.mining.server.controller.AnnotationController.ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL;
import static innowake.mining.server.controller.AnnotationController.ANNOTATION_SEARCH_URL;
import static innowake.mining.server.controller.AnnotationController.ANNOTATION_TYPE_COLLECTION_URL;
import static innowake.mining.server.controller.AnnotationController.MODULE_BY_ANNOTATION_ID_URL;
import static innowake.mining.server.controller.AnnotationController.SEARCH_PARAM_NAME;
import static innowake.mining.server.controller.AnnotationController.ANNOTATION_DATADICTIONARY_BULK_DELETE_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.LIST;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static java.util.Arrays.asList;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import org.apache.http.entity.ContentType;
import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.AnnotationController;

/**
 * Authorization tests for the {@link AnnotationController}.
 */
class AnnotationControllerAuthorizationTests extends RestAuthorizationTests {

	/* Retrieve annotations by project  */
	/* Expecting OK in case of successful authorization */
	@Test void test0001() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0004() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0005() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0006() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0007() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0008() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0009() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0010() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0011() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0012() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0013() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0014() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0015() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0016() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0017() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0018() throws Exception { test(GET, url(ANNOTATION_COLLECTION_BY_PROJECT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve annotation types  */
	/* Expecting OK in case of successful authorization */
	@Test void test0100() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0101() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0103() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0104() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0105() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0106() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0107() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0108() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0109() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0110() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0111() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0112() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0113() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0114() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0115() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0116() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0117() throws Exception { test(GET, url(ANNOTATION_TYPE_COLLECTION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve single annotation by ID */
	/* Expecting OK in case of successful authorization */
	@Test void test0200() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0201() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0202() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0203() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0204() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0205() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0206() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0207() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0208() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0209() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0210() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0211() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0212() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0213() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0214() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0215() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0216() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0217() throws Exception { test(GET, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve module by annotation ID */
	/* Expecting OK in case of successful authorization */
	@Test void test0300() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0301() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0302() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0303() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0304() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0305() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0306() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0307() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0308() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0309() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0310() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0311() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0312() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0313() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0314() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0315() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0316() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0317() throws Exception { test(GET, url(MODULE_BY_ANNOTATION_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Search by name */
	/* Expecting OK in case of successful authorization */
	@Test void test0500() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(admin()), OK); }
	@Test void test0501() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("5"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0502() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0503() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0504() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningManager()), OK); }
	@Test void test0505() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0506() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningEditor()), OK); }
	@Test void test0507() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0508() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningViewer()), OK); }
	@Test void test0509() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0510() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("5"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test0511() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("5"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0512() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0513() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0514() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0515() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0516() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0517() throws Exception { test(GET, url(ANNOTATION_SEARCH_URL), uriVars("1"), params(asList(of(SEARCH_PARAM_NAME, "foo"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Create new annotation */
	/* Expecting NOT_FOUND in case of successful authorization as there is no module with ID 1 in project 1 */
	@Test void test0600() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0601() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0602() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0603() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0604() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0605() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0606() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0607() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0608() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0609() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0610() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0611() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0612() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0613() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0614() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0615() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0616() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0617() throws Exception { test(POST, url(ANNOTATION_COLLECTION_BY_MODULE_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Update annotation */
	@Test void test0700() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0701() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0702() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0703() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0704() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0705() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0706() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0707() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0708() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0709() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0710() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0711() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0712() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0713() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0714() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0715() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0716() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0717() throws Exception { test(PUT, url(ANNOTATION_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Delete annotation */
	/* Expecting NOT_FOUND in case of successful authorization as there is no annotation with ID 0 in project 1 */
	@Test void test0800() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0801() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("5", "0"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0802() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0803() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0804() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0805() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0806() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0807() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0808() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0809() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0810() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("5", "0"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0811() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("5", "0"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0812() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0813() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0814() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0815() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0816() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0817() throws Exception { test(DELETE, url(ANNOTATION_BY_ID_URL), uriVars("1", "0"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Translate annotation */
	/* Expecting NOT_FOUND in case of successful authorization as there is no annotation with ID 0 in project 1 */
	@Test void test0900() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0901() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("5", "0"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0902() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0903() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0904() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0905() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0906() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0907() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0908() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0909() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0910() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("5", "0"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0911() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("5", "0"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0912() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0913() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0914() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0915() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0916() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0917() throws Exception { test(POST, url(AI_ANNOTATION_TRANSLATION_URL), uriVars("1", "0"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve linked Business Variables of annotation by annotation ID. */
	/* Expecting OK in case of successful authorization */
	@Test void test1000() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test1001() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1002() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test1003() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1004() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test1005() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1006() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test1007() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1008() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test1009() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1010() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1011() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1012() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1013() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1014() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1015() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1016() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1017() throws Exception { test(GET, url(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }

	/* Retrieve list of Annotation By module id Based on Offset*/
	@Test void test1101() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(admin()), NOT_FOUND); }
	@Test void test1102() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("5", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(admin()), NOT_FOUND); }
	@Test void test1103() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1104() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1105() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(miningManager()), NOT_FOUND); }
	@Test void test1106() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1107() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(miningEditor()), NOT_FOUND); }
	@Test void test1108() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1109() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(miningViewer()), NOT_FOUND); }
	@Test void test1110() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1111() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("5", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1112() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("5", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1113() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1114() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1115() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1116() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1117() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1118() throws Exception { test(GET, url(ANNOTATION_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(discoveryLightViewer()), FORBIDDEN); }

	/* Retrieve annotations which has given custom property assigned */
	/* Expecting OK in case of successful authorization */
	@Test void test1201() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(admin()), OK); }
	@Test void test1202() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("5"), params(asList(of("propertyName", "foo"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1203() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(clientAdmin()), OK); }
	@Test void test1204() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1205() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(miningManager()), OK); }
	@Test void test1206() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1207() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(miningEditor()), OK); }
	@Test void test1208() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1209() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(miningViewer()), OK); }
	@Test void test1210() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1211() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("5"), params(asList(of("propertyName", "foo"))), OBJECT, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1212() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("5"), params(asList(of("propertyName", "foo"))), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1213() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1214() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1215() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1216() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1217() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1218() throws Exception { test(GET, url(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL), uriVars("1"), params(asList(of("propertyName", "foo"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Import CSV annotations */
	/* Expecting BAD_REQUEST in case of successful authorization */
	@Test void test1301() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(admin()), BAD_REQUEST, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1302() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("3"), noParams(), roles(admin()), BAD_REQUEST, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1303() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(clientAdmin()), BAD_REQUEST, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1304() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(clientAdmin(2)), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1305() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(miningManager()), BAD_REQUEST, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1306() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(miningManager(1, 2)), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1307() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(miningEditor()), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1308() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(miningEditor(1, 2)), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1309() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(miningViewer()), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1310() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(miningViewer(1, 2)), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1311() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("5"), noParams(), roles(miningViewer(1, 5)), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1312() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("5"), noParams(), roles(miningViewer(1, 4)), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1313() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(discoveryManager()), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1314() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(discoveryEditor()), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1315() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(discoveryViewer()), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1316() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(discoveryLightManager()), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1317() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(discoveryLightEditor()), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test1318() throws Exception { multiPartTests(POST, url(ANNOTATION_IMPORT_URL), uriVars("1"), noParams(), roles(discoveryLightViewer()), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	
	/* Delete bulk annotations or data dictionaries*/
	/* Expecting OK in case of successful authorization */
	@Test void test1401() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(admin()), OK); }
	@Test void test1402() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("5", "Annotation"), noParams(), LIST, roles(admin()), NOT_FOUND); }
	@Test void test1403() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(clientAdmin()), OK); }
	@Test void test1404() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1405() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(miningManager()), OK); }
	@Test void test1406() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1407() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(miningEditor()), OK); }
	@Test void test1408() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1409() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(miningViewer()), FORBIDDEN); }
	@Test void test1410() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1411() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("5", "Annotation"), noParams(), LIST, roles(miningEditor(1, 5)), NOT_FOUND); }
	@Test void test1412() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("5", "Annotation"), noParams(), LIST, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1413() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1414() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1415() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1416() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1417() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1418() throws Exception { test(DELETE, url(ANNOTATION_DATADICTIONARY_BULK_DELETE_URL), uriVars("1", "Annotation"), noParams(), LIST, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
