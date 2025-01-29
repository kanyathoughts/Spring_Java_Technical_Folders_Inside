/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.AnnotationToFunctionalBlockController.CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION;
import static innowake.mining.server.controller.AnnotationToFunctionalBlockController.DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS;
import static innowake.mining.server.controller.AnnotationToFunctionalBlockController.FUNCTIONAL_BLOCKS_FROM_ANNOTATION;
import static innowake.mining.server.controller.AnnotationToFunctionalBlockController.FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.LIST;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.AnnotationToFunctionalBlockController;
import innowake.mining.shared.model.AnnotationType;


/**
 * Authorization tests for the {@link  AnnotationToFunctionalBlockController}.
*/
public class AnnotationToFunctionalBlockControllerAuthorizationTest extends RestAuthorizationTests {

			@Test void test0001() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(admin()), OK); }
			@Test void test0002() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("5"), noParams(),
					LIST, roles(admin()), NOT_FOUND); }
			@Test void test0003() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(clientAdmin()), OK); }
			@Test void test0004() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(clientAdmin(2)), FORBIDDEN); }
			@Test void test0005() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(miningManager()), OK); }
			@Test void test0006() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(miningManager(1, 2)), FORBIDDEN); }
			@Test void test0007() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(miningEditor()), OK); }
			@Test void test0008() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(miningEditor(1, 2)), FORBIDDEN); }
			@Test void test0009() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(miningViewer()), OK); }
			@Test void test0010() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(miningViewer(1, 2)), FORBIDDEN); }
			@Test void test00011() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("5"), noParams(),
					LIST, roles(miningViewer(1, 5)), NOT_FOUND); }
			@Test void test0012() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("5"), noParams(),
					LIST, roles(miningViewer(1, 4)), FORBIDDEN); }
			@Test void test0013() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(discoveryManager()), FORBIDDEN); }
			@Test void test0014() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(discoveryEditor()), FORBIDDEN); }
			@Test void test0015() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(discoveryViewer()), FORBIDDEN); }
			@Test void test0016() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(discoveryLightManager()), FORBIDDEN); }
			@Test void test0017() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(discoveryLightEditor()), FORBIDDEN); }
			@Test void test0018() throws Exception { test(POST, url(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(discoveryLightViewer()), FORBIDDEN); }
			@Test void test0019() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(admin()), OK); }
			@Test void test0020() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("5"), noParams(),
					LIST, roles(admin()), NOT_FOUND); }
			@Test void test0021() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(clientAdmin()), OK); }
			@Test void test0022() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(clientAdmin(2)), FORBIDDEN); }
			@Test void test0023() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(miningManager()), OK); }
			@Test void test0024() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(miningManager(1, 2)), FORBIDDEN); }
			@Test void test0025() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(miningEditor()), OK); }
			@Test void test0026() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(miningEditor(1, 2)), FORBIDDEN); }
			@Test void test0027() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(miningViewer()), OK); }
			@Test void test0028() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(miningViewer(1, 2)), FORBIDDEN); }
			@Test void test0029() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("5"), noParams(),
					LIST, roles(miningViewer(1, 5)), NOT_FOUND); }
			@Test void test0030() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("5"), noParams(),
					LIST, roles(miningViewer(1, 4)), FORBIDDEN); }
			@Test void test0031() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(discoveryManager()), FORBIDDEN); }
			@Test void test0032() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(discoveryEditor()), FORBIDDEN); }
			@Test void test0033() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(discoveryViewer()), FORBIDDEN); }
			@Test void test0034() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(discoveryLightManager()), FORBIDDEN); }
			@Test void test0035() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(discoveryLightEditor()), FORBIDDEN); }
			@Test void test0036() throws Exception { test(POST, url(FUNCTIONAL_BLOCKS_FROM_ANNOTATION), uriVars("1"), noParams(),
					LIST, roles(discoveryLightViewer()), FORBIDDEN); }
			@Test void test0037() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(),OBJECT,
					roles(admin()), OK); }
			@Test void test0038() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("5", "1"), noParams(), OBJECT,
					roles(admin()), NOT_FOUND); }
			@Test void test0039() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(clientAdmin()), OK); }
			@Test void test0040() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(clientAdmin(2)), FORBIDDEN); }
			@Test void test0041() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(miningManager()), OK); }
			@Test void test0042() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(miningManager(1, 2)), FORBIDDEN); }
			@Test void test0043() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(miningEditor()), OK); }
			@Test void test0044() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(miningEditor(1, 2)), FORBIDDEN); }
			@Test void test0045() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(miningViewer()), OK); }
			@Test void test0046() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(miningViewer(1, 2)), FORBIDDEN); }
			@Test void test0047() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("5", "1"), noParams(), OBJECT,
					roles(miningViewer(1, 5)), NOT_FOUND); }
			@Test void test0048() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("5", "1"), noParams(), OBJECT,
					roles(miningViewer(1, 4)), FORBIDDEN); }
			@Test void test0049() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(discoveryManager()), FORBIDDEN); }
			@Test void test0050() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(discoveryEditor()), FORBIDDEN); }
			@Test void test0051() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(discoveryViewer()), FORBIDDEN); }
			@Test void test0052() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(discoveryLightManager()), FORBIDDEN); }
			@Test void test0053() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(discoveryLightEditor()), FORBIDDEN); }
			@Test void test0054() throws Exception { test(GET, url(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID), uriVars("1", "1"), noParams(), OBJECT,
					roles(discoveryLightViewer()), FORBIDDEN); }
			/* Delete empty auto generated functional units */
			/* Expecting BAD_REQUEST in case of successful authorization as there is no uid found in project 1*/
			@Test void test0055() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(admin()), NO_CONTENT); }
			@Test void test0056() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("5", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(admin()), NOT_FOUND); }
			@Test void test0057() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(clientAdmin()), NO_CONTENT); }
			@Test void test0058() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
			@Test void test0059() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
			@Test void test0060() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(miningEditor()), NO_CONTENT); }
			@Test void test0061() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
			@Test void test0062() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
			@Test void test0063() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN);}
			@Test void test0064() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
			@Test void test0065() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
			@Test void test0066() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
			@Test void test0067() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
			@Test void test0068() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
			@Test void test0069() throws Exception { test(DELETE, url(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS), uriVars("1", AnnotationType.FUNCTIONAL),
					noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }

			}
