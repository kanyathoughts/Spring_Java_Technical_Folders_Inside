/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.ProjectControllerV2.PROJECT_BY_ID_URL;
import static innowake.mining.server.controller.ProjectControllerV2.PROJECT_COLELCTIONS_URL;
import static innowake.mining.server.controller.ProjectControllerV2.PROJECT_COLLECTIONS_FOR_CLIENT_URL;
import static innowake.mining.server.controller.ProjectControllerV2.PROJECT_COUNT_FOR_CLIENT_URL;
import static innowake.mining.server.controller.ProjectControllerV2.PROJECT_NATURES_BY_ID;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.LIST;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.doReturn;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.controller.ProjectControllerV2;
import innowake.mining.server.service.AuthorizationManagementService;
import innowake.mining.server.service.ConverterService;
import innowake.mining.shared.model.ProjectNature;

/**
 * Authorization Tests for {@link ProjectControllerV2}.
 */
class ProjectControllerV2AuthorizationTests extends RestAuthorizationTests {
	
	@MockBean
	@Nullable
	private AuthorizationManagementService authorizationManagementService;
	
	@MockBean
	@Nullable
	private ConverterService converterService;
	
	@BeforeEach
	void setupMocks() {
		doReturn(new ProjectNature[]{}).when(authorizationManagementService).findProjectNatures(anyLong(),anyLong());
	}
	
	/* Retrieves Project count for Client. */
	/* Expecting OK in case of successful authorization. */
	@Test void test0001() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0003() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0004() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0005() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(miningManager(1, 5)), FORBIDDEN); }
	@Test void test0006() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(miningManager(5, 8)), NOT_FOUND); }
	@Test void test0007() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0008() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(miningEditor(1, 5)), FORBIDDEN); }
	@Test void test0009() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(miningEditor(5, 8)), NOT_FOUND); }
	@Test void test0010() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0011() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0012() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(5, 8)), NOT_FOUND); }
	@Test void test0013() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0014() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryManager(1, 5)), FORBIDDEN); }
	@Test void test0015() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryManager(5, 8)), NOT_FOUND); }
	@Test void test0016() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0017() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryEditor(1, 5)), FORBIDDEN); }
	@Test void test0018() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryEditor(5, 8)), NOT_FOUND); }
	@Test void test0019() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0020() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryViewer(1, 5)), FORBIDDEN); }
	@Test void test0021() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryViewer(5, 8)), NOT_FOUND); }
	@Test void test0022() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), OK); }
	@Test void test0023() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightManager(1, 5)), FORBIDDEN); }
	@Test void test0024() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightManager(5, 8)), NOT_FOUND); }
	@Test void test0025() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), OK); }
	@Test void test0026() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightEditor(1, 5)), FORBIDDEN); }
	@Test void test0027() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightEditor(5, 8)), NOT_FOUND); }
	@Test void test0028() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), OK); }
	@Test void test0029() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightViewer(1, 5)), FORBIDDEN); }
	@Test void test0030() throws Exception { test(GET, url(PROJECT_COUNT_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightViewer(5, 8)), NOT_FOUND); }
	
	/* Retrieves Project for a Client. */
	/* Expecting OK in case of successful authorization. */
	@Test void test0100() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0101() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0102() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0103() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0104() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(miningManager(1, 5)), FORBIDDEN); }
	@Test void test0105() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(miningManager(5, 8)), NOT_FOUND); }
	@Test void test0106() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0107() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(miningEditor(1, 5)), FORBIDDEN); }
	@Test void test0108() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(miningEditor(5, 8)), NOT_FOUND); }
	@Test void test0109() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0110() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0111() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer(5, 8)), NOT_FOUND); }
	@Test void test0112() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0113() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryManager(1, 5)), FORBIDDEN); }
	@Test void test0114() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryManager(5, 8)), NOT_FOUND); }
	@Test void test0115() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0116() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryEditor(1, 5)), FORBIDDEN); }
	@Test void test0117() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryEditor(5, 8)), NOT_FOUND); }
	@Test void test0118() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0119() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryViewer(1, 5)), FORBIDDEN); }
	@Test void test0120() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryViewer(5, 8)), NOT_FOUND); }
	@Test void test0121() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), OK); }
	@Test void test0122() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightManager(1, 5)), FORBIDDEN); }
	@Test void test0123() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightManager(5, 8)), NOT_FOUND); }
	@Test void test0124() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), OK); }
	@Test void test0125() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightEditor(1, 5)), FORBIDDEN); }
	@Test void test0126() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightEditor(5, 8)), NOT_FOUND); }
	@Test void test0127() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), OK); }
	@Test void test0128() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightViewer(1, 5)), FORBIDDEN); }
	@Test void test0129() throws Exception { test(GET, url(PROJECT_COLLECTIONS_FOR_CLIENT_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightViewer(5, 8)), NOT_FOUND); }
	
	/* Creates a Project. */
	/* Expecting BAD_REQUEST in case of successful authorization as we're providing and empty JSON in request. */
	@Test void test0200() throws Exception { test(POST, url(PROJECT_COLELCTIONS_URL), noUriVars(), noParams(), OBJECT, roles(admin()), BAD_REQUEST); }
	@Test void test0201() throws Exception { test(POST, url(PROJECT_COLELCTIONS_URL), noUriVars(), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0202() throws Exception { test(POST, url(PROJECT_COLELCTIONS_URL), noUriVars(), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0203() throws Exception { test(POST, url(PROJECT_COLELCTIONS_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0204() throws Exception { test(POST, url(PROJECT_COLELCTIONS_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0205() throws Exception { test(POST, url(PROJECT_COLELCTIONS_URL), noUriVars(), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0206() throws Exception { test(POST, url(PROJECT_COLELCTIONS_URL), noUriVars(), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0207() throws Exception { test(POST, url(PROJECT_COLELCTIONS_URL), noUriVars(), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0208() throws Exception { test(POST, url(PROJECT_COLELCTIONS_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0209() throws Exception { test(POST, url(PROJECT_COLELCTIONS_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0210() throws Exception { test(POST, url(PROJECT_COLELCTIONS_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieves the Default Natures for a Project. */
	/* Expecting NOT_FOUND in case of successful authorization as we're providing invalid Project ID. */
	@Test void test0300() throws Exception { test(GET, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0301() throws Exception { test(GET, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0302() throws Exception { test(GET, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0303() throws Exception { test(GET, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0304() throws Exception { test(GET, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0305() throws Exception { test(GET, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0306() throws Exception { test(GET, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0307() throws Exception { test(GET, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0308() throws Exception { test(GET, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0309() throws Exception { test(GET, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0310() throws Exception { test(GET, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0311() throws Exception { test(GET, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Updates the Default Natures for a Project. */
	/* Expecting NOT_FOUND in case of successful authorization as we're providing invalid Project ID. */
	@Test void test0400() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), LIST, roles(admin()), BAD_REQUEST); }
	@Test void test0401() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("5"), noParams(), LIST, roles(admin()), NOT_FOUND); }
	@Test void test0402() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), LIST, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0403() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), LIST, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0404() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), LIST, roles(miningManager()), FORBIDDEN); }
	@Test void test0405() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), LIST, roles(miningEditor()), FORBIDDEN); }
	@Test void test0406() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), LIST, roles(miningViewer()), FORBIDDEN); }
	@Test void test0407() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), LIST, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0408() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), LIST, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0409() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), LIST, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0410() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), LIST, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0411() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), LIST, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0412() throws Exception { test(PUT, url(PROJECT_NATURES_BY_ID), uriVars("1"), noParams(), LIST, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Deletes a Project. */
	/* Expecting NOT_FOUND in case of successful authorization as we're providing invalid Project ID. */
	@Test void test0500() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("3"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0501() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0502() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0503() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("2"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0504() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0505() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0506() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0507() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0508() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0509() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0510() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0511() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0512() throws Exception { test(DELETE, url(PROJECT_BY_ID_URL), uriVars("5"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
