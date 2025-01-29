/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.MemberController.CLIENT_ADMINS_URL;
import static innowake.mining.server.controller.MemberController.CLIENT_ADMIN_BY_ID_URL;
import static innowake.mining.server.controller.MemberController.CLIENT_MEMBERS_URL;
import static innowake.mining.server.controller.MemberController.CLIENT_MEMBER_COUNT_URL;
import static innowake.mining.server.controller.MemberController.MEMBER_BY_ID_URL;
import static innowake.mining.server.controller.MemberController.PROJECT_MEMBERS_URL;
import static innowake.mining.server.controller.MemberController.PROJECT_MEMBER_COUNT_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.controller.MemberController;
import innowake.mining.server.service.AuthorizationManagementService;

/**
 * Authorization Tests for {@link MemberController}.
 */
class MemberControllerAuthorizationTests extends RestAuthorizationTests {

	/**
	 * Mocking the service is required for not making actual service calls to Keycloak during testing.
	 */
	@MockBean
	@Nullable
	private AuthorizationManagementService service;

	/* Retrieve Members for a given Client. */
	/* Expecting OK in case of successful authorization */
	@Test void test0001() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0003() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0004() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("5"), noParams(), OBJECT, roles(clientAdmin(5)), NOT_FOUND); }
	@Test void test0005() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0006() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0007() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0008() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0009() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0010() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0011() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0012() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0013() throws Exception { test(GET, url(CLIENT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve Member count for a given Client. */
	/* Expecting OK in case of successful authorization */
	@Test void test0100() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0101() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0102() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0103() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("5"), noParams(), OBJECT, roles(clientAdmin(5)), NOT_FOUND); }
	@Test void test0104() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0105() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0106() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0107() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0108() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0109() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0110() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0111() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0112() throws Exception { test(GET, url(CLIENT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve Members for a given Project. */
	/* Expecting OK in case of successful authorization */
	@Test void test0200() throws Exception { test(GET, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0201() throws Exception { test(GET, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0202() throws Exception { test(GET, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0203() throws Exception { test(GET, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0204() throws Exception { test(GET, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0205() throws Exception { test(GET, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0206() throws Exception { test(GET, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0207() throws Exception { test(GET, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0208() throws Exception { test(GET, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0209() throws Exception { test(GET, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0210() throws Exception { test(GET, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0211() throws Exception { test(GET, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve Member count for a given Project. */
	/* Expecting OK in case of successful authorization */
	@Test void test0300() throws Exception { test(GET, url(PROJECT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0301() throws Exception { test(GET, url(PROJECT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0302() throws Exception { test(GET, url(PROJECT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0303() throws Exception { test(GET, url(PROJECT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0304() throws Exception { test(GET, url(PROJECT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0305() throws Exception { test(GET, url(PROJECT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0306() throws Exception { test(GET, url(PROJECT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0307() throws Exception { test(GET, url(PROJECT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0308() throws Exception { test(GET, url(PROJECT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0309() throws Exception { test(GET, url(PROJECT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0310() throws Exception { test(GET, url(PROJECT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0311() throws Exception { test(GET, url(PROJECT_MEMBER_COUNT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve Member by ID. */
	/* Expecting OK in case of successful authorization */
	@Test void test0400() throws Exception { test(GET, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0401() throws Exception { test(GET, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0402() throws Exception { test(GET, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0403() throws Exception { test(GET, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0404() throws Exception { test(GET, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0405() throws Exception { test(GET, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0406() throws Exception { test(GET, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0407() throws Exception { test(GET, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0408() throws Exception { test(GET, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0409() throws Exception { test(GET, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0410() throws Exception { test(GET, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0411() throws Exception { test(GET, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve Client Admins for a given Client. */
	/* Expecting OK in case of successful authorization */
	@Test void test0500() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0501() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0502() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0503() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("5"), noParams(), OBJECT, roles(clientAdmin(5)), NOT_FOUND); }
	@Test void test0504() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0505() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0506() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0507() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0508() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0509() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0510() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0511() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0512() throws Exception { test(GET, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Add Member as Client Admin for a given Client. */
	/* Expecting OK in case of successful authorization */
	@Test void test0600() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0601() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0602() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0603() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("5"), noParams(), OBJECT, roles(clientAdmin(5)), NOT_FOUND); }
	@Test void test0604() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0605() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0606() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0607() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0608() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0609() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0610() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0611() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0612() throws Exception { test(POST, url(CLIENT_ADMINS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Add Project Role to Member for a given Project. */
	/* Expecting OK in case of successful authorization */
	@Test void test0700() throws Exception { test(POST, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0701() throws Exception { test(POST, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0702() throws Exception { test(POST, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0703() throws Exception { test(POST, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0704() throws Exception { test(POST, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0705() throws Exception { test(POST, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0706() throws Exception { test(POST, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0707() throws Exception { test(POST, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0708() throws Exception { test(POST, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0709() throws Exception { test(POST, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0710() throws Exception { test(POST, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0711() throws Exception { test(POST, url(PROJECT_MEMBERS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Remove Client Admin role for given Member and Client. */
	/* Expecting OK in case of successful authorization */
	@Test void test0800() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0801() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0802() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0803() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("5", "1"), noParams(), OBJECT, roles(clientAdmin(5)), NOT_FOUND); }
	@Test void test0804() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0805() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0806() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0807() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0808() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0809() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0810() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0811() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0812() throws Exception { test(DELETE, url(CLIENT_ADMIN_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Delete Project Role for given Member and Project. */
	/* Expecting OK in case of successful authorization */
	@Test void test0900() throws Exception { test(DELETE, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0901() throws Exception { test(DELETE, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0902() throws Exception { test(DELETE, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0903() throws Exception { test(DELETE, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0904() throws Exception { test(DELETE, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0905() throws Exception { test(DELETE, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0906() throws Exception { test(DELETE, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0907() throws Exception { test(DELETE, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0908() throws Exception { test(DELETE, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0909() throws Exception { test(DELETE, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0910() throws Exception { test(DELETE, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0911() throws Exception { test(DELETE, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Assign Project Role for given Member and Project. */
	/* Expecting OK in case of successful authorization */
	@Test void test1000() throws Exception { test(PUT, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test1001() throws Exception { test(PUT, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(clientAdmin()), FORBIDDEN); }
	@Test void test1002() throws Exception { test(PUT, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test1003() throws Exception { test(PUT, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test1004() throws Exception { test(PUT, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test1005() throws Exception { test(PUT, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1006() throws Exception { test(PUT, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1007() throws Exception { test(PUT, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1008() throws Exception { test(PUT, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1009() throws Exception { test(PUT, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1010() throws Exception { test(PUT, url(MEMBER_BY_ID_URL), uriVars("1", "1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
