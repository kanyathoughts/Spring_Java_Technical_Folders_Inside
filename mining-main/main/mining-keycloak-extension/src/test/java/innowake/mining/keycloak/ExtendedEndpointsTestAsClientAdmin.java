/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak;

import static innowake.mining.keycloak.model.ProjectNature.DISCOVERY;
import static innowake.mining.keycloak.model.ProjectNature.DISCOVERY_LIGHT;
import static innowake.mining.keycloak.model.ProjectNature.MINING;
import static org.apache.http.HttpStatus.SC_FORBIDDEN;
import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.apache.http.HttpStatus.SC_OK;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledIfSystemProperty;
import org.keycloak.representations.idm.MappingsRepresentation;
import org.keycloak.representations.idm.RoleRepresentation;
import org.keycloak.representations.idm.UserRepresentation;
import innowake.mining.keycloak.model.HttpMethod;
import innowake.mining.keycloak.model.Member;
import innowake.mining.keycloak.model.PaginatedResponse;
import innowake.mining.keycloak.model.ProjectNature;
import innowake.mining.keycloak.model.ProjectRole;
import innowake.mining.keycloak.model.UserRole;

/**
 * Test class to validate the functionality of the extended Keycloak endpoints.<br/>
 * The tests are validated with a <i>"Client Admin"</i> user.
 */
@DisabledIfSystemProperty(named = AbstractTest.SYSTEM_PROPERTY_SKIP_INTEGRATION_TESTS, matches = "true")
class ExtendedEndpointsTestAsClientAdmin extends AbstractTest {

	private static final String CLIENT_ADMIN_USERNAME = "client-1-admin@users.com";

	/**
	 * Test to validate that the Client Admin is unable to fetch members of the Client it does not have access to.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetClientMembersNoAccess() throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(CLIENT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, TWO),
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that the Client Admin is unable to fetch member count of the Client it does not have access to.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetClientMemberCountNoAccess() throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(CLIENT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, TWO) + "/count",
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that the Client Admin is unable to check whether there exists a group for a specific client.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotCheckWhetherClientExists() throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, ONE),
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that the Client Admin is unable to check whether there exists roles for a specific project.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotCheckWhetherProjectHasRoles() throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(PROJECT_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, ONE, ONE),
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that the Client Admin is unable to fetch members of the Project it does not have access to.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetProjectMembersNoAccess() throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, TWO, THREE),
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that the Client Admin is unable to fetch member count of the Project it does not have access to.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetProjectMemberCountNoAccess() throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, TWO, THREE) + "/count",
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that the Client Admin is unable to fetch member information from a Project it does not have access to.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetMemberByIdNoAccess() throws IOException {
		final UserRepresentation projectThreeManager = getUserFromRealmByUserName("project-3-manager@users.com");
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, TWO, THREE) + "/" + projectThreeManager.getId(),
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that the Client Admin is unable to fetch the Client Admins it does not have access to.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetClientAdminsNoAccess() throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, TWO) + "/admins",
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that a Client Admin is not able to create Client roles.
	 * They do not have the authorization to do so.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotCreateClientAttributes() throws IOException {
		final Integer groupCountBeforeCreatingClient = getGroupCountInTestRealm();
		assertEquals(GROUP_COUNT, groupCountBeforeCreatingClient);

		final Integer newClientId = THREE;
		final HttpURLConnection createClientAttrCon = makeHttpCallAndGetConnection(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, newClientId),
				HttpMethod.POST,
				getAccessToken(),
				null);
		assertEquals(SC_FORBIDDEN, createClientAttrCon.getResponseCode());

		/* Validate that the new Client Users Group was NOT created. */
		final Integer groupsResponseAfterCreatingClient = getGroupCountInTestRealm();
		assertEquals(groupCountBeforeCreatingClient, groupsResponseAfterCreatingClient);

		/* Validate that the new Client Admin role was NOT created. */
		final String realmManagementAccessToken = getAccessTokenForRealmManagement();
		final HttpURLConnection getAdminRoleForNewClientCon = getRequestWithoutBodyParams(
				AUTH_SERVER_URL + "admin/realms/" + testRealmName + "/roles/" + "client-" + newClientId + "-admin",
				realmManagementAccessToken);
		assertEquals(SC_NOT_FOUND, getAdminRoleForNewClientCon.getResponseCode());

		/* Validate that the new client-3-roles container role was NOT created. */
		final HttpURLConnection getClientRolesContainerRoleForNewClientCon = getRequestWithoutBodyParams(
				AUTH_SERVER_URL + "admin/realms/" + testRealmName + "/roles/" + "client-" + newClientId + "-roles",
				realmManagementAccessToken);
		assertEquals(SC_NOT_FOUND, getClientRolesContainerRoleForNewClientCon.getResponseCode());
	}

	/**
	 * Test to validate that the Client Admin is unable to create {@link ProjectRole Project Roles}
	 * because they do not have the access to the Client to which the Project belongs.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotCreateProjectRolesNoAccessToClient() throws IOException {
		final List<RoleRepresentation> rolesBeforeCreation = getRolesInTestRealm();
		assertThat(rolesBeforeCreation, is(not(empty())));
		final int rolesCountBeforeCreation = rolesBeforeCreation.size();

		final Integer newProjectId = Integer.valueOf(5);
		final HttpURLConnection createProjectAttrCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, TWO, newProjectId) + "?projectNatures=MINING,DISCOVERY",
				HttpMethod.POST,
				getAccessToken(),
				null);
		assertEquals(SC_FORBIDDEN, createProjectAttrCon.getResponseCode());

		final List<RoleRepresentation> rolesAfterCreation = getRolesInTestRealm();
		assertNotNull(rolesAfterCreation);
		/* No new roles were created. */
		assertEquals(rolesCountBeforeCreation, rolesAfterCreation.size());
		/* Ensure that the no new project roles were created. */
		assertTrue(rolesAfterCreation.stream().noneMatch(role -> role.getName().startsWith("client-1-project-" + newProjectId)));
	}

	/**
	 * Test to validate that the Client Admin is unable to fetch Default Natures for the given Project
	 * because they don't have access to the Client to which the Project belongs.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetProjectNaturesNoAccess() throws IOException {
		final HttpURLConnection findProjectNaturesCon = getRequestWithoutBodyParams(
				String.format(PROJECT_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, TWO, THREE) + "/projectNatures",
				getAccessToken());
		assertEquals(SC_FORBIDDEN, findProjectNaturesCon.getResponseCode());
	}

	/**
	 * Test to validate that the Client Admin is unable to modify the Default Natures of the given Project
	 * because they don't have the access to the Client to which the Project belongs.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotChangeProjectNaturesNoAccess() throws IOException {
		final String client2AccessToken = getAccessTokenForTestRealm("client-2-admin@users.com", PASSWORD);
		/* Get project natures for project 3 */
		final List<ProjectNature> projectNatures = getDefaultNaturesForProject(TWO, THREE, client2AccessToken);
		assertEquals(PROJECT_THREE_NATURES_COUNT, projectNatures.size());
		assertThat(projectNatures, containsInAnyOrder(DISCOVERY, DISCOVERY_LIGHT, MINING));

		/* Change project natures for project 3 */
		final HttpURLConnection changeProjectNaturesCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, TWO, THREE) + "?projectNatures=DISCOVERY,DISCOVERY_LIGHT",
				HttpMethod.PUT,
				getAccessToken(), /* This access token belongs to "Client One Admin", who is the Client Admin for Client 1. */
				null);
		assertEquals(SC_FORBIDDEN, changeProjectNaturesCon.getResponseCode());

		/* Validate that project natures have NOT changed */
		final List<ProjectNature> projectNaturesAfterChange = getDefaultNaturesForProject(TWO, THREE, client2AccessToken);
		assertEquals(PROJECT_THREE_NATURES_COUNT, projectNaturesAfterChange.size());
		assertThat(projectNaturesAfterChange, containsInAnyOrder(DISCOVERY, DISCOVERY_LIGHT, MINING));
	}

	/**
	 * Test to validate that the Client Admin is unable to delete {@link ProjectRole Project Roles} for the given Project
	 * because they don't have access to the Client to which the Project belongs.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotDeleteProjectRolesNoAccess() throws IOException {
		final List<RoleRepresentation> rolesBeforeDeleting = getRolesInTestRealm();
		assertThat(rolesBeforeDeleting, is(not(empty())));
		final int roleCountBeforeDeletion = rolesBeforeDeleting.size();

		final HttpURLConnection deleteProjectAttrCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, TWO, THREE),
				HttpMethod.DELETE,
				getAccessToken(),
				null);
		assertEquals(SC_FORBIDDEN, deleteProjectAttrCon.getResponseCode());

		final List<RoleRepresentation> rolesAfterDeleting = getRolesInTestRealm();
		assertNotNull(rolesAfterDeleting);
		/* No roles were deleted. */
		assertEquals(roleCountBeforeDeletion, rolesAfterDeleting.size());
		/* Ensure that no project roles were deleted. */
		final long deletedProjectRoleCount = rolesAfterDeleting.stream()
		                                                       .filter(role -> role.getName().startsWith("client-2-project-3"))
		                                                       .count();
		assertEquals(PROJECT_ROLE_COUNT, deletedProjectRoleCount);
	}

	/**
	 * Test to validate that a Client Admin is unable to delete Client roles for the given Client.
	 * They do not have the authorization to do so.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotDeleteClientRoles() throws IOException {
		final List<RoleRepresentation> rolesBeforeDeleting = getRolesInTestRealm();
		assertThat(rolesBeforeDeleting, is(not(empty())));
		final int clientRolesCountBeforeDeletion = rolesBeforeDeleting.size();

		final HttpURLConnection deleteClientAttrCon = makeHttpCallAndGetConnection(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, TWO),
				HttpMethod.DELETE,
				getAccessToken(),
				null);
		assertEquals(SC_FORBIDDEN, deleteClientAttrCon.getResponseCode());

		final List<RoleRepresentation> rolesAfterDeleting = getRolesInTestRealm();
		assertNotNull(rolesAfterDeleting);
		assertEquals(clientRolesCountBeforeDeletion, rolesAfterDeleting.size());
		final long deletedClientRoleCount = rolesAfterDeleting.stream()
		                                                      .filter(role -> role.getName().startsWith("client-2"))
		                                                      .count();
		assertEquals(16, deletedClientRoleCount);
	}

	/**
	 * Test to validate that the Client Admin is unable to add {@link ProjectRole Project Roles} to given {@link Member}
	 * because they do not have access to the Client to which the Project belongs.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotAddProjectRoleToMemberNoAccess() throws IOException {
		/* Get Project Three Viewer to get the ID. */
		final String realmManagementAccessToken = getAccessTokenForRealmManagement();
		final UserRepresentation projectThreeViewer = getUserFromRealmByUserName("project-3-viewer@users.com");

		/* Member should not have access to Project 2. */

		/* We need to create the Member JSON instead of creating the Member object and converting it to JSON.
		 * We cannot use the constructor provided in the Member class as we do not have
		 * access to the RoleModel to provide the projectRole. */
		final String memberJson = createMemberJsonWithEmail("project-3-viewer@users.com", THREE, UserRole.MANAGER, Collections.emptyList());
		final HttpURLConnection addProjectRoleToMemberCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, TWO, THREE),
				HttpMethod.POST,
				getAccessToken(),
				memberJson);
		assertEquals(SC_FORBIDDEN, addProjectRoleToMemberCon.getResponseCode());

		/* Get Project Three Viewer again, it should NOT have the proper Project Roles. */
		final HttpURLConnection projectThreeViewerAfterUpdateCon = getRequestWithoutBodyParams(
				AUTH_SERVER_URL + "admin/realms/" + testRealmName + "/users/" + projectThreeViewer.getId() + "/role-mappings",
				realmManagementAccessToken);
		assertEquals(SC_OK, projectThreeViewerAfterUpdateCon.getResponseCode());
		final MappingsRepresentation userTenRoleMappings = getResult(projectThreeViewerAfterUpdateCon, MappingsRepresentation.class);
		assertNotNull(userTenRoleMappings);
		assertTrue(userTenRoleMappings.getRealmMappings().stream().noneMatch(role -> role.getName().equalsIgnoreCase("client-2-project-3-manager")));
	}

	/**
	 * Test to validate that the Client Admin is unable to create a new user and assign {@link ProjectRole Project Roles}
	 * to them for the given Project because they don't have access to the Client to which the Project belongs.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotAddProjectRoleCreateUserNoAccess() throws IOException {
		final String newUserEmail = "newUser@users.com";
		/* Try to fetch new user that does not exist. */
		assertUserDoesNotExist(newUserEmail);

		/* We need to create the Member JSON instead of creating the Member object and converting it to JSON.
		 * We cannot use the constructor provided in the Member class as we do not have
		 * access to the RoleModel to provide the projectRole. */
		final String memberJson = createMemberJsonWithEmail(newUserEmail, THREE, UserRole.MANAGER, Arrays.asList(DISCOVERY_LIGHT));
		final HttpURLConnection addProjectRoleToMemberCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, TWO, THREE),
				HttpMethod.POST,
				getAccessToken(),
				memberJson);
		assertEquals(SC_FORBIDDEN, addProjectRoleToMemberCon.getResponseCode());

		/* Get newUser again, it should NOT be created. */
		assertUserDoesNotExist(newUserEmail);
	}

	/**
	 * Test to validate that the Client Admin is unable to add a {@link Member} as a Client Admin for the given Client.
	 * They don't have the access to the given Client.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotAddMemberAsClientAdminNoAccess() throws IOException {
		final String projectThreeManager = "project-3-manager@users.com";
		final String clientTwoAdminAccessToken = getAccessTokenForTestRealm("client-2-admin@users.com", PASSWORD);
		/* Get Client Admins for Client 1. Should have only one admin - User0. */
		final PaginatedResponse<Member> clientAdminsPage = getClientAdmins(TWO, clientTwoAdminAccessToken);
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdminsPage.getContentList().size());
		final Member clientOneAdmin = clientAdminsPage.getContentList().get(0);
		assertEquals("client-2-admin@users.com", clientOneAdmin.getEmail());
		assertEquals("Client Two Admin", clientOneAdmin.getFirstName());
		assertEquals("User", clientOneAdmin.getLastName());

		/* Add User10 as Client Admin for Client 1. */
		/* We need to create the Member JSON instead of creating the Member object and converting it to JSON.
		 * We cannot use the constructor provided in the Member class as we do not have
		 * access to the RoleModel to provide the projectRole. */
		final String memberJson = "{ \"email\": \"" + projectThreeManager + "\" }";
		final HttpURLConnection addAdminClientCon = makeHttpCallAndGetConnection(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, TWO) + "/admins",
				HttpMethod.POST,
				getAccessToken(), /* This Access Token belongs to "Client One Admin" who is a Client Admin for Client 1. */
				memberJson);
		assertEquals(SC_FORBIDDEN, addAdminClientCon.getResponseCode());

		final PaginatedResponse<Member> clientAdminsWithNewAdminPage = getClientAdmins(TWO, clientTwoAdminAccessToken);
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdminsWithNewAdminPage.getContentList().size());
		final String clientAdminEmail = clientAdminsWithNewAdminPage.getContentList().get(0).getEmail();
		assertEquals("client-2-admin@users.com", clientAdminEmail.toLowerCase());
	}

	/**
	 * Test to validate that the Client Admin is unable to delete a {@link Member} as a Client Admin for the specified Client.
	 * They don't have the access to the specified Client.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotDeleteMemberAsClientAdminNoAccess() throws IOException {
		/* Get Client Admins for Client 2. Should have only one admin - Client Two Admin. */
		final String clientTwoAdminAccessToken = getAccessTokenForTestRealm("client-2-admin@users.com", PASSWORD);
		final PaginatedResponse<Member> clientAdminsPage = getClientAdmins(TWO, clientTwoAdminAccessToken);
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdminsPage.getContentList().size());
		final Member clientTwoAdmin = clientAdminsPage.getContentList().get(0);
		assertEquals("client-2-admin@users.com", clientTwoAdmin.getEmail());
		assertEquals("Client Two Admin", clientTwoAdmin.getFirstName());
		assertEquals("User", clientTwoAdmin.getLastName());

		/* Delete Client Two Admin as Client Admin. */
		final HttpURLConnection deleteClientAdminCon = makeHttpCallAndGetConnection(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, TWO) + "/admins/" + clientTwoAdmin.getId(),
				HttpMethod.DELETE,
				getAccessToken(), /* This access token belongs to "Client One Admin", they don't have access to Client 2. */
				null);
		assertEquals(SC_FORBIDDEN, deleteClientAdminCon.getResponseCode());

		/* Get Client Admins again, list should be unchanged. */
		final PaginatedResponse<Member> clientAdminsAfterDeletePage = getClientAdmins(TWO, clientTwoAdminAccessToken);
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdminsAfterDeletePage.getContentList().size());
	}

	/**
	 * Test to validate that the Client Admin is unable to delete a {@link Member} from a Project
	 * because the Client Admin does not have access to the Client to which the Project belongs.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotDeleteMemberFromProjectNoAccess() throws IOException {
		final String clientTwoAccessToken = getAccessTokenForTestRealm("client-2-admin@users.com", PASSWORD);
		final PaginatedResponse<Member> projectMembersPage = getProjectMembers(TWO, THREE, clientTwoAccessToken);
		final int projectMemberCountBeforeDeletion = projectMembersPage.getContentList().size();
		assertEquals(PROJECT_ONE_MEMBER_COUNT, projectMemberCountBeforeDeletion);

		final Member memberToDelete = projectMembersPage.getContentList().get(0);
		/* Delete member from Project. */
		final HttpURLConnection deleteProjectMemberCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, TWO, THREE) + "/" + memberToDelete.getId(),
				HttpMethod.DELETE,
				getAccessToken(), /* This access token belongs to "Client One Admin", who does not have access to Client 2. */
				null);
		assertEquals(SC_FORBIDDEN, deleteProjectMemberCon.getResponseCode());

		/* Get Project Members again, should include same number of members. */
		final PaginatedResponse<Member> projectMembersPageAfterDelete = getProjectMembers(TWO, THREE, clientTwoAccessToken);
		assertEquals(projectMemberCountBeforeDeletion, projectMembersPageAfterDelete.getContentList().size());
		assertTrue(projectMembersPageAfterDelete.getContentList().stream()
		                                                         .map(Member::getEmail)
		                                                         .anyMatch(email -> email.equalsIgnoreCase(memberToDelete.getEmail())));
	}

	/**
	 * Test to validate that the Client Admin is unable to assign {@link ProjectRole Project Roles} to a {@link Member}
	 * because they don't have the access to the Client to which the Project belongs.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotAssignProjectRolesToMemberNoAccess() throws IOException {
		final String clientTwoAdminAccessToken = getAccessTokenForTestRealm("client-2-admin@users.com", PASSWORD);
		final PaginatedResponse<Member> projectMembersPage = getProjectMembers(TWO, THREE, clientTwoAdminAccessToken);
		assertEquals(PROJECT_ONE_MEMBER_COUNT, projectMembersPage.getContentList().size());
		final List<Member> projectMembers = projectMembersPage.getContentList();
		final Member viewerMember = projectMembers.stream()
		                                          .filter(member -> member.getEmail().equalsIgnoreCase("project-3-viewer@users.com"))
		                                          .findFirst()
		                                          .orElse(null);
		assertNotNull(viewerMember);
		final ProjectRole memberProjectRoleForProjectThree = getProjectRoleFromMember(viewerMember, THREE);
		assertNotNull(memberProjectRoleForProjectThree);
		assertEquals(UserRole.VIEWER, memberProjectRoleForProjectThree.getUserRole());

		/* Change VIEWER to EDITOR */
		final String memberToUpdateJson = createMemberJsonWithID(viewerMember.getId(), THREE, UserRole.EDITOR, Collections.emptyList());
		final HttpURLConnection assingEditorRoleToMemberCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, TWO, THREE) + "/" + viewerMember.getId(),
				HttpMethod.PUT,
				getAccessToken(), /* This access token belongs to "Client One Admin", who does not have access to Client 2. */
				memberToUpdateJson);
		assertEquals(SC_FORBIDDEN, assingEditorRoleToMemberCon.getResponseCode());

		/* Get Project Member to validate that the User Role was NOT changed. */
		final Member updatedMember = getProjectMemberByMemberID(TWO, THREE, viewerMember.getId(), clientTwoAdminAccessToken);
		final ProjectRole updatedProjectRole = getProjectRoleFromMember(updatedMember, THREE);
		assertNotNull(updatedProjectRole);
		assertEquals(UserRole.VIEWER, updatedProjectRole.getUserRole());
	}
	
	/**
	 * Test to validate that the given user is unable to find a {@link Member}, when the member does not belong to user project.
	 *
	 * @throws IOException while making HTTP connection
	 */
	@Test
	void testShouldNotGetMemberForbiddenAccess() throws IOException {
		/* Get member that does not belong to the Project. */
		final UserRepresentation projectThreeManager = getUserFromRealmByUserName("project-3-manager@users.com");
		final HttpURLConnection memberToBeFound = makeHttpCallAndGetConnection(
				String.format(MEMBER_URL_PATTERN, testRealmName, SPI_ID, projectThreeManager.getId()),
				HttpMethod.GET,
				getAccessToken(), /* This access token belongs to "Client One Admin", who does not have access to Client 2 project. */
				null);
		assertEquals(SC_FORBIDDEN, memberToBeFound.getResponseCode());
	}
	
	/**
	 * Test to validate that the given user is able to find a {@link Member}, when the user has client-admin access to member's project.
	 *
	 * @throws IOException while making HTTP connection
	 */
	@Test
	void testShouldGetMember() throws IOException {
		/* Get member that does belong to same the client. */
		final UserRepresentation projectOneViewer = getUserFromRealmByUserName("project-1-viewer@users.com");
		final HttpURLConnection memberToBeFound = makeHttpCallAndGetConnection(
				String.format(MEMBER_URL_PATTERN, testRealmName, SPI_ID, projectOneViewer.getId()),
				HttpMethod.GET,
				getAccessToken(),
				null);
		assertEquals(HttpStatus.SC_OK, memberToBeFound.getResponseCode());
		final Member member = getResult(memberToBeFound, Member.class);
		assertEquals(projectOneViewer.getFirstName(), member.getFirstName());
		assertEquals(projectOneViewer.getLastName(), member.getLastName());
	}

	private String getAccessToken() throws IOException {
		return getAccessTokenForTestRealm(CLIENT_ADMIN_USERNAME, PASSWORD);
	}
}
