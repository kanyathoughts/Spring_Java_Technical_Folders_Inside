/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak;

import static org.apache.http.HttpStatus.SC_FORBIDDEN;
import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Collections;
import java.util.List;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledIfSystemProperty;
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
 * The tests are validated with a <i>"manager"</i> user.
 */
@DisabledIfSystemProperty(named = AbstractTest.SYSTEM_PROPERTY_SKIP_INTEGRATION_TESTS, matches = "true")
class ExtendedEndpointsTestAsRegularUser extends AbstractTest {
	
	private static final String MANAGER_USERNAME = "project-1-manager@users.com";

	/**
	 * Test to validate that a manager is unable to fetch members of a Client.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetClientMembers() throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(CLIENT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE),
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that a manager is unable to fetch the Client Member count.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetClientMemberCount() throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(CLIENT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE) + "/count",
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that a manager is unable to fetch the Project members.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetProjectMembers() throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, ONE),
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that a manager is unable to get the Project Member count.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetProjectMemberCount() throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, ONE) + "/count",
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that the manager is unable to fetch a member belonging to a specific project by its ID.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetMemberById() throws IOException {
		final UserRepresentation projectOneViewer = getUserFromRealmByUserName("project-1-viewer@users.com");
		final HttpURLConnection findMemberByIdCon = getRequestWithoutBodyParams(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, ONE) + "/" + projectOneViewer.getId(),
				getAccessToken());
		assertEquals(SC_FORBIDDEN, findMemberByIdCon.getResponseCode());
	}

	/**
	 * Test to validate that a manager is unable to fetch Client Admins for given Client.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetClientAdmins() throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, ONE) + "/admins",
				getAccessToken());
		assertEquals(SC_FORBIDDEN, con.getResponseCode());
	}

	/**
	 * Test to validate that a manager is unable to create Client Roles.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotCreateClientAttributes() throws IOException {
		final HttpURLConnection createClientAttrCon = makeHttpCallAndGetConnection(
				AUTH_SERVER_URL + "realms/" + testRealmName + "/" + SPI_ID + "/clients/4",
				HttpMethod.POST,
				getAccessToken(),
				null);
		assertEquals(SC_FORBIDDEN, createClientAttrCon.getResponseCode());
	}

	/**
	 * Test to validate that a manager is unable to create {@link ProjectRole Project Roles}.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotCreateProjectRoles() throws IOException {
		final HttpURLConnection createProjectAttrCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, ONE, Integer.valueOf(5)) + "?projectNatures=MINING,DISCOVERY",
				HttpMethod.POST,
				getAccessToken(),
				null);
		assertEquals(SC_FORBIDDEN, createProjectAttrCon.getResponseCode());
	}

	/**
	 * Test to validate that a manager is unable to fetch Default Natures for the given Project.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetProjectNatures() throws IOException {
		final HttpURLConnection findProjectNaturesCon = getRequestWithoutBodyParams(
				String.format(PROJECT_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, ONE, TWO) + "/projectNatures",
				getAccessToken());
		assertEquals(SC_FORBIDDEN, findProjectNaturesCon.getResponseCode());
	}

	/**
	 * Test to validate that a manager is unable to modify the Default Natures of the given Project.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotChangeProjectNatures() throws IOException {
		/* Get project natures for project 1*/
		final String clientOneAdminAccessToken = getAccessTokenForTestRealm("client-1-admin@users.com", PASSWORD);
		final List<ProjectNature> projectNatures = getDefaultNaturesForProject(ONE, ONE, clientOneAdminAccessToken);
		assertEquals(PROJECT_ONE_NATURES_COUNT, projectNatures.size());
		final ProjectNature defaultNatureBeforeChange = projectNatures.get(0);
		assertEquals(ProjectNature.MINING, defaultNatureBeforeChange);

		/* Change project natures for project 1 */
		final HttpURLConnection changeProjectNaturesCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, ONE, ONE) + "?projectNatures=DISCOVERY,DISCOVERY_LIGHT",
				HttpMethod.PUT,
				getAccessToken(), /* This access token belongs to "user2", who is not a client admin and can't access the endpoint. */
				null);
		assertEquals(SC_FORBIDDEN, changeProjectNaturesCon.getResponseCode());

		/* Validate that project natures have NOT changed */
		final List<ProjectNature> projectNaturesAfterChange = getDefaultNaturesForProject(ONE, ONE, clientOneAdminAccessToken);
		assertEquals(PROJECT_ONE_NATURES_COUNT, projectNaturesAfterChange.size());
		assertEquals(defaultNatureBeforeChange, projectNaturesAfterChange.get(0));
	}

	/**
	 * Test to validate that a manager is unable to delete {@link ProjectRole Project Roles} for the given Project.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotDeleteProjectRoles() throws IOException {
		final List<RoleRepresentation> rolesBeforeDeleting = getRolesInTestRealm();
		assertThat(rolesBeforeDeleting, is(not(empty())));
		final int roleCountBeforeDeleting = rolesBeforeDeleting.size();

		final HttpURLConnection deleteProjectAttrCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, ONE, TWO),
				HttpMethod.DELETE,
				getAccessToken(),
				null);
		assertEquals(SC_FORBIDDEN, deleteProjectAttrCon.getResponseCode());

		final List<RoleRepresentation> rolesAfterDeleting = getRolesInTestRealm();
		assertNotNull(rolesAfterDeleting);
		assertEquals(roleCountBeforeDeleting, rolesAfterDeleting.size());
	}

	/**
	 * Test to validate that a manager is unable to delete Client roles for the given Client.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotDeleteClientRoles() throws IOException {
		final List<RoleRepresentation> rolesBeforeDeleting = getRolesInTestRealm();
		assertThat(rolesBeforeDeleting, is(not(empty())));
		final int roleCountBeforeDeleting = rolesBeforeDeleting.size();

		final HttpURLConnection deleteClientAttrCon = makeHttpCallAndGetConnection(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, TWO),
				HttpMethod.DELETE,
				getAccessToken(),
				null);
		assertEquals(SC_FORBIDDEN, deleteClientAttrCon.getResponseCode());

		final List<RoleRepresentation> rolesAfterDeleting = getRolesInTestRealm();
		assertNotNull(rolesAfterDeleting);
		assertEquals(roleCountBeforeDeleting, rolesAfterDeleting.size());
	}

	/**
	 * Test to validate that a manager is unable to add {@link ProjectRole Project Roles} to given {@link Member}.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotAddProjectRoleToMember() throws IOException {
		/* We need to create the Member JSON instead of creating the Member object and converting it to JSON.
		 * We cannot use the constructor provided in the Member class as we do not have
		 * access to the RoleModel to provide the projectRole. */
		final String memberJson = createMemberJsonWithEmail("project-3-manager@users.com", TWO, UserRole.MANAGER, Collections.emptyList());
		final HttpURLConnection addProjectRoleToMemberCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, TWO),
				HttpMethod.POST,
				getAccessToken(),
				memberJson);
		assertEquals(SC_FORBIDDEN, addProjectRoleToMemberCon.getResponseCode());
	}

	/**
	 * Test to validate that a manager is unable to add a {@link Member} as a Client Admin for the given Client.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotAddMemberAsClientAdmin() throws IOException {
		/* Add Project Three Manager as Client Admin for Client 1. */
		/* We need to create the Member JSON instead of creating the Member object and converting it to JSON.
		 * We cannot use the constructor provided in the Member class as we do not have
		 * access to the RoleModel to provide the projectRole. */
		final String memberJson = "{ \"email\": \"project-3-manager@users.com\" }";
		final HttpURLConnection addAdminClientCon = makeHttpCallAndGetConnection(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, ONE) + "/admins",
				HttpMethod.POST,
				getAccessToken(),
				memberJson);
		assertEquals(SC_FORBIDDEN, addAdminClientCon.getResponseCode());
	}

	/**
	 * Test to validate that a manager is unable to delete a {@link Member} as a Client Admin for the specified Client.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotDeleteMemberAsClientAdmin() throws IOException {
		final UserRepresentation clientOneAdmin = getUserFromRealmByUserName("client-1-admin@users.com");

		/* Delete Client One Admin as Client Admin. */
		final HttpURLConnection deleteClientAdminCon = makeHttpCallAndGetConnection(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, ONE) + "/admins/" + clientOneAdmin.getId(),
				HttpMethod.DELETE,
				getAccessToken(),
				null);
		assertEquals(SC_FORBIDDEN, deleteClientAdminCon.getResponseCode());
	}

	/**
	 * Test to validate that a manager is unable to delete a {@link Member} from a Project.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotDeleteMemberFromProject() throws IOException {
		final String clientOneAdminAccessToken = getAccessTokenForTestRealm("client-1-admin@users.com", PASSWORD);
		final PaginatedResponse<Member> projectMembersPage = getProjectMembers(ONE, ONE, clientOneAdminAccessToken);
		final int memberCountBeforeDelete = projectMembersPage.getContentList().size();
		assertEquals(PROJECT_ONE_MEMBER_COUNT, memberCountBeforeDelete);

		final Member memberToDelete = projectMembersPage.getContentList().get(0);
		/* Delete member from Project. */
		final HttpURLConnection deleteProjectMemberCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, ONE) + "/" + memberToDelete.getId(),
				HttpMethod.DELETE,
				getAccessToken(), /* This access token belongs to "Project One Manager", who can't access the endpoint as it is a MANAGER. */
				null);
		assertEquals(SC_FORBIDDEN, deleteProjectMemberCon.getResponseCode());

		/* Get Project Members again, count should be same. */
		final PaginatedResponse<Member> projectMembersPageAfterDelete = getProjectMembers(ONE, ONE, clientOneAdminAccessToken);
		assertEquals(memberCountBeforeDelete, projectMembersPageAfterDelete.getContentList().size());
	}

	/**
	 * Test to validate that a manager is unable to assign {@link ProjectRole Project Roles} to a {@link Member}
	 * if the {@link Member} already has access to the Project.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotAssignProjectRolesToMember() throws IOException {
		final String clientOneAdminAccessToken = getAccessTokenForTestRealm("client-1-admin@users.com", PASSWORD);
		final String projectMembersURL = String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, ONE);
		final PaginatedResponse<Member> projectMembersPage = getProjectMembers(ONE, ONE, clientOneAdminAccessToken);
		assertEquals(PROJECT_ONE_MEMBER_COUNT, projectMembersPage.getContentList().size());
		final List<Member> projectMembers = projectMembersPage.getContentList();
		final Member viewerMember = projectMembers.stream()
		                                          .filter(member -> member.getEmail().equalsIgnoreCase("project-1-viewer@users.com"))
		                                          .findFirst()
		                                          .orElse(null);
		assertNotNull(viewerMember);
		final ProjectRole memberProjectRoleForProjectOne = getProjectRoleFromMember(viewerMember, ONE);
		assertNotNull(memberProjectRoleForProjectOne);
		assertEquals(UserRole.VIEWER, memberProjectRoleForProjectOne.getUserRole());

		/* Change VIEWER to EDITOR */
		final String memberToUpdateJson = createMemberJsonWithID(viewerMember.getId(), ONE, UserRole.EDITOR, Collections.emptyList());
		final HttpURLConnection assingEditorRoleToMemberCon = makeHttpCallAndGetConnection(
				projectMembersURL + "/" + viewerMember.getId(),
				HttpMethod.PUT,
				getAccessToken(), /* This access token belongs to "Project One Manager", who can't access the endpoint as it is a MANAGER. */
				memberToUpdateJson);
		assertEquals(SC_FORBIDDEN, assingEditorRoleToMemberCon.getResponseCode());

		/* Get Project Member to validate the new User Role, it should NOT have changed. */
		final Member updatedMember = getProjectMemberByMemberID(ONE, ONE, viewerMember.getId(), clientOneAdminAccessToken);
		final ProjectRole updatedProjectRole = getProjectRoleFromMember(updatedMember, ONE);
		assertNotNull(updatedProjectRole);
		assertEquals(UserRole.VIEWER, updatedProjectRole.getUserRole());
	}
	
	/**
	 * Test to validate that the given user is unable to find a {@link Member}, when the member does not exist.
	 *
	 * @throws IOException while making HTTP connection
	 */
	@Test
	void testShouldNotGetMemberNotFound() throws IOException {
		final String invalidUserId = "this-is-an-invalid-id";
		final HttpURLConnection memberToBeFound = makeHttpCallAndGetConnection(
				String.format(MEMBER_URL_PATTERN, testRealmName, SPI_ID, invalidUserId),
				HttpMethod.GET,
				getAccessToken(),
				null);
		assertEquals(SC_NOT_FOUND, memberToBeFound.getResponseCode());
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
				getAccessToken(), /* This access token belongs to "Project One Manager", who can't access the project 3 user . */
				null);
		assertEquals(SC_FORBIDDEN, memberToBeFound.getResponseCode());
	}
	

	/**
	 * Test to validate that the given user is able to find a {@link Member} who is client admin of user's project.
	 *
	 * @throws IOException while making HTTP connection
	 */
	@Test
	void testShouldGetClientAdminMember() throws IOException {
		final UserRepresentation clientAdmin = getUserFromRealmByUserName("client-1-admin@users.com");
		final HttpURLConnection memberToBeFound = makeHttpCallAndGetConnection(
				String.format(MEMBER_URL_PATTERN, testRealmName, SPI_ID, clientAdmin.getId()),
				HttpMethod.GET,
				getAccessToken(),
				null);
		assertEquals(HttpStatus.SC_OK, memberToBeFound.getResponseCode());
		final Member fetchedMember = getResult(memberToBeFound, Member.class);
		assertEquals(clientAdmin.getFirstName(), fetchedMember.getFirstName());
		assertEquals(clientAdmin.getLastName(), fetchedMember.getLastName());
	}
	
	/**
	 * Test to validate that the given user is able to find a {@link Member}, when the member does belong to user project.
	 *
	 * @throws IOException while making HTTP connection
	 */
	@Test
	void testShouldGetMember() throws IOException {
		/* Get member that does belong to the Project. */
		final UserRepresentation projectOneViewer = getUserFromRealmByUserName("project-1-viewer@users.com");
		final HttpURLConnection memberToBeFound = makeHttpCallAndGetConnection(
				String.format(MEMBER_URL_PATTERN, testRealmName, SPI_ID, projectOneViewer.getId()),
				HttpMethod.GET,
				getAccessToken(),
				null);
		assertEquals(HttpStatus.SC_OK, memberToBeFound.getResponseCode());
		final Member fetchedMember = getResult(memberToBeFound, Member.class);
		assertEquals(projectOneViewer.getFirstName(), fetchedMember.getFirstName());
		assertEquals(projectOneViewer.getLastName(), fetchedMember.getLastName());
	}

	private String getAccessToken() throws IOException {
		return getAccessTokenForTestRealm(MANAGER_USERNAME, PASSWORD);
	}
}
