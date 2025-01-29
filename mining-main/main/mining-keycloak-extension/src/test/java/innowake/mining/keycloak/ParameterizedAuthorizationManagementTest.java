/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak;

import static innowake.mining.keycloak.model.ProjectNature.DISCOVERY;
import static innowake.mining.keycloak.model.ProjectNature.DISCOVERY_LIGHT;
import static innowake.mining.keycloak.model.ProjectNature.MINING;
import static org.apache.http.HttpStatus.SC_BAD_REQUEST;
import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
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
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.io.IOUtils;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.condition.DisabledIfSystemProperty;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.keycloak.representations.idm.RoleRepresentation;
import org.keycloak.representations.idm.UserRepresentation;
import innowake.mining.keycloak.model.HttpMethod;
import innowake.mining.keycloak.model.Member;
import innowake.mining.keycloak.model.PaginatedResponse;
import innowake.mining.keycloak.model.ProjectNature;
import innowake.mining.keycloak.model.ProjectRole;
import innowake.mining.keycloak.model.UserRole;

/**
 * {@link ParameterizedTest Parameterized Tests} to validate the authentication and authorization
 * for <i>Admins</i> and <i>Client Admins</i>, and to test the extended <i>Keycloak</i> endpoints. 
 */
@DisabledIfSystemProperty(named = AbstractTest.SYSTEM_PROPERTY_SKIP_INTEGRATION_TESTS, matches = "true")
class ParameterizedAuthorizationManagementTest extends AbstractTest {

	/**
	 * Test to validate that the given user is able to fetch {@link Member Members} for given Client.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldGetClientMembers(final String userName) throws IOException {
		final PaginatedResponse<Member> clientMembers = getMembersForClient(ONE, getAccessToken(userName));
		assertEquals(CLIENT_ONE_MEMBER_COUNT, clientMembers.getContentList().size());
	}

	/**
	 * Test to validate that the given user is able to paginate over the Client {@link Member Members}.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldGetClientMembersWithPagination(final String userName) throws IOException {
		final PaginatedResponse<Member> clientMembers = getMembersForClientPaginated(ONE, 0, PAGE_SIZE, getAccessToken(userName));
		assertEquals(PAGE_SIZE, clientMembers.getContentList().size());
		assertEquals(CLIENT_ONE_MEMBER_COUNT, clientMembers.getTotalSize());

		final PaginatedResponse<Member> clientMembersPageThree = getMembersForClientPaginated(ONE, 3, PAGE_SIZE, getAccessToken(userName));
		assertEquals(1, clientMembersPageThree.getContentList().size());
		assertEquals(CLIENT_ONE_MEMBER_COUNT, clientMembersPageThree.getTotalSize());
	}

	/**
	 * Test to validate that the given user is able to fetch the Client Member count.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldGetClientMemberCount(final String userName) throws IOException {
		final Integer clientMemberCount = getClientMemberCount(ONE, getAccessToken(userName));
		assertEquals(CLIENT_ONE_MEMBER_COUNT, clientMemberCount);
	}

	/**
	 * Test to validate that the given user is able to fetch the Project {@link Member Members}.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldGetProjectMembers(final String userName) throws IOException {
		final PaginatedResponse<Member> projectMembers = getProjectMembers(ONE, ONE, getAccessToken(userName));
		assertEquals(PROJECT_ONE_MEMBER_COUNT, projectMembers.getContentList().size());
	}

	/**
	 * Test to validate that the given user is able to paginate over the Project {@link Member Members}.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldGetProjectMembersWithPagination(final String userName) throws IOException {
		final PaginatedResponse<Member> projectMembersPageZero = getMembersForProjectPaginated(ONE, ONE, 0, PAGE_SIZE, getAccessToken(userName));
		assertEquals(PAGE_SIZE, projectMembersPageZero.getContentList().size());
		assertEquals(PROJECT_ONE_MEMBER_COUNT, projectMembersPageZero.getTotalSize());

		final PaginatedResponse<Member> projectMembersPageOne = getMembersForProjectPaginated(ONE, ONE, 1, PAGE_SIZE, getAccessToken(userName));
		assertEquals(1, projectMembersPageOne.getContentList().size());
		assertEquals(PROJECT_ONE_MEMBER_COUNT, projectMembersPageOne.getTotalSize());
	}

	/**
	 * Test to validate that the given user is able to get the Project Member count.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldGetProjectMemberCount(final String userName) throws IOException {
		final Integer projectMemberCount = getProjectMemberCount(ONE, ONE, getAccessToken(userName));
		assertEquals(PROJECT_ONE_MEMBER_COUNT, projectMemberCount);
	}

	/**
	 * Test to validate that the given user is able to fetch a member belonging to a specific project by its ID.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldGetMemberById(final String userName) throws IOException {
		final UserRepresentation projectOneManager = getUserFromRealmByUserName("project-1-manager@users.com");
		final Member resultMember = getProjectMemberByMemberID(ONE, ONE, projectOneManager.getId(), getAccessToken(userName));
		assertEquals(projectOneManager.getEmail(), resultMember.getEmail());
		assertEquals(projectOneManager.getFirstName(), resultMember.getFirstName());
		assertEquals(projectOneManager.getLastName(), resultMember.getLastName());
		assertEquals(projectOneManager.getId(), resultMember.getId());
	}

	/**
	 * Test to validate that the given user is not able to fetch a member by its ID because it does not belong to the given Project.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldNotGetMemberByIdNotFound(final String userName) throws IOException {
		final UserRepresentation projectFourManager = getUserFromRealmByUserName("project-4-manager@users.com");
		final HttpURLConnection findMemberByIdCon = getRequestWithoutBodyParams(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, ONE) + "/" + projectFourManager.getId(),
				getAccessToken(userName));
		assertEquals(SC_NOT_FOUND, findMemberByIdCon.getResponseCode());
		final String errorResponse = IOUtils.toString(findMemberByIdCon.getErrorStream());
		assertThat(errorResponse, containsString("No member found"));
	}

	/**
	 * Test to validate that the given user is able to fetch Client Admins for given Client.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldGetClientAdmins(final String userName) throws IOException {
		final PaginatedResponse<Member> clientAdminsPage = getClientAdmins(ONE, getAccessToken(userName));
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdminsPage.getContentList().size());
		final Member clientOneAdmin = clientAdminsPage.getContentList().get(0);
		assertEquals("client-1-admin@users.com", clientOneAdmin.getEmail());
		assertEquals("Client One Admin", clientOneAdmin.getFirstName());
		assertEquals("User", clientOneAdmin.getLastName());
	}

	/**
	 * Test to validate that the given user is able to paginate over Client Admins for given Client.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldGetClientAdminsPaginated(final String userName) throws IOException {
		final PaginatedResponse<Member> clientAdminsPageZero = getClientAdminsPaginated(ONE, 0, PAGE_SIZE, getAccessToken(userName));
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdminsPageZero.getContentList().size());
		final Member clientOneAdmin = clientAdminsPageZero.getContentList().get(0);
		assertEquals("client-1-admin@users.com", clientOneAdmin.getEmail());
		assertEquals("Client One Admin", clientOneAdmin.getFirstName());
		assertEquals("User", clientOneAdmin.getLastName());

		final PaginatedResponse<Member> clientAdminsPageOne = getClientAdminsPaginated(ONE, 1, PAGE_SIZE, getAccessToken(userName));
		assertNotNull(clientAdminsPageOne);
		/* Page should not have any content as there are not enough Client Admins. */
		assertThat(clientAdminsPageOne.getContentList(), is(empty()));
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdminsPageOne.getTotalSize());
	}

	/**
	 * Test to validate that the given user is able to create {@link ProjectRole Project Roles}.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldCreateProjectRoles(final String userName) throws IOException {
		final List<RoleRepresentation> rolesBeforeCreation = getRolesInTestRealm();
		assertThat(rolesBeforeCreation, is(not(empty())));

		final Integer newProjectId = Integer.valueOf(5);
		createProjectRoles(ONE, newProjectId, Arrays.asList(MINING, DISCOVERY), getAccessToken(userName));

		final List<RoleRepresentation> rolesAfterCreation = getRolesInTestRealm();
		assertNotNull(rolesAfterCreation);
		/* 7 additional roles that were created for the new project. */
		assertEquals(PROJECT_ROLE_COUNT, rolesAfterCreation.size() - rolesBeforeCreation.size());
		/* Ensure that the new project roles were created. */
		final long newProjectRoleCount = rolesAfterCreation.stream()
		                                                   .filter(role -> role.getName().startsWith("client-1-project-" + newProjectId))
		                                                   .count();
		assertEquals(PROJECT_ROLE_COUNT, newProjectRoleCount);
	}

	/**
	 * Test to validate that the given user is able to fetch Default Natures for the given Project.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldGetProjectNatures(final String userName) throws IOException {
		final List<ProjectNature> projectNatures = getDefaultNaturesForProject(ONE, TWO, getAccessToken(userName));
		assertEquals(PROJECT_TWO_NATURES_COUNT, projectNatures.size());
		assertThat(projectNatures, containsInAnyOrder(DISCOVERY, MINING));
	}

	/**
	 * Test to validate that the given user is able to modify the Default Natures of the given Project.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldChangeProjectNatures(final String userName) throws IOException {
		/* Get project natures for project 1*/
		final List<ProjectNature> projectNatures = getDefaultNaturesForProject(ONE, ONE, getAccessToken(userName));
		assertEquals(PROJECT_ONE_NATURES_COUNT, projectNatures.size());
		assertEquals(MINING, projectNatures.get(0));
		final UserRepresentation projectOneManager = getUserFromRealmByUserName("project-1-manager@users.com");
		/* User that belongs to this project should have only client-1-project-1-mining as the Project Nature role. */
		final Member memberBeforeChange = getProjectMemberByMemberID(ONE, ONE, projectOneManager.getId(), getAccessToken(userName));
		final ProjectRole memberProjectRoleBeforeChange = getProjectRoleFromMember(memberBeforeChange, ONE);
		assertEquals(PROJECT_ONE_NATURES_COUNT, memberProjectRoleBeforeChange.getProjectNatures().size());
		final ProjectNature memberProjectRoleNature = memberProjectRoleBeforeChange.getProjectNatures().get(0);
		assertEquals(MINING, memberProjectRoleNature);

		/* Change project natures for project 1 */
		changeProjectNatures(ONE, ONE, Arrays.asList(DISCOVERY, DISCOVERY_LIGHT), getAccessToken(userName));

		/* Validate that project natures have changed */
		final List<ProjectNature> projectNaturesAfterChange = getDefaultNaturesForProject(ONE, ONE, getAccessToken(userName));
		assertEquals(PROJECT_ONE_NATURES_COUNT + 1, projectNaturesAfterChange.size());
		assertThat(projectNaturesAfterChange, containsInAnyOrder(DISCOVERY, DISCOVERY_LIGHT));

		/* Validate that default project nature roles were changed for the User */
		final Member memberAfterChange = getProjectMemberByMemberID(ONE, ONE, projectOneManager.getId(), getAccessToken(userName));
		final ProjectRole memberProjectRoleAfterChange = getProjectRoleFromMember(memberAfterChange, ONE);
		assertNotNull(memberProjectRoleAfterChange);
		assertEquals(PROJECT_ONE_NATURES_COUNT + 1, memberProjectRoleAfterChange.getProjectNatures().size());
		assertThat(memberProjectRoleAfterChange.getProjectNatures(), containsInAnyOrder(DISCOVERY, DISCOVERY_LIGHT));
	}

	/**
	 * Test to validate that the given user is able to delete {@link ProjectRole Project Roles} for the given Project.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldDeleteProjectRoles(final String userName) throws IOException {
		final List<RoleRepresentation> rolesBeforeDeleting = getRolesInTestRealm();
		assertThat(rolesBeforeDeleting, is(not(empty())));

		deleteProject(ONE, TWO, getAccessToken(userName));

		final List<RoleRepresentation> rolesAfterDeleting = getRolesInTestRealm();
		assertNotNull(rolesAfterDeleting);
		/* 7 roles that were deleted. */
		assertEquals(PROJECT_ROLE_COUNT, rolesBeforeDeleting.size() - rolesAfterDeleting.size());
		/* Ensure that the project roles for Project 2 were deleted. */
		assertTrue(rolesAfterDeleting.stream().noneMatch(role -> role.getName().startsWith("client-1-project-2")));
	}

	/**
	 * Test to validate that the given user is able to add {@link ProjectRole Project Roles} to given {@link Member}.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldAddProjectRoleToMember(final String userName) throws IOException {
		/* Get Project Three Manager to get the ID. */
		final UserRepresentation projectThreeManager = getUserFromRealmByUserName("project-3-manager@users.com");

		/* Member should not have access to Project 2. */
		final String projectTwoMembersURL = String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, TWO);
		final HttpURLConnection findMemberByIdUserTenCon = getRequestWithoutBodyParams(
				projectTwoMembersURL + "/" + projectThreeManager.getId(),
				getAccessToken(userName));
		assertEquals(SC_NOT_FOUND, findMemberByIdUserTenCon.getResponseCode());

		addProjectRoleToMember(ONE, TWO, "project-3-manager@users.com", UserRole.MANAGER, Collections.emptyList(), getAccessToken(userName));

		/* Get Project Three Manager again, it should now have the proper Project Roles. */
		final Member memberWithProjectRoles = getProjectMemberByMemberID(ONE, TWO, projectThreeManager.getId(), getAccessToken(userName));
		final ProjectRole projectTwoRoleForMember = getProjectRoleFromMember(memberWithProjectRoles, TWO);
		assertNotNull(projectTwoRoleForMember);
		assertEquals(UserRole.MANAGER, projectTwoRoleForMember.getUserRole());
		assertEquals(PROJECT_TWO_NATURES_COUNT, projectTwoRoleForMember.getProjectNatures().size());
		assertThat(projectTwoRoleForMember.getProjectNatures(), containsInAnyOrder(DISCOVERY, MINING));
	}

	/**
	 * Test to validate that the given user is unable to add Project Roles to given Member
	 * if the Member's Email ID is empty, or if the {@link ProjectRole Project Role} is not given.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldNotAddProjectRoleToMemberBadRequest(final String userName) throws IOException {
		/* Get Project Three Manager to get the ID. */
		final UserRepresentation projectThreeViewer = getUserFromRealmByUserName("project-3-viewer@users.com");

		/* Member should not have access to Project 2. */
		final String projectTwoMembersURL = String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, TWO);
		final String findProjectThreeViewerInProjectTwoURL = projectTwoMembersURL + "/" + projectThreeViewer.getId();
		final HttpURLConnection findMemberByIdUserTenCon = getRequestWithoutBodyParams(
				findProjectThreeViewerInProjectTwoURL,
				getAccessToken(userName));
		assertEquals(SC_NOT_FOUND, findMemberByIdUserTenCon.getResponseCode());

		/* We need to create the Member JSON instead of creating the Member object and converting it to JSON.
		 * We cannot use the constructor provided in the Member class as we do not have
		 * access to the RoleModel to provide the projectRole.
		 * 
		 *  The following JSON is a bad request as the "email" is empty. */
		final String memberJsonEmptyEmail = createMemberJsonWithEmail("", THREE, UserRole.MANAGER, Collections.emptyList());
		final HttpURLConnection addProjectRoleToMemberEmptyEmailCon = makeHttpCallAndGetConnection(
				projectTwoMembersURL,
				HttpMethod.POST,
				getAccessToken(userName),
				memberJsonEmptyEmail);
		assertEquals(SC_BAD_REQUEST, addProjectRoleToMemberEmptyEmailCon.getResponseCode());

		/*  The following JSON is a bad request as the "projectRoles" are empty. */
		final String memberJsonEmptyProjectRole = createMemberJsonWithEmail("project-3-viewer@users.com", null, null, null);
		final HttpURLConnection addProjectRoleToMemberEmptyProjectRolesCon = makeHttpCallAndGetConnection(
				projectTwoMembersURL,
				HttpMethod.POST,
				getAccessToken(userName),
				memberJsonEmptyProjectRole);
		assertEquals(SC_BAD_REQUEST, addProjectRoleToMemberEmptyProjectRolesCon.getResponseCode());

		/* Get Project Three Manager again, it should return NotFound for Project 2 because no Project Roles were granted. */
		final HttpURLConnection findMemberByIdUserTenNotFoundCon = getRequestWithoutBodyParams(
				findProjectThreeViewerInProjectTwoURL,
				getAccessToken(userName));
		assertEquals(SC_NOT_FOUND, findMemberByIdUserTenNotFoundCon.getResponseCode());
		final String errorResponse = IOUtils.toString(findMemberByIdUserTenNotFoundCon.getErrorStream());
		assertThat(errorResponse, containsString("No member found"));
	}

	/**
	 * Test to validate that the given user can add {@link ProjectRole Project Roles} to the given {@link Member}.
	 * If any additional {@link ProjectNature Project Nature} has been specified for the {@link Member}, that should be assigned
	 * to the Member along with the Project's Default Natures.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldAddProjectRoleAdditionalProjectNatures(final String userName) throws IOException {
		/* Get Project Three Viewer to get the ID. */
		final UserRepresentation projectThreeViewer = getUserFromRealmByUserName("project-3-viewer@users.com");

		/* Member should not have access to Project 2. */
		final String projectTwoMembersURL = String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, TWO);
		final HttpURLConnection findMemberByIdUserTenCon = getRequestWithoutBodyParams(
				projectTwoMembersURL + "/" + projectThreeViewer.getId(),
				getAccessToken(userName));
		assertEquals(SC_NOT_FOUND, findMemberByIdUserTenCon.getResponseCode());

		addProjectRoleToMember(
				ONE,
				TWO,
				"project-3-viewer@users.com",
				UserRole.MANAGER,
				Arrays.asList(DISCOVERY_LIGHT),
				getAccessToken(userName));

		/* Get Project Three Viewer again, it should now have the proper Project Roles. */
		final Member memberWithProjectRoles = getProjectMemberByMemberID(ONE, TWO, projectThreeViewer.getId(), getAccessToken(userName));
		final ProjectRole projectTwoRoleForMember = getProjectRoleFromMember(memberWithProjectRoles, TWO);
		assertNotNull(projectTwoRoleForMember);
		assertEquals(UserRole.MANAGER, projectTwoRoleForMember.getUserRole());
		assertThat(projectTwoRoleForMember.getProjectNatures(), containsInAnyOrder(DISCOVERY, DISCOVERY_LIGHT, MINING));
	}

	/**
	 * Test to validate that the given user is able to create a new user and assign {@link ProjectRole Project Roles}
	 * to them for the given Project.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldAddProjectRoleCreateUser(final String userName) throws IOException {
		final String newUserEmail = "newUser@users.com";
		/* Try to fetch new user that does not exist. */
		assertUserDoesNotExist(newUserEmail);

		final Member newMember = addProjectRoleToMember(
				ONE,
				TWO,
				newUserEmail,
				UserRole.MANAGER,
				Arrays.asList(DISCOVERY_LIGHT),
				getAccessToken(userName));

		/* Get newUser again, it should now have the proper Project Roles. */
		final Member memberWithProjectRoles = getProjectMemberByMemberID(ONE, TWO, newMember.getId(), getAccessToken(userName));
		final ProjectRole projectTwoRoleForMember = getProjectRoleFromMember(memberWithProjectRoles, TWO);
		assertNotNull(projectTwoRoleForMember);
		assertEquals(UserRole.MANAGER, projectTwoRoleForMember.getUserRole());
		assertEquals(PROJECT_TWO_NATURES_COUNT + 1, projectTwoRoleForMember.getProjectNatures().size());
		assertThat(projectTwoRoleForMember.getProjectNatures(), containsInAnyOrder(DISCOVERY, DISCOVERY_LIGHT, MINING));
	}

	/**
	 * Test to validate that the given user is able to add a {@link Member} as a Client Admin for the given Client.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldAddMemberAsClientAdmin(final String userName) throws IOException {
		final String projectThreeManager = "project-3-manager@users.com";
		/* Get Client Admins for Client 1. Should have only one admin - Client One Admin. */
		final PaginatedResponse<Member> clientAdminsPage = getClientAdmins(ONE, getAccessToken(userName));
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdminsPage.getContentList().size());
		final Member clientOneAdmin = clientAdminsPage.getContentList().get(0);
		assertEquals("client-1-admin@users.com", clientOneAdmin.getEmail());
		assertEquals("Client One Admin", clientOneAdmin.getFirstName());
		assertEquals("User", clientOneAdmin.getLastName());

		/* Add Project Three Manager as Client Admin for Client 1. */
		addMemberAsClientAdmin(ONE, projectThreeManager, getAccessToken(userName));
		final PaginatedResponse<Member> clientAdminsWithNewAdminPage = getClientAdmins(ONE, getAccessToken(userName));
		final List<Member> clientAdminsWithNewAdmin = clientAdminsWithNewAdminPage.getContentList();
		assertEquals(CLIENT_ONE_ADMIN_COUNT + 1, clientAdminsWithNewAdmin.size());
		assertTrue(clientAdminsWithNewAdmin.stream().map(Member::getEmail).anyMatch(email -> email.equalsIgnoreCase(projectThreeManager)));
	}

	/**
	 * Test to validate that the given user is able to create a new user and add it as a Client Admin for the given Client.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldAddMemberAsClientAdminCreateUser(final String userName) throws IOException {
		final String newUserEmail = "newuser@users.com";
		/* Get Client Admins for Client 1. Should have only one admin - User0. */
		final PaginatedResponse<Member> clientAdminsPage = getClientAdmins(ONE, getAccessToken(userName));
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdminsPage.getContentList().size());
		final Member clientOneAdmin = clientAdminsPage.getContentList().get(0);
		assertEquals("client-1-admin@users.com", clientOneAdmin.getEmail());
		assertEquals("Client One Admin", clientOneAdmin.getFirstName());
		assertEquals("User", clientOneAdmin.getLastName());

		/* Add new user as Client Admin for Client 1. */
		addMemberAsClientAdmin(ONE, newUserEmail, getAccessToken(userName));
		final PaginatedResponse<Member> clientAdminsWithNewAdminPage = getClientAdmins(ONE, getAccessToken(userName));
		final List<Member> clientAdminsWithNewAdmin = clientAdminsWithNewAdminPage.getContentList();
		assertEquals(CLIENT_ONE_ADMIN_COUNT + 1, clientAdminsWithNewAdmin.size());
		assertTrue(clientAdminsWithNewAdmin.stream().map(Member::getEmail).anyMatch(email -> email.equalsIgnoreCase(newUserEmail)));
	}

	/**
	 * Test to validate that the given user is not able to add a {@link Member} as a Client Admin if the Email ID has not been specified.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldNotAddMemberAsClientAdminBadRequest(final String userName) throws IOException {
		/* Get Client Admins for Client 1. Should have only one admin - User0. */
		final PaginatedResponse<Member> clientAdminsPage = getClientAdmins(ONE, getAccessToken(userName));
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdminsPage.getContentList().size());
		final Member clientOneAdmin = clientAdminsPage.getContentList().get(0);
		assertEquals("client-1-admin@users.com", clientOneAdmin.getEmail());
		assertEquals("Client One Admin", clientOneAdmin.getFirstName());
		assertEquals("User", clientOneAdmin.getLastName());

		/* Add User10 as Client Admin for Client 1. */
		/* We need to create the Member JSON instead of creating the Member object and converting it to JSON.
		 * We cannot use the constructor provided in the Member class as we do not have
		 * access to the RoleModel to provide the projectRole.
		 * 
		 * The following JSON is a Bad Request as the email is empty. */
		final String memberJson = "{ \"email\": \"\" }";
		final HttpURLConnection addAdminClientCon = makeHttpCallAndGetConnection(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, ONE) + "/admins",
				HttpMethod.POST,
				getAccessToken(userName),
				memberJson);
		assertEquals(SC_BAD_REQUEST, addAdminClientCon.getResponseCode());
		final PaginatedResponse<Member> clientAdminsWithNewAdminPage = getClientAdmins(ONE, getAccessToken(userName));
		final List<Member> clientAdmins = clientAdminsWithNewAdminPage.getContentList();
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdmins.size());
		assertEquals("client-1-admin@users.com", clientAdmins.get(0).getEmail());
	}

	/**
	 * Test to validate that the given user is able to delete a {@link Member} as a Client Admin for the specified Client.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldDeleteMemberAsClientAdmin(final String userName) throws IOException {
		/* Get Project Three Manager and add it as a Client Admin for Client 1. */
		final UserRepresentation projectThreeManager = getUserFromRealmByUserName("project-3-manager@users.com");
		addMemberAsClientAdmin(ONE, projectThreeManager.getEmail(), getAccessToken(userName));

		/* Get Client Admins for Client 1. Should have two Client Admins - "Client One Admin" and "Project Three Manager". */
		final PaginatedResponse<Member> clientAdminsPage = getClientAdmins(ONE, getAccessToken(userName));
		final List<Member> clientAdmins = clientAdminsPage.getContentList();
		assertEquals(CLIENT_ONE_ADMIN_COUNT + 1, clientAdmins.size());
		assertTrue(clientAdmins.stream().map(Member::getEmail).anyMatch(email -> email.equals("client-1-admin@users.com")));
		assertTrue(clientAdmins.stream().map(Member::getEmail).anyMatch(email -> email.equals("project-3-manager@users.com")));

		/* Delete Project Three Manager as Client Admin. */
		deleteClientAdminByMemberId(ONE, projectThreeManager.getId(), getAccessToken(userName));

		/* Get Client Admins again, list should have only one Client Admin. */
		final PaginatedResponse<Member> clientAdminsAfterDeletePage = getClientAdmins(ONE, getAccessToken(userName));
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdminsAfterDeletePage.getContentList().size());
		assertTrue(clientAdminsAfterDeletePage.getContentList().get(0).getEmail().equalsIgnoreCase("client-1-admin@users.com"));
	}

	/**
	 * Test to validate that the given user is not able to delete a {@link Member} as a Client Admin if the {@link Member} does not exist
	 * or if its ID is invalid.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldNotDeleteMemberAsClientAdminUserNotFound(final String userName) throws IOException {
		/* Get Client Admins for Client 1. Should have only one admin - "Client One Admin". */
		final PaginatedResponse<Member> clientAdminsPage = getClientAdmins(ONE, getAccessToken(userName));
		final int clientAdminsBeforeDeletion = clientAdminsPage.getContentList().size();
		assertEquals(CLIENT_ONE_ADMIN_COUNT, clientAdminsBeforeDeletion);
		final Member clientOneAdmin = clientAdminsPage.getContentList().get(0);
		assertEquals("client-1-admin@users.com", clientOneAdmin.getEmail());
		assertEquals("Client One Admin", clientOneAdmin.getFirstName());
		assertEquals("User", clientOneAdmin.getLastName());

		/* Attempt to delete Invalid User as Client Admin. */
		final String invalidUserId = "this-is-an-invalid-id";
		final HttpURLConnection deleteClientAdminCon = makeHttpCallAndGetConnection(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, ONE) + "/admins/" + invalidUserId,
				HttpMethod.DELETE,
				getAccessToken(userName),
				null);
		assertEquals(SC_NOT_FOUND, deleteClientAdminCon.getResponseCode());

		/* Get Client Admins again, list should be unchanged. */
		final PaginatedResponse<Member> clientAdminsAfterDeletePage = getClientAdmins(ONE, getAccessToken(userName));
		assertEquals(clientAdminsBeforeDeletion, clientAdminsAfterDeletePage.getContentList().size());
	}

	/**
	 * Test to validate that the given user is able to delete a {@link Member} from a Project.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldDeleteMemberFromProject(final String userName) throws IOException {
		final PaginatedResponse<Member> projectMembersPage = getProjectMembers(ONE, ONE, getAccessToken(userName));
		assertEquals(PROJECT_ONE_MEMBER_COUNT, projectMembersPage.getContentList().size());

		final Member memberToDelete = projectMembersPage.getContentList().get(0);
		/* Delete member from Project. */
		deleteProjectMemberById(ONE, ONE, memberToDelete.getId(), getAccessToken(userName));

		/* Get Project Members again, should not include deleted member. */
		final PaginatedResponse<Member> projectMembersPageAfterDelete = getProjectMembers(ONE, ONE, getAccessToken(userName));
		assertEquals(PROJECT_ONE_MEMBER_COUNT - 1, projectMembersPageAfterDelete.getContentList().size());
		assertTrue(projectMembersPageAfterDelete.getContentList().stream()
		                                                         .map(Member::getEmail)
		                                                         .noneMatch(email -> email.equalsIgnoreCase(memberToDelete.getEmail())));
	}

	/**
	 * Test to validate that the given user is unable to delete a {@link Member} from the Project if
	 * the {@link Member} does not belong to the Project.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldNotDeleteMemberFromProjectBadRequest(final String userName) throws IOException {
		final PaginatedResponse<Member> projectMembersPage = getProjectMembers(ONE, ONE, getAccessToken(userName));
		final int memberCountBeforeDeletion = projectMembersPage.getContentList().size();
		assertEquals(PROJECT_ONE_MEMBER_COUNT, memberCountBeforeDeletion);

		/* Get member that does not belong to the Project. */
		final UserRepresentation projectThreeManager = getUserFromRealmByUserName("project-3-manager@users.com");

		/* Delete member from Project. */
		final HttpURLConnection deleteProjectMemberCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, ONE) + "/" + projectThreeManager.getId(),
				HttpMethod.DELETE,
				getAccessToken(userName),
				null);
		assertEquals(SC_BAD_REQUEST, deleteProjectMemberCon.getResponseCode());
		final String deleteResponse = IOUtils.toString(deleteProjectMemberCon.getErrorStream());
		assertThat(deleteResponse, containsString("Given user is not assigned to the Project"));

		/* Get Project Members again, count should remain the same. */
		final PaginatedResponse<Member> projectMembersPageAfterDelete = getProjectMembers(ONE, ONE, getAccessToken(userName));
		assertEquals(memberCountBeforeDeletion, projectMembersPageAfterDelete.getContentList().size());
	}

	/**
	 * Test to validate that the given user is able to assign {@link ProjectRole Project Roles} to a {@link Member}
	 * if the {@link Member} already has access to the Project.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldAssignProjectRolesToMember(final String userName) throws IOException {
		final PaginatedResponse<Member> projectMembersPage = getProjectMembers(ONE, ONE, getAccessToken(userName));
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
		assignUserRoleToMember(ONE, ONE, viewerMember.getId(), UserRole.EDITOR, getAccessToken(userName));

		/* Get Project Member to validate the new User Role. */
		final Member updatedMember = getProjectMemberByMemberID(ONE, ONE, viewerMember.getId(), getAccessToken(userName));
		final ProjectRole updatedProjectRole = getProjectRoleFromMember(updatedMember, ONE);
		assertNotNull(updatedProjectRole);
		assertEquals(UserRole.EDITOR, updatedProjectRole.getUserRole());
	}

	/**
	 * Test to validate that the given user is unable to assign {@link ProjectRole Project Roles} to a {@link Member}
	 * if it didn't already have access to the Project.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldNotAssignProjectRolesToMemberUserNotFound(final String userName) throws IOException {
		final String invalidUserId = "this-is-an-invalid-id";
		final String memberToUpdateJson = createMemberJsonWithID(invalidUserId, ONE, UserRole.EDITOR, Collections.emptyList());
		final HttpURLConnection assingEditorRoleToMemberCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, ONE) + "/" + invalidUserId,
				HttpMethod.PUT,
				getAccessToken(userName),
				memberToUpdateJson);
		assertEquals(SC_NOT_FOUND, assingEditorRoleToMemberCon.getResponseCode());
	}

	/**
	 * Test to validate that the given user is unable to assign {@link ProjectRole Project Roles} to a {@link Member}
	 * if the proper {@link ProjectRole Project Role} has not been provided.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldNotAssignProjectRolesToMemberBadRequest(final String userName) throws IOException {
		/* Get member that does not belong to the Project. */
		final UserRepresentation projectThreeManager = getUserFromRealmByUserName("project-2-manager@users.com");
		final Member resultMember = getProjectMemberByMemberID(ONE, TWO, projectThreeManager.getId(), getAccessToken(userName));
		final String memberToUpdateJson = MAPPER.writeValueAsString(resultMember);
		final HttpURLConnection assingEditorRoleToMemberCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, ONE, ONE) + "/" + resultMember.getId(),
				HttpMethod.PUT,
				getAccessToken(userName),
				memberToUpdateJson);
		assertEquals(SC_BAD_REQUEST, assingEditorRoleToMemberCon.getResponseCode());
		final String errorResponse = IOUtils.toString(assingEditorRoleToMemberCon.getErrorStream());
		assertThat(errorResponse, containsString("No project role found for project id :1"));
	}
	
	/**
	 * Test to validate that the given user is able to find a {@link Member}, when the member does belong to user project via admin access.
	 *
	 * @param userName The user to validate the functionality with
	 * @throws IOException while making HTTP connection
	 */
	@ParameterizedTest
	@MethodSource("userNameProvider")
	void testShouldGetMember(final String userName) throws IOException {
		/* Get member that does belong to the Project with admin access. */
		final UserRepresentation projectOneViewer = getUserFromRealmByUserName("project-1-viewer@users.com");
		final HttpURLConnection memberToBeFound = makeHttpCallAndGetConnection(
				String.format(MEMBER_URL_PATTERN, testRealmName, SPI_ID, projectOneViewer.getId()),
				HttpMethod.GET,
				getAccessToken(userName),
				null);
		assertEquals(HttpStatus.SC_OK, memberToBeFound.getResponseCode());
		final Member fecthedMember = getResult(memberToBeFound, Member.class);
		assertEquals(projectOneViewer.getFirstName(), fecthedMember.getFirstName());
		assertEquals(projectOneViewer.getLastName(), fecthedMember.getLastName());
	}

	private Integer getClientMemberCount(final Integer clientId, final String accessToken) throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(CLIENT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, clientId) + "/count",
				accessToken);
		assertEquals(HttpStatus.SC_OK, con.getResponseCode());
		return getResult(con);
	}

	private Integer getProjectMemberCount(final Integer clientId, final Integer projectId, final String accessToken) throws IOException {
		final HttpURLConnection con = getRequestWithoutBodyParams(
				String.format(PROJECT_MEMBERS_URL_PATTERN, testRealmName, SPI_ID, clientId, projectId) + "/count",
				accessToken);
		assertEquals(HttpStatus.SC_OK, con.getResponseCode());
		return getResult(con);
	}

	private void createProjectRoles(
			final Integer clientId,
			final Integer projectId,
			final List<ProjectNature> projectNatures,
			final String accessToken) throws IOException {
		final String projectNatureParams = projectNatures.stream().map(ProjectNature::name).collect(Collectors.joining(","));
		final HttpURLConnection createProjectAttrCon = makeHttpCallAndGetConnection(
				String.format(PROJECT_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, clientId, projectId) + "?projectNatures=" + projectNatureParams,
				HttpMethod.POST,
				accessToken,
				null);
		assertEquals(HttpStatus.SC_CREATED, createProjectAttrCon.getResponseCode());
	}

	private String getAccessToken(final String userName) throws IOException {
		return getAccessTokenForTestRealm(userName, PASSWORD);
	}

	private static Stream<Arguments> userNameProvider() {
		return Stream.of(
				Arguments.of("client-1-admin@users.com"),
				Arguments.of("admin@users.com")
				);
	}
}
