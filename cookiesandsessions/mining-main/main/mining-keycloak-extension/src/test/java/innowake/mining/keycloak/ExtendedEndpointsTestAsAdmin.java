/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak;

import static org.apache.http.HttpStatus.SC_BAD_REQUEST;
import static org.apache.http.HttpStatus.SC_CREATED;
import static org.apache.http.HttpStatus.SC_NO_CONTENT;
import static org.apache.http.HttpStatus.SC_OK;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.List;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledIfSystemProperty;
import org.keycloak.representations.idm.RoleRepresentation;
import innowake.mining.keycloak.model.HttpMethod;

/**
 * Test class to validate the functionality of the extended Keycloak endpoints.<br/>
 * The tests are validated with an <i>"admin"</i> user.
 */
@DisabledIfSystemProperty(named = AbstractTest.SYSTEM_PROPERTY_SKIP_INTEGRATION_TESTS, matches = "true")
class ExtendedEndpointsTestAsAdmin extends AbstractTest {
	
	private static final String ADMIN_USERNAME = "admin@users.com";
	private static final Integer NON_EXISTING_ID = Integer.valueOf(123);

	/**
	 * Test to validate that an admin is unable to fetch Client Admins for given Invalid Client ID.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldNotGetClientAdminsInvalidClientID() throws IOException {
		final HttpURLConnection connection = getRequestWithoutBodyParams(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, THREE) + "/admins",
				getAccessToken());
		assertEquals(SC_BAD_REQUEST, connection.getResponseCode());
		final String response = IOUtils.toString(connection.getErrorStream());
		assertThat(response, containsString("Invalid Role: client-" + THREE + "-admin"));
	}

	/**
	 * Test to validate that an admin is able to create Client roles.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldCreateClientAttributes() throws IOException {
		final Integer groupCountBeforeCreatingClient = getGroupCountInTestRealm();
		assertEquals(GROUP_COUNT, groupCountBeforeCreatingClient);

		final Integer newClientId = THREE;
		createClient(newClientId);

		/* Validate that the new Client Users Group was created. */
		final Integer groupCountAfterCreatingClient = getGroupCountInTestRealm();
		assertEquals(GROUP_COUNT.intValue() + 1, groupCountAfterCreatingClient);

		/* Validate that the new Client Admin role was created. */
		final String newClientAdminRoleName = "client-" + newClientId + "-admin";
		final RoleRepresentation newClientAdminRole = getRoleByNameFromTestRealm(newClientAdminRoleName);
		assertEquals(newClientAdminRoleName, newClientAdminRole.getName());

		/* Validate that the new client-3-roles container role was created. */
		final String clientRolesContainerRole = "client-" + newClientId + "-roles";
		final RoleRepresentation newClientRolesContainerRole = getRoleByNameFromTestRealm(clientRolesContainerRole);
		assertEquals(clientRolesContainerRole, newClientRolesContainerRole.getName());
	}

	/**
	 * Test to validate that an admin is able to check if a client exists.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testClientShouldExist() throws IOException {
		/* Test that existing client exists */
		assertClientExists(ONE, true);

		/* Test that non-existing client does not exist */
		final Integer clientId = NON_EXISTING_ID;
		assertClientExists(clientId, false);

		/* Create new client */
		createClient(clientId);

		/* Test that client exists now */
		assertClientExists(clientId, true);

		/* Delete new client */
		deleteClient(clientId, getAccessToken());

		/* Test that client no longer exists */
		assertClientExists(clientId, false);
	}

	/**
	 * Test to validate that an admin is able to check if a project exists.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testProjectShouldExist() throws IOException {
		/* Test that existing project exists */
		final Integer clientId = ONE;
		assertProjectExists(clientId, ONE, true);

		/* Test that non-existing project does not exist */
		final Integer newProjectId = NON_EXISTING_ID;
		assertProjectExists(clientId, newProjectId, false);

		/* Create new project */
		final HttpURLConnection getProjectNew = makeHttpCallAndGetConnection(
				String.format(PROJECT_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, clientId, newProjectId) + "?projectNatures=MINING",
				HttpMethod.POST,
				getAccessToken(),
				null);
		assertEquals(SC_CREATED, getProjectNew.getResponseCode());

		/* Test that client project now exists */
		assertProjectExists(clientId, newProjectId, true);

		/* Delete new project */
		deleteProject(clientId, newProjectId, getAccessToken());

		/* Test that project no longer exists */
		assertProjectExists(clientId, newProjectId, false);
	}

	/**
	 * Test to validate that an admin is able to delete Client roles for the given Client.
	 *
	 * @throws IOException while making the HTTP Connection
	 */
	@Test
	void testShouldDeleteClientRoles() throws IOException {
		final List<RoleRepresentation> rolesBeforeDeleting = getRolesInTestRealm();
		assertThat(rolesBeforeDeleting, is(not(empty())));

		deleteClient(TWO, getAccessToken());

		final List<RoleRepresentation> rolesAfterDeleting = getRolesInTestRealm();
		assertNotNull(rolesAfterDeleting);
		/* 16 additional roles that were deleted for the new client. */
		final int totalNumberOfRolesPerClient = 16;
		assertEquals(totalNumberOfRolesPerClient, rolesBeforeDeleting.size() - rolesAfterDeleting.size());
		/* Ensure that the client roles were deleted. */
		assertTrue(rolesAfterDeleting.stream().noneMatch(role -> role.getName().startsWith("client-2")));
	}

	private String getAccessToken() throws IOException {
		return getAccessTokenForTestRealm(ADMIN_USERNAME, PASSWORD);
	}

	private void assertClientExists(final Integer clientId, final boolean exists) throws IOException {
		final HttpURLConnection connection = getRequestWithoutBodyParams(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, clientId),
				getAccessToken());
		assertEquals(SC_OK, connection.getResponseCode());
		assertEquals(Boolean.valueOf(exists), getResult(connection));
	}

	private void assertProjectExists(final Integer clientId, final Integer projectId, final boolean exists) throws IOException {
		final HttpURLConnection connection = getRequestWithoutBodyParams(
				String.format(PROJECT_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, clientId, projectId),
				getAccessToken());
		assertEquals(SC_OK, connection.getResponseCode());
		assertEquals(Boolean.valueOf(exists), getResult(connection));
	}

	private void createClient(final Integer clientId) throws IOException {
		final HttpURLConnection connection = makeHttpCallAndGetConnection(
				String.format(CLIENTS_WITH_ID_URL_PATTERN, testRealmName, SPI_ID, clientId),
				HttpMethod.POST,
				getAccessToken(),
				null);
		assertEquals(SC_NO_CONTENT, connection.getResponseCode());
	}
}
