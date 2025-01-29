/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.server.JobTestHelper.waitForJobCompletion;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willDoNothing;
import static org.mockito.BDDMockito.willThrow;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.Instant;
import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import javax.persistence.EntityNotFoundException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.server.ResponseStatusException;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.JobTestHelper;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.config.security.MiningRole;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.job.deletion.BackgroundDeletionJob;
import innowake.mining.server.properties.KeycloakApplicationConfiguration;
import innowake.mining.server.service.KeycloakAuthorizationManagementService;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ClientPojoPrototype;
import innowake.mining.shared.security.RoleType;
import innowake.mining.tags.AuthorizationTest;

/**
 * Tests to validate CRUD operations on Clients in IAM Profile.
 */
@ActiveProfiles(value = Profiles.AUTH_TEST, inheritProfiles = false )
@AutoConfigureMockMvc
@AuthorizationTest
class ClientDbTest extends DatabaseResettingTest {

	@Autowired
	private ClientService clientService;

	@Nullable
	@SpyBean
	private KeycloakAuthorizationManagementService authorizationManagementService;

	@MockBean
	@Nullable
	private KeycloakApplicationConfiguration config;

	@Nullable
	@MockBean
	private RestTemplate keycloakRestTemplate;
	
	@Autowired
	private JobManager jobManager;

	@Autowired
	private MockMvc mvc;

	private static final String MOCK_HOST = "mock-host";
	private static final String MOCK_REALM = "mock-realm";
	private static final Long ZERO = Long.valueOf(0);
	private static final Long ONE = Long.valueOf(1);
	private static final Long TWO = Long.valueOf(2);
	private static final Long INVALID_CLIENT_ID = Long.valueOf(500);

	@BeforeEach
	public void init() {
		AuthorizationTests.setAuthentication(Arrays.asList(new MiningRole(RoleType.ADMIN.getValue())));
		assertNotNull(keycloakRestTemplate);
		final KeycloakApplicationConfiguration config2 = config;
		final KeycloakAuthorizationManagementService authMgmtService = authorizationManagementService;
		if (config2 != null && authMgmtService != null) {
			given(config2.getAuthServerUrl()).willReturn(MOCK_HOST);
			given(config2.getRealm()).willReturn(MOCK_REALM);
			authMgmtService.createKeycloakURI();
		} else {
			fail("Keycloak was not initialized properly.");
		}
	}

	/**
	 * Test to validate that the beans have been autowired/injected appropriately.
	 */
	@Test
	void testAutowiredNotNull() {
		assertNotNull(config);
		assertNotNull(clientService);
		assertNotNull(authorizationManagementService);
		assertNotNull(keycloakRestTemplate);
	}

	/**
	 * Test to validate that user should be able to create a new Client, along with the Keycloak roles.
	 */
	@Test
	void testShouldCreateClient() {
		final ResponseEntity<Void> mockedResponse = new ResponseEntity<>(HttpStatus.CREATED);
		final HttpEntity<Long> request = new HttpEntity<>(Long.valueOf(3));
		given(assertNotNull(keycloakRestTemplate).postForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/", request, Void.class)).willReturn(mockedResponse);
		final long clientCountBeforeInsertion = clientService.count(null);
		clientService.create("Test Client");
		final long clientCountAfterInsertion = clientService.count(null);
		assertEquals(clientCountBeforeInsertion + 1, clientCountAfterInsertion);
	}

	/**
	 * Test to validate that Client creation should be rolled back when an exception is thrown by {@link KeycloakRestTemplate}.
	 */
	@Test
	void testShouldRollBackClientCreation() {
		final HttpEntity<Long> request = new HttpEntity<>(Long.valueOf(3));
		given(assertNotNull(keycloakRestTemplate).postForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/3", request, Void.class))
			.willThrow(new ResponseStatusException(HttpStatus.FORBIDDEN));
		final long clientCountBeforeInsertion = clientService.count(null);
		
		assertThrows(ResponseStatusException.class, () -> clientService.create("Test Client"));
		final long clientCountAfterInsertion = clientService.count(null);
		assertEquals(clientCountBeforeInsertion, clientCountAfterInsertion);
	}

	/**
	 * Test to validate that the Client is successfully deleted, along with its Keycloak roles. 
	 */
	@Test
	void testShouldDeleteClient() {
		final Instant start = Instant.now();
		willDoNothing().given(keycloakRestTemplate).delete(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1",
				null, Void.class);
		final long clientCountBeforeDeletion = clientService.count(null);
		assertTrue(clientCountBeforeDeletion > 0);
		clientService.markForDeletion(EntityId.of(ONE));
		
		final Optional<JobInformation> jobInfo = JobTestHelper
				.findJobByLastSubmitTime(jobManager, BackgroundDeletionJob.DESCRIPTION, start, TWO);
		assertTrue(jobInfo.isPresent(), "Should return submitted job");
		jobInfo.ifPresent(job -> waitForJobCompletion(job.getJobId(), jobManager, 1, TimeUnit.MINUTES));
		final long clientCountAfterDeletion = clientService.count(null);
		assertEquals(clientCountBeforeDeletion - 1, clientCountAfterDeletion);
		final Long projectCountForClientAfterDeletion = projectService.count(q -> q.ofClient(EntityId.of(ONE)));
		assertEquals(Long.valueOf(0), projectCountForClientAfterDeletion);
	}

	/**
	 * Test to validate that the SYSTEM Client cannot be deleted.
	 */
	@Test
	void testShouldNotDeleteSystemClient() {
		willDoNothing().given(keycloakRestTemplate).delete(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/0", null, Void.class);
		final long clientCountBeforeDeletion = clientService.count(null);
		assertThrows(EntityNotFoundException.class, () -> clientService.deleteDirectly(EntityId.of(ZERO)));
		final long clientCountAfterDeletion = clientService.count(null);
		assertEquals(clientCountBeforeDeletion, clientCountAfterDeletion);
		final Long projectCountForClientAfterDeletion = projectService.count(q -> q.ofClient(EntityId.of(ZERO)));
		assertEquals(ONE, projectCountForClientAfterDeletion);
	}

	/**
	 * Test to validate that the Client is not deleted when Client ID is invalid.
	 */
	@Test
	void testShouldNotDeleteClientInvalidId() {
		final long clientCountBeforeDeletion = clientService.count(null);
		assertTrue(clientCountBeforeDeletion > 0);
		final long projectCountBeforeDeletion = projectService.count(q -> q.withIdAbove(ZERO));
		assertTrue(projectCountBeforeDeletion > 0);

		assertThrows(EntityNotFoundException.class, () -> clientService.deleteDirectly(EntityId.of(INVALID_CLIENT_ID)));

		assertEquals(clientCountBeforeDeletion, clientService.count(null));
		assertEquals(projectCountBeforeDeletion, projectService.count(q -> q.withIdAbove(ZERO)));
	}

	/**
	 * Test to validate that the Client deletion is rolled back when Keycloak throws an exception. 
	 */
	@Test
	void testShouldRollbackClientDeletionExceptionThrown() {
		final Instant start = Instant.now();
		willThrow(new HttpClientErrorException(HttpStatus.FORBIDDEN)).given(keycloakRestTemplate).delete(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1");
		final long clientCountBeforeDeletion = clientService.count(null);

		/* If deletion is forbidden, we don't throw an exception anymore but log the error. The deletion in mining DB is done anyway */
		clientService.markForDeletion(EntityId.of(ONE));
		final Optional<JobInformation> jobInfo = JobTestHelper
				.findJobByLastSubmitTime(jobManager, BackgroundDeletionJob.DESCRIPTION, start, TWO);
		assertTrue(jobInfo.isPresent(), "Should return submitted job");
		jobInfo.ifPresent(job -> waitForJobCompletion(job.getJobId(), jobManager, 1, TimeUnit.MINUTES));

		final long clientCountAfterDeletion = clientService.count(null);
		assertEquals(clientCountBeforeDeletion - 1, clientCountAfterDeletion);
		final Long projectCountForClientAfterDeletionAttempted = projectService.count(q -> q.ofClient(EntityId.of(ONE)));
		assertEquals(0, projectCountForClientAfterDeletionAttempted);
	}

	/**
	 * Test to validate that that the v1 endpoint to create a Client fails in IAM Profile.
	 *
	 * @throws Exception while creating the Client JSON and performing the call to the endpoint
	 */
	@Test
	void testShouldFailV1ClientCreation() throws Exception {
		final long clientCountBeforeCreation = clientService.count(null);
		
		final ClientPojoPrototype newClient = new ClientPojoPrototype().setName("Test Client");
		final String clientJson = PojoMapper.jsonWriter().writeValueAsString(newClient);	
		mvc.perform(post("/api/v1/clients").contentType(MediaType.APPLICATION_JSON).content(clientJson))
		   .andExpect(status().isMethodNotAllowed());
		
		final long clientCountAfterCreation = clientService.count(null);
		assertEquals(clientCountBeforeCreation, clientCountAfterCreation);
	}
	
}
