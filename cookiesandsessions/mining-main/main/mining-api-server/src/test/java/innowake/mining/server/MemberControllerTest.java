/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willDoNothing;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.info.BuildProperties;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.context.annotation.Import;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.server.ResponseStatusException;

import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.server.config.AuthTestSecurityConfig;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.MemberController;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.properties.KeycloakApplicationConfiguration;
import innowake.mining.server.service.CookieIdVerifier;
import innowake.mining.server.service.KeycloakAuthorizationManagementService;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.model.Member;
import innowake.mining.shared.model.PaginatedResponse;

/**
 * Mocked tests verifying the behavior of {@link MemberController}.
 */
@WebMvcTest(MemberController.class)
@Import({ CookieIdVerifier.class, BuildProperties.class, AuthTestSecurityConfig.class })
@ActiveProfiles(Profiles.AUTH_TEST)
@WithMockUser
@Tag("mocked")
class MemberControllerTest extends MockedBaseTest {

	@Autowired
	private MockMvc mvc;

	@Nullable
	@SpyBean
	private KeycloakAuthorizationManagementService authorizationManagementService;

	@MockBean
	@Nullable
	private KeycloakApplicationConfiguration config;

	@Nullable
	@MockBean
	private RestTemplate keycloakRestTemplate;
	
	@Nullable
	@MockBean
	private GenericConfigProperties genericConfigProperties;
	
	@Nullable
	@MockBean
	private DiscoveryPersistenceImpl orientDiscoveryPersistence;

	private static final String MOCK_HOST = "mock-host";
	private static final String MOCK_REALM = "mock-realm";

	@Override
	@BeforeEach
	public void setup() {
		super.setup();
		assertNotNull("KeycloakRestTemplate was not properly injected.", keycloakRestTemplate);
		final KeycloakApplicationConfiguration config2 = config;
		if (config2 != null) {
			given(config2.getAuthServerUrl()).willReturn(MOCK_HOST);
			given(config2.getRealm()).willReturn(MOCK_REALM);
			assertNotNull(authorizationManagementService);
			authorizationManagementService.createKeycloakURI();
		} else {
			fail("KeycloakApplicationConfiguration was not properly injected.");
		}

		/* return a client to avoid EntityNotFoundException in ClientController */
		when(Assert.assertNotNull(clientService).find(any(), anyBoolean())).thenReturn(Optional.of(
				new ClientPojo(UUID.randomUUID(), CustomPropertiesMap.empty(), -1l, "MOCKED_CLIENT", false, false)));
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/members} when the endpoint returns valid data.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testGetClientMembers() throws Exception {
		final List<Member> members = new ArrayList<>();
		for (int i = 0; i < 2; i++) {
			members.add(new Member(String.valueOf(i), "FName-"+i, "LName-"+i, "email" + i + "@email.com", Collections.emptyList()));
		}
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		final ResponseEntity<PaginatedResponse<Member>> mockedResponse = new ResponseEntity<>(new PaginatedResponse<>(members, 0),
				HttpStatus.OK);
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.exchange(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/members?page=0&size=0", HttpMethod.GET, null, responseType))
			.willReturn(mockedResponse);
		mvc.perform(get("/api/v2/clients/1/members").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(content().contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isOk())
		   .andExpect(jsonPath("$.content[0].id").value("0"))
		   .andExpect(jsonPath("$.content[0].firstName").value("FName-0"))
		   .andExpect(jsonPath("$.content[0].lastName").value("LName-0"))
		   .andExpect(jsonPath("$.content[0].email").value("email0@email.com"))
		   .andExpect(jsonPath("$.content[1].id").value("1"))
		   .andExpect(jsonPath("$.content[1].firstName").value("FName-1"))
		   .andExpect(jsonPath("$.content[1].lastName").value("LName-1"))
		   .andExpect(jsonPath("$.content[1].email").value("email1@email.com"))
		   .andExpect(jsonPath("$.number").value(Integer.valueOf(0)));
	}

	@Test
	void testGetClientMembersReturnsDifferentPageSize() throws Exception {
		final List<Member> members = new ArrayList<>();
		members.add(new Member("1", "FName-1", "LName-1", "email@email.com", Collections.emptyList()));
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		final ResponseEntity<PaginatedResponse<Member>> mockedResponse = new ResponseEntity<>(new PaginatedResponse<>(members, 1),
				HttpStatus.OK);
		final int maxSize = 2;
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.exchange(
				MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/members?page=0&size=" + maxSize,
				HttpMethod.GET,
				null,
				responseType))
			.willReturn(mockedResponse);
		mvc.perform(get("/api/v2/clients/1/members?page=0&size=" + maxSize).contentType(MediaType.APPLICATION_JSON))
		   .andExpect(content().contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isOk())
		   .andExpect(jsonPath("$.size").value("1"))
		   .andExpect(jsonPath("$.content[0].id").value("1"))
		   .andExpect(jsonPath("$.content[0].firstName").value("FName-1"))
		   .andExpect(jsonPath("$.content[0].lastName").value("LName-1"))
		   .andExpect(jsonPath("$.content[0].email").value("email@email.com"))
		   .andExpect(jsonPath("$.number").value(Integer.valueOf(0)));
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/members} when the endpoint returns empty data.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testGetClientMembersEmptyResponse() throws Exception {
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		final ResponseEntity<PaginatedResponse<Member>> mockedResponse = new ResponseEntity<>(new PaginatedResponse<>(Collections.emptyList(), 0),
				HttpStatus.OK);
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.exchange(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/members?page=0&size=0", HttpMethod.GET, null, responseType))
			.willReturn(mockedResponse);
		mvc.perform(get("/api/v2/clients/1/members").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(content().contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isOk())
		   .andExpect(jsonPath("$.content").isEmpty());
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/members/count} when the endpoint returns valid data.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testGetClientMembersCount() throws Exception {
		final ResponseEntity<Integer> mockedResponse = new ResponseEntity<>(Integer.valueOf(2), HttpStatus.OK);
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/members/count", Integer.class))
			.willReturn(mockedResponse);
		mvc.perform(get("/api/v2/clients/1/members/count").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(content().contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isOk())
		   .andExpect(jsonPath("$").value("2"));
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/members/count} when the endpoint returns empty data.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testGetClientMembersCountEmptyResponse() throws Exception {
		final ResponseEntity<Integer> mockedResponse = new ResponseEntity<>(Integer.valueOf(0), HttpStatus.OK);
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/members/count", Integer.class))
			.willReturn(mockedResponse);
		mvc.perform(get("/api/v2/clients/1/members/count").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(content().contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isOk())
		   .andExpect(jsonPath("$").value("0"));
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/admins} when the endpoint returns valid data.
	 *
	 * @throws Exception while performing the HTTP call
	 */
	@Test
	void testGetClientAdmins() throws Exception {
		final List<Member> members = new ArrayList<>();
		for (int i = 0; i < 2; i++) {
			members.add(new Member(String.valueOf(i), "FName-"+i, "LName-"+i, "email" + i + "@email.com", Collections.emptyList()));
		}
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		final ResponseEntity<PaginatedResponse<Member>> mockedResponse = new ResponseEntity<>(new PaginatedResponse<>(members, 2),
				HttpStatus.OK);
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.exchange(
				MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/admins?page=0&size=0", 
				HttpMethod.GET,
				null,
				responseType))
			.willReturn(mockedResponse);
		mvc.perform(get("/api/v2/clients/1/admins").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(content().contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isOk())
		   .andExpect(jsonPath("$.content[0].id").value("0"))
		   .andExpect(jsonPath("$.content[0].firstName").value("FName-0"))
		   .andExpect(jsonPath("$.content[0].lastName").value("LName-0"))
		   .andExpect(jsonPath("$.content[0].email").value("email0@email.com"))
		   .andExpect(jsonPath("$.content[1].id").value("1"))
		   .andExpect(jsonPath("$.content[1].firstName").value("FName-1"))
		   .andExpect(jsonPath("$.content[1].lastName").value("LName-1"))
		   .andExpect(jsonPath("$.content[1].email").value("email1@email.com"));
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/admins} when the endpoint returns valid data
	 * of different Page Size than the size value given.
	 *
	 * @throws Exception while performing the HTTP call
	 */
	@Test
	void testGetClientAdminsReturnsDifferentPageSize() throws Exception {
		final List<Member> members = new ArrayList<>();
		members.add(new Member("1", "FName", "LName", "email@email.com", Collections.emptyList()));
		final int totalSize = 6;
		final int maxSize = 3;
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		final ResponseEntity<PaginatedResponse<Member>> mockedResponse = new ResponseEntity<>(new PaginatedResponse<>(members, totalSize),
				HttpStatus.OK);
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.exchange(
				MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/admins?page=0&size=" + maxSize, 
				HttpMethod.GET,
				null,
				responseType))
			.willReturn(mockedResponse);
		mvc.perform(get("/api/v2/clients/1/admins?page=0&size=" + maxSize).contentType(MediaType.APPLICATION_JSON))
		   .andExpect(content().contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isOk())
		   .andExpect(jsonPath("$.size").value("1"))
		   .andExpect(jsonPath("$.content[0].id").value("1"))
		   .andExpect(jsonPath("$.content[0].firstName").value("FName"))
		   .andExpect(jsonPath("$.content[0].lastName").value("LName"))
		   .andExpect(jsonPath("$.content[0].email").value("email@email.com"))
		   .andExpect(jsonPath("$.number").value("0"));
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/admins} when the endpoint returns empty data.
	 *
	 * @throws Exception while performing the HTTP call
	 */
	@Test
	void testGetClientAdminsEmptyResponse() throws Exception {
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		final ResponseEntity<PaginatedResponse<Member>> mockedResponse = new ResponseEntity<>(new PaginatedResponse<>(Collections.emptyList(), 0),
				HttpStatus.OK);
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.exchange(
				MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/admins?page=0&size=0", 
				HttpMethod.GET,
				null,
				responseType))
			.willReturn(mockedResponse);
		mvc.perform(get("/api/v2/clients/1/admins").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(content().contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isOk())
		   .andExpect(jsonPath("$.content").isEmpty());
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/admins} when the Client is not found.
	 *
	 * @throws Exception while performing the HTTP call
	 */
	@Test
	void testGetClientAdminsClientNotFound() throws Exception {
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.exchange(
				MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/100/admins?page=0&size=0", 
				HttpMethod.GET,
				null,
				responseType))
			.willThrow(new ResponseStatusException(HttpStatus.NOT_FOUND, "Client Not Found"));
		mvc.perform(get("/api/v2/clients/100/admins").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isNotFound());
	}

	/**
	 * Test case for endpoint "addAdminToClient" to check whether a user is granted admin role for a specific client.
	 *
	 * @throws Exception if error occurs invoking the controller
	 */
	@Test
	void testAddAdminToClient() throws Exception {
		final String url = MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/100/admins";
		final Member member = new Member("1", "FName-1", "LName-1", "email@email.com", Collections.emptyList());
		final String memberJson = new ObjectMapper().writeValueAsString(member);
		final ResponseEntity<Member> mockedResponse = new ResponseEntity<>(member, HttpStatus.OK);
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.postForEntity(eq(url), any(HttpEntity.class), eq(Member.class)))
				.willReturn(mockedResponse);
		mvc.perform(post("/api/v2/clients/100/admins").contentType(MediaType.APPLICATION_JSON).content(memberJson))
				.andExpect(status().isOk())
				.andExpect(jsonPath("$.id").value("1"))
				.andExpect(jsonPath("$.firstName").value("FName-1"))
				.andExpect(jsonPath("$.lastName").value("LName-1"))
				.andExpect(jsonPath("$.email").value("email@email.com"));
		verify(keycloakRestTemplate, times(1)).postForEntity(eq(url), any(HttpEntity.class), eq(Member.class));
	}

	/**
	 * Test case for endpoint "addAdminToClient" when client user group is not available.
	 *
	 * @throws Exception if error occurs invoking the controller
	 */
	@Test
	void testAddAdminToClientGroupNotFound() throws Exception {
		final String url = MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/100/admins";
		final Member member = new Member("1", "FName-1", "LName-1", "email@email.com", Collections.emptyList());
		final String memberJson = new ObjectMapper().writeValueAsString(member);
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.postForEntity(eq(url), any(HttpEntity.class), eq(Member.class)))
				.willThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));
		mvc.perform(post("/api/v2/clients/100/admins").contentType(MediaType.APPLICATION_JSON).content(memberJson)).andExpect(status().isBadRequest());
		verify(keycloakRestTemplate, times(1)).postForEntity(eq(url), any(HttpEntity.class), eq(Member.class));
	}
	
	/**
	 * Test case for endpoint "addAdminToClient" when user mail id is empty.
	 *
	 * @throws Exception if error occurs invoking the controller
	 */
	@Test
	void testAddAdminEmptyUserEmailId() throws Exception {
		final String url = MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/100/admins";
		final Member member = new Member("1", "FName-1", "LName-1", "", Collections.emptyList());
		final String memberJson = new ObjectMapper().writeValueAsString(member);
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.postForEntity(eq(url), any(HttpEntity.class), eq(Member.class)))
				.willThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));
		mvc.perform(post("/api/v2/clients/100/admins").contentType(MediaType.APPLICATION_JSON).content(memberJson)).andExpect(status().isBadRequest());
		verify(keycloakRestTemplate, times(1)).postForEntity(eq(url), any(HttpEntity.class), eq(Member.class));
	}

	/**
	 * Test case for endpoint "addAdminToClient" when either user id or role is not found.
	 *
	 * @throws Exception if error occurs invoking the controller
	 */
	@Test
	void testAddAdminToClientNotFound() throws Exception {
		final String url = MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/100/admins";
		final Member member = new Member("1", "FName-1", "LName-1", "email@email.com", Collections.emptyList());
		final String memberJson = new ObjectMapper().writeValueAsString(member);
		assertNotNull(keycloakRestTemplate);
		given(keycloakRestTemplate.postForEntity(eq(url), any(HttpEntity.class), eq(Member.class)))
				.willThrow(new HttpClientErrorException(HttpStatus.NOT_FOUND));
		mvc.perform(post("/api/v2/clients/100/admins").contentType(MediaType.APPLICATION_JSON).content(memberJson)).andExpect(status().isNotFound())
				.andExpect(result -> assertTrue(result.getResolvedException() instanceof HttpClientErrorException));
		verify(keycloakRestTemplate, times(1)).postForEntity(eq(url), any(HttpEntity.class), eq(Member.class));
	}

	/**
	 * Test case to validate if admin access of the given user is removed for the given client.
	 *
	 * @throws Exception may be thrown while performing the HTTP call
	 */
	@Test
	void testDeleteMemberAsClientAdmin() throws Exception {
		final String url = MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/admins/userId";
		willDoNothing().given(keycloakRestTemplate).delete(url);
		mvc.perform(delete("/api/v2/clients/1/admins/userId")).andExpect(status().isOk());
		verify(keycloakRestTemplate, times(1)).delete(url);
	}

	/**
	 * Test case to validate error if the given user is not valid, while removing the admin access to the given client.
	 *
	 * @throws Exception may be thrown while performing the HTTP call
	 */
	@Test
	void testDeleteMemberAsClientAdminNotValidMember() throws Exception {
		final String url = MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/admins/userId";
		willThrow(new HttpClientErrorException(HttpStatus.NOT_FOUND)).given(keycloakRestTemplate).delete(url);
		mvc.perform(delete("/api/v2/clients/1/admins/userId")).andExpect(status().isNotFound());
		verify(keycloakRestTemplate, times(1)).delete(url);
	}

	/**
	 * Test case to validate error if the given client user group is not found, while deleting admin from client.
	 *
	 * @throws Exception may be thrown while performing the HTTP call
	 */
	@Test
	void testDeleteMemberAsClientAdminGroupNotFound() throws Exception {
		final String url = MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/admins/userId";
		willThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST)).given(keycloakRestTemplate).delete(url);
		mvc.perform(delete("/api/v2/clients/1/admins/userId")).andExpect(status().isBadRequest());
		verify(keycloakRestTemplate, times(1)).delete(url);
	}

}
