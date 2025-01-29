/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;
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
import org.springframework.http.HttpEntity;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.server.config.NoAuthSecurityConfig;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.MemberController;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.properties.KeycloakApplicationConfiguration;
import innowake.mining.server.service.CookieIdVerifier;
import innowake.mining.server.service.NoOpAuthorizationService;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.model.Member;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.ProjectRole;
import innowake.mining.shared.model.UserRole;

/**
 * Controller tests for {@link Member}.
 * 
 * These tests are to validate that the user does not have access
 * to endpoints in API Server that need data from Keycloak in case the Server
 * is running with no authentication.
 */
@WebMvcTest(MemberController.class)
@Import({ CookieIdVerifier.class, BuildProperties.class, NoAuthSecurityConfig.class })
@ActiveProfiles(Profiles.NO_AUTH)
@WithMockUser
@Tag("mocked")
class MemberControllerNoAuthTest extends MockedBaseTest {

	@Autowired
	private MockMvc mvc;

	@Nullable
	@SpyBean
	private NoOpAuthorizationService authorizationManagementService;

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
	
	@BeforeEach
	public void setupLocalMocks() {
		/* return a client to avoid EntityNotFoundException in ClientController */
		when(Assert.assertNotNull(clientService).find(any(EntityId.class), anyBoolean())).thenReturn(Optional.of(
				new ClientPojo(UUID.randomUUID(), CustomPropertiesMap.empty(), -1l, "MOCKED_CLIENT", false, false)));
		/* Without returning a "proper" Project, the BaseController will return 404 */
		when(Assert.assertNotNull(projectService).isValid(any(EntityId.class))).thenReturn(true);
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/members}
	 * returns {@code HttpStatus#METHOD_NOT_ALLOWED} in {@code Profiles#NO_AUTH}.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testGetClientMembersNotAllowed() throws Exception {
		mvc.perform(get("/api/v2/clients/1/members").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isMethodNotAllowed());
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/members/count}
	 * returns {@code HttpStatus#METHOD_NOT_ALLOWED} in {@code Profiles#NO_AUTH}.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testGetClientMembersCountNotAllowed() throws Exception {
		mvc.perform(get("/api/v2/clients/1/members/count").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isMethodNotAllowed());
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/projects/{projectId}/members}
	 * returns {@code HttpStatus#METHOD_NOT_ALLOWED} in {@code Profiles#NO_AUTH}.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testGetProjectMembersNotAllowed() throws Exception {
		mvc.perform(get("/api/v2/projects/1/members").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isMethodNotAllowed());
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/projects/{projectId}/members/count}
	 * returns {@code HttpStatus#METHOD_NOT_ALLOWED} in {@code Profiles#NO_AUTH}.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testGetProjectMembersCountNotAllowed() throws Exception {
		mvc.perform(get("/api/v2/projects/1/members/count").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isMethodNotAllowed());
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/admins}
	 * returns {@code HttpStatus#METHOD_NOT_ALLOWED} in {@code Profiles#NO_AUTH}.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testGetClientAdminsNotAllowed() throws Exception {
		mvc.perform(get("/api/v2/clients/1/admins").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isMethodNotAllowed());
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/clients/{clientId}/projects/{projectId}/members/{memberId}}
	 * returns {@code HttpStatus#METHOD_NOT_ALLOWED} in {@code Profiles#NO_AUTH}.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testFindMemberByIdNotAllowed() throws Exception {
		mvc.perform(get("/api/v2/projects/1/members/1").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isMethodNotAllowed());
	}

	/**
	 * Test to validate the endpoint {@code POST /v2/clients/{clientId}/admins/{memberId}}
	 * returns {@code HttpStatus#METHOD_NOT_ALLOWED} in {@code Profiles#NO_AUTH}.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testAddMemberAsClientAdminNotAllowed() throws Exception {
		mvc.perform(post("/api/v2/clients/1/admins/1").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isMethodNotAllowed());
	}

	/**
	 * Test to validate the endpoint {@code POST /v2/clients/{clientId}/projects/{projectId}/members}
	 * returns {@code HttpStatus#METHOD_NOT_ALLOWED} in {@code Profiles#NO_AUTH}.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testAddMemberToProjectNotAllowed() throws Exception {
		final ProjectRole projectRole = new ProjectRole(Long.valueOf(1), UserRole.MANAGER, Arrays.asList(ProjectNature.MINING));
		final Member member = new Member("test", "test", "test", "test@test.com", Arrays.asList(projectRole));
		final String memberJSON = PojoMapper.jsonWriter().writeValueAsString(member);
		final HttpEntity<Member> request = new HttpEntity<>(member);
		mvc.perform(post("/api/v2/projects/1/members", request).contentType(MediaType.APPLICATION_JSON).content(memberJSON))
		   .andExpect(status().isMethodNotAllowed());
	}

	/**
	 * Test to validate the endpoint {@code DELETE /v2/clients/{clientId}/admins/{memberId}}
	 * returns {@code HttpStatus#METHOD_NOT_ALLOWED} in {@code Profiles#NO_AUTH}.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testDeleteMemberAsClientAdminNotAllowed() throws Exception {
		mvc.perform(delete("/api/v2/clients/1/admins/1").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isMethodNotAllowed());
	}

	/**
	 * Test to validate the endpoint {@code PUT /v2/projects/{projectId}/members/{memberId}}
	 * returns {@code HttpStatus#METHOD_NOT_ALLOWED} in {@code Profiles#NO_AUTH}.
	 *
	 * @throws Exception May be thrown while performing the HTTP call
	 */
	@Test
	void testAssignProjectRoleToMemberNotAllowed() throws Exception {
		final ProjectRole projectRole = new ProjectRole(Long.valueOf(1), UserRole.MANAGER, Arrays.asList(ProjectNature.MINING));
		final Member member = new Member("test", "test", "test", "test@test.com", Arrays.asList(projectRole));
		final String memberJSON = PojoMapper.jsonWriter().writeValueAsString(member);
		mvc.perform(put("/api/v2/projects/1/members/1").contentType(MediaType.APPLICATION_JSON).content(memberJSON))
		   .andExpect(status().isMethodNotAllowed());
	}
}
