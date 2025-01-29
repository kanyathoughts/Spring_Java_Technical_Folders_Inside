/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willReturn;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.config.security.MiningRole;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.properties.KeycloakApplicationConfiguration;
import innowake.mining.server.service.KeycloakAuthorizationManagementService;
import innowake.mining.shared.model.Member;
import innowake.mining.shared.model.PaginatedResponse;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.ProjectRole;
import innowake.mining.shared.model.UserRole;
import innowake.mining.shared.security.RoleType;

/**
 * Tests to validate operations on Members in IAM Profile.
 * 
 */
@ActiveProfiles( value = Profiles.AUTH_TEST,inheritProfiles = false)
@AutoConfigureMockMvc
class MemberDbTest extends DatabaseResettingTest {

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
	private MockMvc mvc;

	private static final String MOCK_HOST = "mock-host";
	private static final String MOCK_REALM = "mock-realm";
	private static final Long ONE = Long.valueOf(1);

	@BeforeEach
	void init() {
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
		assertNotNull(projectService);
		assertNotNull(authorizationManagementService);
		assertNotNull(keycloakRestTemplate);
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/projects/{projectId}/members} when the endpoint returns valid data.
	 * 
	 * @throws Exception while calling {@link KeycloakRestTemplate#exchange KeycloakRestTemplate#exchange} method.
	 */
	@Test
	void testGetProjectMembers() throws Exception {
		final List<Member> members = new ArrayList<>();
		for (int i = 0; i < 2; i++) {
			members.add(new Member(String.valueOf(i), "FName-"+i, "LName-"+i, "email" + i + "@email.com", Collections.emptyList()));
		}
		final int totalSize = 6;
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		final ResponseEntity<PaginatedResponse<Member>> mockedResponse = new ResponseEntity<>(new PaginatedResponse<>(members, totalSize),
				HttpStatus.OK);
		given(assertNotNull(keycloakRestTemplate)
				.exchange(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1/members?page=1&size=2", HttpMethod.GET, null, responseType))
			.willReturn(mockedResponse);
		final Page<Member> membersPage = assertNotNull(authorizationManagementService).findMembersForProject(ONE, 1, 2);
		assertEquals(2, membersPage.getNumberOfElements());
		final List<String> memberFirstNames = membersPage.get().map(Member::getFirstName).collect(Collectors.toList());
		assertTrue(memberFirstNames.contains("FName-0"));
		assertTrue(memberFirstNames.contains("FName-1"));
		assertEquals(totalSize, membersPage.getTotalElements());
		assertEquals(1, membersPage.getNumber());
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/projects/{projectId}/members} when the endpoint returns valid data
	 * but the page size is lesser than the size value provided.
	 *
	 * @throws Exception while calling {@link KeycloakRestTemplate#exchange KeycloakRestTemplate#exchange} method.
	 */
	@Test
	void testGetProjectMembersReturnsDifferentSize() throws Exception {
		final List<Member> members = new ArrayList<>();
		members.add(new Member("1", "FName", "LName", "email@email.com", Collections.emptyList()));
		final int totalSize = 6;
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		final ResponseEntity<PaginatedResponse<Member>> mockedResponse = new ResponseEntity<>(new PaginatedResponse<>(members, totalSize),
				HttpStatus.OK);
		given(assertNotNull(keycloakRestTemplate)
				.exchange(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1/members?page=1&size=2", HttpMethod.GET, null, responseType))
			.willReturn(mockedResponse);
		final Page<Member> membersPage = assertNotNull(authorizationManagementService).findMembersForProject(ONE, 1, 2);
		assertEquals(1, membersPage.getNumberOfElements());
		final String memberFirstName = membersPage.getContent().get(0).getFirstName();
		assertEquals("FName", memberFirstName);
		assertEquals(totalSize, membersPage.getTotalElements());
		assertEquals(1, membersPage.getNumber());
		/* Even though the max size is defined as 2, the Page contains only 1 record and thus, page size should be 1. */
		assertEquals(1, membersPage.getSize());
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/projects/{projectId}/members} when the endpoint returns empty data.
	 * 
	 * @throws Exception while calling {@link KeycloakRestTemplate#exchange KeycloakRestTemplate#exchange} method.
	 */
	@Test
	void testGetProjectMembersEmptyResponse() throws Exception {
		final ResponseEntity<PaginatedResponse<Member>> mockedResponse = new ResponseEntity<>(new PaginatedResponse<>(Collections.emptyList(), 0),
				HttpStatus.OK);
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		given(assertNotNull(keycloakRestTemplate)
				.exchange(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1/members?page=0&size=0", HttpMethod.GET, null, responseType))
			.willReturn(mockedResponse);
		final Page<Member> membersPage = assertNotNull(authorizationManagementService).findMembersForProject(ONE, 0, 0);
		assertTrue(membersPage.getContent().isEmpty());
	}

	/**
	 * Test case to validate that the given Member has been assigned the appropriate ProjectRoles.
	 * 
	 * @throws Exception while calling {@link KeycloakRestTemplate#postForEntity KeycloakRestTemplate#postForEntity} method.
	 */
	@Test
	void testAddMemberToProject() throws Exception {
		final ProjectRole projectRole = new ProjectRole(Long.valueOf(1), UserRole.MANAGER, Arrays.asList(ProjectNature.MINING));
		final Member member = new Member("test", "test", "test", "test@test.com", Arrays.asList(projectRole));
		final ResponseEntity<Member> mockedResponse = new ResponseEntity<>(member, HttpStatus.OK);
		final HttpEntity<Member> request = new HttpEntity<>(member);
		given(assertNotNull(keycloakRestTemplate)
					.postForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1/members", request, Member.class))
				.willReturn(mockedResponse);
		final Member updatedMember = assertNotNull(authorizationManagementService).addMemberToProject(ONE, member);
		assertNotNull(updatedMember);
		assertEquals(1, updatedMember.getProjectRoles().size());
		final ProjectRole memberProjectRole = updatedMember.getProjectRoles().get(0);
		assertEquals(UserRole.MANAGER, memberProjectRole.getUserRole());
		assertEquals(1, memberProjectRole.getProjectNatures().size());
		assertEquals(ProjectNature.MINING, memberProjectRole.getProjectNatures().get(0));
	}

	/**
	 * Test case to validate that the endpoint throws exception if Project Roles are not provided.
	 * 
	 * @throws Exception while calling {@link KeycloakRestTemplate#postForEntity KeycloakRestTemplate#postForEntity} method.
	 */
	@Test
	void testAddMemberToProjectBadRequest() throws Exception {
		final Member member = new Member("test", "test", "test", "test@test.com", new ArrayList<>());
		final HttpEntity<Member> request = new HttpEntity<>(member);
		given(assertNotNull(keycloakRestTemplate)
					.postForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1/members", request, Member.class))
				.willThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST));
		final KeycloakAuthorizationManagementService authorizationServiceCopy = assertNotNull(authorizationManagementService);
		final HttpClientErrorException exception = assertThrows(HttpClientErrorException.class, 
				() -> authorizationServiceCopy.addMemberToProject(ONE, member));
		assertEquals(HttpStatus.BAD_REQUEST, exception.getStatusCode());
	}
	
	/**
	 * Test case for endpoint "assignProjectRoleToMember", when project id is not valid for the given client.
	 *
	 * @throws Exception may be thrown during HTTP calls
	 */
	@Test
	void testAssignProjectRoleToMemberInvalidProjectId() throws Exception {
		final List<ProjectNature> projectNatures = Arrays.asList(ProjectNature.DISCOVERY);
		final List<ProjectRole> projectRoles = Arrays.asList(new ProjectRole(Long.valueOf(1), UserRole.VIEWER, projectNatures));
		final Member member = new Member("user1", "FUser 1", "LUser 1", "user1@deloitte.com", projectRoles);
		final String url = MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1/members/user1";
		final KeycloakAuthorizationManagementService service = assertNotNull(authorizationManagementService);
		final Long projectId = Long.valueOf(500);
		
		assertThrows(MiningEntityNotFoundException.class, () -> service.assignProjectRoleToMember(projectId, "1", member));
		
		/* verify invocation */
		verify(keycloakRestTemplate, never()).exchange(eq(url), eq(HttpMethod.PUT), any(HttpEntity.class), eq(Member.class));
	}
	
	/**
	 * Test case for endpoint "assignProjectRoleToMember".
	 *
	 * @throws Exception may be thrown during HTTP calls
	 */
	@Test
	void testAssignProjectRoleToMember() throws Exception {
		final List<ProjectNature> projectNatures = Arrays.asList(ProjectNature.DISCOVERY);
		final List<ProjectRole> projectRoles = Arrays.asList(new ProjectRole(Long.valueOf(1), UserRole.MANAGER, projectNatures));
		final Member member = new Member("user1", "FUser 1", "LUser 1", "user1@deloitte.com", projectRoles);
		final ResponseEntity<Member> mockedResponse = new ResponseEntity<>(member, HttpStatus.OK);
		final String url = MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1/members/1";
		final KeycloakAuthorizationManagementService service = assertNotNull(authorizationManagementService);
		
		/* mocks */
		willReturn(mockedResponse).given(keycloakRestTemplate).exchange(eq(url), eq(HttpMethod.PUT), any(HttpEntity.class), eq(Member.class));
		
		final Member retrievedMember = service.assignProjectRoleToMember(ONE, "1", member);
		assertEquals(member, retrievedMember);
		
		/* verify invocation */
		verify(keycloakRestTemplate, times(1)).exchange(eq(url), eq(HttpMethod.PUT), any(HttpEntity.class), eq(Member.class));
	}
	
	/**
	 * Test case for endpoint "assignProjectRoleToMember", when user id is not valid.
	 *
	 * @throws Exception may be thrown during HTTP calls
	 */
	@Test
	void testAssignProjectRoleToMemberNotFound() throws Exception {
		final List<ProjectNature> projectNatures = Arrays.asList(ProjectNature.DISCOVERY);
		final List<ProjectRole> projectRoles = Arrays.asList(new ProjectRole(Long.valueOf(1), UserRole.MANAGER, projectNatures));
		final Member member = new Member("user1", "FUser 1", "LUser 1", "user1@deloitte.com", projectRoles);
		final String url = MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1/members/user1";
		final KeycloakAuthorizationManagementService service = assertNotNull(authorizationManagementService);
		
		/* mock */
		willThrow(new HttpClientErrorException(HttpStatus.NOT_FOUND)).given(keycloakRestTemplate)
		.exchange(eq(url), eq(HttpMethod.PUT), any(HttpEntity.class), eq(Member.class));
		
		assertThrows(HttpClientErrorException.class, () -> service.assignProjectRoleToMember(ONE, "user1", member));
		
		/* verify invocation */
		verify(keycloakRestTemplate, times(1)).exchange(eq(url), eq(HttpMethod.PUT), any(HttpEntity.class), eq(Member.class));
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/projects/{projectId}/members/count} when the endpoint returns empty data.
	 *
	 * @throws Exception while performing the HTTP call
	 */
	@Test
	void testGetProjectMembersCountEmptyResponse() throws Exception {
		final ResponseEntity<Integer> mockedResponse = new ResponseEntity<>(Integer.valueOf(0), HttpStatus.OK);
		given(assertNotNull(keycloakRestTemplate).getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1/members/count", Integer.class))
			.willReturn(mockedResponse);
		mvc.perform(get("/api/v2/projects/1/members/count").contentType(MediaType.APPLICATION_JSON))
			.andExpect(status().isOk())
			.andExpect(content().contentType(MediaType.APPLICATION_JSON))
			.andExpect(jsonPath("$").value("0"));
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/projects/{projectId}/members/count} when the endpoint returns valid data.
	 *
	 * @throws Exception while performing the HTTP call
	 */
	@Test
	void testGetProjectMembersCount() throws Exception {
		final ResponseEntity<Integer> mockedResponse = new ResponseEntity<>(Integer.valueOf(2), HttpStatus.OK);
		given(assertNotNull(keycloakRestTemplate).getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1/members/count", Integer.class))
			.willReturn(mockedResponse);
		mvc.perform(get("/api/v2/projects/1/members/count").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isOk())
		   .andExpect(content().contentType(MediaType.APPLICATION_JSON))
		   .andExpect(jsonPath("$").value("2"));
	}


	/**
	 * Test to validate the endpoint {@code GET /v2/projects/{projectId}/members/{memberId}} when the endpoint returns valid data.
	 *
	 * @throws Exception while performing the HTTP call
	 */
	@Test
	void testGetMemberById() throws Exception {
		final Member member = new Member("1", "FName-1", "LName-1", "email@email.com", Collections.emptyList());
		final ResponseEntity<Member> mockedResponse = new ResponseEntity<>(member, HttpStatus.OK);
		given(assertNotNull(keycloakRestTemplate).getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1/members/1", Member.class))
			.willReturn(mockedResponse);
		mvc.perform(get("/api/v2/projects/1/members/1").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isOk())
		   .andExpect(content().contentType(MediaType.APPLICATION_JSON))
		   .andExpect(jsonPath("$.id").value("1"))
		   .andExpect(jsonPath("$.firstName").value("FName-1"))
		   .andExpect(jsonPath("$.lastName").value("LName-1"))
		   .andExpect(jsonPath("$.email").value("email@email.com"));
	}

	/**
	 * Test to validate the endpoint {@code GET /v2/projects/{projectId}/members/{memberId}} when the endpoint returns empty data.
	 *
	 * @throws Exception while performing the HTTP call
	 */
	@Test
	void testGetMemberByIdForbidden() throws Exception {
		given(assertNotNull(keycloakRestTemplate).getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1/members/1", Member.class))
			.willThrow(new HttpClientErrorException(HttpStatus.FORBIDDEN));
		mvc.perform(get("/api/v2/projects/1/members/1").contentType(MediaType.APPLICATION_JSON))
		   .andExpect(status().isForbidden());
	}
}
