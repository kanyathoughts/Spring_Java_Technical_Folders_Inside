/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.server.ResponseStatusException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.properties.KeycloakApplicationConfiguration;
import innowake.mining.server.util.PaginationUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.model.Member;
import innowake.mining.shared.model.PaginatedResponse;
import innowake.mining.shared.model.ProjectNature;

/**
 * Service that handles requests related to Authorization Management
 * when {@code Profiles#IAM} is active.
 */
@Service
@Profile(Profiles.IAM)
public class KeycloakAuthorizationManagementService implements AuthorizationManagementService {

	private static final String PROJECT_NATURES = "projectNatures";

	/* Autowired in constructor */
	private final RestTemplate keycloakRestTemplate;

	@Autowired
	private KeycloakApplicationConfiguration keycloakConfig;

	@Autowired
	private ProjectService projectService;

	@Nullable
	private String realmURI;

	@Nullable
	private String clientURI;

	@Nullable
	private String projectURI;

	@Autowired
	public KeycloakAuthorizationManagementService(final RestTemplate keycloakRestTemplate) {
		this.keycloakRestTemplate = keycloakRestTemplate;
	}

	/**
	 * PostConstruct to initialize Keycloak Configuration
	 * that can be used to interact with the extended Keycloak endpoints.
	 */
	@PostConstruct
	public void createKeycloakURI() {
		realmURI = keycloakConfig.getAuthServerUrl() + "/realms/" + keycloakConfig.getRealm();
		clientURI = realmURI + "/admin/clients/%d";
		projectURI = clientURI + "/projects/%d";
	}

	@Override
	public Page<Member> findMembersForClient(final Long clientId, final int page, final int size) {
		final String url = getClientURI(clientId) + "/members?" + PaginationUtil.getPaginationParamsForUrl(page, size);
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		final ResponseEntity<PaginatedResponse<Member>> response = keycloakRestTemplate.exchange(url, HttpMethod.GET, null, responseType);
		return PaginationUtil.getPageFromResponse(response.getBody(), page);
	}

	@Override
	public int findMemberCountForClient(final Long clientId) {
		final String url = getClientURI(clientId) + "/members/count";
		final ResponseEntity<Integer> response = keycloakRestTemplate.getForEntity(url, Integer.class);
		final Integer responseBody = response.getBody();
		return responseBody != null ? responseBody.intValue() : 0;
	}

	@Override
	public Page<Member> findMembersForProject(final Long projectId, int page, int size) {
		final Long clientId = getClientIdForProject(projectId);
		final String url = getUsersForProjectURI(clientId, projectId) + "?" + PaginationUtil.getPaginationParamsForUrl(page, size);
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		final ResponseEntity<PaginatedResponse<Member>> response = keycloakRestTemplate.exchange(url, HttpMethod.GET, null, responseType);
		return PaginationUtil.getPageFromResponse(response.getBody(), page);
	}

	@Override
	public int findMemberCountForProject(final Long projectId) {
		final Long clientId = getClientIdForProject(projectId);
		final String url = getUsersForProjectURI(clientId, projectId) + "/count";
		final ResponseEntity<Integer> response = keycloakRestTemplate.getForEntity(url, Integer.class);
		final Integer responseBody = response.getBody();
		return responseBody != null ? responseBody.intValue() : 0;
	}

	@Override
	public Member findMemberById(final Long projectId, final String memberId) {
		final Long clientId = getClientIdForProject(projectId);
		final String url = getUsersForProjectURI(clientId, projectId) + "/" + memberId;
		final ResponseEntity<Member> response = keycloakRestTemplate.getForEntity(url, Member.class);
		final Member member = response.getBody();
		if (member != null) {
			return member;
		}
		throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Member with ID not found: " + memberId);
	}

	@Override
	public Page<Member> findClientAdmins(final Long clientId, final int page, final int size) {
		final String url = getClientURI(clientId) + "/admins?" + PaginationUtil.getPaginationParamsForUrl(page, size);
		final ParameterizedTypeReference<PaginatedResponse<Member>> responseType = new ParameterizedTypeReference<PaginatedResponse<Member>>() { };
		final ResponseEntity<PaginatedResponse<Member>> response = keycloakRestTemplate.exchange(url, HttpMethod.GET, null, responseType);
		return PaginationUtil.getPageFromResponse(response.getBody(), page);
	}

	/**
	 * Check if a group with the given {@code clientId} exists.
	 *
	 * @param clientId id to look for
	 * @return {@code true} if the id was found
	 */
	public boolean doesClientGroupExist(final Long clientId) {
		final String clientUrl = getClientURI(clientId);
		final Boolean result = keycloakRestTemplate.getForEntity(clientUrl, Boolean.class).getBody();
		return result != null && result.booleanValue();
	}

	@Override
	public void createClientAttributes(final Long clientId) {
		/* While we do have individual endpoints that are offered by Keycloak, they require data in a specific format.
		 * It is easier and more manageable to create a custom extended endpoint that will create these attributes for us. */
		final String createClientRolesUrl = getClientURI(clientId);
		final HttpEntity<Long> request = new HttpEntity<>(clientId);
		keycloakRestTemplate.postForEntity(createClientRolesUrl, request, Void.class);
	}

	@Override
	public void createProjectAttributes(final Long clientId, final Long projectId, final Set<ProjectNature> projectNatures) {
		/* While we do have individual endpoints that are offered by Keycloak, they require data in a specific format.
		 * It is easier and more manageable to create a custom extended endpoint that will create these attributes for us. */
		if (CollectionUtils.isEmpty(projectNatures)) {
			throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Project Natures have not been defined");
		}
		final String naturesStr = projectNatures.stream().map(ProjectNature::name).collect(Collectors.joining(","));
		final String createProjectRolesUrl = getProjectNaturesURI(clientId, projectId) + naturesStr;
		keycloakRestTemplate.postForEntity(createProjectRolesUrl, null, Void.class);
	}

	/**
	 * Check if there are any roles for the given {@code clientId} and {@code projectid}.
	 *
	 * @param clientId id of the client
	 * @param projectId id of the project
	 * @return {@code true} if there is any role for the given ids
	 */
	public boolean doProjectRolesExist(final Long clientId, final Long projectId) {
		final String projectUrl = getProjectURI(clientId, projectId);
		final Boolean result = keycloakRestTemplate.getForEntity(projectUrl, Boolean.class).getBody();
		return result != null && result.booleanValue();
	}

	@Override
	public ProjectNature[] findProjectNatures(final Long clientId, final Long projectId) {
		final String findProjectNaturesUrl = String.join("/", getProjectURI(clientId, projectId), PROJECT_NATURES);
		final ResponseEntity<ProjectNature[]> response = keycloakRestTemplate.getForEntity(findProjectNaturesUrl, ProjectNature[].class);
		final ProjectNature[] responseBody = response.getBody();
		if (responseBody == null || responseBody.length == 0) {
			throw new ConstraintViolationException(
					String.format("Project with projectId <%d> must have at least 1 Project Nature assigned to it but none are assigned.", projectId));
		}
		return responseBody;
	}

	@Override
	public void changeProjectNatures(final Long clientId, final Long projectId, final Set<ProjectNature> projectNatures) {
		/* While we do have individual endpoints that are offered by Keycloak, they require data in a specific format.
		 * It is easier and more manageable to create a custom extended endpoint that will update these attributes for us. */
		if (CollectionUtils.isEmpty(projectNatures)) {
			throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Project Natures have not been defined.");
		}
		final String naturesStr = projectNatures.stream().map(ProjectNature::name).collect(Collectors.joining(","));
		final String changeProjectNaturesUrl = getProjectNaturesURI(clientId, projectId) + naturesStr;
		keycloakRestTemplate.put(changeProjectNaturesUrl, null);
	}

	@Override
	public void deleteProjectAttributes(final Long clientId, final Long projectId) {
		keycloakRestTemplate.delete(getProjectURI(clientId, projectId));
	}

	@Override
	public void deleteClientAttributes(final Long clientId) {
		keycloakRestTemplate.delete(getClientURI(clientId));
	}

	@Override
	public Member addMemberAsClientAdmin(final Long clientId, final Member member) {
		final HttpEntity<Member> request = new HttpEntity<>(member);
		final String clientAdminUrl = getClientURI(clientId) + "/admins";
		final ResponseEntity<Member> response = keycloakRestTemplate.postForEntity(clientAdminUrl, request, Member.class);
		final Member createdMember = response.getBody();
		if (createdMember !=  null) {
			return createdMember;
		}
		throw new IllegalStateException("Member was not added as client admin");
	}
	
	@Override
	public Member assignProjectRoleToMember(final Long projectId, final String memberId, final Member member) {
		final Long clientId = getClientIdForProject(projectId);
		final String url = getUsersForProjectURI(clientId, projectId) + "/" + memberId;
		final HttpEntity<Member> request = new HttpEntity<>(member);
		final ResponseEntity<Member> response = keycloakRestTemplate.exchange(url, HttpMethod.PUT, request, Member.class);
		final Member createdMember = response.getBody();
		if (createdMember !=  null) {
			return createdMember;
		}
		throw new IllegalStateException("Member was not updated with project role");
	}

	@Override
	public Member addMemberToProject(final Long projectId, final Member member) {
		final Long clientId = getClientIdForProject(projectId);
		final HttpEntity<Member> request = new HttpEntity<>(member);
		final ResponseEntity<Member> response = keycloakRestTemplate.postForEntity(getUsersForProjectURI(clientId, projectId), request, Member.class);
		final Member createdMember = response.getBody();
		if (createdMember !=  null) {
			return createdMember;
		}
		throw new IllegalStateException("Member was not created");
	}
	
	@Override
	public void deleteMemberAsClientAdmin(final Long clientId, final String memberId) {
		keycloakRestTemplate.delete(getClientAdminURI(clientId, memberId));
	}

	@Override
	public void deleteMemberFromProject(final Long projectId, final String memberId) {
		final Long clientId = getClientIdForProject(projectId);
		keycloakRestTemplate.delete(getMemberForProjectURI(clientId, projectId, memberId));
	}

	@Override
	public boolean isAuthorizedAccess() {
		return true;
	}

	private String getClientURI(final Long clientId) {
		return String.format(clientURI, clientId);
	}

	private String getProjectURI(final Long clientId, final Long projectId) {
		return String.format(projectURI, clientId, projectId);
	}

	private String getUsersForProjectURI(final Long clientId, final Long projectId) {
		return String.format(projectURI, clientId, projectId) + "/members";
	}

	private String getClientAdminURI(final Long clientId, final String memberId) {
		return getClientURI(clientId) + "/admins/" + memberId;
	}

	private String getProjectNaturesURI(final Long clientId, final Long projectId) {
		return getProjectURI(clientId, projectId) + "?" + PROJECT_NATURES + "=";
	}

	private String getMemberForProjectURI(final Long clientId, final Long projectId, final String memberId) {
		return getUsersForProjectURI(clientId, projectId) + "/" + memberId;
	}

	private Long getClientIdForProject(final Long projectId) {
		final ProjectPojo project = projectService.get(EntityId.of(projectId));
		return assertNotNull(project.getClientNid());
	}
}
