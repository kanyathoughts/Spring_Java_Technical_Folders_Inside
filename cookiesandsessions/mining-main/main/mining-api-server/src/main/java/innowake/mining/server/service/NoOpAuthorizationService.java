/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Set;

import org.springframework.context.annotation.Profile;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

import innowake.mining.server.config.Profiles;
import innowake.mining.shared.model.Member;
import innowake.mining.shared.model.ProjectNature;

/**
 * Service to handle requests related to Authorization Management
 * when {@code Profiles#NOT_IAM} and {@code Profiles#NO_AUTH} are active.
 */
@Service
@Profile({Profiles.NO_IAM, Profiles.NO_AUTH})
public class NoOpAuthorizationService implements AuthorizationManagementService {

	@Override
	public Page<Member> findMembersForClient(final Long clientId, final int page, final int size) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public int findMemberCountForClient(final Long clientId) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public Page<Member> findMembersForProject(final Long projectId, final int page, final int size) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public int findMemberCountForProject(final Long projectId) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public Member findMemberById(final Long projectId, final String memberId) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public Page<Member> findClientAdmins(final Long clientId, final int page, final int size) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public void createClientAttributes(final Long clientId) {
		/* No implementation as the Keycloak Instance is not available */
	}

	@Override
	public void createProjectAttributes(final Long clientId, final Long projectId, final Set<ProjectNature> projectNatures) {
		/* No implementation as the Keycloak Instance is not available */
	}

	@Override
	public void deleteProjectAttributes(final Long clientId, final Long projectId) {
		/* No implementation as the Keycloak Instance is not available */
	}

	@Override
	public ProjectNature[] findProjectNatures(final Long clientId, final Long projectId) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public void changeProjectNatures(final Long clientId, final Long projectId, final Set<ProjectNature> projectNatures) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public Member addMemberAsClientAdmin(final Long clientId, final Member member) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public void deleteClientAttributes(final Long clientId) {
		/* No implementation as the Keycloak Instance is not available */
	}

	@Override
	public Member addMemberToProject(final Long projectId, final Member member) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public void deleteMemberAsClientAdmin(final Long clientId, final String memberId) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public Member assignProjectRoleToMember(final Long projectId, final String memberId, final Member member) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public void deleteMemberFromProject(final Long projectId, final String memberId) {
		throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED);
	}

	@Override
	public boolean isAuthorizedAccess() {
		return false;
	}
}
