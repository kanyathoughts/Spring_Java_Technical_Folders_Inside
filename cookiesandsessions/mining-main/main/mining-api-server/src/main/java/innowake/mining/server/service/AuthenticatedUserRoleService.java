/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Collection;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.config.security.AuthenticationFacade;
import innowake.mining.server.config.security.MiningRole;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import innowake.mining.shared.service.UserRoleService;

/**
 * Service for retrieving role-related information from the currently logged-in user.
 */
@Service
@Profile(Profiles.IAM)
public class AuthenticatedUserRoleService implements UserRoleService {
	
	@Autowired
	private AuthenticationFacade authenticationFacade;
	
	@Autowired
	private ProjectService projectService;
	
	@Override
	public Collection<Long> getClientIds() {
		return getRoles()
				.map(MiningRole::clientId)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.distinct()
				.collect(Collectors.toList());
	}
	
	@Override
	public Collection<Long> getProjectIds() {
		return getRoles()
				.map(MiningRole::projectId)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.distinct()
				.collect(Collectors.toList());
	}
	
	@Override
	public Collection<Long> getClientAdminIds() {
		return getRoles()
				.filter(role -> RoleType.CLIENT_ADMIN.getValue().equals(role.value()))
				.map(MiningRole::clientId)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.distinct()
				.collect(Collectors.toList());
	}

	@Override
	public boolean isAdmin() {
		return getRoles().anyMatch(role -> RoleType.ADMIN.getValue().equals(role.value()));
	}

	@Override
	public boolean hasPlatformRole() {
		return getRoles().count() > 0;
	}

	@Override
	public Set<NatureType> getNaturesOnProject(final long projectId) {
		return getRoles()
				.filter(role -> filterProject(projectId, role))
				.map(MiningRole::value)
				.map(String::toLowerCase)
				.filter(NatureType.VALUE_SET::contains)
				.map(NatureType::fromName)
				.collect(Collectors.toSet());
	}

	@Override
	public Set<RoleType> getRolesOnProject(final long projectId) {
		return getRoles()
				.filter(role -> filterProject(projectId, role))
				.map(MiningRole::value)
				.map(String::toLowerCase)
				.filter(RoleType.VALUE_SET::contains)
				.map(String::toUpperCase)
				.map(RoleType::valueOf)
				.collect(Collectors.toSet());
	}
	
	@Override
	public boolean hasRequiredRole(final Long projectId, final RoleType requiredRole) {
		return getRolesOnProject(projectId.longValue())
				.stream()
				.filter(Objects::nonNull)
				.anyMatch(role -> role.contains(requiredRole));
	}
	
	@Override
	public boolean isClientAdmin(final Long projectId) {
		final ProjectPojo project = projectService.get(EntityId.of(projectId));
		return getClientAdminIds().contains(project.getClientNid());
	}

	private boolean filterProject(final long projectId, final MiningRole role) {
		final Optional<Long> id = role.projectId();
		return id.isPresent() && id.get().equals(Long.valueOf(projectId));
	}
	
	private Stream<MiningRole> getRoles() {
		return authenticationFacade.getAuthentication().getAuthorities().stream()
				.filter(MiningRole.class::isInstance)
				.map(MiningRole.class::cast);
	}

}
