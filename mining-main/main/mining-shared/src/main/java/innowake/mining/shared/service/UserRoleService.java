/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.service;

import java.util.Collection;
import java.util.Set;

import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Service for retrieving role-related information from the currently logged-in user.
 */
public interface UserRoleService {

	/**
	 * @return {@code true} if the user is an admin, {@code false} otherwise
	 */
	boolean isAdmin();
	
	/**
	 * @return {@code true} if the user has at least one of the following roles: admin, client-admin or any of the project related roles
	 */
	boolean hasPlatformRole();

	/**
	 * @return the client IDs associated with the user
	 */
	Collection<Long> getClientIds();

	/**
	 * @return the client IDs for which the user has client-admin rights
	 */
	Collection<Long> getClientAdminIds();

	/**
	 * @return the project IDs associated with the user
	 */
	Collection<Long> getProjectIds();
	
	/**
	 * Returns the project natures associated with the given project id.
	 *
	 * @param projectId the project id
	 * @return the project natures associated with the given project id
	 */
	Set<NatureType> getNaturesOnProject(long projectId);

	/**
	 * Returns the project roles associated with the given project id.
	 *
	 * @param projectId the project id
	 * @return the project roles associated with the given project id
	 */
	Set<RoleType> getRolesOnProject(long projectId);

	/**
	 * @return {@code true} if the user is an client admin, {@code false} otherwise
	 *
	 * @param projectId of the project
	 */
	boolean isClientAdmin(Long projectId);

	/**
	 * @return {@code true} if the user as required role, {@code false} otherwise
	 *
	 * @param projectId of the project
	 * @param requiredRole requiredRole for the user
	 */
	boolean hasRequiredRole(Long projectId, RoleType requiredRole);

}
