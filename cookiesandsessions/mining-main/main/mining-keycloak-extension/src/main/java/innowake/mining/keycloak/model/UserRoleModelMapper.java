/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak.model;

import org.keycloak.models.GroupModel;
import org.keycloak.models.RoleModel;
import org.keycloak.models.UserModel;

/**
 * Wrapper class to hold user, role and client user group data.
 */
public class UserRoleModelMapper {

	private final UserModel user;
	private final RoleModel role;
	private final GroupModel clientUsersGroup;
	
	/**
	 * Initializes the user, role and client users group data.
	 * 
	 * @param user the user
	 * @param role the realm role
	 * @param clientUsersGroup the client users group
	 */
	public UserRoleModelMapper(final UserModel user, final RoleModel role, final GroupModel clientUsersGroup) {
		this.user = user;
		this.role = role;
		this.clientUsersGroup = clientUsersGroup;
	}
	
	/**
	 * Returns the {@link UserModel user}.
	 *
	 * @return  the {@link UserModel user}
	 */
	public UserModel getUser() {
		return user;
	}

	/**
	 * Returns the {@link RoleModel role}.
	 *
	 * @return the {@link RoleModel role}
	 */
	public RoleModel getRole() {
		return role;
	}

	/**
	 * Returns the {@link GroupModel client user group}.
	 *
	 * @return {@link GroupModel client user group}
	 */
	public GroupModel getClientUsersGroup() {
		return clientUsersGroup;
	}
	
}
