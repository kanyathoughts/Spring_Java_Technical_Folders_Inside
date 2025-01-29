/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak.model;

import java.util.Optional;

/**
 * The roles a User can have.
 * It specifies the kind of access that the user has to the project
 * and how the user interacts with the Project.
 */
public enum UserRole {

	VIEWER,
	EDITOR,
	MANAGER,
	ADMIN;

	/**
	 * Returns the {@link Optional#of(UserRole)} for corresponding name.
	 *
	 * @param name The name associated with the {@link UserRole}
	 * @return The corresponding {@link Optional#of(UserRole)} value
	 */
	public static Optional<UserRole> fromName(final String name) {
		for (final UserRole value : values()) {
			if (value.name().equalsIgnoreCase(name)) {
				return Optional.of(value);
			}
		}
		return Optional.empty();
	}

	/**
	 * Method to get the {@link UserRole} value as a Keycloak Role.
	 *
	 * @param rolePrefix The prefix to add to the {@link UserRole} value
	 * @return The Keycloak role
	 */
	public String getAsKeycloakRole(final String rolePrefix) {
		return rolePrefix + "-" + name().toLowerCase();
	}
}
