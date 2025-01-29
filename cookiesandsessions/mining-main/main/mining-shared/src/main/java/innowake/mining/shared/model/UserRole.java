/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

/**
 * The roles a User can have.
 * It specifies the kind of access that the user has to the project
 * and how the user interacts with the Project.
 */
public enum UserRole {

	VIEWER,
	EDITOR,
	MANAGER,
	CLIENT_ADMIN,
	ADMIN;

	/**
	 * Returns the {@link UserRole} for corresponding name.
	 *
	 * @param name The name associated with the {@link UserRole}
	 * @return The corresponding {@link UserRole} value
	 */
	public static UserRole fromName(final String name) {
		for (final UserRole value : values()) {
			if (value.name().equalsIgnoreCase(name)) {
				return value;
			}
		}
		throw new IllegalArgumentException("Invalid value for UserRole: " + name);
	}
}
