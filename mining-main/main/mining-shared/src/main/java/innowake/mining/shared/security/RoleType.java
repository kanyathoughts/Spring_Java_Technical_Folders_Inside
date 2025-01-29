/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.security;

import java.util.Arrays;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Representation of the different user roles. 
 */
public enum RoleType {

	/**
	 * A project viewer with read-only access.
	 */
	VIEWER("viewer"),

	/**
	 * A project editor with read-write access.
	 */
	EDITOR("editor"),

	/**
	 * A project manager.
	 */
	MANAGER("manager"),

	/**
	 * A client administrator.
	 */
	CLIENT_ADMIN("client-admin"),

	/**
	 * A system administrator.
	 */
	ADMIN("admin");
	
	public static final Set<String> VALUE_SET = Collections.unmodifiableSet(Arrays.stream(values()).map(RoleType::getValue).collect(Collectors.toSet()));

	private final String value;

	private RoleType(final String value) {
		this.value = value;
	}

	private RoleType() {
		value = name().toLowerCase();
	}

	/**
	 * Checks that the given user role is contained within the hierarchy of the roles.
	 * <p>
	 * The hierarchy is: MANAGER > EDITOR > VIEWER, where 'X > Y' means that the role
	 * X inherits the access rights of Y.
	 * <p>
	 * The ADMIN role is orthogonal and is not part of the hierarchy.
	 *
	 * @param type the user role type to check
	 * @return {@code true} if it is contained, {@code false} otherwise
	 */
	public boolean contains(final RoleType type) {
		return this.ordinal() >= type.ordinal();
	}

	/**
	 * @return the value used in the raw Keycloak role
	 */
	public String getValue() {
		return value;
	}

}
