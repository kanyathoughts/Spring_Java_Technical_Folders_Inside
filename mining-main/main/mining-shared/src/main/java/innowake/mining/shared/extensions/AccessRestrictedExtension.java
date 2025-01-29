/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.extensions;

import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Defines the access restriction for mining  export extension.
 */
public interface AccessRestrictedExtension {

	/**
	 * Returns the required nature type for access.
	 *
	 * @return the required nature type for access
	 */
	NatureType getRequiredNature();

	/**
	 * Returns the required role type for access.
	 *
	 * @return the required role type for access
	 */
	RoleType getRequiredRole();
}
