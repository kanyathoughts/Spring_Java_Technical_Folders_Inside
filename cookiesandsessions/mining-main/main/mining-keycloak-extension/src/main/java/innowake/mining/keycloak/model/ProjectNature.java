/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak.model;

import java.util.Optional;

import org.keycloak.models.RoleModel;

/**
 * The nature that can be associated with a Project.
 * It specifies the Project Natures the user has access to.
 */
public enum ProjectNature {

	DISCOVERY,
	DISCOVERY_LIGHT,
	MINING;

	/**
	 * Returns the {@link Optional#of(ProjectNature)} for corresponding name.
	 *
	 * @param name The name associated with the {@link ProjectNature}
	 * @return The corresponding {@link Optional#of(ProjectNature)} value
	 */
	public static Optional<ProjectNature> fromName(final String name) {
		for (final ProjectNature value : values()) {
			if (value.name().equalsIgnoreCase(name)) {
				return Optional.of(value);
			}
		}
		return Optional.empty();
	}

	/**
	 * Method to get the instance of {@link ProjectNature} from natures Keycloak RoleModel.
	 *
	 * @param roleModel The instance of {@link RoleModel} representing the project nature
	 * @param rolePrefix The prefix present in the {@link RoleModel#getName()}
	 * @return The instance of {@link ProjectNature} if a match is found
	 * @throws IllegalStateException exception throw if {@link RoleModel} is not corresponding to a {@link ProjectNature}
	 */
	public static ProjectNature getFromKeycloakRoleModel(final RoleModel roleModel, final String rolePrefix) {
		return fromName(roleModel.getName().replace(rolePrefix + "-", "").replace("-", "_")).orElseThrow(
				() -> new IllegalStateException(String.format("RoleModel %s is not corresponding to a project nature", roleModel.getName())));
	}

	/**
	 * Method to get the {@link ProjectNature} value as a Keycloak Role.
	 *
	 * @param rolePrefix The prefix to add to the {@link ProjectNature} value
	 * @return The Keycloak role
	 */
	public String getAsKeycloakRole(final String rolePrefix) {
		return rolePrefix + "-" + name().toLowerCase().replace("_", "-");
	}
}
