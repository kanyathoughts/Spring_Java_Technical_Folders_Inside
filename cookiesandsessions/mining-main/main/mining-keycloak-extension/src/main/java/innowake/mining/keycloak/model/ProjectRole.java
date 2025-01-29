/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak.model;

import java.util.Collections;
import java.util.List;

/**
 * POJO Class to represent the Project Roles that a user can have.
 */
public class ProjectRole {

	private final Long projectId;
	private final UserRole userRole;
	private final List<ProjectNature> projectNatures;

	/**
	 * Default constructor that is needed by Jackson for serialization/deserialization.
	 * JsonCreator annotations would not work.
	 */
	public ProjectRole() {
		projectId = Long.valueOf(0);
		userRole = null;
		projectNatures = Collections.emptyList();
	}

	/**
	 * Instantiates {@link ProjectRole Project Role} for a Member.
	 * 
	 * @param projectId The ID of the Project
	 * @param userRole The {@link UserRole User Role} assigned for the given Project
	 * @param projectNatures The list of {@link ProjectNature Project Natures} assigned for the given Project
	 */
	public ProjectRole(final Long projectId, final UserRole userRole, final List<ProjectNature> projectNatures) {
		this.projectId = projectId;
		this.userRole = userRole;
		this.projectNatures = projectNatures;
	}

	/**
	 * Gets the project Id which is used to uniquely identify the project to which
	 * the {@link ProjectRole Project Role} belongs.
	 *
	 * @return Project ID
	 */
	public Long getProjectId() {
		return projectId;
	}

	/**
	 * Gets the User Role for the specified Project.
	 *
	 * @return The {@link UserRole User Role}
	 */
	public UserRole getUserRole() {
		return userRole;
	}

	/**
	 * Gets the Project Natures for the specified Project.
	 *
	 * @return Unmodifiable List of {@link ProjectNature Project Natures}
	 */
	public List<ProjectNature> getProjectNatures() {
		return projectNatures != null ? Collections.unmodifiableList(projectNatures) : Collections.emptyList();
	}
}
