/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.Collections;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * POJO Class to represent the Project Roles that a user can have.
 */
public class ProjectRole {

	private final Long projectId;
	private final UserRole userRole;
	private final List<ProjectNature> projectNatures;

	/**
	 * Instantiates {@link ProjectRole Project Role} with specified properties.
	 * 
	 * @param projectId The ID of the Project
	 * @param userRole The {@link UserRole User Role} for given Project
	 * @param projectNatures The list of {@link ProjectNature Project Natures} for given Project
	 */
	@JsonCreator
	public ProjectRole(@JsonProperty("projectId") final Long projectId, @JsonProperty("userRole") final UserRole userRole,
			@JsonProperty("projectNatures") final List<ProjectNature> projectNatures) {
		this.projectId = projectId;
		this.userRole = userRole;
		this.projectNatures = projectNatures;
	}

	/**
	 * Gets the project Id which is used to uniquely identify the project
	 * to which the {@link ProjectRole Project Role} belongs.
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
		return Collections.unmodifiableList(projectNatures);
	}
}
