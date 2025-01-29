/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.StringUtils;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;

/**
 * POJO class to represent a Keycloak User.
 */
public class Member {

	@Nullable
	private final String id;
	private final String firstName;
	private final String lastName;
	private final String email;
	private final List<ProjectRole> projectRoles;

	/**
	 * Instantiates a {@link Member} with specified properties.
	 * 
	 * @param id The ID of the Member
	 * @param firstName The First Name of the Member
	 * @param lastName The Last Name of the Member
	 * @param email The Email ID of the Member
	 * @param projectRoles The list of {@link ProjectRole} assigned to the Member
	 */
	@JsonCreator
	public Member(@Nullable @JsonProperty("id") final String id, @JsonProperty("firstName") final String firstName, @JsonProperty("lastName") final String lastName,
			@JsonProperty("email") final String email, @JsonProperty("projectRoles") final List<ProjectRole> projectRoles) {
		this.id = id;
		this.firstName = firstName;
		this.lastName = lastName;
		this.email = StringUtils.trimToEmpty(email);
		this.projectRoles = projectRoles;
	}

	/**
	 * Gets the ID of the user.
	 *
	 * @return ID of the user
	 */
	@Nullable
	public String getId() {
		return id;
	}

	/**
	 * Gets the first name of the user.
	 *
	 * @return First name of the user
	 */
	public String getFirstName() {
		return firstName;
	}

	/**
	 * Gets the last name of the user.
	 *
	 * @return Last name of the user
	 */
	public String getLastName() {
		return lastName;
	}

	/**
	 * Gets the Email ID of the user
	 *
	 * @return Email ID of the user
	 */
	public String getEmail() {
		return email;
	}

	/**
	 * Gets the Project Roles assigned to the user.
	 *
	 * @return Unmodifiable List of {@link ProjectRole Project Roles} assigned to the user 
	 */
	public List<ProjectRole> getProjectRoles() {
		return Collections.unmodifiableList(this.projectRoles);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + email.hashCode();
		final String idCopy = id;
		if(idCopy != null) {
			result = prime * result + idCopy.hashCode();
		}
		return result;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		final Member other = (Member) obj;
		if ( ! email.equals(other.email)) {
			return false;
		}
		final String idCopy = id;
		if (idCopy == null) {
			if (other.id != null) {
				return false;
			}
		} else if (! idCopy.equals(other.id)) {
			return false;
		}
		return true;
	}
}