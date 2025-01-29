/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.keycloak.models.RoleModel;

/**
 * POJO Class to represent a User
 */
public class Member {

	private static final String ROLE_REGEX = "client-\\d+-project-\\d+-";

	private final String id;
	private final String firstName;
	private final String lastName;
	private final String email;
	private final List<ProjectRole> projectRoles;

	/**
	 * Default constructor that is needed by Jackson for serialization/deserialization.
	 * JsonCreator annotations would not work.
	 */
	public Member() {
		id = StringUtils.EMPTY;
		firstName = StringUtils.EMPTY;
		lastName = StringUtils.EMPTY;
		email = StringUtils.EMPTY;
		projectRoles = Collections.emptyList();
	}

	/**
	 * Instantiates a {@link Member}.
	 * 
	 * @param id The ID of the Member
	 * @param firstName The First Name of the Member
	 * @param lastName The Last Name of the Member
	 * @param email The Email ID of the Member
	 * @param keycloakRoles The roles assigned to the Member
	 */
	public Member(final String id, final String firstName, final String lastName, final String email, final List<RoleModel> keycloakRoles) {
		this.id = id;
		this.firstName = firstName;
		this.lastName = lastName;
		this.email = email;
		this.projectRoles = getProjectRoles(keycloakRoles);
	}

	/**
	 * Gets the ID of the user.
	 *
	 * @return ID of the user
	 */
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
		return Collections.unmodifiableList(projectRoles);
	}

	private List<ProjectRole> getProjectRoles(final List<RoleModel> keycloakRoles) {
		final List<ProjectRole> roles = new ArrayList<>();
		final Map<Long, UserRole> userRoleMap = new HashMap<>();
		final Map<Long, List<ProjectNature>> projectNaturesMap = new HashMap<>();
		final Set<Long> projectIds = new HashSet<>();
		final Set<RoleModel> compositeRoles = new HashSet<>();
		keycloakRoles.stream().filter(RoleModel::isComposite).flatMap(RoleModel::getCompositesStream).forEach(compositeRoles::add);
		keycloakRoles.addAll(compositeRoles);
		keycloakRoles.stream().filter(role -> ! role.isComposite()).forEach(role -> {
			final String roleName = role.getName();
			final String[] projectRoleArr = roleName.split("-");
			if (projectRoleArr.length >= 5) {
				final Long projectId = Long.valueOf(projectRoleArr[3]);
				projectIds.add(projectId);
				if (Pattern.compile(ROLE_REGEX).matcher(roleName).find()) {
					final String roleToParse = roleName.replaceFirst(ROLE_REGEX, "").replace("-", "_");
					final Optional<UserRole> userRole = UserRole.fromName(roleToParse);
					if (userRole.isPresent()) {
						userRoleMap.put(projectId, userRole.get());
					}
					final Optional<ProjectNature> projectNature = ProjectNature.fromName(roleToParse);
					if (projectNature.isPresent()) {
						projectNaturesMap.computeIfAbsent(projectId, key -> new ArrayList<>()).add(projectNature.get());
					}
				}
			}
		});
		for (final Long projectId : projectIds) {
			roles.add(new ProjectRole(projectId, userRoleMap.get(projectId), projectNaturesMap.get(projectId)));
		}
		return roles;
	}
}
