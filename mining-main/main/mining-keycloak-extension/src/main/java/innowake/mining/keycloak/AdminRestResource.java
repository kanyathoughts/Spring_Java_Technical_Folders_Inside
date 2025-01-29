/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.Spliterators;
import java.util.regex.Matcher;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.keycloak.models.GroupModel;
import org.keycloak.models.KeycloakSession;
import org.keycloak.models.RealmModel;
import org.keycloak.models.RoleModel;
import org.keycloak.models.UserModel;
import org.keycloak.models.UserModel.RequiredAction;
import org.keycloak.services.managers.AppAuthManager;
import org.keycloak.services.managers.AuthenticationManager;
import org.keycloak.storage.GroupStorageManager;

import innowake.mining.keycloak.model.Member;
import innowake.mining.keycloak.model.PaginatedResponse;
import innowake.mining.keycloak.model.ProjectNature;
import innowake.mining.keycloak.model.ProjectRole;
import innowake.mining.keycloak.model.UserRole;
import innowake.mining.keycloak.model.UserRoleModelMapper;
import innowake.mining.keycloak.util.PatternStreamer;

import jakarta.ws.rs.*;
import jakarta.ws.rs.core.*;

/**
 * Extended Keycloak endpoints to aid in authorization management.
 */
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class AdminRestResource {

	private static final String CLIENT_PREFIX = "client-";
	private static final String CLIENT_ROLE = CLIENT_PREFIX + "%d";
	private static final String CLIENT_ROLES_GROUP = CLIENT_PREFIX + "%d-roles";
	private static final String CLIENT_USERS_GROUP = CLIENT_PREFIX + "%d-users";
	private static final String CLIENT_ADMIN_ROLE = CLIENT_PREFIX + "%d-admin";
	private static final String PROJECT_ROLE = CLIENT_PREFIX + "%d-project-%d";
	private static final String SUPER_ADMIN_ROLE = "admin";
	private static final String NATURES_POSTFIX = "-natures";
	private static final String INVALID_GROUP = "Invalid group name: %s";
	private static final int RECORDS_LIMIT = Integer.MAX_VALUE - 1;
	private static final PatternStreamer CLIENT_ADMIN_PATTERN = new PatternStreamer("(client-[\\d]+)-admin");
	private static final PatternStreamer PROJECT_ROLE_PATTERN = new PatternStreamer("(client-([\\d]+))-project-([\\d]+)-");

	private final KeycloakSession session;
	private final AuthenticationManager.AuthResult auth;
	private final RealmModel realm;
	private final GroupStorageManager groupStorageManager;

	/**
	 * Constructor to instantiate {@link AdminRestResource}.
	 * 
	 * @param session The {@link KeycloakSession}
	 */
	public AdminRestResource(final KeycloakSession session) {
		this.session = session;
		realm = session.getContext().getRealm();
		auth = new AppAuthManager.BearerTokenAuthenticator(session).setRealm(realm).authenticate();
		groupStorageManager = new GroupStorageManager(session);
	}

	/**
	 * Endpoint that returns paginated users belonging to specified Client.
	 *
	 * @param clientId The ID of the Client
	 * @param page The page number
	 * @param size The number of {@link Member Members} to return. If the size is {@code 0}, it returns all entries in a single page
	 * @return {@link PaginatedResponse Paginated Response} of {@link Member Members} that are part of specified Client
	 */
	@Path("clients/{clientId}/members")
	@GET
	public PaginatedResponse<Member> getAllClientMembers(
			@PathParam("clientId") final Long clientId,
			@QueryParam("page") final int page,
			@QueryParam("size") final int size) {
		ensureClientAdmin(clientId);
		return getPaginatedUserIdsByGroup(String.format(CLIENT_USERS_GROUP, clientId), page, size);
	}

	/**
	 * Endpoint that returns the count of Members that have roles for given Client.
	 *
	 * @param clientId The ID of the Client
	 * @return The number of Members that have the roles for Client.
	 */
	@Path("clients/{clientId}/members/count")
	@GET
	public int getClientMemberCount(@PathParam("clientId") final Long clientId) {
		ensureClientAdmin(clientId);
		return getUserIdsByGroup(String.format(CLIENT_USERS_GROUP, clientId)).size();
	}

	/**
	 * Endpoint that returns paginated users belonging to specified Client and Project.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 * @param page The page number
	 * @param size The number of {@link Member Members} to return. If the size is {@code 0}, it returns all entries in a single page
	 * @return {@link PaginatedResponse Paginated Response} of {@link Member Members} that belong to specified Client and Project
	 */
	@Path("clients/{clientId}/projects/{projectId}/members")
	@GET
	public PaginatedResponse<Member> getAllProjectMembers(
			@PathParam("clientId") final Long clientId,
			@PathParam("projectId") final Long projectId,
			@QueryParam("page") final int page,
			@QueryParam("size") final int size) {
		ensureClientAdmin(clientId);
		return getPaginatedUsersByRole(String.format(PROJECT_ROLE, clientId, projectId), page, size);
	}

	/**
	 * Endpoint that returns the count of Members that have the roles for given Client and Project.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 * @return The number of Members that have the roles for given Client and Project
	 */
	@Path("clients/{clientId}/projects/{projectId}/members/count")
	@GET
	public int getProjectMembersCount(@PathParam("clientId") final Long clientId, @PathParam("projectId") final Long projectId) {
		ensureClientAdmin(clientId);
		return getAllUserIdsByRole(String.format(PROJECT_ROLE, clientId, projectId)).size();
	}

	/**
	 * Endpoint that returns a {@link Member} of specified ID that has access to specified Client and Project.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 * @param userId The ID of the User
	 * @return {@link Response} wrapping the {@link Member} of given ID
	 * @throws NotFoundException if the given User does not have the required Project Roles
	 */
	@Path("clients/{clientId}/projects/{projectId}/members/{userId}")
	@GET
	public Response getMember(
			@PathParam("clientId") final Long clientId,
			@PathParam("projectId") final Long projectId,
			@PathParam("userId") final String userId) {
		ensureClientAdmin(clientId);
		final Set<String> userIdsWithRole = getAllUserIdsByRole(String.format(PROJECT_ROLE, clientId, projectId));
		if (userIdsWithRole.contains(userId)) {
			final List<Member> members = getMembersFromUserIds(Arrays.asList(userId));
			return Response.ok(members.get(0)).build();
		}
		throw new NotFoundException("No member found");
	}

	/**
	 * Endpoint that returns paginated list of {@link Member} that has admin access for the specified Client.
	 *
	 * @param clientId The ID of the client
	 * @param page The page number
	 * @param size The number of {@link Member Members} to return. If the size is {@code 0}, it returns all entries in a single page
	 * @return {@link PaginatedResponse Paginated Response} of {@link Member Members} that has admin access for specified Client
	 */
	@Path("clients/{clientId}/admins")
	@GET
	public PaginatedResponse<Member> getClientAdmins(
			@PathParam("clientId") final Long clientId,
			@QueryParam("page") final int page,
			@QueryParam("size") final int size) {
		ensureClientAdmin(clientId);
		return getPaginatedUsersByRole(String.format(CLIENT_ADMIN_ROLE, clientId), page, size);
	}

	/**
	 * Endpoint that checks if a group with the given {@code clientId} exists.
	 *
	 * @param clientId id to look for
	 * @return {@code true} if the id was found
	 */
	@Path("clients/{clientId}")
	@GET
	public boolean doesClientGroupExist(@PathParam("clientId") final Long clientId) {
		ensureUserIsSuperAdmin();
		final String groupName = String.format(CLIENT_USERS_GROUP, clientId);
		final Long groupCount = realm.getGroupsCountByNameContaining(groupName);
		return groupCount != null && groupCount.longValue() > 0;
	}

	/**
	 * Endpoint to create Client attributes - Client Roles, User Groups and Client Admin Role.
	 *
	 * @param clientId The ID of the Client
	 */
	@Path("clients/{clientId}")
	@POST
	public void createClientAttributes(@PathParam("clientId") final Long clientId) {
		ensureUserIsSuperAdmin();
		realm.createGroup(String.format(CLIENT_USERS_GROUP, clientId));
		realm.addRole(String.format(CLIENT_ADMIN_ROLE, clientId));
		realm.addRole(String.format(CLIENT_ROLES_GROUP, clientId));
	}

	/**
	 * Endpoint to create Project Attributes - Project User Roles and Natures.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 * @param projectNatures The default Project Natures
	 * @return The {@link Response} with appropriate status code
	 */
	@Path("clients/{clientId}/projects/{projectId}")
	@POST
	public Response createProjectAttributes(
			@PathParam("clientId") final Long clientId,
			@PathParam("projectId") final Long projectId,
			@QueryParam("projectNatures") final String projectNatures) {
		ensureClientAdmin(clientId);
		final String rolePrefix = String.format(PROJECT_ROLE, clientId, projectId);
		Arrays.stream(UserRole.values())
		      .filter(role -> ! role.name().equalsIgnoreCase(SUPER_ADMIN_ROLE))
		      .map(role -> rolePrefix + "-" + role.name().toLowerCase())
		      .forEach(realm::addRole);
		final RoleModel compositeNaturesRole = realm.addRole(rolePrefix + NATURES_POSTFIX);
		final Set<ProjectNature> projectDefaultNatures = new HashSet<>();
		Arrays.stream(projectNatures.split(","))
		      .map(ProjectNature::fromName)
		      .filter(Optional::isPresent)
		      .map(Optional::get)
		      .forEach(projectDefaultNatures::add);
		Arrays.stream(ProjectNature.values())
			.forEach(role -> {
				final String roleStr = role.getAsKeycloakRole(rolePrefix);
				final RoleModel natureRole = realm.addRole(roleStr);
				if (projectDefaultNatures.contains(role)) {
					compositeNaturesRole.addCompositeRole(natureRole);
				}
			});
		return Response.status(Response.Status.CREATED).build();
	}

	/**
	 * Endpoint that checks if there are any roles for the given {@code clientId} and {@code projectid}.
	 *
	 * @param clientId id of the client
	 * @param projectId id of the project
	 * @return {@code true} if there is any role for the given ids
	 */
	@Path("clients/{clientId}/projects/{projectId}")
	@GET
	public boolean doProjectRolesExist(
			@PathParam("clientId") final Long clientId,
			@PathParam("projectId") final Long projectId) {
		ensureUserIsSuperAdmin();
		final String projectName = String.format(PROJECT_ROLE, clientId, projectId);
		return realm.searchForRolesStream(projectName, Integer.valueOf(0), Integer.valueOf(1)).findFirst().isPresent();
	}

	/**
	 * Endpoint to get the default {@link ProjectNature project natures} assigned to the specified Project.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 * @return The Set of {@link ProjectNature project natures} assigned to the project
	 */
	@Path("clients/{clientId}/projects/{projectId}/projectNatures")
	@GET
	public Set<ProjectNature> findProjectNatures(
			@PathParam("clientId") final Long clientId,
			@PathParam("projectId") final Long projectId) {
		ensureClientAdmin(clientId);
		final String rolePrefix = String.format(PROJECT_ROLE, clientId, projectId);
		return realm.getRole(rolePrefix + NATURES_POSTFIX).getCompositesStream()
				.map(roleModel -> ProjectNature.getFromKeycloakRoleModel(roleModel, rolePrefix))
				.collect(Collectors.toSet());
	}

	/**
	 * Endpoint to change the default {@link ProjectNature project natures} for the specified Project.
	 * This also changes the nature roles assigned to the project users replacing the default roles with the new ones being assigned.
	 * All users' "custom" natures are left untouched, this means that for every user we only remove/change the natures that are part of the default set.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 * @param projectNatures The array of {@link ProjectNature project natures} to assign
	 */
	@Path("clients/{clientId}/projects/{projectId}")
	@PUT
	public void changeProjectNatures(
			@PathParam("clientId") final Long clientId,
			@PathParam("projectId") final Long projectId,
			@QueryParam("projectNatures") final String projectNatures) {
		ensureClientAdmin(clientId);
		final String rolePrefix = String.format(PROJECT_ROLE, clientId, projectId);
		final RoleModel compositeNaturesRole = realm.getRole(rolePrefix + NATURES_POSTFIX);
		final Set<RoleModel> newProjectNatureRoles = Arrays.stream(projectNatures.split(","))
				.map(ProjectNature::fromName)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.map(projectNatureKeycloakRole -> projectNatureKeycloakRole.getAsKeycloakRole(rolePrefix))
				.map(realm::getRole)
				.filter(Objects::nonNull)
				.collect(Collectors.toSet());
		
		/* Fetch the users which have default nature roles as the project. */ 
		final Set<String> projectUsersIds = compositeNaturesRole.getCompositesStream()
				.flatMap(roleModel -> session.users().getRoleMembersStream(realm, roleModel))
				.map(UserModel::getId)
				.collect(Collectors.toSet());
		/*
		 * Apply the new default nature roles to the users associated with the project.
		 * Separate UserModal is required to delete role mappings and for grant role to flush the changes properly to the keycloak backend
		 * so there are separate streams created respectively for these tasks.
		 * Note: Deleting as well as Granting the roles in a single stream makes use of the same UserEntity causing changes to not reflect immediately
		 * in the keycloak backend unless 
		 * a) keycloak server is restarted for changes to reflect for all the project users.
		 * b) each user of the project manually logs in, goto 'sessions' tab and clicks on 'Log out all sessions' button for changes to reflect for
		 * that particular user.
		 */
		projectUsersIds.stream().map(id -> session.users().getUserById(realm, id)).forEach(user -> compositeNaturesRole.getCompositesStream().forEach(user::deleteRoleMapping));
		projectUsersIds.stream().map(id -> session.users().getUserById(realm, id)).forEach(user -> newProjectNatureRoles.forEach(user::grantRole));
		
		/* Apply the new default nature roles to the project. */
		compositeNaturesRole.getCompositesStream().forEach(compositeNaturesRole::removeCompositeRole);
		newProjectNatureRoles.forEach(compositeNaturesRole::addCompositeRole);
	}

	/**
	 * Endpoint to delete Project Roles for the given Client ID and Project ID.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 */
	@Path("clients/{clientId}/projects/{projectId}")
	@DELETE
	public void deleteProjectRoles(
			@PathParam("clientId") final Long clientId,
			@PathParam("projectId") final Long projectId) {
		ensureClientAdmin(clientId);
		final String rolePrefix = String.format(PROJECT_ROLE, clientId, projectId);
		Arrays.stream(UserRole.values())
		      .filter(role -> ! role.name().equalsIgnoreCase(SUPER_ADMIN_ROLE))
		      .map(role -> role.getAsKeycloakRole(rolePrefix))
		      .map(realm::getRole)
		      .forEach(realm::removeRole);
		Arrays.stream(ProjectNature.values())
		      .map(role -> role.getAsKeycloakRole(rolePrefix))
		      .map(realm::getRole)
		      .forEach(realm::removeRole);
		final RoleModel projectNaturesRole = realm.getRole(rolePrefix + NATURES_POSTFIX);
		realm.removeRole(projectNaturesRole);
	}

	/**
	 * Endpoint to delete all Client related roles and groups for the given Client.
	 *
	 * @param clientId The ID of the Client
	 */
	@Path("clients/{clientId}")
	@DELETE
	public void deleteClientAttributes(
			@PathParam("clientId") final Long clientId) {
		ensureUserIsSuperAdmin();
		groupStorageManager.searchForGroupByNameStream(realm, String.format(CLIENT_USERS_GROUP, clientId), Boolean.TRUE, Integer.valueOf(0), Integer.valueOf(1))
		     .forEach(realm::removeGroup);
		realm.searchForRolesStream(String.format(CLIENT_ROLE, clientId), Integer.valueOf(0), Integer.valueOf(RECORDS_LIMIT))
		     .forEach(realm::removeRole);
	}

	/**
	 * Endpoint to grant the given {@link ProjectRole Project Role} to the given {@link Member Member}.
	 * If the given {@link Member Member} does not exist in Keycloak, a new User will be created.
	 * From the {@link Member Member} object, we are assigning the provided {@link UserRole User Role}
	 * and adding the given {@link ProjectNature Project Natures} in addition to the default {@link ProjectNature Project Natures}.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 * @param member The {@link Member}
	 * @return The {@link Response} wrapping the {@link Member} that was granted the access
	 * @throws BadRequestException if the given {@link Member} does not 
	 *         contain {@link ProjectRole Project Roles} for the given Project ID
	 */
	@Path("clients/{clientId}/projects/{projectId}/members")
	@POST
	public Response addProjectRolesToMember(
			@PathParam("clientId") final Long clientId,
			@PathParam("projectId") final Long projectId,
			final Member member) {
		ensureClientAdmin(clientId);
		final UserModel user = getOrCreateUser(clientId, member);
		final List<ProjectRole> memberProjectRoles = member.getProjectRoles();
		/* Extracting the project specific roles that need to be assigned to the User from the Member object. */
		final Optional<ProjectRole> rolesToAssignOp = memberProjectRoles.stream().filter(role -> projectId.equals(role.getProjectId())).findFirst();
		if (rolesToAssignOp.isPresent()) {
			final ProjectRole projectRolesToAssign = rolesToAssignOp.get();
			final String rolePrefix = String.format(PROJECT_ROLE, clientId, projectId);
			/* Removing already existing UserRole that may have been assigned to the User. */
			Arrays.stream(UserRole.values())
			      .map(userRole -> userRole.getAsKeycloakRole(rolePrefix))
			      .map(realm::getRole)
			      .forEach(user::deleteRoleMapping);
			/* Assigning the specified UserRole to the User. */
			grantUserRoleToMember(projectRolesToAssign, clientId, projectId, user);
			/* Assigning the default project nature roles to the User. */
			final RoleModel defaultProjectNatures = realm.getRole(rolePrefix + NATURES_POSTFIX);
			defaultProjectNatures.getCompositesStream().forEach(user::grantRole);
			/* Assigning any other ProjectNature roles that may have been specified in the Member object to the user. */
			projectRolesToAssign.getProjectNatures()
				.stream()
				.map(nature -> nature.getAsKeycloakRole(rolePrefix))
				.map(realm::getRole)
				.forEach(user::grantRole);
			return Response.ok(new Member(user.getId(), user.getFirstName(), user.getLastName(), user.getEmail(),
					user.getRoleMappingsStream().collect(Collectors.toList()))).build();
		}
		/* An exception is thrown if there are no project specific roles provided in the Member for the given project ID. */
		throw new BadRequestException("Invalid member provided");
	}

	/**
	 * Endpoint to add a user as a Client Admin for specified Client.
	 * If the user exists, then the Client Admin role is assigned,
	 * else the user is created and added as a Client Admin.
	 *
	 * @param clientId The Client ID
	 * @param member The member to be added as Client Admin
	 * @return The {@link Response} wrapping the {@link Member} that was granted the Client Admin access to the given Client
	 * @throws NotFoundException if there are no Client Admin roles associated with the given Client ID
	 */
	@Path("clients/{clientId}/admins")
	@POST
	public Response addMemberAsClientAdmin(@PathParam("clientId") final Long clientId, final Member member) {
		ensureClientAdmin(clientId);
		final UserModel user = getOrCreateUser(clientId, member);
		final String clientAdminId = String.format(CLIENT_ADMIN_ROLE, clientId);
		final RoleModel role = realm.getRole(clientAdminId);
		if (role == null) {
			throw new NotFoundException("No role found " + clientAdminId);
		}
		user.grantRole(role);
		return Response.ok(new Member(user.getId(), user.getFirstName(), user.getLastName(), user.getEmail(),
				user.getRoleMappingsStream().collect(Collectors.toList()))).build();
	}

	/**
	 * Endpoint to remove a user as a Client Admin for specified client.
	 *
	 * @param clientId The Client ID
	 * @param userId The User ID
	 */
	@Path("clients/{clientId}/admins/{userId}")
	@DELETE
	public void deleteMemberAsClientAdmin(@PathParam("clientId") final Long clientId, @PathParam("userId") final String userId) {
		ensureClientAdmin(clientId);
		final UserRoleModelMapper value = getUserRole(clientId, userId);
		final UserModel user = value.getUser();
		final RoleModel role = value.getRole();
		user.deleteRoleMapping(role);
	}

	/**
	 * Endpoint to remove Project Roles for the given {@link Member}.
	 * The Project Roles are identified by the given Client ID and Project ID.
	 *
	 * @param clientId The Client ID
	 * @param projectId The Project ID
	 * @param userId The User ID
	 * @throws NotFoundException if no User was found for the given User ID
	 * @throws BadRequestException if the User was not previously assigned the specified Project Roles
	 */
	@Path("clients/{clientId}/projects/{projectId}/members/{userId}")
	@DELETE
	public void deleteMemberFromProject(@PathParam("clientId") final Long clientId,
			@PathParam("projectId") final Long projectId, @PathParam("userId") final String userId) {
		ensureClientAdmin(clientId);
		final UserModel user = session.users().getUserById(realm, userId);
		if (user == null) {
			throw new NotFoundException("No user found with id " + userId);
		}
		final String rolePrefix = String.format(PROJECT_ROLE, clientId, projectId);
		final List<RoleModel> userRolesToRemove = user.getRoleMappingsStream()
				.filter(role -> role.getName().startsWith(rolePrefix)).filter(user::hasRole)
				.collect(Collectors.toList());
		if (userRolesToRemove.isEmpty()) {
			throw new BadRequestException("Given user is not assigned to the Project");
		}
		userRolesToRemove.forEach(user::deleteRoleMapping);
	}

	/**
	 * Endpoint to assign the given {@link ProjectRole Project Role} to the given {@link Member Member}.
	 * Assigning new project roles would override user role while project natures are added for the given user.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 * @param userId The ID of the User
	 * @param member The {@link Member}
	 * @return {@link Response} wrapping the {@link Member} with updated {@link ProjectRole Project Role}
	 * @throws NotFoundException if no User was found for the given User ID
	 */
	@Path("clients/{clientId}/projects/{projectId}/members/{userId}")
	@PUT
	public Response assignProjectRoleToMember(@PathParam("clientId") final Long clientId, @PathParam("projectId") final Long projectId,
			@PathParam("userId") final String userId, final Member member) {
		ensureClientAdmin(clientId);
		final UserModel user = session.users().getUserById(realm, userId);
		if (user == null) {
			throw new NotFoundException("No user found with id " + userId);
		}
		final ProjectRole projectRole = member.getProjectRoles().stream()
				.filter(role -> role.getProjectId().equals(projectId)).findFirst()
				.orElseThrow(() -> new BadRequestException("No project role found for project id :" + projectId));
		final String rolePrefix = String.format(PROJECT_ROLE, clientId, projectId);
		user.getRoleMappingsStream()
			.filter(userRole -> isValidUserProjectRole(rolePrefix, userRole))
			.forEach(user::deleteRoleMapping);
		grantUserRoleToMember(projectRole, clientId, projectId, user);
		projectRole.getProjectNatures().stream()
			.map(projectNature -> projectNature.getAsKeycloakRole(rolePrefix))
			.map(realm::getRole)
			.forEach(user::grantRole);
		return Response.ok(new Member(user.getId(), user.getFirstName(), user.getLastName(), user.getEmail(),
				user.getRoleMappingsStream().collect(Collectors.toList()))).build();
	}
	
	/**
	 * Endpoint to retrieve user information by the give user ID, if the user is assigned to same project as the current user.
	 * If the id is not found, a {@link NotFoundException} is thrown.<br/>
	 * If the requested user is a client admin and current user belong's to same client then the member information is returned.
	 * If the current user and requested user do not belong to same project or does not have access to view the requested user information, 
	 * then {@link ForbiddenException} is thrown.
	 *
	 * @param userId the id of the requested user
	 * @return the information about the requested user
	 */
	@Path("users/{userId}")
	@GET
	public Response getMember(@PathParam("userId") final String userId) {
		final UserModel requestedUser = session.users().getUserById(realm, userId);
		if (requestedUser == null) {
			throw new NotFoundException("No user found with id " + userId);
		}
		final Member member = new Member(requestedUser.getId(), requestedUser.getFirstName(), requestedUser.getLastName(), requestedUser.getEmail(),
				requestedUser.getRoleMappingsStream().collect(Collectors.toList()));
		if (isSuperUser()) {
			return Response.ok(member).build();
		}
		final Set<String> userRoles = auth.getToken().getRealmAccess().getRoles();
		if (hasAccessToClientAdminMember(requestedUser, userRoles)) {
			return Response.ok(member).build();
		}
		final Set<Long> userProjectIds = new HashSet<>();
		userRoles.forEach(role -> {
			final Matcher clientAdminMatcher = CLIENT_ADMIN_PATTERN.getPattern().matcher(role);
			if (clientAdminMatcher.find()) {
				final String clientId = clientAdminMatcher.group(1);
				realm.getRolesStream()
								.map(RoleModel::getName)
								.filter(projectRole -> projectRole.startsWith(clientId))
								.map(this::getProjectIdFromRole)
								.filter(Objects::nonNull)
								.forEach(userProjectIds::add);
			} else {
				final Long projectId  = getProjectIdFromRole(role);
				if (projectId != null) {
					userProjectIds.add(projectId);
				}
			}
		});
		final boolean hasAccess = member.getProjectRoles().stream()
													.map(ProjectRole::getProjectId)
													.anyMatch(userProjectIds::contains);
		if (hasAccess) {
			return Response.ok(member).build();
		}
		throw new ForbiddenException("No access");
	}

	/**
	 * Returns true if the requested user is a client admin and current user belongs to same client.
	 *
	 * @param requesterUser the requested user
	 * @param userRoles current users project roles
	 * @return true if the requested user is a client admin and current user belongs to same client
	 */
	private boolean hasAccessToClientAdminMember(final UserModel requesterUser, final Set<String> userRoles) {
		final Set<String> requestedUserRoles = requesterUser.getRoleMappingsStream().map(RoleModel::getName)
												.flatMap(CLIENT_ADMIN_PATTERN::results)
												.map(role -> role.group(1))
												.collect(Collectors.toSet());
		if ( ! requestedUserRoles.isEmpty()) {
			final Set<String> matchedIds = userRoles.stream()
											.flatMap(PROJECT_ROLE_PATTERN::results)
											.map(role -> role.group(1))
											.filter(requestedUserRoles::contains)
											.collect(Collectors.toSet());
			return ! matchedIds.isEmpty();
		}
		return false;
	}

	private Long getProjectIdFromRole(final String projectRole) {
		final Matcher projectRoleMatcher = PROJECT_ROLE_PATTERN.getPattern().matcher(projectRole);
		if (projectRoleMatcher.find()) {
			return Long.valueOf(projectRoleMatcher.group(3));
		}
		return null;
	}

	private void ensureClientAdmin(final Long clientId) {
		if ( ! isClientAdmin(clientId) && ! isSuperUser()) {
			throw new ForbiddenException("No client admin role");
		}
	}

	private void ensureUserIsSuperAdmin() {
		if ( ! isSuperUser()) {
			throw new ForbiddenException("No super admin role");
		}
	}

	private boolean isClientAdmin(final Long clientId) {
		ensureUserHasRealmAccess();
		final Set<String> roles = auth.getToken().getRealmAccess().getRoles();
		return roles.stream().anyMatch(role -> role.equals(String.format(CLIENT_ADMIN_ROLE, clientId)));
	}

	private boolean isSuperUser() {
		ensureUserHasRealmAccess();
		final Set<String> roles = auth.getToken().getRealmAccess().getRoles();
		return roles.stream().anyMatch(role -> role.equals(SUPER_ADMIN_ROLE));
	}

	private void ensureUserHasRealmAccess() {
		if (auth == null) {
			throw new NotAuthorizedException("Bearer");
		} else if (auth.getToken().getRealmAccess() == null) {
			throw new ForbiddenException("No realm access");
		}
	}

	private Set<String> getAllUserIdsByRole(final String testRole) {
		final Iterator<RoleModel> roles = realm.searchForRolesStream(testRole, Integer.valueOf(0), Integer.valueOf(RECORDS_LIMIT)).iterator();
		if ( ! roles.hasNext()) {
			throw new BadRequestException("Invalid Role: " + testRole);
		}
		return StreamSupport.stream(Spliterators.spliteratorUnknownSize(roles, 0), false)
				.flatMap(role -> session.users().getRoleMembersStream(realm, role))
				.map(UserModel::getId)
				.collect(Collectors.toSet());
	}

	private PaginatedResponse<Member> getPaginatedUsersByRole(final String testRole, final int page, final int size) {
		final int firstResult = page * size;
		final int maxResults = size == 0 ? RECORDS_LIMIT : size;
		final Set<String> userIds = getAllUserIdsByRole(testRole);
		final int userIdsCount = userIds.size();
		if (userIdsCount > firstResult) {
			final List<String> paginatedUserIds = userIds.stream()
														 .skip(firstResult)
														 .limit(maxResults)
														 .collect(Collectors.toList());
			return new PaginatedResponse<>(getMembersFromUserIds(paginatedUserIds), userIdsCount);
		}
		return new PaginatedResponse<>(Collections.emptyList(), userIdsCount);
	}

	private List<String> getUserIdsByGroup(final String testGroup) {
		final GroupModel clientUsersGroup = realm.getGroupsStream()
												 .filter(group -> group.getName().equals(testGroup))
												 .findFirst()
												 .orElseThrow(() -> new BadRequestException(String.format(INVALID_GROUP, testGroup)));
		return session.users().getGroupMembersStream(realm, clientUsersGroup).map(UserModel::getId).collect(Collectors.toList());
	}

	private PaginatedResponse<Member> getPaginatedUserIdsByGroup(final String testGroup, final int page, final int size) {
		final List<String> userIdsInGroup = getUserIdsByGroup(testGroup);
		final int totalSize = userIdsInGroup.size();
		final int firstResult = page * size;
		final int maxResults = size == 0 ? RECORDS_LIMIT : size;
		if (totalSize > firstResult) {
			final List<String> paginatedUserIds = userIdsInGroup.stream()
			                                                    .skip(firstResult)
			                                                    .limit(maxResults)
			                                                    .collect(Collectors.toList());
			return new PaginatedResponse<>(getMembersFromUserIds(paginatedUserIds), totalSize);
		}
		return new PaginatedResponse<>(Collections.emptyList(), 0);
	}

	private void grantUserRoleToMember(final ProjectRole projectRole, final Long clientId, final Long projectId, final UserModel user) {
		final String rolePrefix = String.format(PROJECT_ROLE, clientId, projectId);
		final UserRole userRole = projectRole.getUserRole();
		if (userRole != UserRole.ADMIN) {
			final RoleModel realmRole = realm.getRole(userRole.getAsKeycloakRole(rolePrefix));
			user.grantRole(realmRole);
		}
	}

	private UserModel createUser(final Long clientId, final String userEmail) {
		ensureClientAdmin(clientId);
		final UserModel user = session.users().addUser(realm, userEmail);
		user.setEmail(userEmail);
		user.setEnabled(true);
		user.addRequiredAction(RequiredAction.VERIFY_EMAIL);
		user.addRequiredAction(RequiredAction.UPDATE_PASSWORD);
		final Optional<GroupModel> group = realm.getGroupsStream().filter(g -> g.getName().equals(String.format(CLIENT_USERS_GROUP, clientId))).findFirst();
		if ( ! group.isPresent()) {
			throw new NotFoundException("Invalid Client Users Group!");
		}
		user.joinGroup(group.get());
		return user;
	}

	private List<Member> getMembersFromUserIds(final Collection<String> userIds) {
		return userIds.stream()
		              .map(userId -> session.users().getUserById(realm, userId))
		              .filter(Objects::nonNull)
		              .map(user -> new Member(user.getId(), user.getFirstName(), user.getLastName(), user.getEmail(),
		            		  user.getRoleMappingsStream().collect(Collectors.toList())))
		              .collect(Collectors.toList());
	}

	private boolean isValidUserProjectRole(final String userProjectRole, final RoleModel userRole) {
		final String userRoleName = userRole.getName();
		
		return userRoleName.startsWith(userProjectRole) && realm.getRole(userProjectRole + NATURES_POSTFIX).getCompositesStream().noneMatch(userRole::equals);
	}

	private GroupModel getClientUserGroup(final Long clientId) {
		final String groupName = String.format(CLIENT_USERS_GROUP, clientId);
		return groupStorageManager.searchForGroupByNameStream(realm, groupName, Boolean.TRUE, Integer.valueOf(0), Integer.valueOf(1))
				.findFirst()
				.orElseThrow(() -> new BadRequestException("Invalid group name: " + groupName));
	}

	private UserRoleModelMapper getUserRole(final Long clientId, final String userId) {
		final UserModel user = session.users().getUserById(realm, userId);
		if (user == null) {
			throw new NotFoundException("No user with id " + userId + " was found");
		}
		final String clientAdminId = String.format(CLIENT_ADMIN_ROLE, clientId);
		final RoleModel role = realm.getRole(clientAdminId);
		if (role == null) {
			throw new NotFoundException("No role found " + clientAdminId);
		}
		final GroupModel clientUsersGroup = getClientUserGroup(clientId);
		return new UserRoleModelMapper(user, role, clientUsersGroup);
	}

	private UserModel getOrCreateUser(final Long clientId, final Member member) {
		final String userEmail = member.getEmail();
		if (userEmail.isEmpty()) {
			throw new BadRequestException("User Email ID not provided");
		}
		UserModel user = session.users().getUserByEmail(realm, userEmail);
		if (user == null) {
			user = createUser(clientId, userEmail);
		}
		/* Adding member to the "client-<id>-users" group. If the user is already a part of the group then Keycloak ignores the addition. */
		final GroupModel clientUsersGroup = getClientUserGroup(clientId);
		user.joinGroup(clientUsersGroup);
		return user;
	}
}
