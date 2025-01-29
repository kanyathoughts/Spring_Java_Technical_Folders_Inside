/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.*;
import static innowake.mining.shared.security.RoleType.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.service.AuthorizationManagementService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.server.service.TryLock;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.model.Member;
import innowake.mining.shared.model.ProjectRole;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * REST Controller for {@link Member} requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class MemberController extends BaseController {
	
	private static final String URI_VAR_MEMBER_ID = "memberId";

	/**
	 * URL pattern for Client Members.
	 */
	public static final String CLIENT_MEMBERS_URL = "/v2/clients/{clientId}/members";

	/**
	 * URL pattern for Client Member count.
	 */
	public static final String CLIENT_MEMBER_COUNT_URL = "/v2/clients/{clientId}/members/count";

	/**
	 * URL pattern for Project Members.
	 */
	public static final String PROJECT_MEMBERS_URL = "/v2/projects/{projectId}/members";

	/**
	 * URL pattern for Project Member count.
	 */
	public static final String PROJECT_MEMBER_COUNT_URL = "/v2/projects/{projectId}/members/count";

	/**
	 * URL pattern for Member by ID.
	 */
	public static final String MEMBER_BY_ID_URL = "/v2/projects/{projectId}/members/{memberId}";

	/**
	 * URL pattern for Client Admins.
	 */
	public static final String CLIENT_ADMINS_URL = "/v2/clients/{clientId}/admins";

	/**
	 * URL pattern for Client Admin by ID.
	 */
	public static final String CLIENT_ADMIN_BY_ID_URL = "/v2/clients/{clientId}/admins/{memberId}";

	@Autowired
	private AuthorizationManagementService authorizationManagementService;
	
	@Override
	protected void validateUriVars(UriVarsValidation validation) {
		super.validateUriVars(validation);
		
		if (validation.needCheck(URI_VAR_PROJECT_ID, URI_VAR_MEMBER_ID)) {
			validation.pass(URI_VAR_MEMBER_ID);
		}
	}

	/**
	 * Endpoint to get all members paginated for the specified Client.
	 *
	 * @param request access to the request
	 * @param response The HTTP Response
	 * @param clientId The ID of the Client
	 * @param page The page number
	 * @param size The number of {@link Member Members} to return, all records are returned in a single page if value is 0
	 * @return Page of {@link Member Members}
	 */
	@GetMapping(value = CLIENT_MEMBERS_URL)
	@Operation(summary = "Lists all members for a Client", operationId = "findMembersForClient")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given client does not exist")
	@ApiResponse(responseCode = "405", description = "if not using Keycloak authentication")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({CLIENT_ADMIN})
	public Page<Member> findMembersForClient(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the client to search", required = true, example = "0")
			@PathVariable final Long clientId,
			@Parameter(description  = "the page number to be fetched", schema = @Schema(type="integer", format = "int32", defaultValue = "0"), required = false,
			example = "0")
			@RequestParam(value = "page", required = false, defaultValue = "0") final int page,
			@Parameter(description = "the size of each page", example = "10", schema = @Schema(type="integer", format = "int32", defaultValue = "0", 
			minimum = "0"
			,maximum = "2147483646"), required = false)
			@RequestParam(value = "size", required = false, defaultValue = "0") final int size) {
		validate(request);
		checkClientValid(EntityId.of(clientId));
		return authorizationManagementService.findMembersForClient(clientId, page, size);
	}

	/**
	 * Endpoint to get the count of all Members for the specified Client.
	 *
	 * @param request access to the request
	 * @param response The HTTP Response
	 * @param clientId The ID of the Client
	 * @return Number of Members for specified Client
	 */
	@GetMapping(value = CLIENT_MEMBER_COUNT_URL)
	@Operation(summary = "Gets the count of all members for a Client", operationId = "findMemberCountForClient")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given client does not exist")
	@ApiResponse(responseCode = "405", description = "if not using Keycloak authentication")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({CLIENT_ADMIN})
	public int findMemberCountForClient(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the client to search", required = true, example = "0")
			@PathVariable final Long clientId) {
		validate(request);
		checkClientValid(EntityId.of(clientId));
		return authorizationManagementService.findMemberCountForClient(clientId);
	}

	/**
	 * Endpoint to get all members paginated for the specified Project.
	 *
	 * @param request access to the request
	 * @param projectId The ID of the Project
	 * @param page The page number
	 * @param size The number of {@link Member Members} to return, all records are returned in a single page if value is 0
	 * @return {@link Page Page} of {@link Member Members}
	 */
	@GetMapping(value = PROJECT_MEMBERS_URL)
	@Operation(summary = "Lists all members for a Project", operationId = "findMembersForProject")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist for the given Project ID")
	@ApiResponse(responseCode = "405", description = "if not using Keycloak authentication")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({CLIENT_ADMIN})
	public Page<Member> findMembersForProject(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the project to search", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the page number to be fetched", schema = @Schema(type="integer", format = "int32", defaultValue = "0"), required = false, 
			example = "0")
			@RequestParam(value = "page", required = false, defaultValue = "0") final int page,
			@Parameter(description = "the size of each page", schema = @Schema(type="integer", format = "int32", defaultValue = "0", minimum = "0", 
			maximum = "2147483646"), 
			required = false, example = "10")
			@RequestParam(value = "size", required = false, defaultValue = "0") final int size) {
		validate(request);
		return authorizationManagementService.findMembersForProject(projectService.getNid(projectId), page, size);
	}

	/**
	 * Endpoint to get the count of all Members for the specified Project.
	 *
	 * @param request access to the request
	 * @param projectId The ID of the Project
	 * @return Number of Members for specified Project
	 */
	@GetMapping(value = PROJECT_MEMBER_COUNT_URL)
	@Operation(summary = "Gets the count of all members for a Project", operationId = "findMemberCountForProject")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "405", description = "if not using Keycloak authentication")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({CLIENT_ADMIN})
	public int findMemberCountForProject(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the project to search", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		return authorizationManagementService.findMemberCountForProject(projectService.getNid(projectId));
	}

	/**
	 * Endpoint to get {@link Member Member} specified by the ID which also has access to the specified Project.
	 *
	 * @param request access to the request
	 * @param projectId The ID of the Project
	 * @param memberId The ID of the Member
	 * @return {@link Member Member} specified by the ID
	 */
	@GetMapping(value = MEMBER_BY_ID_URL)
	@Operation(summary = "Gets the member specified by the ID if it has access to given Project",
			operationId = "findMemberById")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project, or the member does not exist")
	@ApiResponse(responseCode = "405", description = "if not using Keycloak authentication")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({CLIENT_ADMIN})
	public Member findMemberById(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the project to search", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the member to search", required = true, example = "0")
			@PathVariable final String memberId) {
		validate(request);
		return authorizationManagementService.findMemberById(projectService.getNid(projectId), memberId);
	}

	/**
	 * Endpoint to return the paginated list of Client Admins.
	 *
	 * @param request access to the request
	 * @param clientId The ID of the Client
	 * @param page The page number
	 * @param size The number of {@link Member Members} to return, all records are returned in a single page if value is 0
	 * @return Page of {@link Member Members} that have Client Admin access
	 */
	@GetMapping(value = CLIENT_ADMINS_URL)
	@Operation(summary = "Gets the list of Admins for specified Client",
			operationId = "findClientAdmins")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given client does not exist")
	@ApiResponse(responseCode = "405", description = "if not using Keycloak authentication")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({CLIENT_ADMIN})
	public Page<Member> findClientAdmins(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the client to search", required = true, example = "0")
			@PathVariable final Long clientId,
			@Parameter(description = "the page number to be fetched",schema = @Schema(type="integer", format = "int32", defaultValue = "0"), required = false, 
			example = "0")
			@RequestParam(value = "page", required = false, defaultValue = "0") final int page,
			@Parameter(description = "the size of each page", schema = @Schema(type = "integer", format = "int32", defaultValue = "0", 
                    minimum = "0", maximum = "2147483646"), required = false, example = "10")
			@RequestParam(value = "size", required = false, defaultValue = "0") final int size) {
		validate(request);
		checkClientValid(EntityId.of(clientId));
		return authorizationManagementService.findClientAdmins(clientId, page, size);
	}
	
	/**
	 * Endpoint to add an admin to a client. If the user exists, then admin role is assigned, else the user is saved with admin role.
	 *
	 * @param request access to the request
	 * @param clientId the id of the client
	 * @param member the member to be added as client admin
	 * 
	 * @return the {@link Member Member} that has been granted the admin access
	 */
	@PostMapping(value = CLIENT_ADMINS_URL)
	@Operation(summary = "Adds the admin for specified Client", operationId = "addMemberAsClientAdmin")
	@ApiResponse(responseCode = "200", description = "successfully added member as client admin")
	@ApiResponse(responseCode = "404", description = "if the given client does not exist")
	@ApiResponse(responseCode = "405", description = "if not using Keycloak authentication")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({CLIENT_ADMIN})
	public Member addMemberAsClientAdmin(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the client to which admin is added", required = true, example = "0") @PathVariable final Long clientId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The member to be added as admin to given client", required = true) 
			@RequestBody final Member member) {
		validate(request);
		checkClientValid(EntityId.of(clientId));
		return authorizationManagementService.addMemberAsClientAdmin(clientId, member);
	}

	/**
	 * Endpoint to grant given {@link ProjectRole Project Role} to the given {@link Member}.
	 * If the {@link Member} does not exist, it will be created,
	 * added to the Client Users Group and granted the {@link ProjectRole Project Role}.
	 *
	 * @param request Access to the request
	 * @param projectId The ID of the Project
	 * @param member The {@link Member} to grant the role
	 * @return The {@link Member} that has been granted the roles
	 */
	@PostMapping(value = PROJECT_MEMBERS_URL)
	@Operation(summary = "Adds the member to the given project", operationId = "addMemberToProject")
	@ApiResponse(responseCode = "200", description = "successfully granted project roles to the user")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "405", description = "if not using Keycloak authentication")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({CLIENT_ADMIN})
	@TryLock(lockCategory = ProjectLockCategory.MEMBER, reasonPhrase = "Applied lock on the method which Adds the member to the given project")
	public Member addMemberToProject(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The member to grant the project roles", required = true) 
			@RequestBody final Member member) {
		validate(request);
		return authorizationManagementService.addMemberToProject(projectService.getNid(projectId), member);
	}
	
	/**
	 * Endpoint to remove the given member's admin access to the given client.
	 *
	 * @param request access to the request
	 * @param clientId the id of the client
	 * @param memberId the id of the member
	 */
	@DeleteMapping(value = CLIENT_ADMIN_BY_ID_URL)
	@Operation(summary = "Removes the member as the client admin", operationId = "deleteMemberAsClientAdmin")
	@ApiResponse(responseCode = "200", description = "successfully removed the admin access of the given member for the given client")
	@ApiResponse(responseCode = "404", description = "if the given client or member does not exist")
	@ApiResponse(responseCode = "405", description = "if not using Keycloak authentication")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({CLIENT_ADMIN})
	public void deleteMemberAsClientAdmin(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the client", required = true, example = "0")
			@PathVariable final Long clientId, 
			@Parameter(description = "the ID of the member whose admin access to given client is to be removed", required = true, example = "0")
			@PathVariable final String memberId
			) {
		validate(request, URI_VAR_MEMBER_ID);
		checkClientValid(EntityId.of(clientId));
		authorizationManagementService.deleteMemberAsClientAdmin(clientId, memberId);
	}
	
	/**
	 * Endpoint to remove the given member's role access in the given project.
	 *
	 * @param request access to the request
	 * @param projectId the id of the project
	 * @param memberId the id of the member
	 */
	@DeleteMapping(value = MEMBER_BY_ID_URL)
	@Operation(summary = "Removes the member roles from the project", operationId = "deleteMemberFromProject")
	@ApiResponse(responseCode = "200", description = "successfully removed the roles of the given member for the given project")
	@ApiResponse(responseCode = "404", description = "if the given project or member does not exist")
	@ApiResponse(responseCode = "405", description = "if not using Keycloak authentication")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({CLIENT_ADMIN})
	@TryLock(lockCategory = ProjectLockCategory.MEMBER, reasonPhrase = "Applied lock on the method which Removes the member roles from the project.")
	public void deleteMemberFromProject(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the member whose admin access to given client is to be removed", required = true, example = "0")
			@PathVariable final String memberId
			) {
		validate(request);
		authorizationManagementService.deleteMemberFromProject(projectService.getNid(projectId), memberId);
	}
	
	/**
	 * Endpoint to assign the given {@link ProjectRole Project Role} to the given {@link Member Member}.
	 * Assigning new project role would override user role while project natures are added for the given user.
	 *
	 * @param request access to the request
	 * @param projectId the id of the project
	 * @param memberId the id of the user
	 * @param member the {@link Member Member}
	 * @return the {@link Member Member} with updated project role
	 */
	@PutMapping(value = MEMBER_BY_ID_URL)
	@Operation(summary = "Assigns the specified project role to the user. User role is mandatory and existing user role is overriden whereas"
			+ " project nature may exist and existing project nature is not removed, only new project nature is added.",
			operationId = "assignProjectRoleToMember")
	@ApiResponse(responseCode = "200", description = "successfully assigned the project role to the given member")
	@ApiResponse(responseCode = "404", description = "if the given project or the member does not exist")
	@ApiResponse(responseCode = "405", description = "if not using Keycloak authentication")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({ADMIN})
	@TryLock(lockCategory = ProjectLockCategory.MEMBER, reasonPhrase = "Applied lock on the method which Assigns the specified project role to the user.")
	public Member assignProjectRoleToMember(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the project to search", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the member to search", required = true, example = "0")
			@PathVariable final String memberId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The member to assign the project roles", required = true) 
			@RequestBody final Member member) {
		validate(request);
		return authorizationManagementService.assignProjectRoleToMember(projectService.getNid(projectId), memberId, member);
	}
}
