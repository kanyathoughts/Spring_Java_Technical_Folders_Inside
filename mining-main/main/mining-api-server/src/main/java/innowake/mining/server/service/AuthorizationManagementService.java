/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Set;

import org.springframework.data.domain.Page;

import innowake.mining.server.config.Profiles;
import innowake.mining.shared.model.Member;
import innowake.mining.shared.model.ProjectNature;

/**
 * Service to handle requests related to Authorization Management.
 */
public interface AuthorizationManagementService {

	/**
	 * Method to return {@link Page Page} of {@link Member Member} for the specified Client.
	 *
	 * @param clientId The ID of the client
	 * @param page The page number
	 * @param size The number of {@link Member Members} to return
	 * @return Page of {@link Member Members}
	 */
	Page<Member> findMembersForClient(Long clientId, int page, int size);

	/**
	 * Method to return the count of Members associated with a specific Client.
	 *
	 * @param clientId The ID of the Client
	 * @return The number of members for the Client
	 */
	int findMemberCountForClient(Long clientId);

	/**
	 * Method to return {@link Page} of {@link Member} for the specified Project.
	 *
	 * @param clientId The ID of the client
	 * @param projectId The ID of the project
	 * @param page The page number
	 * @param size The number of {@link Member Members} to return
	 * @return Page of {@link Member Members}
	 */
	Page<Member> findMembersForProject(Long projectId, int page, int size);

	/**
	 * Method to return the count of Members associated with a Project.
	 *
	 * @param projectId The ID of the Project
	 * @return The number of members for the Project
	 */
	int findMemberCountForProject(Long projectId);

	/**
	 * Returns Member with specified ID that has access to specified Client and Project.
	 *
	 * @param clientId The ID of the client
	 * @param projectId The ID of the project
	 * @param memberId The ID of the member
	 * @return Member of specified ID
	 */
	Member findMemberById(Long projectId, String memberId);

	/**
	 * Method to return {@link Page Page} of {@link Member Members} that have Admin access for specified Client.
	 *
	 * @param clientId The ID of the client
	 * @param page The page number
	 * @param size The number of {@link Member Members} to return
	 * @return Page of {@link Member Members} that have Admin access for the specified Client
	 */
	Page<Member> findClientAdmins(Long clientId, int page, int size);

	/**
	 * Method to create Client Roles and User Groups for the specified Client ID.
	 *
	 * @param clientId The ID of the Client
	 */
	void createClientAttributes(Long clientId);

	/**
	 * Method to create Project Roles and Natures for the given Project ID.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 * @param projectNatures The Project Natures to assign
	 */
	void createProjectAttributes(Long clientId, Long projectId, Set<ProjectNature> projectNatures);

	/**
	 * Method to get Project Natures for the specific client's Project ID.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 * @return Array of {@link ProjectNature project natures} assigned to the project
	 */
	ProjectNature[] findProjectNatures(Long clientId, Long projectId);

	/**
	 * Method to change Project Natures for the specific client's Project ID.
	 * This also changes the nature roles assigned to the project users replacing the default roles with the new ones being assigned.
	 * All users' "custom" natures are left untouched, this means that for every user we only remove/change the natures that are part of the default set.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 * @param projectNatures The set of {@link ProjectNature project natures} to assign
	 */
	void changeProjectNatures(Long clientId, Long projectId, Set<ProjectNature> projectNatures);

	/**
	 * Method to delete all Project Roles for the specified Client and Project.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 */
	void deleteProjectAttributes(final Long clientId, final Long projectId);
	
	/**
	 * Add a user as an admin for specified client.
	 *
	 * @param clientId the client id
	 * @param member the member to be added as client admin
	 * @return the {@link Member Member} that has been granted the admin access
	 */
	Member addMemberAsClientAdmin(Long clientId, Member member);

	/**
	 * Method to delete all Client roles and groups for specified Client.
	 *
	 * @param clientId The ID of the Client
	 */
	void deleteClientAttributes(Long clientId);

	/**
	 * Method to grant Project Roles to the given {@link Member}.
	 *
	 * @param projectId The ID of the Project
	 * @param member The {@link Member}
	 * @return The {@link Member} that was granted the roles
	 */
	Member addMemberToProject(Long projectId, Member member);

	/**
	 * Removes the member as admin for the given client.
	 *
	 * @param clientId the id of the client
	 * @param memberId the id of the member
	 */
	void deleteMemberAsClientAdmin(Long clientId, String memberId);
	
	/**
	 * Removes the member roles for the given project.
	 *
	 * @param projectId the id of the project
	 * @param memberId the id of the member
	 */
	void deleteMemberFromProject(Long projectId, String memberId);
	
	/**
	 * Assign project role to the given {@link Member Member}.
	 * Assigning new project role would override user role while project natures are added for the given user.
	 *
	 * @param projectId the ID of the Project
	 * @param memberId the ID of the user
	 * @param member the {@link Member Member}
	 * @return the {@link Member Member} with updated project role
	 */
	Member assignProjectRoleToMember(Long projectId, String memberId, Member member);

	/**
	 * Method to check if the access is authorized or not.
	 *
	 * @return {@code true} if the active profiles contain {@link Profiles#IAM}, {@code false} otherwise
	 */
	boolean isAuthorizedAccess();
}
