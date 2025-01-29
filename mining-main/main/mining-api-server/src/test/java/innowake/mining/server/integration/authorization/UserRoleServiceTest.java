/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.lang.Boxing.box;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.service.AuthenticatedUserRoleService;
import innowake.mining.shared.service.UserRoleService;

/**
 * Tests for the {@link AuthenticatedUserRoleService}.
 */
@ActiveProfiles(value = {"test", Profiles.AUTH_TEST}, inheritProfiles = false )
class UserRoleServiceTest extends AuthorizationTests {

	@Autowired
	private UserRoleService userRoleService;
	
	/**
	 * User without any mining-related roles should not have any client IDs.
	 */
	@Test
	@WithMockUser
	void returnsAnEmptyListOfClientIdsIfNoClientRolesAssociatedWithUser() {
		final Collection<Long> actualClientIds = userRoleService.getClientIds();
		assertEquals(Collections.emptyList(), actualClientIds);
	}
	
	/**
	 * User without any mining-related roles should not have any project IDs.
	 */
	@Test
	@WithMockUser
	void returnsAnEmptyListOfProjectIdsIfNoProjectRolesAssociatedWithUser() {
		final Collection<Long> actualProjectIds = userRoleService.getProjectIds();
		assertEquals(Collections.emptyList(), actualProjectIds);
	}

	/**
	 * User with 1 set of mining-related roles should have 1 client ID.
	 */
	@Test
	public void returnsAListOfClientIdsContainingOneEntryIfOneClientRolesAssociatedWithUser() {
		setAuthentication(miningRoles("client-1-project-1-manager", "client-1-project-1-mining"));
		final Collection<Long> actualClientIds = userRoleService.getClientIds();
		assertEquals(Arrays.asList(box(1L)), actualClientIds);
	}
	
	/**
	 * User with 1 set of mining-related roles should have 1 project ID.
	 */
	@Test
	void returnsAListOfProjectIdsContainingOneEntryIfOneProjectRolesAssociatedWithUser() {
		setAuthentication(miningRoles("client-1-project-1-manager", "client-1-project-1-mining"));
		final Collection<Long> actualProjectIds = userRoleService.getProjectIds();
		assertEquals(Arrays.asList(box(1L)), actualProjectIds);
	}
	
	/**
	 * User with multiple sets of mining-related roles all pertaining to 1 client should have 1 client ID.
	 */
	@Test
	public void returnsAListOfClientIdsContainingOneEntryIfOneClientWithMultipleProjectsAssociatedWithUser() {
		setAuthentication(miningRoles(
				"client-1-project-1-manager", "client-1-project-1-mining",
				"client-1-project-2-editor", "client-1-project-2-mining"));
		final Collection<Long> actualClientIds = userRoleService.getClientIds();
		assertEquals(Arrays.asList(box(1L)), actualClientIds);
	}
	
	/**
	 * User with multiple sets of mining-related roles pertaining to different clients should have multiple client IDs.
	 */
	@Test
	void returnsAListOfClientIdsWithMultipleEntriesIfMultipleClientsAreAssociated() {
		setAuthentication(miningRoles(
				"client-1-project-1-manager", "client-1-project-1-mining",
				"client-2-project-3-editor", "client-2-project-3-mining"));
		final Collection<Long> actualClientIds = userRoleService.getClientIds();
		assertEquals(2, actualClientIds.size());
		assertTrue("The list must contain the client IDs 1 and 2", actualClientIds.containsAll(Arrays.asList(box(1L), box(2L))));
	}
	
	/**
	 * User with multiple sets of mining-related roles pertaining to different projects should have multiple project IDs.
	 */
	@Test
	void returnsAListProjectIdsWithMultipleEntriesIfMultipleProjectAreAssociated() {
		setAuthentication(miningRoles(
				"client-1-project-1-manager", "client-1-project-1-mining",
				"client-2-project-3-editor", "client-2-project-3-mining"));
		final Collection<Long> actualProjectIds = userRoleService.getProjectIds();
		assertEquals(2, actualProjectIds.size());
		assertTrue("The list must contain the project IDs 1 and 3", actualProjectIds.containsAll(Arrays.asList(box(1L), box(3L))));
	}

	/**
	 * User with 1 client-admin role should have 1 client ID.
	 */
	@Test
	void clientAdminRole() {
		setAuthentication(miningRoles("client-1-admin", "client-1-project-1-manager", "client-1-project-1-mining"));
		final Collection<Long> actualClientIds = userRoleService.getClientIds();
		assertEquals(Arrays.asList(box(1L)), actualClientIds);
		final Collection<Long> actualProjectIds = userRoleService.getProjectIds();
		assertEquals(Arrays.asList(box(1L)), actualProjectIds);
	}

}
