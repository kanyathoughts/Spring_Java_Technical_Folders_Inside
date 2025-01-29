/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsEmptyCollection.empty;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Iterator;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.service.UserRoleService;
import innowake.mining.tags.AuthorizationTest;

/**
 * Client and Project collections must be filtered based on the role of the user.
 */
@AuthorizationTest
class FilteredCollectionsTest extends AuthorizationTests {
	
	private static final String CLIENT1 = "Demo Client 1";
	private static final String CLIENT2 = "Demo Client 2";
	
	private static final Long ZERO = Long.valueOf(0);
	private static final Long ONE = Long.valueOf(1);
	private static final Long TWO = Long.valueOf(2);

	@Autowired
	private ClientService clientService;
	
	@Autowired
	protected UserRoleService user;
	
	@Test
	void adminHasAccessToAllClientsViaTheDataAccessObject() {
		setAuthentication(miningRoles("admin"));
		/* Check for V1 */
		final List<ClientPojo> clients = getClients();
		assertEquals(2, clients.size());
		final ClientPojo client1 = clients.get(0);
		final ClientPojo client2 = clients.get(1);
		assertEquals(CLIENT1, client1.getName());
		assertEquals(CLIENT2, client2.getName());
	}
	
	@Test
	void adminHasAccessToAllClientsViaTheClientService() {
		setAuthentication(miningRoles("admin"));
		/* Check for V2 */
		final List<ClientPojo> clients = getClients();
		assertEquals(2, clients.size());
		/* Wrapping this into a new list because clientPage.getContent returns an UnmodifiableCollection */
		
		for (final Iterator<ClientPojo> iterator = clients.iterator(); iterator.hasNext();) {
			final String clientName = iterator.next().getName();
			if (CLIENT1.equals(clientName) || CLIENT2.equals(clientName)) {
				iterator.remove();
			}
		}
		
		assertThat(clients, is(empty()));
	}
	
	@Test
	void adminHasAccessToAllProjectsViaTheDataAccessObject() {
		setAuthentication(miningRoles("admin"));
		final List<ProjectPojo> projects = getProjects();
		assertEquals(4, projects.size());
	}
	
	@Test
	void clientAdminOnlyHasAccessToHisClientsWithClient1() {
		setAuthentication(miningRoles("client-1-admin"));
		final List<ClientPojo> clients = getClients();
		assertEquals(1, clients.size());
		final ClientPojo client1 = clients.get(0);
		assertEquals(CLIENT1, client1.getName());
	}

	@Test
	void clientAdminOnlyHasAccessToHisClientsWithClient2() {
		setAuthentication(miningRoles("client-2-admin"));
		final List<ClientPojo> clients = getClients();
		assertEquals(1, clients.size());
		final ClientPojo client1 = clients.get(0);
		assertEquals(CLIENT2, client1.getName());
	}
	
	@Test
	void clientAdminOnlyHasAccessToHisClientsWithClient1AndSeparateRegularRole() {
		setAuthentication(miningRoles("client-1-admin", "client-2-project-3-editor", "client-2-project-3-mining"));
		final List<ClientPojo> clients = getClients();
		assertEquals(2, clients.size());
		for (final Iterator<ClientPojo> iterator = clients.iterator(); iterator.hasNext();) {
			final String clientName = iterator.next().getName();
			if (CLIENT1.equals(clientName) || CLIENT2.equals(clientName)) {
				iterator.remove();
			}
		}
		
		assertThat(clients, is(empty()));
	}

	
	@Test
	void clientAdminOnlyHasAccessToTheProjectsOfClient1AndSeparateProject() {
		setAuthentication(miningRoles("client-1-admin", "client-2-project-3-editor", "client-2-project-3-mining"));
		final List<ProjectPojo> projects = getProjects();
		assertEquals(3, projects.size());
		
		for (Iterator<ProjectPojo> iterator = projects.iterator(); iterator.hasNext();) {
			final String projectName = iterator.next().getName();
			if ("Demo Project A".equals(projectName) || "Demo Project B".equals(projectName) || "Demo Project C".equals(projectName)) {
				iterator.remove();
			}
		}
		
		assertThat(projects, is(empty()));
	}
	
	@Test
	void clientAdminOnlyHasAccessToTheProjectsOfClient2() {
		setAuthentication(miningRoles("client-2-admin"));
		final List<ProjectPojo> projects = getProjects();
		assertEquals(2, projects.size());
		
		for (Iterator<ProjectPojo> iterator = projects.iterator(); iterator.hasNext();) {
			final String projectName = iterator.next().getName();
			if ("Demo Project C".equals(projectName) || "Demo Project D".equals(projectName)) {
				iterator.remove();
			}
		}
		
		assertThat(projects, is(empty()));
	}


	@Test
	void clientAdminOnlyHasAccessToHisClientsWithClient1ViaTheClientService() {
		setAuthentication(miningRoles("client-1-admin"));
		/* Check for V2 */
		final List<ClientPojo> clients = getClients();
		assertEquals(1, clients.size());
		/* Wrapping this into a new list because clientPage.getContent returns an UnmodifiableCollection */
		
		for (final Iterator<ClientPojo> iterator = clients.iterator(); iterator.hasNext();) {
			final String clientName = iterator.next().getName();
			if (CLIENT1.equals(clientName)) {
				iterator.remove();
			}
		}
		
		assertThat(clients, is(empty()));
	}
	
	@Test
	void clientAdminOnlyHasAccessToHisClientsWithClient1AndSeparateRegularRoleViaTheClientService() {
		setAuthentication(miningRoles("client-1-admin", "client-2-project-3-editor", "client-2-project-3-mining"));
		/* Check for V2 */
		final List<ClientPojo> clients = getClients();
		/* Wrapping this into a new list because clientPage.getContent returns an UnmodifiableCollection */
		
		for (final Iterator<ClientPojo> iterator = clients.iterator(); iterator.hasNext();) {
			final String clientName = iterator.next().getName();
			if (CLIENT1.equals(clientName) || CLIENT2.equals(clientName)) {
				iterator.remove();
			}
		}
		
		assertThat(clients, is(empty()));
	}


	@Test
	void clientAdminOnlyHasAccessToHisClientsWithClient2ViaTheClientService() {
		setAuthentication(miningRoles("client-2-admin"));
		final List<ClientPojo> clients = getClients();
		assertEquals(1, clients.size());
		
		for (final Iterator<ClientPojo> iterator = clients.iterator(); iterator.hasNext();) {
			final String clientName = iterator.next().getName();
			if (CLIENT2.equals(clientName)) {
				iterator.remove();
			}
		}
		
		assertThat(clients, is(empty()));
	}

	@Test
	void regularUserOnlyHasAccessToHisClientsWithClient1() {
		setAuthentication(miningRoles("client-1-project-1-editor", "client-1-project-1-mining"));
		final List<ClientPojo> clients = getClients();
		assertEquals(1, clients.size());
		assertEquals(CLIENT1, clients.get(0).getName());
	}
	
	@Test
	void regularUserOnlyHasAccessToHisClientsWithClient2() {
		setAuthentication(miningRoles("client-2-project-3-editor", "client-2-project-3-mining"));
		final List<ClientPojo> clients = getClients();
		assertEquals(1, clients.size());
		assertEquals(CLIENT2, clients.get(0).getName());
	}
	
	@Test
	void regularUserOnlyHasAccessToHisClientsWithClient1ViaTheClientService() {
		setAuthentication(miningRoles("client-1-project-1-editor", "client-1-project-1-mining"));
		/* Check for V2 */
		final List<ClientPojo> clients = getClients();
		assertEquals(1, clients.size());
		/* Wrapping this into a new list because clientPage.getContent returns an UnmodifiableCollection */
		
		for (final Iterator<ClientPojo> iterator = clients.iterator(); iterator.hasNext();) {
			final String clientName = iterator.next().getName();
			if (CLIENT1.equals(clientName)) {
				iterator.remove();
			}
		}
		
		assertThat(clients, is(empty()));
	}

	@Test
	void regularUserOnlyHasAccessToHisClientsWithClient2ViaTheClientService() {
		setAuthentication(miningRoles("client-2-project-3-editor", "client-2-project-3-mining"));
		/* Check for V2 */
		final List<ClientPojo> clients = getClients();
		assertEquals(1, clients.size());
		
		for (final Iterator<ClientPojo> iterator = clients.iterator(); iterator.hasNext();) {
			final String clientName = iterator.next().getName();
			if (CLIENT2.equals(clientName)) {
				iterator.remove();
			}
		}
		
		assertThat(clients, is(empty()));
	}
	
	@Test
	void regularUserOnlyHasAccessToTheProjectsAssigned() {
		setAuthentication(miningRoles("client-1-project-1-mining", "client-1-project-1-manager", "client-2-project-3-mining", "client-2-project-3-manager"));
		final List<ProjectPojo> projects = getProjects();
		assertEquals(2, projects.size());

		for (Iterator<ProjectPojo> iterator = projects.iterator(); iterator.hasNext(); ) {
			final String projectName = iterator.next().getName();
			if ("Demo Project A".equals(projectName) || "Demo Project C".equals(projectName)) {
				iterator.remove();
			}
		}		
		assertThat(projects, is(empty()));
	}

	@Test
	void adminHasNoAccessToSystemProjectWithProjectService() {
		setAuthentication(miningRoles("admin"));
		final List<ProjectPojo> projectList = getProjects(ZERO);
		assertEquals(0, projectList.size());
	}
	
	@Test
	void clientAdminHasAccessToClientProjectsWithProjectService() {
		setAuthentication(miningRoles("client-1-admin"));
		final List<ProjectPojo> projectList = getProjects(ONE);
		assertEquals(2, projectList.size());

		for (Iterator<ProjectPojo> iterator = projectList.iterator(); iterator.hasNext();) {
			final String projectName = iterator.next().getName();
			if ("Demo Project A".equals(projectName) || "Demo Project B".equals(projectName)) {
				iterator.remove();
			}
		}

		assertThat(projectList, is(empty()));
	}

	@Test
	void clientAdminHasNoAccessToOtherClientProjectsWithProjectService() {
		setAuthentication(miningRoles("client-1-admin"));
		final List<ProjectPojo> projectList = getProjects(TWO);
		assertEquals(0, projectList.size());
	}

	@Test
	void clientAdminHasAccessToClientProjectsAndSeparateProjectsWithProjectService() {
		setAuthentication(miningRoles("client-1-admin", "client-2-project-3-mining", "client-2-project-3-editor"));
		final List<ProjectPojo> projectList = getProjects(TWO);
		assertEquals(1, projectList.size());

		for (Iterator<ProjectPojo> iterator = projectList.iterator(); iterator.hasNext();) {
			final String projectName = iterator.next().getName();
			if ("Demo Project C".equals(projectName)) {
				iterator.remove();
			}
		}

		assertThat(projectList, is(empty()));
	}

	@Test
	void clientAdminProjectCountIsNonZeroForAssignedClientWithProjectService() {
		setAuthentication(miningRoles("client-1-admin"));
		final List<ProjectPojo> projectList = getProjects(ONE);
		assertEquals(TWO, projectList.size());
	}

	@Test
	void clientAdminProjectCountIsNonZeroForSeparateProjectsWithProjectService() {
		setAuthentication(miningRoles("client-1-admin", "client-2-project-3-mining", "client-2-project-3-editor"));
		final List<ProjectPojo> projectList = getProjects(TWO);
		assertEquals(Long.valueOf(1), projectList.size());
	}
	
	@Test
	void clientAdminProjectCountIsZeroForNotAssignedClientWithProjectService() {
		setAuthentication(miningRoles("client-1-admin"));
		final List<ProjectPojo> projectList = getProjects(TWO);
		assertEquals(Long.valueOf(0), projectList.size());
	}

	@Test
	void regularUserHasAccessToAssignedProjectsWithProjectService() {
		setAuthentication(miningRoles("client-2-project-3-mining", "client-2-project-3-editor"));
		final List<ProjectPojo> projectList = getProjects(TWO);
		assertEquals(1, projectList.size());

		for (Iterator<ProjectPojo> iterator = projectList.iterator(); iterator.hasNext();) {
			final String projectName = iterator.next().getName();
			if ("Demo Project C".equals(projectName)) {
				iterator.remove();
			}
		}

		assertThat(projectList, is(empty()));
	}

	@Test
	void regularUserHasNoAccessToNotAssignedClientsProjectsWithProjectService() {
		setAuthentication(miningRoles("client-2-project-3-mining", "client-2-project-3-editor"));
		final List<ProjectPojo> projectList = getProjects(ONE);
		assertEquals(0, projectList.size());
		assertThat(projectList, is(empty()));
	}
	
	@Test
	void regularUserProjectCountIsNonZeroForAssignedProjectsWithProjectService() {
		setAuthentication(miningRoles("client-2-project-3-mining", "client-2-project-3-editor"));
		final List<ProjectPojo> projectList = getProjects(TWO);
		assertEquals(1, projectList.size());
	}

	@Test
	void regularUserProjectCountIsZeroForNotAssignedProjectsWithProjectService() {
		setAuthentication(miningRoles("client-2-project-3-mining", "client-2-project-3-editor"));
		final List<ProjectPojo> projectList = getProjects(ONE);
		assertThat(projectList, is(empty()));
	}

	private List<ClientPojo> getClients() {
		return clientService.find(q -> {
			if (user.isAdmin()) {
				q.withIdAbove(Long.valueOf(0L));
			} else {
				q.withIds(user.getClientIds());
			}
			q.withMarkedForDeletion(false);
			q.sortNid(SortDirection.ASCENDING);
		});
	}

	private List<ProjectPojo> getProjects(final Long clientId) {
		return projectService.find(q -> {
			if (user.isAdmin()) {
				q.withIdAbove(ZERO);
			} else {
				q.withIds(user.getProjectIds(), user.getClientAdminIds());
			}

			q.ofClient(EntityId.of(clientId));
		});
	}

	private List<ProjectPojo> getProjects() {
		return projectService.find(user.isAdmin() ? q -> q.withIdAbove(ZERO).filterMarkedForDeletion(Boolean.FALSE) : 
													q -> q.withIds(user.getProjectIds(), user.getClientAdminIds()).filterMarkedForDeletion(Boolean.FALSE));
	}
}
