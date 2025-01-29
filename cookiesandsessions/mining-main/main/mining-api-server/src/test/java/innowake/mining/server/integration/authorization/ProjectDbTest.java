/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.server.JobTestHelper.waitForJobCompletion;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willDoNothing;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.Instant;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.server.ResponseStatusException;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.JobTestHelper;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.config.security.MiningRole;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.job.deletion.BackgroundDeletionJob;
import innowake.mining.server.properties.KeycloakApplicationConfiguration;
import innowake.mining.server.service.KeycloakAuthorizationManagementService;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.security.RoleType;
import innowake.mining.tags.AuthorizationTest;

/**
 * Tests to validate CRUD operations on Projects in IAM Profile.
 * 
 */
@ActiveProfiles(value = Profiles.AUTH_TEST, inheritProfiles = false)
@AutoConfigureMockMvc
@AuthorizationTest
class ProjectDbTest extends DatabaseResettingTest {

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private ClientService clientService;
	
	@Autowired
	private JobManager jobManager;

	@Nullable
	@SpyBean
	private KeycloakAuthorizationManagementService authorizationManagementService;

	@MockBean
	@Nullable
	private KeycloakApplicationConfiguration config;

	@Nullable
	@MockBean
	private RestTemplate keycloakRestTemplate;

	@Autowired
	private MockMvc mvc;

	private static final String MOCK_HOST = "mock-host";
	private static final String MOCK_REALM = "mock-realm";
	private static final EntityId PROJECT_ONE = EntityId.of(Long.valueOf(1));
	private static final Long TWO = Long.valueOf(2);
	private static final Long INVALID_PROJECT_ID = Long.valueOf(500);

	private static final Long ZERO = Long.valueOf(0);
	
	private EntityId createModule() {
		return moduleService.create(new ModulePojoPrototype()
			.setTechnology(Technology.NATURAL)
			.setType(Type.PROGRAM)
			.setStorage(Storage.FILE)
			.setOrigin(Origin.CUSTOM)
			.setName("PRG1")
			.setPath("src-natural/LibA/PRG1.nsp")
			.setProject(PROJECT_ONE)
			.setIdentification(Identification.IDENTIFIED)
			.setCreator(Creator.DISCOVERY));
	}
	
	@BeforeEach
	void init() {
		AuthorizationTests.setAuthentication(Arrays.asList(new MiningRole(RoleType.ADMIN.getValue())));
		assertNotNull(keycloakRestTemplate);
		final KeycloakApplicationConfiguration config2 = config;
		final KeycloakAuthorizationManagementService authMgmtService = authorizationManagementService;
		if (config2 != null && authMgmtService != null) {
			given(config2.getAuthServerUrl()).willReturn(MOCK_HOST);
			given(config2.getRealm()).willReturn(MOCK_REALM);
			authMgmtService.createKeycloakURI();
		} else {
			fail("Keycloak was not initialized properly.");
		}
	}

	/**
	 * Test to validate that the beans have been autowired/injected appropriately.
	 */
	@Test
	void testAutowiredNotNull() {
		assertNotNull(config);
		assertNotNull(projectService);
		assertNotNull(authorizationManagementService);
		assertNotNull(keycloakRestTemplate);
	}

	/**
	 * Test to validate that user should be able to create a new Project, along with the Keycloak roles.
	 */
	@Test
	void testShouldCreateProject() {
		final ResponseEntity<Void> mockedResponse = new ResponseEntity<>(HttpStatus.CREATED);
		given(assertNotNull(keycloakRestTemplate)
				.postForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/5?projectNatures=MINING,DISCOVERY", null, Void.class))
						.willReturn(mockedResponse);
		final long originalProjectCount = projectService.count(q -> q.withIdAbove(ZERO));
		final Set<ProjectNature> projectNatures = new HashSet<>();
		projectNatures.add(ProjectNature.MINING);
		projectNatures.add(ProjectNature.DISCOVERY);
		final ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setName("Test Project");
		project.setNatures(projectNatures);
		final ClientPojo client = clientService.get(PROJECT_ONE, false);
		project.setClient(client.identity());
		projectService.create(project);
		final long updatedProjectCount = projectService.count(q -> q.withIdAbove(ZERO));
		assertEquals(originalProjectCount + 1, updatedProjectCount);
	}

	/**
	 * Test to validate that Project creation should be rolled back when an exception is thrown by {@link KeycloakRestTemplate}.
	 */
	@Test
	void testShouldRollBackProjectCreation() {
		given(assertNotNull(keycloakRestTemplate).postForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/5?projectNatures=MINING", null,
				Void.class)).willThrow(new ResponseStatusException(HttpStatus.FORBIDDEN));
		final long originalProjectCount = projectService.count(q -> q.withIdAbove(ZERO));
		final Set<ProjectNature> projectNatures = new HashSet<>();
		projectNatures.add(ProjectNature.MINING);
		final ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setName("Test Project");
		project.setNatures(projectNatures);
		final ClientPojo client = clientService.get(PROJECT_ONE, false);
		project.setClient(client.identity());
		assertThrows(ResponseStatusException.class, () -> projectService.create(project));
		final long updatedProjectCount = projectService.count(q -> q.withIdAbove(ZERO));
		assertEquals(originalProjectCount, updatedProjectCount);
	}

	/**
	 * Test to validate that the Project is not created if the Natures have not been specified.
	 */
	@Test
	void testShouldNotCreateProjectEmptyNatures() {
		final ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setName("Test Project");
		project.setNatures(Collections.emptySet());
		final ClientPojo client = clientService.get(PROJECT_ONE, false);
		project.setClient(client.identity());
		assertThrows(ResponseStatusException.class, () -> projectService.create(project));
	}

	/**
	 * Test to validate that V1 Create Project endpoint is not allowed in IAM profile.
	 *
	 * @throws Exception during creation of Project JSON or performing the POST call
	 */
	@Test
	void testShouldNotAllowProjectCreationForV1() throws Exception {
		final long originalProjectCount = projectService.count(q -> q.withIdAbove(ZERO));
		final Set<ProjectNature> projectNatures = new HashSet<>();
		projectNatures.add(ProjectNature.MINING);
		final ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setName("Test Project");
		project.setNatures(projectNatures);
		final ClientPojo client = clientService.get(PROJECT_ONE, false);
		project.setClient(client.identity());
		final String projectJson = PojoMapper.jsonWriter().writeValueAsString(project);
		mvc.perform(post("/api/v1/projects").contentType(MediaType.APPLICATION_JSON).content(projectJson))
		   .andExpect(status().isMethodNotAllowed());
		final long updatedProjectCount = projectService.count(q -> q.withIdAbove(ZERO));
		assertEquals(originalProjectCount, updatedProjectCount);
	}

	/**
	 * Test to find the project natures for a specific clientId and projectId.
	 */
	@Test
	void testShouldFindProjectNatures() {
		final String findProjectNaturesUrl = String.format(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/%d/projects/%d/projectNatures",
				PROJECT_ONE.getNid(), PROJECT_ONE.getNid());
		final ProjectNature[] expectedProjectNaturesFromKeycloak = new ProjectNature[] {
				ProjectNature.DISCOVERY_LIGHT, ProjectNature.MINING
		};
		final ResponseEntity<ProjectNature[]> mockedResponse = new ResponseEntity<ProjectNature[]>(expectedProjectNaturesFromKeycloak, HttpStatus.OK);
		given(assertNotNull(keycloakRestTemplate).getForEntity(findProjectNaturesUrl, ProjectNature[].class)).willReturn(mockedResponse);
		assertTrue(CollectionUtils.isEqualCollection(Arrays.asList(expectedProjectNaturesFromKeycloak), projectService.findProjectNatures(PROJECT_ONE)));
	}
	
	/**
	 * Test to validate exception on findProjectNatures if the response from keycloak is empty.
	 */
	@Test
	void testFindProjectNaturesThrowsExceptionOnEmptyResponse() {
		final String findProjectNaturesUrl = String.format(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/%d/projects/%d/projectNatures",
				PROJECT_ONE.getNid(), PROJECT_ONE.getNid());
		final ResponseEntity<ProjectNature[]> mockedResponse = new ResponseEntity<ProjectNature[]>(new ProjectNature[0], HttpStatus.OK);
		given(assertNotNull(keycloakRestTemplate).getForEntity(findProjectNaturesUrl, ProjectNature[].class)).willReturn(mockedResponse);
		final ConstraintViolationException exception = assertThrows(ConstraintViolationException.class, 
				() -> projectService.findProjectNatures(PROJECT_ONE));
		assertEquals(String.format("Constraint violation. Reason: Project with projectId <%d> must have at least 1 Project Nature assigned to it but none are assigned.",
				PROJECT_ONE.getNid()), exception.getMessage());
	}

	/**
	 * Test to change the project natures for a specific clientId and projectId.
	 */
	@Test
	void testShouldChangeProjectNatures() {
		final String natures = Arrays.asList(ProjectNature.DISCOVERY, ProjectNature.MINING).stream().map(ProjectNature::name).collect(Collectors.joining(","));
		assertTrue(natures.length() > 0);
		final String changeProjectNaturesUrl = String.format(MOCK_HOST + "realms/" + MOCK_REALM + "/admin/clients/%d/projects/%d?projectNatures=" + natures,
				PROJECT_ONE.getNid(), PROJECT_ONE.getNid());
		willDoNothing().given(keycloakRestTemplate).put(changeProjectNaturesUrl, null);
		projectService.changeProjectNatures(PROJECT_ONE, EnumSet.of(ProjectNature.DISCOVERY, ProjectNature.MINING));
		/* Implicit assertion that no runtime exception is thrown while changing the project natures. */
	}

	/**
	 * Test to validate exception on changeProjectNatures if the Set of project natures to be assigned is empty.
	 */
	@Test
	void testChangeProjectNaturesThrowsExceptionOnEmptyNatures() {
		final Set<ProjectNature> emptyProjectNaturesSet = Collections.emptySet();
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
				() -> projectService.changeProjectNatures(PROJECT_ONE, emptyProjectNaturesSet));
		assertTrue(assertNotNull(exception.getMessage()).contains("Project Natures have not been defined."));
	}

	/**
	 * Test to validate that the Project and it's attributes get deleted in a cascading manner.
	 */
	@Test
	void testShouldDeleteProject() {
		createModule();
		
		final Instant start = Instant.now();
		final String deleteProjectUrl = MOCK_HOST + "realms/" + MOCK_REALM + "/admin/clients/1/projects/1";
		willDoNothing().given(keycloakRestTemplate).delete(deleteProjectUrl);
		final long projectCountBeforeDeletion = projectService.count(q -> q.withIdAbove(ZERO));
		final long moduleCountBeforeDeletion = moduleService.countModules(q -> {});
		projectService.markForDeletion(PROJECT_ONE, true);

		Optional<JobInformation> jobInfo = JobTestHelper
				.findJobByLastSubmitTime(jobManager, BackgroundDeletionJob.DESCRIPTION, start, TWO);
		jobInfo.ifPresent(job -> waitForJobCompletion(job.getJobId(), jobManager, 1, TimeUnit.MINUTES));
		final long projectCountAfterDeletion = projectService.count(q -> q.withIdAbove(ZERO));
		final long moduleCountAfterDeletion = moduleService.countModules(q -> {});
		final Long moduleCountForProjectAfterDeletion = moduleService.countModules(q -> q.ofProject(PROJECT_ONE));
		assertEquals(projectCountBeforeDeletion - 1, projectCountAfterDeletion);
		/* Project's attributes (such as Modules) should be deleted as well. */
		assertTrue(moduleCountBeforeDeletion > moduleCountAfterDeletion);
		assertEquals(Long.valueOf(0), moduleCountForProjectAfterDeletion);
	}

	/**
	 * Test to validate that the Project deletion rolls back if an exception is thrown.
	 */
	@Test
	void testShouldDeleteProjectIgnoringExceptionsWhenCommunicatingWithKeycloak() {
		createModule();
		
		final Instant start = Instant.now();
		final String deleteProjectUrl = MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/clients/1/projects/1";
		willThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST)).given(keycloakRestTemplate).delete(deleteProjectUrl);
		final long projectCountBeforeDeletion = projectService.count(q -> q.withIdAbove(ZERO));
		final long moduleCountBeforeDeletion = moduleService.countModules(q -> {});
		projectService.markForDeletion(PROJECT_ONE, true);

		Optional<JobInformation> jobInfo = JobTestHelper
				.findJobByLastSubmitTime(jobManager, BackgroundDeletionJob.DESCRIPTION, start, TWO);
		jobInfo.ifPresent(job -> waitForJobCompletion(job.getJobId(), jobManager, 1, TimeUnit.MINUTES));
		final long projectCountAfterDeletion = projectService.count(q -> q.withIdAbove(ZERO));
		final long moduleCountAfterDeletion = moduleService.countModules(q -> {});
		final Long moduleCountForProjectAfterDeletion = moduleService.countModules(q -> q.ofProject(PROJECT_ONE));

		verify(keycloakRestTemplate).delete(deleteProjectUrl);
		assertEquals(projectCountBeforeDeletion - 1, projectCountAfterDeletion);
		/* Project's attributes (such as Modules) should be deleted as well. */
		assertTrue(moduleCountBeforeDeletion > moduleCountAfterDeletion);
		assertEquals(Long.valueOf(0), moduleCountForProjectAfterDeletion);
	}

	/**
	 * Tests that no projects and modules are deleted but a {@link NoRecordFoundException} is thrown if an unknown project ID is used for project deletion.
	 */
	@Test
	void testShouldNotDeleteUnknownProject() {
		createModule();
		
		final long projectCountBeforeDeletion = projectService.count(q -> q.withIdAbove(ZERO));
		assertTrue(projectCountBeforeDeletion > 0);
		final long moduleCountBeforeDeletion = moduleService.countModules(q -> {});
		assertTrue(moduleCountBeforeDeletion > 0);

		assertThrows(MiningEntityNotFoundException.class, () -> projectService.deleteDirectly(EntityId.of(INVALID_PROJECT_ID)));

		assertEquals(projectCountBeforeDeletion, projectService.count(q -> q.withIdAbove(ZERO)));
		assertEquals(moduleCountBeforeDeletion, moduleService.countModules(q -> {}));
	}
}
