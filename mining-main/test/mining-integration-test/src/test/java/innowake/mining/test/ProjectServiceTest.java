package innowake.mining.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.apache.http.HttpStatus;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.GenericRestService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.client.service.project.ProjectServiceProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.test.util.RestTemplateUtil;


/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
/**
 * Integration unit tests for the Project service.
 */
class ProjectServiceTest extends IntegrationTest {
	private final ConnectionInfo info = getConnectionInfo();
	private final ProjectServiceProvider projectServiceProvider = MiningApiClient.projectService(info);
	private final RestTemplate restTemplate = new RestTemplate();
	private static final String BASE_PROJECT_URI = RouteConfiguration.API_BASE + "/v1/clients/{clientId}/projects";
	private static final ProjectPojoPrototype TEST_PROJECT_1 = new ProjectPojoPrototype();
	private static final ProjectPojoPrototype TEST_PROJECT_2 = new ProjectPojoPrototype();
	private static final ProjectPojoPrototype INVALID_PROJECT = new ProjectPojoPrototype();
	private static final String CUSTOM_PROJECT_PROPERTY_NAME = "customProjectProperty";
	private static final String CUSTOM_PROJECT_PROPERTY_VALUE = "A value for the custom project property";
	private static final String AUTO_COMPLETION_MAP = "autoCompletionMap";
	private static final String AUTO_COMPLETION_MAP_VALUE = "{\"tag1\":[\"sampleTags\"]}";

	private static final Long ONE = Long.valueOf(1);
	private static final Long TWO = Long.valueOf(2);
	private static final Long NON_EXISTING_ID = Long.valueOf(Long.MAX_VALUE);
	private static final String BUSINESS_TAXONOMIES = "Business Taxonomies";
	private static final String TECHNICAL_TAXONOMIES = "Technical Taxonomies";
	
	@BeforeAll
	public static void init() {
		TEST_PROJECT_1.setName("TEST PROJECT 1");
		TEST_PROJECT_1.setClient(EntityId.of(ONE));
		TEST_PROJECT_1.setNatures(new HashSet<>(Arrays.asList(ProjectNature.MINING)));
		TEST_PROJECT_2.setName("TEST PROJECT 2");
		TEST_PROJECT_2.setClient(EntityId.of(TWO));
		TEST_PROJECT_2.setNatures(new HashSet<>(Arrays.asList(ProjectNature.MINING)));
	}
	
	@Test
	void testFindAllProjects() throws IOException {
		final Result<ProjectPojo[]> resultFindAll1 = projectServiceProvider.findAllProjects().execute();
		createProject(TEST_PROJECT_1);
		createProject(TEST_PROJECT_2);
		final Result<ProjectPojo[]> resultFindAll2 = projectServiceProvider.findAllProjects().execute();
		assertEquals(200, resultFindAll2.getStatusCode());
		final ProjectPojo[] projectsBeforeCreate = resultFindAll1.getValue().get();
		final ProjectPojo[] allProjects = resultFindAll2.getValue().get();
		assertEquals(projectsBeforeCreate.length + 2, allProjects.length);
		assertFalse(resultContains(projectsBeforeCreate, TEST_PROJECT_1.name.getNonNull()));
		assertFalse(resultContains(projectsBeforeCreate, TEST_PROJECT_2.name.getNonNull()));
		assertTrue(resultContains(allProjects, TEST_PROJECT_1.name.getNonNull()));
		assertTrue(resultContains(allProjects, TEST_PROJECT_2.name.getNonNull()));
		
		for (final ProjectPojo project : projectsBeforeCreate) {
			if ("Demo Project A".equals(project.getName())) {
				CustomProperties.verifyNumberOfCustomProperties(project, 1);
				testCustomProperties(project);
				break;
			}
		}
	}

	@Test
	void testFindAllProjectsForClient() throws IOException {
		final ParameterizedTypeReference<ProjectPojo[]> responseType = new ParameterizedTypeReference<ProjectPojo[]>() { };
		final HttpEntity<String> request = new HttpEntity<String>(RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<ProjectPojo[]> resultFindAllBeforeCreate = restTemplate.exchange(
				info.getUrl() + BASE_PROJECT_URI,
				HttpMethod.GET,
				request,
				responseType,
				ONE );
		createProject(TEST_PROJECT_1);
		createProject(TEST_PROJECT_2);
		final ResponseEntity<ProjectPojo[]> resultFindAllAfterCreate = restTemplate.exchange(
				info.getUrl() + BASE_PROJECT_URI,
				HttpMethod.GET,
				request,
				responseType,
				ONE );
		assertEquals(200, resultFindAllAfterCreate.getStatusCodeValue());
		
		final ProjectPojo[] projectsBeforeCreate = resultFindAllBeforeCreate.getBody();
		final ProjectPojo[] allProjects = resultFindAllAfterCreate.getBody();
		assertNotNull(projectsBeforeCreate);
		assertNotNull(allProjects);
		assertEquals(projectsBeforeCreate.length + 1, allProjects.length);
		assertFalse(resultContains(projectsBeforeCreate, TEST_PROJECT_1.name.getNonNull()));
		assertFalse(resultContains(projectsBeforeCreate, TEST_PROJECT_2.name.getNonNull()));
		assertTrue(resultContains(allProjects, TEST_PROJECT_1.name.getNonNull()));
		assertFalse(resultContains(allProjects, TEST_PROJECT_2.name.getNonNull()));

		for (final ProjectPojo project : projectsBeforeCreate) {
			if ("Demo Project A".equals(project.getName())) {
				CustomProperties.verifyNumberOfCustomProperties(project, 1);
				testCustomProperties(project);
				break;
			}
		}
	}

	@Test
	void testFindAllProjectsWithDatabaseAndNewProjects() throws IOException {
		createProject(TEST_PROJECT_1);
		createProject(TEST_PROJECT_2);
		final Result<ProjectPojo[]> resultFindAll = projectServiceProvider.findAllProjects().execute();
		assertEquals(200, resultFindAll.getStatusCode());
		verifyFindAll(resultFindAll.getValue().get(), findAllByJDBC());
	}
	
	@Test
	void testFindById() throws IOException {
		final var id = createProject(TEST_PROJECT_1).identity();
		final Result<ProjectPojo> resultFind = projectServiceProvider.findProjectById().setProjectId(id).execute();
		assertEquals(200, resultFind.getStatusCode());
		verifyProjectWithoutIdAndRid(TEST_PROJECT_1, resultFind.getValue().get());
	}

	@Test
	void testFindByRecordId() throws IOException {
		final EntityId id = createProject(TEST_PROJECT_1).identity();
		final Result<ProjectPojo> resultFind = projectServiceProvider.findProjectById().setProjectId(id).execute();
		assertEquals(200, resultFind.getStatusCode());
		verifyProjectWithoutIdAndRid(TEST_PROJECT_1, resultFind.getValue().get());
	}

	@Test
	void testFindByRecordIdIncludesCustomProperties() throws IOException {
		final ProjectPojo projectById = projectServiceProvider.findProjectById().setProjectId(ONE).execute().getValue().get();
		final ProjectPojo projectByRecordId = projectServiceProvider.findProjectById()
																.setProjectId(projectById.identity())
																.execute().getValue().get();
		CustomProperties.verifyNumberOfCustomProperties(projectByRecordId, 1);
		testCustomProperties(projectByRecordId);
	}
	
	@Test
	void testFindByIdNotFound() throws IOException {
		final Result<ProjectPojo> resultFind = projectServiceProvider.findProjectById().setProjectId(EntityId.of(NON_EXISTING_ID)).execute();
		assertEquals(404, resultFind.getStatusCode());
		assertFalse(resultFind.getValue().isPresent());
	}
	
	@Test
	void testCreateProject() throws IOException {
		verifyProjectWithoutIdAndRid(TEST_PROJECT_1, createProject(TEST_PROJECT_1));
	}
	
	@Test
	void testCreateProjectWithCustomProperties() throws IOException {
		final Map<String, Object> props = Map.of(
				CustomPropertyClass.ProjectCustomProperties.name(),
					Map.of(
						AUTO_COMPLETION_MAP, AUTO_COMPLETION_MAP_VALUE,
						CUSTOM_PROJECT_PROPERTY_NAME, CUSTOM_PROJECT_PROPERTY_VALUE
					));
		TEST_PROJECT_1.setCustomProperties(props);

		final ProjectPojo createdProject = projectServiceProvider.createProject().setProject(TEST_PROJECT_1).execute().getValue().get();
		CustomProperties.verifyNumberOfCustomProperties(createdProject, 2);
		testCustomProperties(createdProject);

		final ProjectPojo projectById = projectServiceProvider.findProjectById().setProjectId(createdProject.getId()).execute().getValue().get();
		CustomProperties.verifyNumberOfCustomProperties(projectById, 2);
		testCustomProperties(projectById);
	}
	
	/*
	TODO
		Currently we have no checks on whether a custom property is defined. 
		We could reinstate this once the Orient classes have been migrated to Postgres.
	@Test
	void testCreateProjectWithNonExistingCustomProperty() throws IOException {
		final ProjectPojoPrototype newProject = new ProjectPojoPrototype();
		newProject.setName("New Project");
		newProject.setClientId(1L);
		newProject.setCustomProperties(new HashMap<>(Map.of(CustomPropertyClass.ProjectCustomProperties.name(),new HashMap<>(Map.of("NON_EXISTING_CUSTOM_PROPERTY", "a created value for the custom property")))));

		final Result<ProjectPojo> result = projectServiceProvider.createProject().setProject(newProject).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	*/

	@Test
	void testCreateProjectDuplicate() throws IOException {
		verifyProjectWithoutIdAndRid(TEST_PROJECT_1, createProject(TEST_PROJECT_1));
		final Result<ProjectPojo> resultFail = projectServiceProvider.createProject().setProject(TEST_PROJECT_1).execute();
		assertEquals(400, resultFail.getStatusCode());
		assertFalse(resultFail.getValue().isPresent());
	}
	
	@Test
	void testCreateProjectWithoutNameAndClient() throws IOException {
		final Result<ProjectPojo> result = projectServiceProvider.createProject().setProject(new ProjectPojoPrototype()).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void testCreateProjectWithoutName() throws IOException {
		final ProjectPojoPrototype incompleteProject = new ProjectPojoPrototype();
		incompleteProject.setClient(EntityId.of(1L));
		final Result<ProjectPojo> result = projectServiceProvider.createProject().setProject(incompleteProject).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void testCreateProjectWithoutClient() throws IOException {
		final ProjectPojoPrototype incompleteProject = new ProjectPojoPrototype();
		incompleteProject.setName("I HAVE A NAME");
		final Result<ProjectPojo> result = projectServiceProvider.createProject().setProject(incompleteProject).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void testCreateProjectWithoutExistingClient() throws IOException {
		final ProjectPojoPrototype incompleteProject = new ProjectPojoPrototype();
		incompleteProject.setName("I HAVE A NAME");
		incompleteProject.setClient(EntityId.of(NON_EXISTING_ID));
		incompleteProject.setNatures(new HashSet<>(Arrays.asList(ProjectNature.MINING)));
		final Result<ProjectPojo> result = projectServiceProvider.createProject().setProject(incompleteProject).execute();
		/* Server returns 400 due to constraint violation if client does not exist */
		assertEquals(HttpStatus.SC_BAD_REQUEST, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}

	@Test
	void testUpdate() throws IOException {
		final ProjectPojo resultProject = createProject(TEST_PROJECT_1);
		final ProjectPojoPrototype projectExpected = getUpdateProject(resultProject.identity(), TWO);
		verifyProjectWithId(projectExpected, updateProject(projectExpected));
	}
	
	@Test
	void testUpdateCustomProperties() throws IOException {
		final String updatedCustomProjectPropertyValue = "An updated value for the custom property";
		ProjectPojoPrototype proto = new ProjectPojoPrototype()
			.withId(EntityId.of(ONE))
			.setCustomProperties(new NestedMap()
				.set(CustomPropertyClass.ProjectCustomProperties.name(), CUSTOM_PROJECT_PROPERTY_NAME, updatedCustomProjectPropertyValue)
				.set(CustomPropertyClass.ProjectCustomProperties.name(), AUTO_COMPLETION_MAP, AUTO_COMPLETION_MAP_VALUE));
		
		final ProjectPojo updatedProject = projectServiceProvider.updateProject().setProject(proto).execute().getValue().get();
		final Map<String, Object> updatedProjectCustomProperties = getCustomPropertiesMap(updatedProject);
		assertEquals(AUTO_COMPLETION_MAP_VALUE, updatedProjectCustomProperties.get(AUTO_COMPLETION_MAP));
		assertEquals(updatedCustomProjectPropertyValue, updatedProjectCustomProperties.get(CUSTOM_PROJECT_PROPERTY_NAME));

		final ProjectPojo updatedProjectById = projectServiceProvider.findProjectById().setProjectId(ONE).execute().getValue().get();
		final Map<String, Object> updatedProjectByIdCustomProperties = getCustomPropertiesMap(updatedProjectById);
		assertEquals(AUTO_COMPLETION_MAP_VALUE, updatedProjectByIdCustomProperties.get(AUTO_COMPLETION_MAP));
		assertEquals(updatedCustomProjectPropertyValue, updatedProjectByIdCustomProperties.get(CUSTOM_PROJECT_PROPERTY_NAME));
	}

	@Test
	void testUpdateWithoutClientId() throws IOException {
		final ProjectPojo resultProject = createProject(TEST_PROJECT_1);
		final ProjectPojoPrototype projectExpected = getUpdateProjectWithNullClient(resultProject.getId());
		final Result<ProjectPojo> resultUpdate = projectServiceProvider.updateProject().setProject(projectExpected).execute();
		assertEquals(200, resultUpdate.getStatusCode());
		assertTrue(resultUpdate.getValue().isPresent());
	}

	@Test
	void testUpdateProjectNotFound() throws IOException {
		INVALID_PROJECT.setNid(NON_EXISTING_ID);
		final Result<ProjectPojo> resultUpdate = projectServiceProvider.updateProject().setProject(INVALID_PROJECT).execute();
		assertEquals(404, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}

	@Test
	void testUpdateUniqueName() throws IOException {
		final ProjectPojo project1 = createProject(TEST_PROJECT_1);
		final ProjectPojoPrototype projectExpected1 = getUpdateProject(project1.identity(), project1.getClientNid());
		final ProjectPojoPrototype projectExpected2 = getUpdateProject(createProject(TEST_PROJECT_2).identity(), project1.getClientNid());
		
		final ProjectPojo updatedProject = updateProject(projectExpected1);
		final Result<ProjectPojo> resultUpdate = projectServiceProvider.updateProject().setProject(projectExpected2).execute();
		assertEquals(400, resultUpdate.getStatusCode());
		
		verifyProjectWithId(projectExpected1, updatedProject);
		assertFalse(resultUpdate.getValue().isPresent());
	}

	@Test
	void testUpdateUniqueNameDifferentClient() throws IOException {
		final ProjectPojo project1 = createProject(TEST_PROJECT_1);
		final ProjectPojoPrototype projectExpected1 = getUpdateProject(project1.identity(), project1.getClientNid());
		final ProjectPojo project2 = createProject(TEST_PROJECT_2);
		final ProjectPojoPrototype projectExpected2 = getUpdateProject(project2.identity(), project2.getClientNid());
		verifyProjectWithId(projectExpected1, updateProject(projectExpected1));
		verifyProjectWithId(projectExpected2, updateProject(projectExpected2));
	}

	@Test
	void testAutoCompletionList() throws IOException {
		final GenericRestService<Void> api = new GenericRestService<>(getConnectionInfo());
		final GenericRestService<List<String>> apiList = new GenericRestService<>(getConnectionInfo());
		Result<Void> result;
		Result<List<String>> resultList;
		List<String> response;
		
		/* should not be able to write to inexistent auto-completion key */
		result = api.put("/api/v1/projects/1/autoCompletion/tagTest", Arrays.asList("dummy")).execute();
		assertEquals(400, result.getStatusCode());
		
		/* create new custom property with auto-completion key */
		final CustomPropertyMetadata propDef = new CustomPropertyMetadata();
		propDef.setName("autoCompleteProp1");
		propDef.setDataType("STRING");
		propDef.setFieldType(CustomPropertyFieldType.TAG);
		propDef.setAutoCompletionKey("tagTest");
		propDef.setLabel("Tag 1");
		propDef.setPluginVisible(false);
		assertEquals(200, api.post("/api/v1/projects/1/metamodel/Annotation/autoCompleteProp1", propDef).execute().getStatusCode());
		
		/* auto-completion key should now be present */
		resultList = apiList.get("/api/v1/projects/1/autoCompletion").execute();
		assertEquals(200, resultList.getStatusCode());
		assertTrue(resultList.getValue().isPresent());
		response = resultList.getValue().get();
		assertTrue(response.contains("tagTest"));
		
		/* write auto-completion list */
		result = api.put("/api/v1/projects/1/autoCompletion/tagTest", Arrays.asList("red", "green", "blue")).execute();
		assertEquals(200, result.getStatusCode());
		
		/* read and compare auto-completion list */
		resultList = apiList.get("/api/v1/projects/1/autoCompletion/tagTest").execute();
		assertEquals(200, resultList.getStatusCode());
		assertTrue(resultList.getValue().isPresent());
		response = resultList.getValue().get();
		assertEquals(3, response.size());
		assertTrue(response.contains("red"));
		assertTrue(response.contains("green"));
		assertTrue(response.contains("blue"));
	}

	private ProjectPojo updateProject(final ProjectPojoPrototype projectExpected) throws IOException {
		final Result<ProjectPojo> resultUpdate = projectServiceProvider.updateProject().setProject(projectExpected).execute();
		assertEquals(200, resultUpdate.getStatusCode());
		assertTrue(resultUpdate.getValue().isPresent());
		return resultUpdate.getValue().get();
	}

	@Test
	void testUpdateWithSameName() throws IOException {
		final ProjectPojo resultProject1 = createProject(TEST_PROJECT_1);
		final ProjectPojoPrototype projectExpected = getUpdateProject(resultProject1.identity(), resultProject1.getClientNid());
		updateProject(projectExpected);
		verifyProjectWithId(projectExpected, updateProject(projectExpected));
	}

	@Test
	void testUpdateWithoutName() throws IOException {
		final ProjectPojoPrototype projectExpected = new ProjectPojoPrototype();
		projectExpected.setNid(createProject(TEST_PROJECT_1).getId());
		final Result<ProjectPojo> resultUpdate = projectServiceProvider.updateProject().setProject(projectExpected).execute();
		assertEquals(400, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}

	@Test
	void testUpdateNonExistingWithoutName() throws IOException {
		final ProjectPojoPrototype projectExpected = new ProjectPojoPrototype();
		projectExpected.setNid(NON_EXISTING_ID);
		final Result<ProjectPojo> resultUpdate = projectServiceProvider.updateProject().setProject(projectExpected).execute();
		assertEquals(404, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}
	
	@Test
	void testSearchOrders() throws IOException {
		final Result<ProjectPojo> resultFind = projectServiceProvider.findProjectById().setProjectId(EntityId.of(ONE)).execute();
		assertEquals(200, resultFind.getStatusCode());
		final List<SearchOrder> searchOrders = resultFind.getValue().get().getSearchOrders();
		assertNotNull(searchOrders);
		assertEquals(2, searchOrders.size());
	}
	
	@Test
	void testTaxonomyCategoryOnProjectCreation() throws IOException {
		/* Checking if the taxonomy category objects are created on project creation */
		final ProjectPojo createdProject = projectServiceProvider.createProject()
				.setProject(TEST_PROJECT_1).execute().getValue().get();
		
		final HashMap<Long, String> taxonomiesCategories = TaxonomyServiceTest.queryTaxonomyCategories(createdProject.getId());
		
		final Long defaultTaxonomyCategory = createdProject.getDefaultTaxonomyCategoryId();
		assertNotNull(defaultTaxonomyCategory);
		assertEquals(BUSINESS_TAXONOMIES, taxonomiesCategories.get(defaultTaxonomyCategory));
		final Long technicalTaxonomyCategory = createdProject.getTechnicalTaxonomyCategoryId();
		assertNotNull(technicalTaxonomyCategory);
		assertEquals(TECHNICAL_TAXONOMIES, taxonomiesCategories.get(technicalTaxonomyCategory));
	}
	
	private ProjectPojoPrototype getUpdateProject(final EntityId projectId, final Long clientId) {
		return new ProjectPojoPrototype()
			.withId(projectId)
			.setClient(EntityId.of(clientId))
			.setName("UPDATE TEST CLIENT")
			.setMetricsBaseRevision(Long.valueOf(0))
			.setSourceCodeRevision(Long.valueOf(0));
	}
	
	private ProjectPojoPrototype getUpdateProjectWithNullClient(final Long id) {
		return new ProjectPojoPrototype()
			.setNid(id)
			.setName("UPDATE TEST CLIENT")
			.setMetricsBaseRevision(Long.valueOf(0))
			.setSourceCodeRevision(Long.valueOf(0));
	}
	
	private boolean resultContains(final ProjectPojo[] result, final String projectName) {
		for (int i = 0; i < result.length; i++) {
			if (projectName.equals(result[i].getName())) {
				return true;
			}
		}
		return false;
	}
	
	private void verifyProjectWithoutIdAndRid(final ProjectPojoPrototype input, final ProjectPojo result) {
		assertEquals(input.name.getNonNull(), result.getName());
		assertEquals(input.client.getNonNull().getNid(), result.getClientNid());
	}
	
	private void verifyProjectWithId(final ProjectPojoPrototype input, final ProjectPojo result) {
		assertEquals(input.nid.get(), result.getId());
		assertEquals(input.client.getNonNull().getNid(), result.getClientNid());
		assertEquals(input.name.getNonNull(), result.getName());
		if (input.searchOrders.isDefined()) {
			assertEquals(input.searchOrders.getNonNull().size(), result.getSearchOrders().size());
		}
	}

	private Map<Long, ProjectPojoPrototype> findAllByJDBC() {
		final Map<Long, ProjectPojoPrototype> result = new HashMap<>();
		try {
			if (connectionPostgres != null) {
				try (final Statement stmt = connectionPostgres.createStatement()) {
					final ResultSet resultSet = stmt.executeQuery(
							"SELECT a.nid id, a.name, b.nid clientId FROM project a INNER JOIN client b on b.uid = a.client WHERE a.nid > 0");
					while (resultSet.next()) {
						final Long id = (Long) resultSet.getObject("id");
						final ProjectPojoPrototype project = new ProjectPojoPrototype();
						project.setNid(id);
						project.setName(resultSet.getString("name"));
						project.setClient(EntityId.of(resultSet.getLong("clientId")));
						result.put(id, project);
					}
				}
			} else {
				throw new IllegalStateException("Database connection was lost.");
			}
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
		return result;
	}
	
	private void verifyFindAll(final ProjectPojo[] projects, final Map<Long, ProjectPojoPrototype> databaseResult) {
		for (final ProjectPojo project : projects) {
			final Long id = project.getId();
			final ProjectPojoPrototype expected = databaseResult.get(id);
			assertNotNull(expected);
			verifyProjectWithId(expected, project);
			databaseResult.remove(id);
		}
		assertTrue(databaseResult.isEmpty());
	}
	
	private ProjectPojo createProject(final ProjectPojoPrototype project) throws IOException {
		final Result<ProjectPojo> result = projectServiceProvider
				.createProject()
				.setProject(project)
				.execute();
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		return result.getValue().get();
	}
	
	private void testCustomProperties(final ProjectPojo project) {
		assertEquals(1, project.getCustomProperties().values().size());
		final Map<String, Object> customProperties = getCustomPropertiesMap(project);
		assertTrue("Project does not contain the property: " + CUSTOM_PROJECT_PROPERTY_NAME, customProperties.containsKey(CUSTOM_PROJECT_PROPERTY_NAME));
		/* Verifies the project's custom property values */
		assertEquals(CUSTOM_PROJECT_PROPERTY_VALUE, customProperties.get(CUSTOM_PROJECT_PROPERTY_NAME));
	}

	private Map<String, Object> getCustomPropertiesMap(final ProjectPojo project) {
		return project.getCustomProperties().getSub(CustomPropertyClass.ProjectCustomProperties.name());
	}

}
