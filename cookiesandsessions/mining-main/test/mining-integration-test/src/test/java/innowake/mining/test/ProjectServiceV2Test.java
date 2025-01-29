/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.lang.Assert;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.test.util.RestTemplateUtil;

/**
 * Integration Test for Project.
 */
class ProjectServiceV2Test extends IntegrationTest {

	private static final String BASE_PROJECT_URI = RouteConfiguration.API_BASE + "/v2/clients/{clientId}/projects";
	private static final String BASE_PROJECT_URI_POST = RouteConfiguration.API_BASE + "/v2/projects";
	
	private static final Long ONE = Long.valueOf(1);
	private static final Long TWO = Long.valueOf(2);

	private final RestTemplate restTemplate = new RestTemplate();
	private final ConnectionInfo info = getConnectionInfo();
	
	@Test
	void testFindProjectCountForClient() {
		final String url = BASE_PROJECT_URI + "/count";
		final HttpEntity<String> request = new HttpEntity<>(RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<Long> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + url,
				HttpMethod.GET,
				request,
				Long.class,
				ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(200, clientOneResponseEntity.getStatusCodeValue());
		final Long clientOneProjectCount = clientOneResponseEntity.getBody();
		assertEquals(2, clientOneProjectCount);

		executeStatement(connectionPostgres, "Delete from Project where nid = 3", null);

		final ResponseEntity<Long> clientTwoResponseEntity = restTemplate.exchange(
				info.getUrl() + url,
				HttpMethod.GET,
				request,
				Long.class,
				TWO);
		assertNotNull(clientTwoResponseEntity);
		assertEquals(200, clientTwoResponseEntity.getStatusCodeValue());
		final Long clientTwoProjectCount = clientTwoResponseEntity.getBody();
		assertEquals(1, clientTwoProjectCount);

		try {
			restTemplate.exchange(
					info.getUrl() + url,
					HttpMethod.GET,
					request,
					Long.class,
					Long.valueOf(3));
		} catch (final HttpClientErrorException e) {
			/* Expecting a 404 error code as we don't have a Client with the ID of 3*/
			assertEquals(404, e.getRawStatusCode());
		}
	}

	@Test
	void testFindAllProjectsForClient() {
		final ResponseEntity<RestResponsePage<ProjectPojo>> responseEntityClientOne = getPageOfProjectsForClient(ONE);
		assertNotNull(responseEntityClientOne);
		assertEquals(200, responseEntityClientOne.getStatusCode().value());
		final Page<ProjectPojo> clientOneProjectsPage = responseEntityClientOne.getBody();
		assertNotNull(clientOneProjectsPage);
		final List<ProjectPojo> clientOneProjects = clientOneProjectsPage.getContent();
		assertNotNull(clientOneProjects);
		assertEquals(2, clientOneProjects.size());
		final List<Long> clientOneProjectIds = clientOneProjects.stream().map(ProjectPojo::getId).collect(Collectors.toList());
		assertTrue(clientOneProjectIds.contains(Long.valueOf(1)));
		assertTrue(clientOneProjectIds.contains(Long.valueOf(2)));

		final ResponseEntity<RestResponsePage<ProjectPojo>> responseEntityClientTwo = getPageOfProjectsForClient(TWO);
		assertNotNull(responseEntityClientTwo);
		assertEquals(200, responseEntityClientTwo.getStatusCode().value());
		final Page<ProjectPojo> clientTwoProjectsPage = responseEntityClientTwo.getBody();
		assertNotNull(clientTwoProjectsPage);
		final List<ProjectPojo> clientTwoProjects = clientTwoProjectsPage.getContent();
		assertNotNull(clientTwoProjects);
		assertEquals(2, clientTwoProjects.size());
		final List<Long> clientTwoProjectIds = clientTwoProjects.stream().map(ProjectPojo::getId).collect(Collectors.toList());
		assertTrue(clientTwoProjectIds.contains(Long.valueOf(3)));
		assertTrue(clientTwoProjectIds.contains(Long.valueOf(4)));
	}

	private ResponseEntity<RestResponsePage<ProjectPojo>> getPageOfProjectsForClient(final Long clientId) {
		final ParameterizedTypeReference<RestResponsePage<ProjectPojo>> responseType = new ParameterizedTypeReference<RestResponsePage<ProjectPojo>>() { };
		final HttpEntity<String> request = new HttpEntity<String>(RestTemplateUtil.getHttpHeaders(info));
		return restTemplate.exchange(
				info.getUrl() + BASE_PROJECT_URI,
				HttpMethod.GET,
				request,
				responseType,
				clientId);
	}
	
	/**
	 * Test read and save of ProjectV2 entity that contains transient field natures
	 * 
	 * @throws IOException 
	 */
	@Test
	void testCreateProjectForClientWithTransientFieldIsSavingAsExpected() throws IOException {
		final ProjectPojo projectV2Response = getProjectResponse();
		Assert.assertNotNull(projectV2Response.getId(), "Project Id can not be null");
	}
	
	/*/*
	 * Test save and read of ProjectV2 entity and verify that it loads default configuration value 
	 */
//	@Test
//	void testCreateProjectForDefaultConfigurationValue() {
//		final ProjectDTO projectV2Response = getProjectResponse();
//		
//		final ResponseEntity<RestResponsePage<ProjectV2>> responseEntityClientOne =
//				getPageOfProjectsForClient(Assert.assertNotNull(projectV2Response.getClientId(), "Client ID cannot be null for a project"));
//		assertNotNull(responseEntityClientOne);
//		assertEquals(200, responseEntityClientOne.getStatusCode().value());
//		final List<ProjectV2> clientOneProjects = Assert.assertNotNull(responseEntityClientOne.getBody()).getContent();
//		final ProjectV2 createdProject = clientOneProjects.stream().filter(p -> p.getId() == projectV2Response.getId()).findFirst().get();
//		assertTrue("Configuration should load default value", Assert.assertNotNull(Assert.assertNotNull(createdProject).getConfigurations()).containsKey("Discovery_Config.xml"));
//	}

	@Test
	void testTaxonomyCategoryOnProjectCreation() throws IOException {
		final ProjectPojo projectV2Response = getProjectResponse();
		final HashMap<Long, String> taxonomiesCategories = TaxonomyServiceTest.queryTaxonomyCategories(projectV2Response.getId());
		assertEquals("Business Taxonomies", taxonomiesCategories.get(projectV2Response.getDefaultTaxonomyCategoryId()));
		assertEquals("Technical Taxonomies", taxonomiesCategories.get(projectV2Response.getTechnicalTaxonomyCategoryId()));
	}

	private ProjectPojo getProjectResponse() throws IOException {
		final ProjectPojoPrototype newProject = new ProjectPojoPrototype();
		newProject.setName("Test");
		newProject.setClient(EntityId.of(ONE));
		final Set<ProjectNature> natures = new HashSet<ProjectNature>();
		natures.add(ProjectNature.MINING);
		newProject.setNatures(natures);
		
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(newProject), RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<ProjectPojo> createdProjectResponse = restTemplate.exchange(
				info.getUrl() + BASE_PROJECT_URI_POST,
				HttpMethod.POST,
				request,
				ProjectPojo.class);
		
		assertNotNull(createdProjectResponse);
		assertEquals(HttpStatus.CREATED, createdProjectResponse.getStatusCode());
		final ProjectPojo projectV2Response = createdProjectResponse.getBody();
		assertNotNull(Assert.assertNotNull(projectV2Response).getId());
		assertNotNull(projectV2Response);
		return projectV2Response;
	}
}
