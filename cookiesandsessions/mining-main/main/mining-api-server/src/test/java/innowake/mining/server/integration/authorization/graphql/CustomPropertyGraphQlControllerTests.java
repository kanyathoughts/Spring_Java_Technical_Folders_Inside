/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.authorization.graphql;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.apache.commons.lang.math.NumberUtils.LONG_ONE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.BDDMockito.given;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.graphql.test.tester.GraphQlTester.Response;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.graphql.test.tester.WebGraphQlTester;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.test.web.servlet.client.MockMvcWebTestClient;
import org.springframework.web.context.WebApplicationContext;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.config.security.MiningRole;
import innowake.mining.server.graphql.controller.ControllerLocalContext;
import innowake.mining.server.graphql.controller.CustomPropertyGraphQlController;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.service.UserRoleService;
import innowake.mining.tags.AuthorizationTest;

/**
 * Tests for the schema mapping methods of the {@link CustomPropertyGraphQlController}.
 * <p>Tests that GraphQL queries can reach other projects only through the GraphQL API if the user has access to them.</p>
 */
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
@AuthorizationTest
class CustomPropertyGraphQlControllerTests extends DatabaseRelatedTest {

	private static final String annotationEntityName = "Annotation";
	private static final String moduleEntityName = "Module";
	private static EntityId projectId = EntityId.of(0L);
	private static CustomPropertyMetadata annotationCustomProperty = new CustomPropertyMetadata();
	private static CustomPropertyMetadata moduleCustomProperty = new CustomPropertyMetadata();

	private static final String QUERY_TEMPLATE = "query ($projectId: Long!, $entityName: String!) {" +
				"customProperties(projectId: $projectId, entityName: $entityName) {" +
					"name, "+
					"label, "+
					"dataType, " +
					"description, " +
					"fieldType, " +
					"customViewIndex, " +
					"autoCompletionKey" +
				"}" +
			"}";
	
	@Nullable
	@SpyBean
	private UserRoleService userRoleService;
	@Nullable
	private WebGraphQlTester tester;

	@Autowired
	private WebApplicationContext webAppContext;
	
	/**
	 * Creates the test data as following:
	 * <ul>
	 * <li>Creates a new project with the given {@code projectName}</li>
	 * <li>Creates one {@link Module Modules} in the new project</li>
	 * <li>Creates a reference with dummy module location</li>
	 * <li>Creates three {@link DataDictionaryPojo DataDictionaries in the new project}</li>
	 * </ul>
	 */
	@BeforeAll
	void init() {
		final WebTestClient client = MockMvcWebTestClient.bindToApplicationContext(webAppContext)
				.configureClient()
				.baseUrl(GraphQlAuthorizationTests.GRAPHQL_ENDPOINT)
				.build();

		tester = HttpGraphQlTester.create(client);

		final ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setName("Project_1");
		project.setClient(EntityId.of(LONG_ONE));
		project.setNatures(Collections.emptySet());
		projectId = projectService.create(project).identity();

		annotationCustomProperty.setName("sampleAnnotationCustomProperty");
		annotationCustomProperty.setLabel("Sample Annotation Custom Label");
		annotationCustomProperty.setDescription("Sample Annotation Custom Property for graph ql test");
		annotationCustomProperty.setDataType("STRING");
		annotationCustomProperty.setFieldType(CustomPropertyFieldType.DEFAULT);
		annotationCustomProperty.setCustomViewIndex(1);
		annotationCustomProperty.setAutoCompletionKey("Sample");
		annotationCustomProperty.setPluginVisible(false);
		
		customPropertiesService.defineProperty(projectId, annotationEntityName, annotationCustomProperty.getName(), annotationCustomProperty);

		moduleCustomProperty.setName("sampleModuleCustomProperty");
		moduleCustomProperty.setLabel("Sample Module Custom Label");
		moduleCustomProperty.setDescription("Sample Module Custom Property for graph ql test");
		moduleCustomProperty.setDataType("boolean");
		moduleCustomProperty.setFieldType(CustomPropertyFieldType.DEFAULT);
		moduleCustomProperty.setCustomViewIndex(1);
		moduleCustomProperty.setAutoCompletionKey("Sample");
		moduleCustomProperty.setPluginVisible(false);
		
		customPropertiesService.defineProperty(projectId, moduleEntityName, moduleCustomProperty.getName(), moduleCustomProperty);
	}

	/**
	 * Tests that the GraphQL query returns expected{@linkplain CustomPropertyMetadata}
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for Annotation Entity")
	@Test
	void testForAnnotationCustomProperty() {

		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId.getNid()), Collections.singletonList(projectId.getNid()));

		/* Execute GraphQL request to fetch the data sorted by name in ascending order and get response data */
		final var customPropertiesRetrieved = getCustomProperties(executeQuery(projectId.getNid(), annotationEntityName));
		/* Test that the length of created and fetched dataDictionaries are same */
		assertFalse(customPropertiesRetrieved.isEmpty(), "At least one Annotation custom property must exist");
		assertCustomPropertyMetaData(annotationCustomProperty, customPropertiesRetrieved);
	}

	/**
	 * Tests that the GraphQL query returns expected{@linkplain CustomPropertyMetadata}
	 * <p>The user has full access to the created test project.</p>
	 */
	@DisplayName("Test for Module Entity")
	@Test
	void testForModuleCustomProperty() {

		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId.getNid()), Collections.singletonList(projectId.getNid()));

		/* Execute GraphQL request to fetch the data sorted by name in ascending order and get response data */
		final var customPropertiesRetrieved = getCustomProperties(executeQuery(projectId.getNid(), moduleEntityName));
		/* Test that the length of created and fetched dataDictionaries are same */
		assertFalse(customPropertiesRetrieved.isEmpty(), "At least one Module custom property must exist");
		assertCustomPropertyMetaData(moduleCustomProperty, customPropertiesRetrieved);
	}
	
	@AfterAll
	void cleanUp() {
		resetCustomProperties(annotationEntityName, projectId);
		resetCustomProperties(moduleEntityName, projectId);
	}
	
	private void assertCustomPropertyMetaData(final CustomPropertyMetadata expected, final List<CustomPropertyMetadata> candidates) {
		final var match = candidates.stream()
									.filter(candidate -> candidate.getName().equals(expected.getName()))
									.findFirst();
		assertTrue(match.isPresent(), "Matching custom property must exist");

		final var actual = match.get();
		assertEquals(expected.getName(), actual.getName(), "Custom property name must match");
		assertEquals(expected.getLabel(), actual.getLabel(), "Custom property label must match");
		assertEquals(expected.getDescription(), actual.getDescription(), "Custom property description must match");
		assertEquals(expected.getDataType(), actual.getDataType(), "Custom property data type must match");
		assertEquals(expected.getFieldType(), actual.getFieldType(), "Custom property field type must match");
		assertEquals(expected.getAutoCompletionKey(), actual.getAutoCompletionKey(), "Custom property auto completion key must match");
		assertEquals(expected.getCustomViewIndex(), actual.getCustomViewIndex(), "Custom property view index must match");
	}

	private static List<CustomPropertyMetadata> getCustomProperties(final Response response) {
		return response.path("customProperties").entityList(CustomPropertyMetadata.class).get();
	}

	/**
	 * Executes the GraphQL query {@link #QUERY_TEMPLATE}.
	 * 
	 * @param projectId The project to query
	 * @param entityName the name of the mining entity
	 * @return {@link Response} from GraphQL with the query result
	 */
	private Response executeQuery(final Long projectId, final String entityName) {
		final Response Response= assertNotNull(tester)
				.document(QUERY_TEMPLATE)
				.variable("projectId", projectId)
				.variable("entityName", entityName)
				.execute();
		return  Response;
	}

	/**
	 * Sets the project accesses for the current user. The given {@code authProjectIds} are used for setting the user's {@link Authentication}. The given
	 * {@code userRoleProjectIds} are used for the {@link UserRoleService}. Both lists can differ.
	 * 
	 * <p>Usually the associated project IDs in the UserRoleService should match the project authorities. If a user has no authorization for a project
	 * ({@link SimpleGrantedAuthority}), the server responses with access {@code FORBIDDEN}. For explicitly testing of {@link ControllerLocalContext}
	 * and {@link UserRoleService} both project ID lists can differ.</p>
	 *
	 * @param authProjectIds project IDs for the {@link Authentication}
	 * @param userRoleProjectIds project IDs for the {@link UserRoleService}
	 */
	private void setupProjectAccesses(final List<Long> authProjectIds, final List<Long> userRoleProjectIds) {
		final UserRoleService userRoleService = assertNotNull(this.userRoleService);
		given(userRoleService.getProjectIds()).willReturn(userRoleProjectIds);

		final List<GrantedAuthority> authorities = new ArrayList<>(authProjectIds.size() * 2);
		authProjectIds.forEach(projectId -> {
			authorities.add(new MiningRole(String.format("client-1-project-%d-viewer", projectId)));
			authorities.add(new MiningRole(String.format("client-1-project-%d-mining", projectId)));
		});

		final Authentication auth = new UsernamePasswordAuthenticationToken("", "", authorities);
		final SecurityContext context = SecurityContextHolder.createEmptyContext();
		context.setAuthentication(auth);
		SecurityContextHolder.setContext(context);
	}
}
