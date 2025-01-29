package innowake.mining.server.integration.authorization.graphql;

import innowake.mining.server.graphql.controller.SchedulerInfoGraphQlController;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SchedulerInfoService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerType;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.test.web.servlet.client.MockMvcWebTestClient;
import org.springframework.web.context.WebApplicationContext;

import java.time.Instant;
import java.util.Collections;
import java.util.Map;
import java.util.UUID;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.apache.commons.lang.math.NumberUtils.LONG_ONE;
import static org.mockito.Mockito.when;

/**
 * Tests for the schema mapping methods of the {@link SchedulerInfoGraphQlController}.
 */
public class SchedulerInfoGraphQlControllerTests extends AbstractGraphQlControllerTests  {

	private static EntityId projectId = EntityId.VOID;
	private static final String QUERY_TEMPLATE = """
			query ($projectId: Long!) {
				schedulerImports(projectId: $projectId) {
					content {
						identifier
						importedOn
						importerUsed
						operationsSelected
						description
						schedulerType
					}
				}
			}
			""";

	private final SchedulerInfoService schedulerInfoService;
	private final SourceService sourceService;
	private final WebApplicationContext webAppContext;

	@Autowired
	public SchedulerInfoGraphQlControllerTests(final SchedulerInfoService schedulerInfoService,
			final SourceService sourceService, final WebApplicationContext webAppContext) {
		this.schedulerInfoService = schedulerInfoService;
		this.sourceService = sourceService;
		this.webAppContext = webAppContext;
	}

	@BeforeAll
	void init() {
		when(assertNotNull(userRoleService).isAdmin()).thenReturn(Boolean.TRUE);

		projectId = projectService.create(new ProjectPojoPrototype()
				.setName("Project_1")
				.setClient(EntityId.of(LONG_ONE))
				.setNatures(Collections.emptySet())
		).identity();

		final WebTestClient client = MockMvcWebTestClient.bindToApplicationContext(webAppContext)
				.configureClient()
				.baseUrl(GraphQlAuthorizationTests.GRAPHQL_ENDPOINT + "?projectId=" + projectId.getNid())
				.build();
		tester = HttpGraphQlTester.create(client);

		final UUID source1 = sourceService.put(projectId, new BinaryString("test1"));
		final UUID source2 = sourceService.put(projectId, new BinaryString("test2"));
		final UUID source3 = sourceService.put(projectId, new BinaryString("test3"));
		final UUID source4 = sourceService.put(projectId, new BinaryString("test4"));
		schedulerInfoService.createSchedulerImport(new SchedulerImportPojoPrototype()
				.setProject(projectId)
				.setSchedulerType(SchedulerType.CONTROL_M)
				.setImportedOn(Instant.EPOCH)
				.setDescription("Test description")
				.setSource(source1)
				.setImporterUsed("default")
				.setIdentifier("Test1")
				.setProperties(Map.of("createModuleIfMissing", false,
						"establishModuleRelationship", false)));
		schedulerInfoService.createSchedulerImport(new SchedulerImportPojoPrototype()
				.setProject(projectId)
				.setSchedulerType(SchedulerType.CONTROL_M)
				.setImportedOn(Instant.EPOCH)
				.setDescription("Test description")
				.setSource(source2)
				.setImporterUsed("default")
				.setIdentifier("Test2")
				.setProperties(Map.of("createModuleIfMissing", true,
						"establishModuleRelationship", false)));
		schedulerInfoService.createSchedulerImport(new SchedulerImportPojoPrototype()
				.setProject(projectId)
				.setSchedulerType(SchedulerType.CONTROL_M)
				.setImportedOn(Instant.EPOCH)
				.setDescription("Test description")
				.setSource(source3)
				.setImporterUsed("default")
				.setIdentifier("Test3")
				.setProperties(Map.of("createModuleIfMissing", false,
						"establishModuleRelationship", true)));
		schedulerInfoService.createSchedulerImport(new SchedulerImportPojoPrototype()
				.setProject(projectId)
				.setSchedulerType(SchedulerType.CONTROL_M)
				.setImportedOn(Instant.EPOCH)
				.setDescription("Test description")
				.setSource(source4)
				.setImporterUsed("default")
				.setIdentifier("Test4")
				.setProperties(Map.of("createModuleIfMissing", true,
						"establishModuleRelationship", true)));
	}

	@Test
	void testQueryDefault() {
		/* Set full user accesses to project */
		setupProjectAccesses(Collections.singletonList(projectId), Collections.singletonList(projectId));

		final var response = executeQuery(QUERY_TEMPLATE, projectId, null, null);
		response.path("").matchesJson("""
				{
					"schedulerImports": {
						"content": [
							{
								"identifier": "Test1",
								"importedOn": "1970-01-01T00:00:00.000Z",
								"importerUsed": "default",
								"operationsSelected": [
									"None"
								],
								"description": "Test description",
								"schedulerType": "CONTROL_M"
							},
							{
								"identifier": "Test2",
								"importedOn": "1970-01-01T00:00:00.000Z",
								"importerUsed": "default",
								"operationsSelected": [
									"Created Missing Module"
								],
								"description": "Test description",
								"schedulerType": "CONTROL_M"
							},
							{
								"identifier": "Test3",
								"importedOn": "1970-01-01T00:00:00.000Z",
								"importerUsed": "default",
								"operationsSelected": [
									"Established Module Relationship"
								],
								"description": "Test description",
								"schedulerType": "CONTROL_M"
							},
							{
								"identifier": "Test4",
								"importedOn": "1970-01-01T00:00:00.000Z",
								"importerUsed": "default",
								"operationsSelected": [
									"Created Missing Module",
									"Established Module Relationship"
								],
								"description": "Test description",
								"schedulerType": "CONTROL_M"
							}
						]
					}
				}
			""");
	}
}
