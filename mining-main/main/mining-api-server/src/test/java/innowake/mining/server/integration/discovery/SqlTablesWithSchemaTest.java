package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.innowake.innovationlab.commons.model.LegacyDatabase;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.service.DataSchemaImportService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import java.util.List;

/**
 * Test for verifying that tables declared with Schema included are correctly resolved to tables imported with schema from DB_CRAWLER.
 */
@WithMockUser
class SqlTablesWithSchemaTest extends BaseDiscoveryTest {

	@Autowired
	ObjectMapper objectMapper;
	@Autowired
	DataSchemaImportService dataSchemaImportService;

	final String legacyDatabaseJson = "{" +
			"    \"name\": \"jdbc:sqlserver://localhost;databaseName=unit_test_1\"," +
			"    \"schemes\": [" +
			"        {" +
			"            \"name\": \"TEST1\"," +
			"            \"tables\": [" +
			"                {" +
			"                    \"name\": \"DEPT\"," +
			"                    \"columns\": [" +
			"                        {" +
			"                            \"name\": \"optname\"," +
			"                            \"dataType\": \"sysname\"," +
			"                            \"defaultValue\": null," +
			"                            \"autoIncremented\": false" +
			"                        }," +
			"                        {" +
			"                            \"name\": \"value\"," +
			"                            \"dataType\": \"bit\"," +
			"                            \"defaultValue\": null," +
			"                            \"autoIncremented\": false" +
			"                        }" +
			"                    ]," +
			"                    \"indices\": []," +
			"                    \"primaryKey\": null," +
			"                    \"foreignKeys\": []" +
			"                }" +
			"            ]," +
			"            \"views\": []," +
			"            \"routines\": []," +
			"            \"triggers\": []" +
			"        }," +
			"        {" +
			"            \"name\": \"TEST2\"," +
			"            \"tables\": [" +
			"                {" +
			"                    \"name\": \"TABLE2\"," +
			"                    \"columns\": [" +
			"                        {" +
			"                            \"name\": \"optname\"," +
			"                            \"dataType\": \"sysname\"," +
			"                            \"defaultValue\": null," +
			"                            \"autoIncremented\": false" +
			"                        }," +
			"                        {" +
			"                            \"name\": \"value\"," +
			"                            \"dataType\": \"bit\"," +
			"                            \"defaultValue\": null," +
			"                            \"autoIncremented\": false" +
			"                        }" +
			"                    ]," +
			"                    \"indices\": []," +
			"                    \"primaryKey\": null," +
			"                    \"foreignKeys\": []" +
			"                }" +
			"            ]," +
			"            \"views\": [" +
			"                {" +
			"                    \"name\": \"DEPT\"," +
			"                    \"viewColumns\": [" +
			"                        {" +
			"                            \"name\": \"CO_ID\"," +
			"                            \"targetColumn\": \"AACH00.TDDCOMP.CO_ID\"" +
			"                        }" +
			"                    ]" +
			"                }" +
			"            ]," +
			"            \"routines\": []," +
			"            \"triggers\": []" +
			"        }" +
			"    ]" +
			"}";

	@Test
	void testContainsSqlTablesWithSchema() throws Exception {
		final var legacyDatabase = objectMapper.readValue(legacyDatabaseJson, LegacyDatabase.class);
		projectId = assertNotNull(createProject()).identity();
		dataSchemaImportService.importSchema(projectId, legacyDatabase);

		sourceService.resetCaches();
		uploadResources(assertNotNull(projectId));
		submitJob(jobManager, tracer, new DiscoverCodeJob(assertNotNull(projectId)));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(assertNotNull(projectId), false));

		final var module1 = moduleService.findModules(q -> q.ofProject(assertNotNull(projectId)).withNames(List.of("MGOPRGM1")));
		final var module2 = moduleService.findModules(q -> q.ofProject(assertNotNull(projectId)).withNames(List.of("SQLCOPY1")));
		assertEquals(1, module1.size());
		assertEquals(1, module2.size());
		final var dependencies1 = moduleService.findRelatedModules(module1.get(0).identity(), RelationshipType.ACCESSES, RelationshipDirection.OUT);
		final var dependencies2 = moduleService.findRelatedModules(module2.get(0).identity(), RelationshipType.ACCESSES, RelationshipDirection.OUT);
		assertEquals(1, dependencies1.size());
		assertEquals(1, dependencies2.size());

		/* Assert that the type of dependency is correct */
		final var tableModule  = moduleService.findModules(q -> q.byUid(dependencies1.get(0)));
		final var viewModule = moduleService.findModules(q -> q.byUid(dependencies2.get(0)));
		assertEquals(1, tableModule.size());
		assertEquals(1, viewModule.size());
		assertEquals("DEPT", tableModule.get(0).getName());
		assertEquals(Type.TABLE, tableModule.get(0).getType());
		assertEquals("DEPT", viewModule.get(0).getName());
		assertEquals(Type.VIEW, viewModule.get(0).getType());

		/* assert the parents of the dependency table and view are SCHEMA modules */
		assertSchemaModule("TEST1", tableModule.get(0).identity());
		assertSchemaModule("TEST2", viewModule.get(0).identity());
	}

	private void assertSchemaModule(final String schemaName, final EntityId entityId) {
		final var parent = moduleService.findRelationship(q -> q.ofDestination(entityId)
				.withType(RelationshipType.CONTAINS)
				.includeModuleDetails(true,false));
		assertEquals(1, parent.size());
		assertTrue(parent.get(0).getSrcModuleDetails().isPresent());
		assertEquals(schemaName, parent.get(0).getSrcModuleDetails().get().getName());
		assertEquals(Type.SCHEMA, parent.get(0).getSrcModuleDetails().get().getType());
	}

	@Override
	protected String getTestFolder() {
		return "WMIN12136";
	}
}
