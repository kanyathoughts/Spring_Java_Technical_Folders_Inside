/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration;

import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.shared.access.SqlDetailsService;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.TestPropertySource;

import java.util.List;
import java.util.Optional;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test class to test the behavior of the application when the dbcutter connection is disabled
 */
@TestPropertySource(properties = { "dbcutter-db-postgres.enabled=false" })
class DbCutterPgDbDisabledTest extends DatabaseRelatedTest {
	@Autowired
	DataPointRegistry registry;
	private static final Long PROJECT_ID = 1L;
	@Autowired
	private SqlDetailsService sqlDetailsService;

	@Test
	void testSqlDetailsDataPointIsNotPresent() {
		final MiningDataPointDefinition queryDef = registry.getQueryDefinitions()
				.get(MiningGraphQLQueries.REACHABILITY_DATA);
		final List<MiningDataPointDefinitionWithPath> dataPoints = registry.getDataPointsForTypeRecursively(Optional.of(PROJECT_ID),
				assertNotNull(queryDef.getReferenceTypeName()));
		final long sqlDetailsDatapoint = dataPoints.stream()
				.map(MiningDataPointDefinitionWithPath::getName)
				.filter("sqlDetails"::equals)
				.count();
		assertEquals(0, sqlDetailsDatapoint, "sqlDetails should not be present in the data points when the dbcutter connection is disabled");
	}

	@Test
	void testGetSqlDetails() {
		assertThrows(UnsupportedOperationException.class, () -> sqlDetailsService.findSqlDetails(x -> x.ofProject(PROJECT_ID.intValue())));
	}
}
