/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.parser.sql;

 import static org.junit.jupiter.api.Assertions.assertEquals;

 import java.io.File;
 import java.io.IOException;
 import java.nio.charset.StandardCharsets;
 import java.util.List;
 import java.util.Map;

 import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
 import innowake.mining.server.discovery.dawn.metrics.impl.contributor.DiscoveryBuilderImpl;
 import innowake.mining.shared.model.DatabaseAccessType;
 import innowake.mining.shared.model.ModuleType;
 import org.apache.commons.io.FileUtils;
 import org.junit.jupiter.api.Test;

 /* Test to verify the functionality of the SqlTableExtractor */
 class SqlTablesExtractorTest {
	 private static final String FAKE_CLASS_NAME = "innowake.mining.server.discovery.dawn.metrics.contributors.TestContributor";

	 final DiscoveryBuilderImpl discoveryBuilder =  new DiscoveryBuilderImpl(FAKE_CLASS_NAME);
	 final DiscoveryBuilder.ModuleBuilder moduleBuilder = discoveryBuilder.declareExternalModule("TEST_MODULE", ModuleType.JCL_EXEC_PGM);

 	@Test
 	 void testExtractTablesWithTrailingNumbers() throws IOException {
		final var sqlString = FileUtils.readFileToString(new File("test-resources/innowake/mining/server/discovery" +
						"/source/SqlTableExtractor/SQLFILE1"),
				StandardCharsets.UTF_8);
 		final var result = SqlTableExtractor.extract(moduleBuilder, sqlString);
 		final Map<String, List<DatabaseAccessType>> expectedMap = Map.of("TABLE16", List.of(DatabaseAccessType.READ, DatabaseAccessType.DELETE,
				DatabaseAccessType.UPDATE));
 		assertEqualsMap(expectedMap, result.getFirst());
		assertEquals(3, result.getSecond().size());
 	}

	 /* Test getSqlContent method */
	 @Test
	 void testGetSqlContentWithComments() throws IOException {
		final var sqlString = FileUtils.readFileToString(new File("test-resources/innowake/mining/server/discovery" +
						"/source/SqlTableExtractor/SQLFILE2"),
				StandardCharsets.UTF_8);
		final var result = SqlTableExtractor.extract(moduleBuilder, sqlString);
		 final Map<String, List<DatabaseAccessType>> expectedMap = Map.of("TABLE12", List.of(DatabaseAccessType.STORE), "TABLE13",
				 List.of(DatabaseAccessType.UPDATE), "VAVT_PRC_TYP", List.of(DatabaseAccessType.DELETE), "TABLE5", List.of(DatabaseAccessType.UPDATE));
		 assertEqualsMap(expectedMap, result.getFirst());
		 assertEquals(3, result.getSecond().size());
	 }

	 @Test
	 void testGetSqlContentWithInvalidSql() throws IOException {
		 final DiscoveryBuilderImpl discoveryBuilder2 =  new DiscoveryBuilderImpl(FAKE_CLASS_NAME);
		 final DiscoveryBuilder.ModuleBuilder moduleBuilder2 = discoveryBuilder2.declareExternalModule("TEST_MODULE", ModuleType.JCL_EXEC_PGM);
		 final var sqlString = FileUtils.readFileToString(new File("test-resources/innowake/mining/server/discovery" +
						 "/source/SqlTableExtractor/SQLFILE3"),
				 StandardCharsets.UTF_8);
		 final var result = SqlTableExtractor.extract(moduleBuilder2, sqlString);
		 final Map<String, List<DatabaseAccessType>> expectedMap = Map.of("USERS", List.of(DatabaseAccessType.READ));
		 assertEqualsMap(expectedMap, result.getFirst());
		 assertEquals(0, result.getSecond().size());
		 /* Assert that a warning is added for invalid sql */
		 final var builderResults = discoveryBuilder2.buildResults();
		 assertEquals(1, builderResults.size());
		 final var contributorResult = builderResults.get(0);
		 assertEquals(1, contributorResult.getErrors().size());
	 }

	 @Test
	 void testExtractTablesWithJoins() throws IOException {
		 final var sqlString = FileUtils.readFileToString(new File("test-resources/innowake/mining/server/discovery" +
						 "/source/SqlTableExtractor/SQLFILE4"),
				 StandardCharsets.UTF_8);
		 final var result = SqlTableExtractor.extract(moduleBuilder, sqlString);
		 final Map<String, List<DatabaseAccessType>> expectedMap = Map.of("CUSTOMERS", List.of(DatabaseAccessType.READ),
				 "ORDERS", List.of(DatabaseAccessType.READ), "SHIPPERS", List.of(DatabaseAccessType.READ));
		 assertEqualsMap(expectedMap, result.getFirst());
		 assertEquals(0, result.getSecond().size());
	 }

	 @Test
	 void testExtractTablesForWithAs() throws IOException {
		 final var sqlString = FileUtils.readFileToString(new File("test-resources/innowake/mining/server/discovery" +
						 "/source/SqlTableExtractor/SQLFILE5"),
				 StandardCharsets.UTF_8);
		 final var result = SqlTableExtractor.extract(moduleBuilder, sqlString);
		 final Map<String, List<DatabaseAccessType>> expectedMap = Map.of("SALESORDERHEADER", List.of(DatabaseAccessType.READ),
				 "EMPLOYEE", List.of(DatabaseAccessType.READ), "SALES_CTE", List.of(DatabaseAccessType.READ));
		 assertEqualsMap(expectedMap, result.getFirst());
		 assertEquals(0, result.getSecond().size());
	 }

	 private void assertEqualsMap(Map<String, List<DatabaseAccessType>> expected, Map<String, List<DatabaseAccessType>> actual) {
		 assertEquals(expected.size(), actual.size());
		 expected.forEach((k, v) -> assertEquals(v, actual.get(k)));
	 }
 }

