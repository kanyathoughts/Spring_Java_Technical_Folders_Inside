/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.csv;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.server.service.MiningJobService;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.access.EntityId;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.extensions.export.utils.ExportTestUtils;
import org.springframework.security.test.context.support.WithMockUser;

/**
 * Test GraphQL query result to CSV conversion
 */
@WithMockUser /* required to be able to submit Jobs */
public class DataPointCSVExporterTest extends AbstractCSVExporterTest {

	@Autowired
	protected DataPointCSVExporter csvExporter;

	@Autowired
	protected Tracer tracer;
	@Autowired
	protected JobManager jobManager;
	@Autowired
	protected MiningJobService miningJobService;
	
	@Test
	void testModules() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.MODULES));
		parameters.put("$columns", Arrays.asList( 
				"content.name", "content.technology", "content.type"
		));

		ExportTestUtils.exportAndCompare(getJobResult(TEST_PROJECT_ID, parameters),
				"\"Module Name\",\"Technology\",\"Type\"",
				"\"DUMMYPRG\",\"BASIC\",\"PROGRAM\"",
				"\"ROOTJOB\",\"COBOL\",\"COPYBOOK\"",
				"\"ROOTJOB.STEPA\",\"COBOL\",\"EXEC_PGM\""
		);
	}
	
	@Test
	void testModulesWithTechnologyFilter() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.MODULES));
		parameters.put("$columns", Arrays.asList(
				"content.name", "content.technology", "content.type"
		));
		parameters.put("filterObject", Collections.singletonList("{\"content_technology\":{\"eq\":\"COBOL\"}}"));
		
		ExportTestUtils.exportAndCompare(getJobResult(TEST_PROJECT_ID, parameters),
				"\"Module Name\",\"Technology\",\"Type\"",
				"\"ROOTJOB\",\"COBOL\",\"COPYBOOK\"",
				"\"ROOTJOB.STEPA\",\"COBOL\",\"EXEC_PGM\""
		);
	}
	
	@Test
	void testModulesWithTechnologyAndTypeFilter() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.MODULES));
		parameters.put("$columns", Arrays.asList(
				"content.name", "content.path", "content.technology", "content.type"
		));
		parameters.put("filterObject", Collections.singletonList("{\"content_technology\":{\"eq\":\"COBOL\"},\"content_type\":{\"eq\":\"COPYBOOK\"}}"));
		
		ExportTestUtils.exportAndCompare(getJobResult(TEST_PROJECT_ID, parameters),
				"\"Module Name\",\"Path\",\"Technology\",\"Type\"",
				"\"ROOTJOB\",\"ROOTJOB.CBL\",\"COBOL\",\"COPYBOOK\""
		);
	}
	
	@Test
	void testModulesWithSizeParameter() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.MODULES));
		parameters.put("$columns", Arrays.asList(
				"content.name", "content.technology", "content.type"
		));
		/* tests that number parameter is parsed correctly */
		parameters.put("size", Arrays.asList("2"));
		
		/* expected is 3 because there is one additional row for the column headers */
		assertEquals(3, ExportTestUtils.jobResultToString(getJobResult(TEST_PROJECT_ID, parameters)).split("\n").length);
	}
	
	@Test
	void testAnnotations() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.ANNOTATIONS));
		parameters.put("$columns", Arrays.asList(
				"content.module.name", "content.type", "content.categoryName",
				"content.state", "content.name", "content.sourceAttachment",
				"content.module.taxonomies.name", "content.updatedByUserName"
		));
		parameters.put("filterObject", Arrays.asList("{ \"content_module_name\": { \"eq\": \"PRG1\" }, \"content_type\": { \"eq\": \"DEAD_CODE\" } }"));
		
		ExportTestUtils.exportAndCompare(getJobResult(EXISTING_PROJECT_ID, parameters), input -> fixOrdering(input, 6),
				"\"Module Name\",\"Annotation Type\",\"Category\",\"State\",\"Annotation Description\",\"Source Code\",\"\",\"Modified By\"",
				"\"PRG1\",\"DEAD_CODE\",\"Annotation Category A\",\"CANDIDATE\",\"Test\",\"\",\"ARB100, Employee domain\",\"SYSTEM\""
		);
	}

	private String fixOrdering(final String value, final int... indexes) {
		/* split by ',' outside quoted strings: "\"A\", \"B\", \"C, D\", \"E\"" => "\"A\"" | "\"B\"" | "\"C, D\"" | "\"E\"" */
		final String[] vals = value.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
		for (final int index : indexes) {
			if (vals.length > index) {
				vals[index] = "\"" + Arrays.stream(vals[index].replace("\"", "").split(","))
											.map(String::trim)
											.sorted()
											.collect(Collectors.joining(", ")) + "\"";
			}
		}

		return Arrays.stream(vals).collect(Collectors.joining(","));
	}

	@Test
	void testAnnotationsWithUnroll() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.ANNOTATIONS));
		parameters.put("$columns", Arrays.asList(
				"content.module.name", "content.type", "content.categoryName",
				"content.state", "content.name", "content.sourceAttachment",
				"content.module.taxonomies.name", "content.updatedByUserName"
		));
		parameters.put("filterObject", Arrays.asList("{ \"content_module_name\": { \"eq\": \"PRG1\" }, \"content_type\": { \"eq\": \"DEAD_CODE\" } }"));
		parameters.put("$unroll", Collections.singletonList("true"));

		/* instead of one row containing the nested array [ARB100, Employee domain] we now expect two rows, one with "ARB100" and one with "Employee domain" */
		ExportTestUtils.exportAndCompare(getJobResult(EXISTING_PROJECT_ID, parameters),
				"\"Module Name\",\"Annotation Type\",\"Category\",\"State\",\"Annotation Description\",\"Source Code\",\"\",\"Modified By\"",
				"\"PRG1\",\"DEAD_CODE\",\"Annotation Category A\",\"CANDIDATE\",\"Test\",\"\",\"ARB100\",\"SYSTEM\"",
				"\"PRG1\",\"DEAD_CODE\",\"Annotation Category A\",\"CANDIDATE\",\"Test\",\"\",\"Employee domain\",\"SYSTEM\""
		);
	}
	
	@Test
	void testModuleDetailsDataDictionary() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.DATA_DICTIONARY));
		parameters.put("$columns", Arrays.asList("content.name", "content.fieldType",
				"content.format", "content.length", "content.description", "content.createdByUserName"));
		parameters.put("filterObject", Collections.singletonList("{\"content_module_id\":{\"eq\":2000}}"));
		ExportTestUtils.exportAndCompare(getJobResult(EXISTING_PROJECT_ID, parameters),
				"\"Field Name\",\"Field Type\",\"Field Format\",\"Length\",\"Field Description\",\"Created By\"",
				"\"PROGRAM-NAME\",\"ELEMENTARY\",\"PICX\",\"\",\"My Description\",\"\""
		);
	}
	
	@Test
	void testModuleDetailsAnnotations() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.ANNOTATIONS));
		parameters.put("$columns", Arrays.asList("content.categoryName", "content.state",
				"content.name", "content.sourceAttachment", "content.createdByUserName", "content.updatedByUserName"
		));
		parameters.put("filterObject", Collections.singletonList("{\"content_module_id\":{\"eq\":\"2000\"},\"content_name\":{\"eq\":\"*Annotation*\"}}"));
		ExportTestUtils.exportAndCompare(getJobResult(EXISTING_PROJECT_ID, parameters),
				"\"Category\",\"State\",\"Annotation Description\",\"Source Code\",\"Created By\",\"Modified By\"",
				"\"Annotation Category A\",\"CANDIDATE\",\"Annotation 1\",\"abcd\",\"admin\",\"admin\"",
				"\"Annotation Category A\",\"CANDIDATE\",\"Annotation 2\",\"1234\",\"admin\",\"SYSTEM\"",
				"\"Annotation Category A\",\"CANDIDATE\",\"Annotation 3\",\"efgh\",\"admin\",\"SYSTEM\"",
				"\"Annotation Category A\",\"CANDIDATE\",\"Annotation 4\",\"5678\",\"admin\",\"SYSTEM\"",
				"\"Business Rule\",\"CANDIDATE\",\"BusinessRuleAnnotation2\",\"5678\",\"admin\",\"SYSTEM\""
		);
	}
	
	@Test
	void testTaxonomies() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.MODULES));
		parameters.put("$columns", Arrays.asList("content.name", "content.technology", "content.type", "content.taxonomyCount"));
		parameters.put("filterObject", Collections.singletonList("{\"content_taxonomyCount\": {\"gte\": \"1\"}}"));
		ExportTestUtils.exportAndCompare(getJobResult(EXISTING_PROJECT_ID, parameters),
				"\"Module Name\",\"Technology\",\"Type\",\"Number of assigned taxonomies\"",
				"\"PRG1\",\"NATURAL\",\"PROGRAM\",\"2\"",
				"\"QBGPSLP1MMRS710A.STEP01.MMRS7102\",\"JCL\",\"EXEC_PGM\",\"1\""
		);
	}
	
	@Test
	void testCandidateRule() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.ANNOTATIONS));
		parameters.put("$columns", Arrays.asList("content.module.name", "content.module.technology", "content.module.type"));
		parameters.put("filterObject", Collections.singletonList("{\"content_categoryName\": {\"in\": [\"Business Rule\"]}}"));
		ExportTestUtils.exportAndCompare(getJobResult(EXISTING_PROJECT_ID, parameters),
				"\"Module Name\",\"Technology\",\"Type\"",
				"\"EXECSQL\",\"COBOL\",\"PROGRAM\"",
				"\"PRG1\",\"NATURAL\",\"PROGRAM\""
		);
	}
	
	@Test
	void testModuleDependencies() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("moduleDependencies"));
		parameters.put("$columns", Arrays.asList("content.targetName", "content.relationship", "content.direction"));
		parameters.put("moduleId", Collections.singletonList("2000"));
		ExportTestUtils.exportAndCompare(getJobResult(EXISTING_PROJECT_ID, parameters),
				"\"Target Name\",\"Relationship Type\",\"Direction\"",
				"\"DPGM1\",\"CALLS\",\"OUT\"",
				"\"MMRS7101\",\"CALLS\",\"OUT\"",
				"\"QBGPSLP1MMRS710A.STEP01.MMRS7102\",\"CALLS\",\"OUT\""
		);
	}
	
	@Test
	void testAnnotationReportModifiedByHasValidName() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.ANNOTATIONS));
		parameters.put("$columns", Arrays.asList(
				"content.module.name", "content.type", "content.categoryName",
				"content.state", "content.name", "content.sourceAttachment",
				"content.module.taxonomies.name", "content.updatedByUserName"
		));
		parameters.put("filterObject", Collections.singletonList("{\"content_module_id\":{\"eq\":2000}, \"content_type\": { \"eq\": \"DEAD_CODE\" } }}"));
		
		ExportTestUtils.exportAndCompare(getJobResult(EXISTING_PROJECT_ID, parameters), input -> fixOrdering(input, 6),
				"\"Module Name\",\"Annotation Type\",\"Category\",\"State\",\"Annotation Description\",\"Source Code\",\"\",\"Modified By\"",
				"\"PRG1\",\"DEAD_CODE\",\"Annotation Category A\",\"CANDIDATE\",\"Test\",\"\",\"ARB100, Employee domain\",\"SYSTEM\""
		);
	}

	@Test
	void testAnnotationCreatedUpdatedByHasValidName() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.ANNOTATIONS));
		parameters.put("$columns", Arrays.asList("content.categoryName", "content.state", "content.name",
				"content.sourceAttachment", "content.createdByUserName", "content.updatedByUserName"
		));
		parameters.put("filterObject", Collections.singletonList("{\"content_module_id\":{\"eq\": 2000},\"content_name\":{\"eq\":\"*Annotation*\"}}"));
		
		ExportTestUtils.exportAndCompare(getJobResult(EXISTING_PROJECT_ID, parameters),
				"\"Category\",\"State\",\"Annotation Description\",\"Source Code\",\"Created By\",\"Modified By\"",
				"\"Annotation Category A\",\"CANDIDATE\",\"Annotation 1\",\"abcd\",\"admin\",\"admin\"",
				"\"Annotation Category A\",\"CANDIDATE\",\"Annotation 2\",\"1234\",\"admin\",\"SYSTEM\"",
				"\"Annotation Category A\",\"CANDIDATE\",\"Annotation 3\",\"efgh\",\"admin\",\"SYSTEM\"",
				"\"Annotation Category A\",\"CANDIDATE\",\"Annotation 4\",\"5678\",\"admin\",\"SYSTEM\"",
				"\"Business Rule\",\"CANDIDATE\",\"BusinessRuleAnnotation2\",\"5678\",\"admin\",\"SYSTEM\""
		);
	}
	
	@Test
	void testDataDictionaryCreatedByHasValidName() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.DATA_DICTIONARY));
		parameters.put("$columns", Arrays.asList("content.name", "content.fieldType",
				"content.format", "content.length", "content.description", "content.createdByUserName"));
		parameters.put("filterObject", Collections.singletonList("{\"content_module_id\":{\"eq\":2000}}"));
		
		ExportTestUtils.exportAndCompare(getJobResult(EXISTING_PROJECT_ID, parameters),
				"\"Field Name\",\"Field Type\",\"Field Format\",\"Length\",\"Field Description\",\"Created By\"",
				"\"PROGRAM-NAME\",\"ELEMENTARY\",\"PICX\",\"\",\"My Description\",\"\""
		);
	}
	
	@Test
	void testModuleDetailsTaxonomies() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.MODULES));
		parameters.put("$columns", Arrays.asList("content.taxonomies.type.name", "content.taxonomies.name"));
		parameters.put("filterObject", Collections.singletonList("{\"content_id\":{\"eq\":2000}}"));
		ExportTestUtils.exportAndCompare(getJobResult(EXISTING_PROJECT_ID, parameters), input -> fixOrdering(input, 0, 1),
				"\"\",\"\"",
				"\"BusinessSubsystem, DataDomain\",\"ARB100, Employee domain\"");
	}
	
	@Test
	void testModulesWithStatementsProperty() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList(MiningGraphQLQueries.STATEMENTS));
		parameters.put("$columns", Arrays.asList("content.module.name", "content.type", "content.sqlLength", "content.tables", "content.distinctTables",
				"content.customComplexity", "content.halsteadComplexity", "content.halsteadDifficulty", "content.text"));
		
		parameters.put("filterObject", Arrays.asList("{ \"content_technology\": { \"eq\": \"SQL\" }, \"content_type\": { \"eq\": \"SELECT\" } }"));

		ExportTestUtils.exportAndCompare(getJobResult(EXISTING_PROJECT_ID, parameters), "\"Module Name\",\"Statement Type\",\"Length\",\"Tables\","
				+ "\"Distinct Tables\",\"Custom Complexity\",\"Halstead Complexity\",\"Halstead Difficulty\",\"Text\"", 
				"\"MMRS7101\",\"SELECT\",\"8\",\"10\",\"28\",\"27\",\"3.4\",\"2.8\",\"TestStatementB\"");
	}
	
	protected JobResult getJobResult(final EntityId projectId, final Map<String, List<String>> parameters) {
		return miningJobService.getJobResult(submitJob(csvExporter.createJob(projectId, parameters)))
				.orElseThrow(() -> new IllegalStateException("Failed to execute export Job."));
	}

	protected String submitJob(final Job<?> job) {
		final Span rootSpan = tracer.newTrace();
		try (final Tracer.SpanInScope scope = tracer.withSpanInScope(rootSpan)) {
			final CountDownLatch latch = new CountDownLatch(1);
			final Throwable[] error = new Throwable[1];
			final JobMonitor monitor = jobManager.submit(job, new JobExecutionCallback() {

				@Override
				public void onCompletion() {
					latch.countDown();
				}

				@Override
				public void onFailure(@Nullable final Throwable throwable) {
					error[0] = throwable;
					latch.countDown();
				}
			});

			try {
				final boolean countReachedZero = latch.await(10, TimeUnit.MINUTES);
				if ( ! countReachedZero) {
					throw new IllegalStateException(
							"CountDownLatch timed out in DataFlowGraphTest.submitJob(), possible deadlock! (" + latch.toString() + ")");
				}
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}

			if (error[0] != null) {
				throw new IllegalStateException(error[0]);
			}
			return monitor.getJobId();
		} finally {
			rootSpan.finish();
		}
	}
}
