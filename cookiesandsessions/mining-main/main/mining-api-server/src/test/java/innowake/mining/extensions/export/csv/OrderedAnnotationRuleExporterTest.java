/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.extensions.export.csv;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.server.integration.discovery.BaseDiscoveryTest.submitJob;
import static innowake.mining.shared.model.Identification.IDENTIFIED;
import static innowake.mining.shared.model.Origin.CUSTOM;
import static org.apache.commons.lang.math.NumberUtils.LONG_ONE;
import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Serializable;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.ff4j.FF4j;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.job.identification.IdentifyCandidatesJob;
import innowake.mining.server.service.MiningJobService;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Test Ordered Business Rule result to CSV conversion
 */
@WithMockUser /* required to be able to submit Jobs */
class OrderedAnnotationRuleExporterTest extends DatabaseRelatedTest {

	private static final String RESOURCE_PATH = "/test-resources/innowake/mining/server/job/identification/";
	private EntityId projectId = EntityId.of(1L);

	@Autowired
	private OrderedAnnotationRuleCSVExporter orderedAnnotationRuleCSVExporter;
	@Autowired
	private Tracer tracer;
	@Autowired
	private JobManager jobManager;
	@Autowired
	private MiningJobService miningJobService;
	@Autowired
	private ModuleService moduleService;

	@Autowired
	private FF4j ff4j;

	/**
	 * Enable {@linkplain FeatureId#ORDERED_ANNOTATION_RULE_CSV_EXPORTER} toggle.
	 */
	@Override
	@BeforeAll
	public void setup() {
		ff4j.enable(FeatureId.ORDERED_ANNOTATION_RULE_CSV_EXPORTER.getId());
		createProject();
	}

	/**
	 * Reset the test data after every test method.
	 * @throws IOException If an IO related error occurs.
	 */
	@AfterAll
	public void resetData() throws IOException {
		resetTestData();
	}

	@Test
	void testOrderedAnnotationRulesLinearControlFlow() throws IOException {
		final var moduleId = createModule(projectId, "MMRS7112", "MMRS7112.cbl", RESOURCE_PATH);
		submitIdentifyCandidatesJob(projectId, moduleId);
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("moduleId", Collections.singletonList(moduleId.getNid().toString()));
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", Arrays.asList("content.type", "content.categoryName", "content.state", "content.name",
				"content.sourceAttachment"));
		final String actualJobResult = convertJobResultToString(getJobResult(projectId, parameters));
		final String exp = """
			"Execution Order","Annotation Type","Category","State","Annotation Description","Source Code"
			"1","RULE","Technical Rule","CANDIDATE","Technical Rule Candidate [System identified]","IF MYSQLCA-SQLCODE = ZERO
			             PERFORM FETCH-C-VSAMK
			           END-IF"
			"2","DATABASE","Read","CANDIDATE","System identified Database Query","EXEC SQL
			             FETCH  C-VSAMK
			             INTO :KSDS-PRIMARY-INDEX
			           END-EXEC"
			"3","RULE","Technical Rule","CANDIDATE","Technical Rule Candidate [System identified]","IF SQLCODE = ZERO
			             ADD 1 TO MYSQLIN-COUNTER
			             DISPLAY 'C-VSAMK FETCH=' MYSQLCA-SQLCODE
			                   ' rec=' MYSQLIN-COUNTER
			                   ' data=' KSDS-PRIMARY-INDEX
			           ELSE
			             DISPLAY 'C-VSAMK FETCH SQLCODE=' MYSQLCA-SQLCODE
			           END-IF"
			"4","DATABASE","Close","CANDIDATE","System identified Database Query","EXEC SQL CLOSE C-VSAMK END-EXEC\"
			""";
		assertEquals(exp, actualJobResult);
	}

	@Test
	void testOrderedAnnotationRulesNonLinearControlFlow() throws IOException {
		final var moduleId = createModule(projectId, "BR_NON_LINEAR_FLOW", "BR_NON_LINEAR_FLOW.cbl", RESOURCE_PATH);
		submitIdentifyCandidatesJob(projectId, moduleId);
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("moduleId", Collections.singletonList(moduleId.getNid().toString()));
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", Arrays.asList("content.name", "content.sourceAttachment"));

		//Ast Node tracing is really broken due to the broken parser.
		//Fails due to the 'WHEN 5 OR 6' statement, because the start node is the DISPLAY a line below, and the End node it the 6.
		String actualJobResult = convertJobResultToString(getJobResult(projectId, parameters));
		String expected = """
			"Execution Order","Annotation Description","Source Code"
			"1","Data Validation Rule Candidate [System identified]","EVALUATE A
			             WHEN 5 OR 6
			               DISPLAY '5'
			           END-EVALUATE"
			"2","Data Validation Rule Candidate [System identified]","EVALUATE TRUE
			             WHEN A 
			                DISPLAY '1'
			             WHEN OTHER
			                EVALUATE TRUE
			                   WHEN A 
			                      DISPLAY '1'
			                   WHEN OTHER
			                      DISPLAY 'OTHER'
			                END-EVALUATE"
			"3","Data Validation Rule Candidate [System identified]","EVALUATE A
			             WHEN 5
			               DISPLAY '5'
			             WHEN 42
			               EVALUATE A
			                  WHEN 5
			                     DISPLAY '5'
			                  WHEN 42
			                     DISPLAY '42'
			               END-EVALUATE"
			"4","Data Validation Rule Candidate [System identified]","IF A = 1
			              EVALUATE A
			                 WHEN 5
			                    DISPLAY '5'
			                 WHEN 42
			                    DISPLAY '42'
			              END-EVALUATE"
			"5","Data Validation Rule Candidate [System identified]","EVALUATE A
			              WHEN 5
			                DISPLAY '5'
			              WHEN 42
			                IF A = 1
			                  DISPLAY '1'
			                END-IF
			           END-EVALUATE"
			"6","Data Validation Rule Candidate [System identified]","EVALUATE TRUE ALSO TRUE
			             WHEN B > 2 ALSO B < 4
			                DISPLAY 'WHAT'
			           END-EVALUATE"
			"7","Data Validation Rule Candidate [System identified]","EVALUATE TRUE ALSO TRUE
			             WHEN B > 2 ALSO A < 4
			               EVALUATE TRUE ALSO TRUE
			                  WHEN B > 3 ALSO A < 3
			                     DISPLAY 'WHAT 2'
			                  END-EVALUATE"
			"8","Data Validation Rule Candidate [System identified]","EVALUATE TRUE ALSO A
			             WHEN B = 4
			                DISPLAY 'WHAT'
			             WHEN A AND B ALSO 42
			                DISPLAY 'IS THIS'
			           END-EVALUATE"
			"9","Data Validation Rule Candidate [System identified]","EVALUATE TRUE ALSO A
			             WHEN B = 4
			                DISPLAY 'WHAT'
			             WHEN A = 42
			                DISPLAY 'IS THIS'
			           END-EVALUATE"
			"10","Data Validation Rule Candidate [System identified]","EVALUATE A
			                 WHEN 5
			                    DISPLAY '5'
			                 WHEN 42
			                    DISPLAY '42'
			           END-EVALUATE"
			"11","Data Validation Rule Candidate [System identified]","EVALUATE A
			              WHEN 0 THRU 5
			                 DISPLAY ""0-5""
			              WHEN 6
			                 DISPLAY ""0""
			              WHEN OTHER
			                 DISPLAY ""OTHER""
			           END-EVALUATE"
			""";
		actualJobResult = actualJobResult.trim();
		expected = expected.trim();

		/* Replace multiple consecutive spaces with a single space */
		actualJobResult = actualJobResult.replaceAll("\\s+", " ");
		expected = expected.replaceAll("\\s+", " ");

		/* Normalize line endings to Unix LF */
		actualJobResult = actualJobResult.replace("\r\n", "\n");
		expected = expected.replace("\r\n", "\n");
		assertEquals(expected, actualJobResult);
	}

	@SuppressWarnings("unchecked")
	@Test
	void testOrderedAnnotationRuleJobThrowsExceptionIfNoAnnotationRulesPresent() {
		final var moduleId = createModule(projectId, "MMRS71Z3", "MMRS71Z3.cbl", RESOURCE_PATH);
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("moduleId", Collections.singletonList(moduleId.getNid().toString()));
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", Arrays.asList("content.type", "content.categoryName", "content.state", "content.name",
				"content.sourceAttachment", "content.module.taxonomies.name", "content.updatedByUserId"));
		final Result<Serializable> result = (Result<Serializable>) jobManager
				.getJobResult(submitJob(jobManager, tracer, orderedAnnotationRuleCSVExporter.createJob(projectId, parameters)));
		assertEquals("No Annotations are present for this Module with ID :" + EntityId.of(moduleId.getNid()).toString(), assertNotNull(result).status.getMessage());
	}

	@SuppressWarnings("unchecked")
	@Test
	void testOrderedAnnotationRuleJobThrowsExceptionIfModuleIdIsNull() {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", Arrays.asList("content.type", "content.categoryName", "content.state", "content.name",
				"content.sourceAttachment", "content.module.taxonomies.name", "content.updatedByUserId"));

		final Result<Serializable> result = (Result<Serializable>) jobManager
				.getJobResult(submitJob(jobManager, tracer, orderedAnnotationRuleCSVExporter.createJob(projectId, parameters)));
		assertEquals("Module Id is null", assertNotNull(result).status.getMessage());
	}

	private void submitIdentifyCandidatesJob(final EntityId projectId, final EntityId moduleId) {
		submitJob(jobManager, tracer, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
	}

	private JobResult getJobResult(final EntityId projectId, final Map<String, List<String>> parameters) {
		return miningJobService.getJobResult(UUID.fromString(submitJob(jobManager, tracer, orderedAnnotationRuleCSVExporter.createJob(projectId, parameters))))
				.orElseThrow(() -> new IllegalStateException("Failed to execute export Job."));
	}

	private EntityId createModule(final EntityId projectId, final String name, final String file, final String dir) {
		final String content = getContent(file, dir);
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(name);
		module.setProject(projectId);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(IDENTIFIED);
		module.setOrigin(CUSTOM);
		module.setPath(dir + file);
		module.setContent(content);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}

	/**
	 * Creates the Project.
	 */
	private void createProject() {
		final ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setName("Project_1");
		project.setClient(EntityId.of(LONG_ONE));
		project.setNatures(Collections.emptySet());
		projectId = projectService.create(project).identity();
	}

	private String getContent(final String file, final String dir) {
		final Path path = Paths.get(System.getProperty("user.dir"), dir, file);
		try {
			return new String(Files.readAllBytes(path), Charset.forName("Cp1252"));
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			throw new IllegalStateException("Exception occured while file reading", e);
		}
	}

	private static String convertJobResultToString(final JobResult result) throws IOException {
		final StringBuilder stringBuilder = new StringBuilder();
		final BufferedReader reader = new BufferedReader(new InputStreamReader(result.getContent(), StandardCharsets.UTF_8));

		String line;
		while ((line = reader.readLine()) != null) {
			stringBuilder.append(line).append("\n");
		}
		return stringBuilder.toString();
	}
}
