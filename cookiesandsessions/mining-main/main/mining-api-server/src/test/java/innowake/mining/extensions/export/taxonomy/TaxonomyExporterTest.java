/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.taxonomy;

import static innowake.lib.core.lang.Assert.fail;

import java.io.IOException;
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

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.extensions.export.utils.ExportTestUtils;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.DatabaseRelatedTest.ResetScriptFile;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.service.MiningJobService;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.access.EntityId;

/**
 * Tests for {@link TaxonomyExporter}.
 */
@WithMockUser
class TaxonomyExporterTest extends DatabaseRelatedTest {

	private static final Long PROJECT_ID_FOR_MULTIPLE_TAXONOMY_TYPES = Long.valueOf(1);
	private static final Long PROJECT_ID_FOR_MULTIPLE_TAXONOMY_VALUES_FOR_TYPE = Long.valueOf(2);
	private static final Long PROJECT_ID_FOR_NO_TAXONOMY_TYPES = Long.valueOf(3);

	private static final Path EXPECTED_FOLDER = Paths.get("./test-resources/innowake/mining/extensions/export/taxonomy");
	
	private static final Map<Long, String> EXPECTED_FILE_FOR_PROJECT_ID_MAP = new HashMap<>();
	
	static {
		EXPECTED_FILE_FOR_PROJECT_ID_MAP.put(PROJECT_ID_FOR_MULTIPLE_TAXONOMY_TYPES, "testMultipleTaxonomyTypes.csv.dump");
		EXPECTED_FILE_FOR_PROJECT_ID_MAP.put(PROJECT_ID_FOR_MULTIPLE_TAXONOMY_VALUES_FOR_TYPE, "testMultipleTaxonomyValuesForType.csv.dump");
		EXPECTED_FILE_FOR_PROJECT_ID_MAP.put(PROJECT_ID_FOR_NO_TAXONOMY_TYPES, "testNoTaxonomyTypes.csv.dump");
	}
	
	@Autowired
	private TaxonomyExporter taxonomyExporter;
	@Autowired
	private Tracer tracer;
	@Autowired
	private JobManager jobManager;
	@Autowired
	private MiningJobService miningJobService;
	
	@Autowired
	private ApplicationEventPublisher eventPublisher;

	@Test
	void testExportWithMultipleTaxonomyTypes() throws IOException {
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(PROJECT_ID_FOR_MULTIPLE_TAXONOMY_TYPES));
		testForProject(PROJECT_ID_FOR_MULTIPLE_TAXONOMY_TYPES);
	}
	
	@Test
	void testExportWithMultipleTaxonomyValuesForType() throws IOException {
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(PROJECT_ID_FOR_MULTIPLE_TAXONOMY_VALUES_FOR_TYPE));
		testForProject(PROJECT_ID_FOR_MULTIPLE_TAXONOMY_VALUES_FOR_TYPE);
	}
	
	@Test
	void testExportWithNoTaxonomyTypes() throws IOException {
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(PROJECT_ID_FOR_NO_TAXONOMY_TYPES));
		testForProject(PROJECT_ID_FOR_NO_TAXONOMY_TYPES);
	}

	private void testForProject(final Long projectId) throws IOException {
		final Path expectedFilePath = getExpectedFilePath(EXPECTED_FILE_FOR_PROJECT_ID_MAP.get(projectId));
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("modules"));
		parameters.put("$columns", Arrays.asList( "content.name","content.path", "content.technology", "content.type"));

		ExportTestUtils.exportAndCompare(getJobResult(projectId, parameters), read(expectedFilePath));
	}

	private Path getExpectedFilePath(final String expectedFileName) {
		final Path expectedFile = EXPECTED_FOLDER.resolve(expectedFileName);
		if (Files.notExists(expectedFile)) {
			fail("Expected file does not exist : " + expectedFile.toString());
		}
		return expectedFile;
	}

	private String[] read(final Path path) throws IOException {
		return Files.readAllLines(path, StandardCharsets.UTF_8).toArray(new String[0]);
	}
	
	/**
	 * The {@link ResetScriptFile#getScriptFile()} to be executed by default.
	 *
	 * @return the SQL script file to be executed
	 */
	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}
	
	private JobResult getJobResult(final Long projectId, final Map<String, List<String>> parameters) {
		return miningJobService.getJobResult(UUID.fromString(BaseDiscoveryTest.submitJob(jobManager, tracer, taxonomyExporter.createJob(EntityId.of(projectId), parameters))))
				.orElseThrow(() -> new IllegalStateException("Failed to execute export Job."));
	}
}
