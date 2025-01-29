/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration;

import static innowake.lib.core.lang.Assert.fail;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
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
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import brave.Tracer;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.extensions.export.taxonomy.TaxonomyExporter;
import innowake.mining.extensions.export.utils.ExportTestUtils;
import innowake.mining.server.JobTestHelper;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.TaxonomyController;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.service.MiningJobService;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.access.EntityId;

/**
 * Test case to validate Export and Import of taxonomy assignments with physical and virtual modules
 * @author aasaraf
 *
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
@WithMockUser
class TaxonomyExportImportTest  extends DatabaseResettingTest {

	private static final Long PROJECT_ID_FOR_JCL_TAXONOMY = Long.valueOf(1);
	
	private static final Path EXPECTED_EXPORT_FOLDER = Paths.get("./test-resources/innowake/mining/server/integration/taxonomy/export");
	private static final Path ROOT = Paths.get("./test-resources/innowake/mining/server/integration/taxonomy/import");
	
	private static final String EXPECTED_FILE_FOR_JCL_TAXONOMY = "testForJclTaxonomy.csv";
	private static final String EXPECTED_FILE_NAME = "VirtualModuleImport.csv";
	private static final String EXPECTED_VALIDATE_FOLDER = ROOT + "/expected-validate/";
	private static final String EXPECTED_IMPORT_FOLDER = ROOT + "/expected-import/";
	private static final String TAXONOMY_IMPORT_VALIDATE_URL = "/api/v1/projects/1/taxonomies/import/validate";
	@Autowired
	private TaxonomyExporter taxonomyExporter;
	
	@Autowired
	private MockMvc mvc;
	
	@Autowired
	private JobConfigurationProperties jobConfig;
	
	@Autowired
	private Tracer tracer;
	@Autowired
	private JobManager jobManager;
	@Autowired
	private MiningJobService miningJobService;
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	@Test
	void testExportForJclTaxonomyTypes() throws Exception {
		/* Test Virtual Modules file export. */
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(PROJECT_ID_FOR_JCL_TAXONOMY));
		final Path expectedFilePath = getExpectedFilePath(EXPECTED_FILE_FOR_JCL_TAXONOMY);
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("modules"));
		parameters.put("$columns", Arrays.asList( 
				"content.name","content.path", "content.objectTypeLink.technologyLink", "content.objectTypeLink.typeLink"
		));
		final JobResult exportValue = getJobResult(EntityId.of(PROJECT_ID_FOR_JCL_TAXONOMY), parameters);
		final String[] actualCsvArr = ExportTestUtils.jobResultToString(exportValue).split("\n");
		Arrays.sort(actualCsvArr, 1, actualCsvArr.length);
		final String actualCsv = ExportTestUtils.jobResultToString(exportValue);
		areStringsEqualRegardlessOfOrder(read(expectedFilePath), actualCsv.trim());
		
		final FileInputStream fis = new FileInputStream(expectedFilePath.toString());
		final MockMultipartFile source = new MockMultipartFile("file", fis);
		
		/* Test Virtual Module file import validation. */
		final File validExpected = new File(EXPECTED_VALIDATE_FOLDER + StringUtils.replace(EXPECTED_FILE_NAME, ".csv", ".json"));
		final MvcResult result = mvc.perform(multipart(TAXONOMY_IMPORT_VALIDATE_URL).file(source)
				.contentType(MediaType.MULTIPART_FORM_DATA))
				.andDo(print())
				.andExpect(status().isOk()).andReturn();
		final String jobId = result.getResponse().getContentAsString().replace("\"", "");
		
		JobTestHelper.waitForJobCompletion(jobId, jobManager, 2, TimeUnit.SECONDS);
		final String expectedJSON = FileUtils.readFileToString(validExpected, StandardCharsets.UTF_8);
		final String actualJSON = FileUtils.readFileToString(Paths.get(jobConfig.getJobResultFolder(), jobId).toFile(), StandardCharsets.UTF_8);
		areStringsEqualRegardlessOfOrder(expectedJSON, actualJSON);
		
		/* Test Virtual Module file import validation. */
		mvc.perform(multipart("/api" + TaxonomyController.TAXONOMY_IMPORT_URL, PROJECT_ID_FOR_JCL_TAXONOMY)
				.file(source)
				.contentType(MediaType.MULTIPART_FORM_DATA))
				.andExpect(status().isAccepted())
				.andReturn();
		final File importExpected = new File(EXPECTED_IMPORT_FOLDER + EXPECTED_FILE_NAME);
		/* The exported file and the expected file should match. */
		areStringsEqualRegardlessOfOrder(FileUtils.readFileToString(importExpected, StandardCharsets.UTF_8), actualCsv);
	}
	
	private Path getExpectedFilePath(final String expectedFileName) {
		final Path expectedFile = EXPECTED_EXPORT_FOLDER.resolve(expectedFileName);
		if (Files.notExists(expectedFile)) {
			fail("Expected file does not exist : " + expectedFile.toString());
		}
		return expectedFile;
	}
	
	private String read(final Path path) throws IOException {
		return Files.readAllLines(path, Charset.forName("Cp1252")).stream().collect(Collectors.joining("\n"));
	}

	/**
	 * Optionally returns an additional SQL script file to be executed against Postgres.
	 *
	 * @return SQLs to be executed 
	 */
	@Override
	protected List<String> getAdditionalPgScriptFile() {
		return List.of("test-data-taxonomy-export-import");
	}

	private JobResult getJobResult(final EntityId projectId, final Map<String, List<String>> parameters) {
		return miningJobService.getJobResult(UUID.fromString(BaseDiscoveryTest.submitJob(jobManager, tracer, taxonomyExporter.createJob(projectId, parameters))))
				.orElseThrow(() -> new IllegalStateException("Failed to execute export Job."));
	}

	private boolean areStringsEqualRegardlessOfOrder(final String string1, final String string2) {
		/* Split the strings into lines */
		final String[] linesInString1 = getSortedProcessedLines(string1);
		final String[] linesInString2 = getSortedProcessedLines(string2);

		/* Compare the sorted lines */
		return Arrays.equals(linesInString1, linesInString2);
	}

	private String[] getSortedProcessedLines(final String input) {
		/* Split the strings into lines */
		final String[] lines = input.split("\\r?\\n");

		/* Remove leading and trailing whitespace from each line */
		for (int i = 0; i < lines.length; i++) {
			lines[i] = lines[i].trim();
		}

		return lines;
	}
}
