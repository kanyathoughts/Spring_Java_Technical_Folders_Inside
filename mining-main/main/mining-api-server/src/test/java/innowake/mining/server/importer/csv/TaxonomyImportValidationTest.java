/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.importer.csv;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.mining.server.JobTestHelper;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.model.TaxonomyImportValidationResult;

/**
 * Test cases for taxonomy import validation. The file's header, modules, taxonomy types and
 * taxonomies are validated and the expected json
 * {@linkplain TaxonomyImportValidationResult response} is compared.
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
@WithMockUser
class TaxonomyImportValidationTest extends DatabaseRelatedTest {

	@Autowired
	private JobManager jobManager;
	@Autowired
	private JobConfigurationProperties jobConfig;
	@Autowired
	private MockMvc mvc;

	private static final String ROOT = "./test-resources/innowake/mining/extensions/import/taxonomy";
	private static final String EXPECTED_FOLDER = ROOT + "/expected-validate/";
	private static final String SOURCE_FOLDER = ROOT + "/source";
	private static final String FILE_PARSE_FAILURE = SOURCE_FOLDER + "/overall-result-error/file-parse-error";
	private static final String TAXONOMY_IMPORT_VALIDATE_URL = "/api/v1/projects/2/taxonomies/import/validate";

	@ParameterizedTest
	@MethodSource("fileProvider")
	void testTaxonomyImportValidation(final String filePath) throws Exception {
		final String fileDirectory = StringUtils.substringBeforeLast(filePath, File.separator);
		final String fileName = StringUtils.substringAfterLast(filePath, File.separator);
		final FileInputStream fis = new FileInputStream(filePath);
		final File expected = new File(EXPECTED_FOLDER +
				StringUtils.replace(fileName, ".csv", ".json"));
		final MockMultipartFile source = new MockMultipartFile("file", fis);

		final String expectedJSON = FileUtils.readFileToString(expected, StandardCharsets.UTF_8);
		final String actualJSON;

		final MvcResult result = mvc.perform(multipart(TAXONOMY_IMPORT_VALIDATE_URL).file(source)
				.contentType(MediaType.MULTIPART_FORM_DATA_VALUE))
				.andDo(print())
				.andExpect(status().isOk())
				.andReturn();

		if( ! Paths.get(fileDirectory).equals(Paths.get(FILE_PARSE_FAILURE))) {
			final String jobId = result.getResponse().getContentAsString().replace("\"", "");
			JobTestHelper.waitForJobCompletion(jobId, jobManager, 10, TimeUnit.SECONDS);
			actualJSON = FileUtils.readFileToString(Paths.get(jobConfig.getJobResultFolder(), jobId).toFile(), StandardCharsets.UTF_8);
		} else {
			actualJSON = result.getResponse().getContentAsString();
		}

		Assertions.assertEquals(expectedJSON, actualJSON);
	}

	private Stream<String> fileProvider() throws IOException {
		return Files.walk(Paths.get(SOURCE_FOLDER)).filter(Files::isRegularFile)
				.map(path -> path.toFile().getPath());
	}

	@Override
	protected List<String> getAdditionalPgScriptFile() {
		return List.of("test-data-taxonomy-import");
	}
}
