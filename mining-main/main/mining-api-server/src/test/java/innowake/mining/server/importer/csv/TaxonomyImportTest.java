/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.importer.csv;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.mining.server.event.TaxonomiesModifiedEvent;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultMatcher;

import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.extensions.export.taxonomy.TaxonomyExporter;
import innowake.mining.extensions.export.utils.ExportTestUtils;
import innowake.mining.server.JobTestHelper;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.TaxonomyController;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.service.MiningJobService;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.service.UserRoleService;

/**
 * Test cases for taxonomy import service. The file is imported, the taxonomies are associated with
 * the modules and then the taxonomy export is performed to check if the taxonomies are assigned
 * properly.
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
@WithMockUser
class TaxonomyImportTest extends DatabaseResettingTest {
	private static final Long TEST_PROJECT_ID = Long.valueOf(2);
	private static final String ROOT = "./test-resources/innowake/mining/extensions/import/taxonomy";
	private static final String EXPECTED_FOLDER = ROOT + "/expected-import/";
	private static final String SOURCE_FOLDER = ROOT + "/source/";
	private static final String SOURCE_FOLDER_FAILURE = SOURCE_FOLDER + "overall-result-error";
	private static final String FILE_PARSE_FAILURE = SOURCE_FOLDER_FAILURE + "/file-parse-error";

	@Autowired
	private MockMvc mvc;

	@Autowired
	private TaxonomyExporter taxonomyExporter;
	@Autowired
	private JobManager jobManager;
	@Autowired
	private Tracer tracer;
	@Autowired
	private MiningJobService miningJobService;
	@Nullable
	@MockBean
	private UserRoleService userRoleService;
	@Autowired
	private transient ApplicationEventPublisher eventPublisher;

	@ParameterizedTest
	@MethodSource("fileProvider")
	void testTaxonomyImport(final String filePath) throws Exception {
		//This Event will reload the Taxonomies added via the SQL Script.
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(TEST_PROJECT_ID));

		final String fileDirectory = StringUtils.substringBeforeLast(filePath, File.separator);
		final FileInputStream fis = new FileInputStream(filePath);
		final MockMultipartFile source = new MockMultipartFile("file", fis);
		final ResultMatcher expectedResult;
		final String expectedFileName;
		if (Paths.get(fileDirectory).equals(Paths.get(FILE_PARSE_FAILURE))) {
			expectedResult = status().isBadRequest();
			expectedFileName = "ErrorWhileImportExpectedFile.csv";
		} else if (Paths.get(fileDirectory).equals(Paths.get(SOURCE_FOLDER_FAILURE))) {
			expectedResult = status().isAccepted();
			expectedFileName = "ErrorWhileImportExpectedFile.csv";
		} else {
			expectedResult = status().isAccepted();
			expectedFileName = StringUtils.substringAfterLast(filePath, File.separator);
		}
		final MvcResult result = mvc.perform(multipart("/api" + TaxonomyController.TAXONOMY_IMPORT_URL, TEST_PROJECT_ID)
				.file(source)
				.contentType(MediaType.MULTIPART_FORM_DATA))
				.andExpect(expectedResult)
				.andReturn();
		
		final String jobId = result.getResponse().getContentAsString().replaceAll("\"", "");
		
		try {
			JobTestHelper.waitForJobCompletion(jobId, jobManager, 10, TimeUnit.SECONDS);
		} catch (final IllegalArgumentException e) {
			/* Do nothing since it is expected to NOT have job ID for error files */
		}
		
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("modules"));
		parameters.put("$columns", Arrays.asList( 
				"content.name", "content.path", "content.technology", "content.type"
		));
		setupProjectAccesses(Arrays.asList(TEST_PROJECT_ID), Arrays.asList(TEST_PROJECT_ID));
		final JobResult jobResult = getJobResult(TEST_PROJECT_ID, parameters);
		final File expected = new File(EXPECTED_FOLDER + expectedFileName);
		
		/* The exported file and the expected file should match. */
		final String[] actualData = ExportTestUtils.jobResultToString(jobResult).split("\n");
		Arrays.sort(actualData, 1, actualData.length);
		final String[] expectedData = FileUtils.readFileToString(expected, StandardCharsets.UTF_8).split("\n");
		Arrays.sort(expectedData, 1, actualData.length);
		assertEquals(expectedData.length, actualData.length);

		final String expectedList = Stream.of(expectedData)
				.map(this::sortTaxonomyData)
				.collect(Collectors.joining(System.lineSeparator()));
		final String actualList = Stream.of(actualData)
				.map(this::sortTaxonomyData)
				.collect(Collectors.joining(System.lineSeparator()));

		MatcherAssert.assertThat(actualList, Matchers.equalTo(expectedList));
	}

	private static Stream<String> fileProvider() throws IOException {
		return Files.walk(Paths.get(SOURCE_FOLDER)).filter(Files::isRegularFile)
				.map(path -> path.toFile().getPath());
	}

	@Override
	protected List<String> getAdditionalPgScriptFile() {
		return List.of("test-data-taxonomy-import");
	}

	private JobResult getJobResult(final Long projectId, final Map<String, List<String>> parameters) {
		return miningJobService.getJobResult(UUID.fromString(BaseDiscoveryTest.submitJob(jobManager, tracer, taxonomyExporter.createJob(EntityId.of(projectId), parameters))))
				.orElseThrow(() -> new IllegalStateException("Failed to execute export Job."));
	}

	private void setupProjectAccesses(final List<Long> authProjectIds, final List<Long> userRoleProjectIds) {
		final UserRoleService userRoleService = assertNotNull(this.userRoleService);
		given(userRoleService.getProjectIds()).willReturn(userRoleProjectIds);
		
		final List<SimpleGrantedAuthority> authorities = new ArrayList<>(authProjectIds.size() * 2);
		authProjectIds.forEach(projectId -> {
				authorities.add(new SimpleGrantedAuthority(String.format("client-1-project-%d-viewer", projectId)));
				authorities.add(new SimpleGrantedAuthority(String.format("client-1-project-%d-mining", projectId)));
		});
		
		final Authentication auth = new UsernamePasswordAuthenticationToken("", "", authorities);
		final SecurityContext context = SecurityContextHolder.createEmptyContext();
		context.setAuthentication(auth);
		SecurityContextHolder.setContext(context);
	}
	
	private String sortTaxonomyData(final String input) {
		final List<String> placeholders = new ArrayList<>();
		final StringBuffer result = new StringBuffer();
		final Matcher matcher = Pattern.compile("\"(.*?)\"").matcher(input);
		while (matcher.find()) {
			placeholders.add(matcher.group(1));
			matcher.appendReplacement(result, "<placeholder" + (placeholders.size() - 1) + ">");
		}
		matcher.appendTail(result);
		final String[] elements = result.toString().split(",");
		final String[] resultList = new String[elements.length];
		for (int i = 0; i < elements.length; i++) {
			final String element = elements[i].trim().replaceAll("<placeholder" + i + ">", placeholders.get(i));
			if (element.contains(",")) {
				final String[] innerArray = Arrays.stream(element.split(",")).map(String::trim).sorted().toArray(String[]::new);
				resultList[i] = String.join(", ", innerArray);
			} else {
				resultList[i] = element;
			}
		}
		return String.join(",", resultList);
	}
}
