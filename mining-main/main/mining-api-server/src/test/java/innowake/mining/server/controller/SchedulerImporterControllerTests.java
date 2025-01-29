/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.controller;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.JobTestHelper;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SchedulerInfoService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.scheduler.SchedulerEntryPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryType;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojo;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import javax.persistence.EntityNotFoundException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test for {@link SchedulerImporterController}
 */
@AutoConfigureMockMvc
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class SchedulerImporterControllerTests extends DatabaseRelatedTest {

	private final MockMvc mockMvc;
	private final ModuleService moduleService;
	private final SourceService sourceService;
	private final SchedulerInfoService schedulerInfoService;
	private final JobManager jobManager;

	@Autowired
	public SchedulerImporterControllerTests(final MockMvc mockMvc, final ModuleService moduleService, final SchedulerInfoService schedulerInfoService,
			final SourceService sourceService, final JobManager jobManager) {
		this.mockMvc = mockMvc;
		this.moduleService = moduleService;
		this.sourceService = sourceService;
		this.schedulerInfoService = schedulerInfoService;
		this.jobManager = jobManager;
	}

	private static final Long ONE = 1L;
	private static final Path BASE_PATH = Paths.get("./test-resources/innowake/mining/server/scheduler");

	@BeforeEach
	void deleteAllModules() {
		moduleService.deleteModules(EntityId.of(ONE), true, false);
	}

	@Test
	void testBareMinimum() throws Exception {
		setupTestDataAndRunImport();

		final List<SchedulerEntryPojo> entries = schedulerInfoService.findEntries(builder -> builder.ofProject(EntityId.of(ONE)));
		final List<SchedulerImportPojo> imports = schedulerInfoService.findImports(Pagination.FIRST, q -> q.ofProject(EntityId.of(ONE))).getContent();

		assertEquals(1, imports.size(), "Import count does not match");
		assertTrue(imports.get(0).getProperties().isEmpty(), "Import properties should be empty");
		assertEquals(13, entries.size(), "Total entry count does not match");
		assertEquals(3, entries.stream()
				.map(SchedulerEntryPojo::getType)
				.filter(SchedulerEntryType.JOB::equals)
				.count(), "Total job entry count does not match");
		assertEquals(6, entries.stream()
				.map(SchedulerEntryPojo::getType)
				.filter(SchedulerEntryType.CONDITION::equals)
				.count(), "Total condition entries does not match");
		assertEquals(2, entries.stream()
				.map(SchedulerEntryPojo::getType)
				.filter(SchedulerEntryType.FOLDER::equals)
				.count(), "Total folder entries does not match");

		final SchedulerEntryPojo root = entries.stream()
				.filter(entry -> entry.getIdentifier()
						.equals("root"))
				.findFirst()
				.orElseThrow(() -> new AssertionError("Root entry not found"));

		final SchedulerEntryPojo table = entries.stream()
				.filter(entry -> entry.getIdentifier()
						.equals("DEFTABLE"))
				.findFirst()
				.orElseThrow(() -> new AssertionError("Def Table entry not found"));

		final SchedulerEntryPojo smartFolder = entries.stream()
				.filter(entry -> entry.getIdentifier()
						.equals("SMART_FOLDER"))
				.findFirst()
				.orElseThrow(() -> new AssertionError("Smart Folder entry not found"));

		final SchedulerEntryPojo subFolder = entries.stream()
				.filter(entry -> entry.getIdentifier()
						.equals("SUB_FOLDER"))
				.findFirst()
				.orElseThrow(() -> new AssertionError("Sub Folder entry not found"));

		final SchedulerEntryPojo anyJob = entries.stream()
				.filter(entry -> entry.getIdentifier()
						.equals("JOB"))
				.findFirst()
				.orElseThrow(() -> new AssertionError("JOB entry not found"));

		assertEquals(root.getUid(), table.getContainedIn(), "Def table not contained in root");
		assertEquals(table.getUid(), smartFolder.getContainedIn(), "Smart Folder not contained in Def table");
		assertEquals(smartFolder.getUid(), subFolder.getContainedIn(), "Sub Folder not contained in Smart Folder");
		assertEquals(subFolder.getUid(), anyJob.getContainedIn(), "Job not contained in Sub Folder");
	}

	@Test
	void testFileUploadAndDelete() throws Exception {
		final Path path = Paths.get("simple_controlm_import.xml");
		final byte[] content = Files.readAllBytes(BASE_PATH.resolve(path));
		final MockMultipartFile schedulerImportFile = new MockMultipartFile("file", "filename.xml", "text/xml", content);
		final var uploadResult = mockMvc.perform(multipart("/api" + IoController.PROJECT_AS_FILE_URL, ONE)
						.file(schedulerImportFile)
						.contentType(MediaType.MULTIPART_FORM_DATA_VALUE))
				.andExpect(status().isOk())
				.andReturn();
		final UUID sourceId = UUID.fromString(uploadResult.getResponse().getContentAsString().replace("\"", ""));

		final BinaryString savedContent = sourceService.getContent(sourceId, EntityId.of(ONE));
		assertEquals(content.length, savedContent.get().length);

		mockMvc.perform(delete("/api" + IoController.PROJECT_AS_FILE_URL, ONE)
						.queryParam("fileId", sourceId.toString())
						.contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isNoContent());
		assertThrows(EntityNotFoundException.class, () -> sourceService.getContent(sourceId, EntityId.of(ONE)));
	}

	private void setupTestDataAndRunImport() throws Exception {
		schedulerInfoService.deleteSchedulerImport(builder -> builder.ofProject(EntityId.of(ONE)));

		final Path path = Paths.get("simple_controlm_import.xml");
		final byte[] content = Files.readAllBytes(BASE_PATH.resolve(path));
		final MockMultipartFile schedulerImportFile = new MockMultipartFile("file", "filename.xml", "text/xml", content);
		final var result = mockMvc.perform(multipart("/api" + IoController.PROJECT_AS_FILE_URL, ONE)
						.file(schedulerImportFile)
						.contentType(MediaType.MULTIPART_FORM_DATA_VALUE))
				.andExpect(status().isOk())
				.andReturn();
		final UUID sourceId = UUID.fromString(result.getResponse().getContentAsString().replace("\"", ""));

		final String json = """
				{
					"schedulerType": "CONTROL_M",
					"identifier": "controlM1",
					"importerUsed": "default",
					"description": "ControlM Import",
					"source": "%s"
				}
				""".formatted(sourceId);

		final var importResult = mockMvc.perform(post("/api" + SchedulerImporterController.CREATE_SCHEDULER_IMPORT, ONE)
						.content(json)
						.contentType("application/json"))
				.andExpect(status().isCreated())
				.andReturn();
		final String jobId = importResult.getResponse().getContentAsString();
		assertNotNull(jobId, "Job ID is null");
		JobTestHelper.waitForJobCompletion(jobId.replace("\"", ""), jobManager, 30, TimeUnit.SECONDS);
	}
}
