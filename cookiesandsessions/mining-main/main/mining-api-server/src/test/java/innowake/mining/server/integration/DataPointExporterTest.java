/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration;

import static innowake.mining.server.JobTestHelper.waitForJobCompletion;
import static org.junit.Assert.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.extensions.export.csv.AbstractCSVExporterTest;
import innowake.mining.extensions.export.utils.ExportTestUtils;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.MetamodelController;
import innowake.mining.server.controller.job.JobController;
import innowake.mining.server.service.MiningJobService;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Integration Tests For DataPointConfluenceTableExporter and DataPointCsvExporter
 */
@AutoConfigureMockMvc
@ActiveProfiles(value =  Profiles.NO_AUTH, inheritProfiles = false )
class DataPointExporterTest extends AbstractCSVExporterTest {
	
	/* empty project */
	private static final EntityId PROJECT_ID = EntityId.of(4L);
	
	@Autowired
	private MockMvc mockMvc;
	
	@Autowired
	private JobManager jobManager;
	
	@Autowired
	protected MiningJobService jobResultService;

	@Autowired
	private ObjectMapper objectMapper;

	private EntityId rootJobId = EntityId.VOID;
	private EntityId stepAId = EntityId.VOID;
	private EntityId dummyprgId = EntityId.VOID;
	private EntityId dummyModuleId = EntityId.VOID;
	private EntityId annotationId = EntityId.VOID;
	private final CustomPropertyMetadata customPropertyMetadata = new CustomPropertyMetadata();

	final MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
	final MultiValueMap<String, String> formDataAnnotation = new LinkedMultiValueMap<>();
	
	@BeforeAll
	@Override
	public void insertTestData() {
		ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(PROJECT_ID);
		module.setName("ROOTJOB");
		module.setTechnology(Technology.COBOL);
		module.setType(Type.COPYBOOK);
		module.setStorage(Storage.FILE);
		module.setPath("ROOTJOB.CBL");
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		rootJobId = moduleService.create(module);

		module = new ModulePojoPrototype();
		module.setProject(PROJECT_ID);
		module.setName("ROOTJOB.STEPA");
		module.setTechnology(Technology.COBOL);
		module.setType(Type.EXEC_PGM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		stepAId = moduleService.create(module);

		module = new ModulePojoPrototype();
		module.setProject(PROJECT_ID);
		module.setName("DUMMYPRG");
		module.setTechnology(Technology.BASIC);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		dummyprgId = moduleService.create(module);
		formData.put("$query", Collections.singletonList("modules"));
		formData.put("$columns", Arrays.asList("content.name", "content.technology", "content.type"));

		module = new ModulePojoPrototype();
		module.setProject(PROJECT_ID);
		module.setName("DummyModule");
		module.setTechnology(Technology.BASIC);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		dummyModuleId = moduleService.create(module);
		annotationId = annotationService.create(createTestAnnotation(dummyModuleId));
		
		customPropertiesService.deleteProperties(PROJECT_ID);
		
		customPropertyMetadata.setName("AnnotationCustomcategory1");
		customPropertyMetadata.setLabel("AnnotationCustomcategory1");
		customPropertyMetadata.setFieldType(CustomPropertyFieldType.DEFAULT);
		customPropertyMetadata.setPluginVisible(false);
		customPropertyMetadata.setCustomViewIndex(Integer.valueOf(2));
		customPropertyMetadata.setDataType("STRING");
		customPropertyMetadata.setCustomCategory("Customcategory1");

		final Map<String, List<String>> annotationparameters = new HashMap<>();
		annotationparameters.put("$query", Collections.singletonList("annotations"));
		annotationparameters.put("$columns", Arrays.asList("content.module.name", "content.type",
				"content.customProperties.AnnotationCustomProperties4.AnnotationCustomcategory1"));
		for (final Map.Entry<String, List<String>> entry : annotationparameters.entrySet()) {
			final String key = entry.getKey();
			final List<String> values = entry.getValue();
			formDataAnnotation.put(key, values);
		}
	}
	
	
	@AfterAll
	void shutdown() {
		customPropertiesService.deleteProperties(PROJECT_ID);
	}
	
	@Test
	void testDataPointCsvExporter() throws Exception {
		final MvcResult postResult = mockMvc.perform(post("/api" + JobController.JOB_EXTENSION_V2_URL, PROJECT_ID.getNid(), "datapoint-csv")
				.contentType(MediaType.MULTIPART_FORM_DATA).params(formData))
				.andDo(print())
				.andExpect(status().isOk())
				.andReturn();
		
		final String testJobId = postResult.getResponse().getContentAsString().replaceAll("\"", "").trim();
		waitForJobCompletion(testJobId, jobManager, 1, TimeUnit.MINUTES);
		final Optional<JobResult> resultList = jobResultService.getJobResult(testJobId);
		assertFalse("Job Result Should Not be Empty", resultList.isEmpty());
		final JobResult jobResult = resultList.get();
		assertEquals("text/csv", jobResult.getContentType());

		ExportTestUtils.exportAndCompare(jobResult, "\"Module Name\",\"Technology\",\"Type\"",
													"\"DUMMYPRG\",\"BASIC\",\"PROGRAM\"",
													"\"DummyModule\",\"BASIC\",\"PROGRAM\"",
													"\"ROOTJOB\",\"COBOL\",\"COPYBOOK\"",
													"\"ROOTJOB.STEPA\",\"COBOL\",\"EXEC_PGM\"");
	}
	
	@Test
	void testDataPointConfluenceTableExporter() throws Exception {
		final MvcResult postResult = mockMvc.perform(post("/api" + JobController.JOB_EXTENSION_V2_URL, PROJECT_ID.getNid(), "datapoint-confluence-table")
				.contentType(MediaType.MULTIPART_FORM_DATA).params(formData))
				.andDo(print())
				.andExpect(status().isOk())
				.andReturn();
		
		final String JobId = postResult.getResponse().getContentAsString().replaceAll("\"", "").trim();
		waitForJobCompletion(JobId, jobManager, 1, TimeUnit.MINUTES);
		final Optional<JobResult> resultList = jobResultService.getJobResult(JobId);
		assertFalse("Job Result Should Not be Empty", resultList.isEmpty());
		final JobResult jobResult = resultList.get();
		assertEquals("text/plain", jobResult.getContentType());
		
		ModulePojo dummyprg = moduleService.getModule(dummyprgId);
		ModulePojo stepA = moduleService.getModule(stepAId);
		ModulePojo rootJob = moduleService.getModule(rootJobId);
		ModulePojo dummyModule = moduleService.getModule(dummyModuleId);
		ExportTestUtils.exportAndCompare(jobResult, "||Module Name||Technology||Type||",
				String.format("|[DUMMYPRG|http://localhost/#/project-4/module-%s/details/overview]|BASIC|PROGRAM|", dummyprg.getLinkHash()),
				String.format("|[DummyModule|http://localhost/#/project-4/module-2oNkon92C7v68iyWlNnkwh/details/overview]|BASIC|PROGRAM|",
						dummyModule.getLinkHash()),
				String.format("|[ROOTJOB.STEPA|http://localhost/#/project-4/module-%s/details/overview]|COBOL|EXEC_PGM|", stepA.getLinkHash()),
				String.format("|[ROOTJOB|http://localhost/#/project-4/module-%s/details/overview]|COBOL|COPYBOOK|", rootJob.getLinkHash()));
	}
	
	@Test
	void testDataPointCsvExporterForAnnotation() throws Exception {
		mockMvc.perform(post("/api" + MetamodelController.DEFINE_METADATA_DEFAULT_PROPERTY_URL, PROJECT_ID.getNid(), "Annotation", customPropertyMetadata.getName())
					.contentType("application/json")
					.content(objectMapper.writeValueAsString(customPropertyMetadata)))
					.andExpect(status().isOk()).andReturn();

		annotationService.update(new AnnotationPojoPrototype().withId(annotationId)
				.setCustomProperties(Map.of("AnnotationCustomProperties4",
						Map.of(customPropertyMetadata.getName(), List.of("red", "blue", "green")))));

		final MvcResult postResult = mockMvc.perform(post("/api" + JobController.JOB_EXTENSION_V2_URL, PROJECT_ID.getNid(), "datapoint-csv")
				.contentType(MediaType.MULTIPART_FORM_DATA).params(formDataAnnotation))
				.andDo(print())
				.andExpect(status().isOk())
				.andReturn();
		
		final String testJobId = postResult.getResponse().getContentAsString().replaceAll("\"", "").trim();
		waitForJobCompletion(testJobId, jobManager, 1, TimeUnit.MINUTES);
		final Optional<JobResult> resultList = jobResultService.getJobResult(testJobId);
		assertFalse("Job Result Should Not be Empty", resultList.isEmpty());
		final JobResult jobResult = resultList.get();
		assertEquals("text/csv", jobResult.getContentType());
		
		ExportTestUtils.exportAndCompare(jobResult, "\"Module Name\",\"Annotation Type\",\"AnnotationCustomcategory1\"",
													"\"DummyModule\",\"RULE\",\"[red, blue, green]\"");
	}
	
	private AnnotationPojoPrototype createTestAnnotation(final EntityId moduleId) {
		return new AnnotationPojoPrototype()
				.setState(WorkingState.CANDIDATE)
				.setType(AnnotationType.RULE)
				.setName("Annotation 1")
				.setModule(moduleId)
				.setLocation(new ModuleLocation(0, 10))
				.setCreatedByUserId("createdByUserId");
	}
	
}
