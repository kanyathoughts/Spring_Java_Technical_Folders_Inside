/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.job;

import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.job.genai.MonitoredTaskManager;
import innowake.mining.server.job.genai.MonitoredTaskManagerService;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;

import java.io.Serializable;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import static org.mockito.Mockito.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.job.api.JobInfoService;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.error.PermissionException;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.job.genai.GenerateModuleDescriptionsJob;
import innowake.mining.server.service.GenAIAvailabilityService;
import innowake.mining.server.service.GenerativeModuleDescriptionService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests the bulk module description generation. We don't actually create any modules in the DB:
 * We mock the {@linkplain ModuleService} and make it return certain Modules for certain module IDs.
 * The {@linkplain GenerativeModuleDescriptionService} and {@linkplain GenAIAvailabilityService} are also mocked and we check that the {@link GenerateModuleDescriptionsJob} calls them correctly.
 */
@WithMockUser
class GenerateModuleDescriptionsJobTest extends DatabaseRelatedTest {
	
	private static final EntityId PROJECT_ID = EntityId.of(1L);
	
	private ModulePojo moduleEmptyDescription;
	private ModulePojo moduleExistingDescription;
	private ModulePojo moduleNullDescription;
	private ModulePojo moduleNoContent;
	
	@MockBean
	ModuleService moduleService;
	
	@MockBean
	GenAIAvailabilityService availabilityService;
	
	@MockBean
	GenerativeModuleDescriptionService generativeModuleDescriptionService;

	@MockBean
	MonitoredTaskManagerService monitoredTaskManagerService;

	@MockBean
	ProgressMonitor progressMonitor;

	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private JobInfoService jobInfoService;
	
	@Autowired
	private Tracer tracer;
	
	@BeforeEach
	void setupTestDataAndMocks() {
		moduleEmptyDescription = ModulePojoDummy.build(new ModulePojoPrototype()
			.setProject(PROJECT_ID)
			.setName("Module 1")
			.setTechnology(Technology.COBOL)
			.setType(Type.PROGRAM)
			.setStorage(Storage.FILE)
			.setIdentification(Identification.IDENTIFIED)
			.setOrigin(Origin.CUSTOM)
			.setNid(1L)
			.setContent("some cobol code")
			.setDescription("")
		);

		moduleExistingDescription = ModulePojoDummy.build(new ModulePojoPrototype()
			.setProject(PROJECT_ID)
			.setName("Module 2")
			.setTechnology(Technology.COBOL)
			.setType(Type.PROGRAM)
			.setStorage(Storage.FILE)
			.setIdentification(Identification.IDENTIFIED)
			.setOrigin(Origin.CUSTOM)
			.setNid(2L)
			.setContent("some cobol code")
			.setDescription("Some user written description")
		);
		
		moduleNullDescription = ModulePojoDummy.build(new ModulePojoPrototype()
			.setProject(PROJECT_ID)
			.setName("Module 2")
			.setTechnology(Technology.COBOL)
			.setType(Type.PROGRAM)
			.setStorage(Storage.FILE)
			.setIdentification(Identification.IDENTIFIED)
			.setOrigin(Origin.CUSTOM)
			.setNid(3L)
			.setContent("some cobol code")
		);
		
		moduleNoContent = ModulePojoDummy.build(new ModulePojoPrototype()
				.setProject(PROJECT_ID)
				.setName("Module 3")
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setNid(4L)
				.setContent("")
			);

		when(moduleService.findAnyModule(any())).then(i -> {
			@SuppressWarnings("unchecked")
			final BuildingConsumer<ModuleService.ModuleInquiryBuilder> builderConsumer = (BuildingConsumer<ModuleService.ModuleInquiryBuilder>) i.getArgument(0);
			final ModuleService.ModuleInquiryBuilder builder = Mockito.mock(ModuleService.ModuleInquiryBuilder.class);
			final ArgumentCaptor<EntityId> entityIdCaptor = ArgumentCaptor.forClass(EntityId.class);

			/* Avoid NPEs when chaining is used when calling filter methods */
			when(builder.includeContent(anyBoolean())).thenReturn(builder);
			when(builder.byId(any())).thenReturn(builder);
			when(builder.ofProject(any())).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder).byId(entityIdCaptor.capture());

			switch (entityIdCaptor.getValue().getNid().intValue()) {
				case 1:
					return Optional.of(moduleEmptyDescription);
				case 2:
					return Optional.of(moduleExistingDescription);
				case 3:
					return Optional.of(moduleNullDescription);
				case 4:
					return Optional.of(moduleNoContent);
				default:
					return Optional.empty();
			}
		});
		Mockito.when(moduleService.update(any())).thenReturn(null);
		Mockito.when(generativeModuleDescriptionService.deduceDescription(any()))
				.thenReturn("");
		final ExecutorService executorService =  Executors.newFixedThreadPool(5);
		MonitoredTaskManager monitoredTaskManager = new MonitoredTaskManager(progressMonitor, executorService, 1);
		Mockito.when(monitoredTaskManagerService.newTaskManager(any(), anyInt())).thenReturn(monitoredTaskManager);
	}
	
	/**
	 * Sets the availabilityService service to return false on call of {@linkplain GenAIAvailabilityService#isGenAIServiceAvailable()} and runs the Job.
	 * We expect the job to return a {@linkplain Severity#ERROR} result and we expect {@linkplain GenerativeModuleDescriptionService#deduceDescription} to
	 * never be called.
	 */
	@Test
	void testServiceUnavailable() {
		Mockito.doReturn(false).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("ids", Collections.singletonList(moduleEmptyDescription.getId().toString()));
		final GenerateModuleDescriptionsJob job = new GenerateModuleDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = JobUtil.submitJob(jobManager, tracer, job);

		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(generativeModuleDescriptionService, times(0)).deduceDescription(any());
		verify(monitoredTaskManagerService, times(0)).newTaskManager(any(), anyInt());
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.ERROR, result.status.getSeverity());
	}
	
	/**
	 * Sets the availabilityService service to return true on call of {@linkplain GenAIAvailabilityService#isGenAIServiceAvailable()} and runs the Job.
	 * All test modules IDs are passed with the id parameter and the overwrite flag is set to false. 
	 * We expect {@linkplain GenerativeModuleDescriptionService#deduceDescription(ModulePojo)} ()} to be called twice: once for the empty description and once the null
	 * description.
	 */
	@Test
	void testRunJobNoOverwrite() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		final List<String> moduleIds = new ArrayList<>();
		moduleIds.add(moduleEmptyDescription.getId().toString());
		moduleIds.add(moduleExistingDescription.getId().toString());
		moduleIds.add(moduleNullDescription.getId().toString());
		parameters.put("ids", moduleIds);
		final GenerateModuleDescriptionsJob job = new GenerateModuleDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = JobUtil.submitJob(jobManager, tracer, job);

		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(generativeModuleDescriptionService, times(1)).deduceDescription(moduleEmptyDescription);
		verify(generativeModuleDescriptionService, times(1)).deduceDescription(moduleNullDescription);
		verify(generativeModuleDescriptionService, times(0)).deduceDescription(moduleExistingDescription);
		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());


		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.OK, result.status.getSeverity());
	}
	
	/**
	 * Sets the availabilityService service to return true on call of {@linkplain GenAIAvailabilityService#isGenAIServiceAvailable()} and runs the Job.
	 * All test modules IDs are passed with the id parameter and the overwrite flag is set to true. 
	 * We expect {@linkplain GenerativeModuleDescriptionService#deduceDescription(ModulePojo)} ()} to be called three times: once for each module.
	 */
	@Test
	void testRunJobOverwrite() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		final List<String> moduleIds = new ArrayList<>();
		moduleIds.add(moduleEmptyDescription.getId().toString());
		moduleIds.add(moduleExistingDescription.getId().toString());
		moduleIds.add(moduleNullDescription.getId().toString());
		parameters.put("ids", moduleIds);
		parameters.put("overwrite", Collections.singletonList("true"));
		final GenerateModuleDescriptionsJob job = new GenerateModuleDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = JobUtil.submitJob(jobManager, tracer, job);
		
		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(generativeModuleDescriptionService, times(3)).deduceDescription(any());
		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());


		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.OK, result.status.getSeverity());
	}
	
	/**
	 * Sets the availabilityService service to return true on call of {@linkplain GenAIAvailabilityService#isGenAIServiceAvailable()} and runs the Job.
	 * Non-numeric values and the ID of a module with an empty description are passed with the ids parameter. 
	 * We expect {@linkplain GenerativeModuleDescriptionService#deduceDescription(ModulePojo)} ()} to be called once and the job to finish with errors.
	 */
	@Test
	void testRunJobInvalidIds() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		final List<String> moduleIds = new ArrayList<>();
		moduleIds.add("id-1");
		moduleIds.add(moduleEmptyDescription.getId().toString());
		moduleIds.add("id-2");
		moduleIds.add("");
		moduleIds.add("99");
		moduleIds.add("999");
		parameters.put("ids", moduleIds);
		final GenerateModuleDescriptionsJob job = new GenerateModuleDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = JobUtil.submitJob(jobManager, tracer, job);
		
		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(generativeModuleDescriptionService, times(1)).deduceDescription(moduleEmptyDescription);
		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());

		final var jobInfo = jobInfoService.get(UUID.fromString(jobId));
		assertEquals(5, jobInfo.getMessages().size());
		
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.ERROR, result.status.getSeverity());
		assertEquals("Some descriptions could not be generated", result.value);
	}
	
	/**
	 * Sets the availabilityService service to return true on call of {@linkplain GenAIAvailabilityService#isGenAIServiceAvailable()} and runs the Job.
	 * The ID of a module with no source content is passed with the ids parameter. 
	 * We expect the job to finish with a warning message indicating the description couldn't be created for the module without source content..
	 */
	@Test
	void testRunJobNoSourceContent() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		final List<String> moduleIds = new ArrayList<>();
		moduleIds.add(moduleNoContent.getId().toString());
		parameters.put("ids", moduleIds);
		final GenerateModuleDescriptionsJob job = new GenerateModuleDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = JobUtil.submitJob(jobManager, tracer, job);
		
		final var jobInfo = jobInfoService.get(UUID.fromString(jobId));
		assertEquals(1, jobInfo.getMessages().size());
		
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.WARNING, result.status.getSeverity());
		assertEquals("Some descriptions could not be generated", result.value);
	}
	
	@Test
	void testRunJobNoPermission() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		Mockito.when(generativeModuleDescriptionService.deduceDescription(any())).thenThrow(PermissionException.class);
		final var parameters = new HashMap<String, List<String>>();
		final List<String> moduleIds = new ArrayList<>();
		moduleIds.add(moduleEmptyDescription.getId().toString());
		moduleIds.add(moduleNullDescription.getId().toString());
		parameters.put("ids", moduleIds);
		final GenerateModuleDescriptionsJob job = new GenerateModuleDescriptionsJob(EntityId.of(1l), parameters);
		final String jobId = JobUtil.submitJob(jobManager, tracer, job);
		
		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(generativeModuleDescriptionService, times(2)).deduceDescription(any());
		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());

		final var jobInfo = jobInfoService.get(UUID.fromString(jobId));
		assertNotNull(jobInfo);
		
		assertEquals(2, jobInfo.getMessages().size());
		
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.WARNING, result.status.getSeverity());
	}

	@Test
	void testRunJobFailedInGenAi() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		Mockito.when(generativeModuleDescriptionService.deduceDescription(any())).thenThrow(UserFacingException.class);
		final var parameters = new HashMap<String, List<String>>();
		final List<String> moduleIds = new ArrayList<>();
		moduleIds.add(moduleEmptyDescription.getId().toString());
		moduleIds.add(moduleNullDescription.getId().toString());
		parameters.put("ids", moduleIds);
		final GenerateModuleDescriptionsJob job = new GenerateModuleDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = JobUtil.submitJob(jobManager, tracer, job);

		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(generativeModuleDescriptionService, times(2)).deduceDescription(any());
		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());


		final var jobInfo = jobInfoService.get(UUID.fromString(jobId));
		assertEquals(2, jobInfo.getMessages().size());

		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.ERROR, result.status.getSeverity());

	}

}
