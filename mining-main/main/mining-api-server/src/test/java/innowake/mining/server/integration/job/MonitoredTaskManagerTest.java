/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.job;


import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.UUID;
import java.util.function.Consumer;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.job.genai.MonitoredTaskManager;
import innowake.mining.server.job.genai.MonitoredTaskManagerService;
import innowake.mining.server.job.genai.MonitoredTaskParameter;
import innowake.mining.server.service.GenerativeReachabilityBlockDescriptionService;
import innowake.mining.shared.access.EntityId;

@WithMockUser
class MonitoredTaskManagerTest extends DatabaseRelatedTest {

	@Autowired
	MonitoredTaskManagerService monitoredTaskManagerService;

	@MockBean
	ProgressMonitor progressMonitor;

	@MockBean
	JobMonitor jobMonitor;

	@MockBean
	GenerativeReachabilityBlockDescriptionService generativeReachabilityBlockDescriptionService;

	final UUID uuid = UUID.randomUUID();
	MonitoredTaskManager monitoredTaskManager;
	String text;

	@BeforeEach
	void setupTestDataAndMocks() {
		 monitoredTaskManager = monitoredTaskManagerService.newTaskManager(progressMonitor,1);
		 text = "test";
	}

	@Test
	void testRunWithNumberOfProgressSteps() {
		monitoredTaskManager.run(this::doSomething, text);
		monitoredTaskManager.join();
		verify(progressMonitor, times(1)).begin(1);
		verify(progressMonitor, times(3)).checkCanceled();
		verify(generativeReachabilityBlockDescriptionService, times(1)).generateDescription(EntityId.of(uuid), uuid, true);

	}

	@Test
	void testRunWithStepDescription() {
		monitoredTaskManager.run(this::doSomething, text, monitoredTaskParameter -> "for step description supplier");
		monitoredTaskManager.join();
		verify(progressMonitor, times(2)).setStepDescription("for step description supplier");

	}

	@Test
	void testRunWithException() {
		when(generativeReachabilityBlockDescriptionService.generateDescription(any(),any(),anyBoolean())).thenThrow(new RuntimeException());
		final Consumer<Throwable> exceptionHandler = ex -> { jobMonitor.modifyPendingTasks(1);};
		monitoredTaskManager.run(this::doSomething, text, null, exceptionHandler);
		monitoredTaskManager.join();
		verify(jobMonitor, times(1)).modifyPendingTasks(1);

	}

	public void doSomething(final MonitoredTaskParameter parameter, final String text){
		generativeReachabilityBlockDescriptionService.generateDescription(EntityId.of(uuid), uuid, true);
	}

}
