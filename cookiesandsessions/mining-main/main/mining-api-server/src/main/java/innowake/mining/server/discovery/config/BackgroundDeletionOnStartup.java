/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.MiningApiApplication;
import innowake.mining.server.aspect.WithSystemUser;
import innowake.mining.server.job.deletion.BackgroundDeletionExecutionCallback;
import innowake.mining.server.job.deletion.BackgroundDeletionJob;
import innowake.mining.server.util.TracingHelper;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.ProjectService;

/**
 * Triggers {@link BackgroundDeletionJob} during server startup.
 */
@Component
public class BackgroundDeletionOnStartup {

	@Autowired
	private transient ProjectService projectService;

	@Autowired
	private transient ClientService clientService;
	
	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private TracingHelper tracingHelper;
	private static final Logger LOG = LoggerFactory.getLogger(MiningApiApplication.class);
	private static final String PROPERTY_NAME = "startup.background-deletion-job";
	
	/**
	 * Checks if there were projects or clients marked as to be deleted and submits the {@link BackgroundDeletionJob}.
	 *
	 * @return indicates whether the {@link BackgroundDeletionJob} is submitted
	 */
	@EventListener(value = ApplicationReadyEvent.class, condition = "@environment.getProperty('startup.background-deletion-job')")
	@WithSystemUser
	public boolean checkAndTriggerBackgroundDeletionJob() {
		final long toBeDeletedProjects = countToBeDeletedProjects();
		final long toBeDeletedClients = countToBeDeletedClients();
		if (toBeDeletedProjects + toBeDeletedClients > 0) {
			final String jobId = BackgroundDeletionExecutionCallback.submit(jobManager, tracingHelper);
			LOG.info(() -> String.format("Identified %d client(s) and %d project(s) marked as to-be-deleted.", toBeDeletedClients, toBeDeletedProjects));
			LOG.info(() -> String.format(
					"BackgroundDeletionJob: %s is submitted. This job may consume lots of resources. To skip from auto-triggering, "
					+ "restart the server with the %s configuration set to false.", jobId, PROPERTY_NAME));
			return true;
		} else {
			LOG.debug(() -> "No clients and projects marked for deletion");
		}
		return false;
	}

	/**
	 * @return number of Projects marked to be deleted.
	 */
	long countToBeDeletedProjects() {
		return projectService.count(q -> q.filterMarkedForDeletion(Boolean.TRUE)).longValue();
	}

	/**
	 * @return number of Clients marked to be deleted.
	 */
	long countToBeDeletedClients() {
		return clientService.count(Boolean.TRUE).longValue();
	}

}
