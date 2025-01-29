/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.deletion;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.util.TracingHelper;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.ProjectPojo;

/**
 * Job for background deletion of projects and clients.
 * <p>Before submitting this job you must ensure that it's not already running as it tries to delete all projects and clients marked for deletion.
 * Use method {@link BackgroundDeletionExecutionCallback#submit(JobManager, TracingHelper)} to submit this job, which does the check for you. Otherwise
 * use the job description ({@value #DESCRIPTION}) for checking with the {@link JobManager}.</p>
 * <p>Additionally use the {@link BackgroundDeletionExecutionCallback} as a {@link JobExecutionCallback} when submitting this job.</p>
 */
public class BackgroundDeletionJob extends MiningJob<Serializable> {

	private static final Logger LOG = LoggerFactory.getLogger(BackgroundDeletionJob.class);

	/** The description of the job. Can be used to check if the job is already running:
	 * <pre>
	 * final Optional<JobInformation> deletionJob = jobManager.getJobs(new JobPropertyFilter(FilterProperty.STATUS, CompareOperator.EQUALS, JobStatus.RUNNING))
	 *	.stream()
	 *	.filter(jobInfo -> BackgroundDeletionJob.DESCRIPTION.equals(jobInfo.getJobDescription()))
	 *	.findAny();
	 * </pre>*/
	public static final String DESCRIPTION = "BACKGROUND DELETION JOB";

	@Autowired
	private transient ProjectService projectService;

	@Autowired
	private transient ClientService clientService;

	public BackgroundDeletionJob() {
		super(EntityId.VOID);
	}
	
	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription(DESCRIPTION);

		final Result<Serializable> projectResult = deleteProjects();
		Result<Serializable> clientResult;
		try {
			clientService.deleteClients();
			clientResult = new Result<>(Status.OK); 
		} catch (final Exception e) {
			clientResult = new Result<>(new Status(e));
		}

		return projectResult.status == Status.OK ? clientResult : projectResult;
	}

	@Override
	public String getJobName() {
		return "Background Deletion";
	}

	/**
	 * @return a {@link Page} of {@code project} that were marked as to be deleted.
	 */
	List<ProjectPojo> getToBeDeletedProjects() {
		return projectService.find(q -> q.filterMarkedForDeletion(Boolean.TRUE));
	}

	private Result<Serializable> deleteProjects() {
		LOG.debug("Deleting all projects marked for deletion.");

		final List<ProjectPojo> projects = getToBeDeletedProjects();
		LOG.debug(() -> String.format("Found %d projects to be deleted.", Integer.valueOf(projects.size())));

		Result<Serializable> result = null;
		for (final ProjectPojo project : projects) {
			try {
				final Long clientId = assertNotNull(project.getClientNid());
				final Long projectId = assertNotNull(project.getId());

				LOG.debug(() -> String.format("Deleting project '%s' (ID: %d)", project.getName(), projectId));
				projectService.deleteProjectCascading(clientId, projectId);
				LOG.info(() -> String.format("Deleted project '%s' (ID: %d)", project.getName(), projectId));
			} catch (final Exception e) {
				LOG.error(() -> String.format("An error occurred while deleting project '%s' (ID: %d)", project.getName(), project.getId()), e);
				result = new Result<>(new Status(e));
			}
		}

		LOG.debug(() -> "All projects deleted.");
		return result == null ? new Result<>(Status.OK) : result;
	}

	/**
	 * @return number of Clients marked to be deleted.
	 */
	long countToBeDeletedClients() {
		return clientService.count(Boolean.TRUE).longValue();
	}

}
