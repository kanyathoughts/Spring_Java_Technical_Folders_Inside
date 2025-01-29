/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.manager;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.net.ConnectException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.apache.http.HttpStatus;
import org.apache.http.impl.EnglishReasonPhraseCatalog;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.security.storage.StorageException;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.job.JobServiceProvider;
import innowake.mining.job.plugin.MiningJobPlugin;
import innowake.mining.job.plugin.progress.RemoteProgressJob;
import innowake.mining.plugin.MiningJobGroup;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.model.JobInfoFieldName;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.ResultStatus;

/**
 * Manages all jobs that have been or are currently being executed in the context of the current workspace.
 */
public class RemoteJobManager {

	private static final String ERROR_TITLE_ADD = "Unable to get initial job information";
	private static final String ERROR_TITLE_CANCEL = "Unable to cancel job";
	private static final String ERROR_TITLE_READ_JOB_INFO = "Unable to read persisted job information";
	private static final String ERROR_TITLE_WRITE_JOB_INFO = "Unable to persist job information";
	private static final String ERROR_TITLE_CONNECTION = "Unable to connect";
	private static final String ERROR_TITLE_UPDATE_JOBS = "Error while updating the workspace job information";
	private static final String ERROR_TITLE_NO_RESULT = "No job result";
	private static final String ERROR_TITLE_REQUESTING_RESULT = "Unable to get job result";
	private static final String ERROR_TITLE_UNSUPPORTED_RESULT = "Unsupported job result";
	private static final EnumSet<JobStatus> ACTIVE_JOB_STATUS = EnumSet.of(JobStatus.RUNNING, JobStatus.SCHEDULED, JobStatus.CANCEL_REQUESTED);

	private static final long SCHEDULE_IN_1_MINUTE = 60000;
	private static final long SCHEDULE_IN_5_SECONDS = 5000;

	private final Path jobInfoFile;
	private final UpdateAllJob updateAllJob;
	private final UpdateActiveJob updateActiveJob;
	private final Map<String, RemoteProgressJob> remoteProgressJobs = new ConcurrentHashMap<>();
	private final JobResultHandler jobResultHandler;

	/* Using project name as key instead of IProject, as we need to be able to persist the map. */
	private final Map<String, Set<RemoteJobInfo>> jobsForProject = new ConcurrentHashMap<>();
	private final Map<String, IProject> projectNameToInstance = new ConcurrentHashMap<>();
	@Nullable
	private StateUpdateListener stateUpdateListener;
	private boolean isOffline = true;
	private int statusCode;

	/**
	 * Returns {@code true} if the job is currently in an active state. Meaning {@link JobStatus#SCHEDULED}, {@link JobStatus#RUNNING}
	 * or {@link JobStatus#CANCEL_REQUESTED}.
	 *
	 * @param jobInfo the {@link JobInformation} instance
	 * @return {@code true} if the job is in an active state; {@code false} otherwise
	 */
	public static boolean isJobInActiveState(final JobInformation jobInfo) {
		final JobStatus status = jobInfo.getStatus();
		return status == JobStatus.SCHEDULED || status == JobStatus.RUNNING || status == JobStatus.CANCEL_REQUESTED;
	}

	/**
	 * Constructor.
	 *
	 * @param storagePath the {@link IPath} pointing to a folder where the internal state should be persisted to
	 */
	public RemoteJobManager(final IPath storagePath) {
		final IPath jobInformation = storagePath.append("jobInformation.json");
		/* An IPath outside of the eclipse workspace (.metadata) cannot be directly accessed as an eclipse resource, therefore plain Java NIO. */
		this.jobInfoFile = jobInformation.toFile().toPath();
		this.updateAllJob = new UpdateAllJob(this);
		this.updateActiveJob = new UpdateActiveJob(this);
		this.jobResultHandler = new JobResultHandler();
	}

	/**
	 * Sets the {@link StateUpdateListener} that will be informed whenever this managers state is updated.
	 * <p>
	 * This replaces any previous set listener.
	 *
	 * @param listener the {@link StateUpdateListener}
	 */
	public void setStateUpdateListener(final StateUpdateListener listener) {
		this.stateUpdateListener = listener;
		/* Directly populate the current connection state to the view. */
		stateUpdateListener.transitionedConnectionMode(isOffline);
	}

	/**
	 * Removes a previously assigned {@link StateUpdateListener}.
	 */
	public void removeStateUpdateListener() {
		this.stateUpdateListener = null;
	}

	/**
	 * Initializes this manager with the state that had been previously persisted.
	 */
	public void init() {
		if (Files.exists(jobInfoFile)) {
			try {
				final String content = new String(Files.readAllBytes(jobInfoFile), StandardCharsets.UTF_8);
				/* Get the persisted job information and move them to concurrent Map and Set instances. */
				final Map<String, Set<RemoteJobInfo>> persistedJobs = PojoMapper.jsonReaderFor(new TypeReference<Map<String, Set<RemoteJobInfo>>>() {})
																					.readValue(content);
				for (final Map.Entry<String, Set<RemoteJobInfo>> entry : persistedJobs.entrySet()) {
					final Set<RemoteJobInfo> remoteJobInfos = ConcurrentHashMap.newKeySet();
					entry.getValue().forEach(remoteJobInfos::add);
					jobsForProject.put(entry.getKey(), remoteJobInfos);
				}
			} catch (final IOException e) {
				MiningJobPlugin.handleError(ERROR_TITLE_READ_JOB_INFO, e, true);
			}
		}

		if ( ! jobsForProject.isEmpty()) {
			/* Get the actual IProject instances for the project names. */
			jobsForProject.keySet().forEach(projectName -> {
				final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
				if (project != null) {
					projectNameToInstance.put(projectName, project);
				}
			});
		}

		refreshUi();
		if ( ! jobsForProject.isEmpty()) {
			/* Only trigger the global update job if there are any jobs known to this workspace. This avoids unnecessary
			 * re-scheduling of this job in case there is no reachable server. */
			updateAllJob.schedule();
		}
	}

	/**
	 * Stops this manager by persisting the current state.
	 */
	public void stop() {
		updateAllJob.cancel();
		updateActiveJob.cancel();
		remoteProgressJobs.entrySet().removeIf(entry -> {
			final RemoteProgressJob job = entry.getValue();
			/* Do not actually cancel the job here, as closing of eclipse must not send any
			 * cancel requests to the remote jobs! */
			job.setForceFinish(true);
			return true;
		});

		try {
			final String content = PojoMapper.jsonWriter().writeValueAsString(jobsForProject);
			Files.createDirectories(jobInfoFile.getParent());
			Files.write(jobInfoFile, content.getBytes(StandardCharsets.UTF_8));
		} catch (final IOException e) {
			MiningJobPlugin.handleError(ERROR_TITLE_WRITE_JOB_INFO, e, true);
		}
	}

	/**
	 * Updates the state of all jobs known to this manager. This will be called by {@link UpdateAllJob}.
	 */
	public void updateJobInformation() {
		if (jobsForProject.isEmpty()) {
			return;
		}

		/* Query the latest job info from the server and update the local instances. */
		final Set<Tuple2<String, JobInformation>> remoteJobInfos = queryJobInformations(jobsForProject);
		final Set<String> receivedJobIds = new HashSet<>();
		for (final Tuple2<String, JobInformation> remoteJobInfo : remoteJobInfos) {
			final String jobId = remoteJobInfo.b.getJobId();
			final Set<RemoteJobInfo> existingRemoteJobInfos = jobsForProject.get(remoteJobInfo.a);
			if (existingRemoteJobInfos != null) {
				final Optional<RemoteJobInfo> existingRemoteJobInfo = existingRemoteJobInfos.stream()
						.filter(rji -> rji.getJobInfo().getJobId().equals(jobId))
						.findFirst();
				if (existingRemoteJobInfo.isPresent()) {
					existingRemoteJobInfo.get().setJobInfo(remoteJobInfo.b);
				}
			}
			receivedJobIds.add(jobId);
		}

		/* Remove any orphaned entries that do not exist on the server side. */
		jobsForProject.values().stream()
						.flatMap(Set<RemoteJobInfo>::stream)
						.filter(rji -> ! receivedJobIds.contains(rji.getJobInfo().getJobId()))
						.collect(Collectors.toSet())
						.forEach(this::removeJob);

		refreshUi();
	}

	/**
	 * Updates the state of all jobs known to this manager that are in an active state, meaning scheduled,
	 * running or requested to cancel. This will be called by {@link UpdateActiveJob}.
	 */
	public void updateActiveJobInformation() {
		if (jobsForProject.isEmpty()) {
			return;
		}

		/* Get all jobs that are in a running state. */
		final Map<String, Set<RemoteJobInfo>> runningJobs = new HashMap<>();
		for (final Map.Entry<String, Set<RemoteJobInfo>> entry : jobsForProject.entrySet()) {
			final Set<RemoteJobInfo> jobs = entry.getValue();
			final Set<RemoteJobInfo> running = jobs.stream().filter(rji -> isJobInActiveState(rji.getJobInfo())).collect(Collectors.toSet());
			runningJobs.put(entry.getKey(), running);
		}
		if ( ! runningJobs.isEmpty()) {
			/* Query the latest job info from the server and update the local instances. */
			final Set<Tuple2<String, JobInformation>> remoteJobInfos = queryJobInformations(runningJobs);
			final Map<String, JobInformation> receivedJobInformations = new HashMap<>();
			for (final Tuple2<String, JobInformation> remoteJobInfo : remoteJobInfos) {
				receivedJobInformations.put(remoteJobInfo.b.getJobId(), remoteJobInfo.b);
				final Set<RemoteJobInfo> existingRemoteJobInfos = jobsForProject.get(remoteJobInfo.a);
				if (existingRemoteJobInfos != null) {
					final Optional<RemoteJobInfo> existingRemoteJobInfo = existingRemoteJobInfos.stream()
							.filter(rji -> rji.getJobInfo().getJobId().equals(remoteJobInfo.b.getJobId()))
							.findFirst();
					if (existingRemoteJobInfo.isPresent()) {
						existingRemoteJobInfo.get().setJobInfo(remoteJobInfo.b);
					}
				}
			}

			if (statusCode == HttpStatus.SC_INTERNAL_SERVER_ERROR) {
				final String statusMsg = String.format("Update of job information failed. Contact an administrator. Error: %d - %s",
											Integer.valueOf(statusCode), EnglishReasonPhraseCatalog.INSTANCE.getReason(statusCode, null));
				jobsForProject.values().stream()
								.flatMap(Set<RemoteJobInfo>::stream)
								.filter(rji -> ACTIVE_JOB_STATUS.contains(rji.getJobInfo().getStatus()) && ! receivedJobInformations.containsKey(rji.getJobInfo().getJobId()))
								.forEach(rji -> rji.setJobInfo(JobInformation.copy(rji.getJobInfo())
										 .setStepDescription(statusMsg)
										 .setStatus(JobStatus.UNKNOWN)
										 .build()));
			}

			refreshUi();
		}
	}

	/**
	 * Adds a new job to this manager. The manager will request the initial {@link JobInformation} for the job
	 * with the provided Id upon calling this method.
	 *
	 * @param project the {@link IProject} that submitted the job
	 * @param jobId the Id of the job
	 */
	public void addJob(final IProject project, final String jobId) {
		final Optional<JobInformation> jobInformationOptional = MiningServiceExecutor
				.create(() -> getJobServiceProvider(project).getJobInfo().setJobId(jobId))
				.setValidResultConsumer(jobInfoFromResult -> {
					transitionToOnlineMode();
					if (jobInfoFromResult == null) {
						MiningJobPlugin.handleError(ERROR_TITLE_ADD, "Did not receive valid information for job with Id '" + jobId + "'", true);
					}
				})
				.setInvalidResultConsumer(invalidResult -> {
					transitionToOnlineMode();
					if (invalidResult.getStatusCode() == 404) {
						/* The server doesn't know a job with this Id, therefore we don't manage it. */
						MiningJobPlugin.handleError(ERROR_TITLE_ADD, "Job with the Id '" + jobId + "' is unknown to the job server.", true);
					} else {
						MiningJobPlugin.handleError(ERROR_TITLE_ADD, invalidResult.getExtendedStatusMessage(), true);
					}
				})
				.setExceptionConsumer(exception -> {
					if (exception instanceof ConnectException) {
						transitionToOfflineMode();
						MiningJobPlugin.handleError(ERROR_TITLE_CONNECTION, exception, true);
					} else {
						MiningJobPlugin.handleError(ERROR_TITLE_ADD, exception, true);
					}
				})
				.execute();

		if ( ! jobInformationOptional.isPresent()) {
			return;
		}

		try {
			final String projectName = project.getName();
			final Set<RemoteJobInfo> remoteJobInfos = jobsForProject.computeIfAbsent(projectName, name -> ConcurrentHashMap.newKeySet());
			remoteJobInfos.add(new RemoteJobInfo(jobInformationOptional.get(), ApiClient.getConnectionInfo(project).getUrl(), projectName));
			projectNameToInstance.put(projectName, project);
			refreshUi();
		} catch (final CoreException | StorageException e) {
			MiningJobPlugin.handleError(ERROR_TITLE_ADD, e, true);
		}
	}

	/**
	 * Removes a job from this manager.
	 * <p>
	 * This will not trigger an implicit refresh of the UI.
	 *
	 * @param remoteJobInfo the {@link RemoteJobInfo}
	 */
	public void removeJob(final RemoteJobInfo remoteJobInfo) {
		jobsForProject.get(remoteJobInfo.getProjectName()).remove(remoteJobInfo);

		final RemoteProgressJob remoteProgressJob = removeRemoteProgressJob(remoteJobInfo.getJobInfo().getJobId());
		if (remoteProgressJob != null) {
			remoteProgressJob.setForceFinish(true);
		}
	}

	/**
	 * Removes the {@link RemoteProgressJob} from this manager matching the provided job Id.
	 * This will be called by the {@link RemoteProgressJob} itself upon finishing, so there
	 * is no need to cancel it here.
	 *
	 * @param jobId the Id of the job
	 * @return the removed {@link RemoteProgressJob} or {@code null}
	 */
	@Nullable
	public RemoteProgressJob removeRemoteProgressJob(final String jobId) {
		return remoteProgressJobs.remove(jobId);
	}

	/**
	 * Requests to cancel the job with the provided Id.
	 *
	 * @param jobId the Id of the job to cancel
	 */
	public void requestJobCancel(final String jobId) {
		IProject tempProjectVariable = null;
		for (final Map.Entry<String, Set<RemoteJobInfo>> entry : jobsForProject.entrySet()) {
			if (entry.getValue().stream().anyMatch(rji -> rji.getJobInfo().getJobId().equals(jobId))) {
				tempProjectVariable = projectNameToInstance.get(entry.getKey());
				break;
			}
		}
		final IProject project = assertNotNull(tempProjectVariable);
		MiningServiceExecutor
		.create(() -> getJobServiceProvider(project).cancelJob().setJobId(jobId))
		.setValidResultConsumer(jobInfo -> transitionToOnlineMode())
		.setInvalidResultConsumer(invalidResult -> {
			transitionToOnlineMode();
			MiningJobPlugin.handleError(ERROR_TITLE_CANCEL, invalidResult.getExtendedStatusMessage(), true);
		})
		.setExceptionConsumer(exception -> {
			if (exception instanceof ConnectException) {
				transitionToOfflineMode();
				MiningJobPlugin.handleError(ERROR_TITLE_CONNECTION, exception, true);
			} else {
				MiningJobPlugin.handleError(ERROR_TITLE_CANCEL, exception, true);
			}
		})
		.execute();
	}

	/**
	 * Handles the result of a job if it has one.
	 *
	 * @param remoteJobInfo the {@link RemoteJobInfo} instance of the job to handle the result for
	 */
	public void handleJobResult(final RemoteJobInfo remoteJobInfo) {
		final JobInformation jobInfo = remoteJobInfo.getJobInfo();
		final ResultStatus resultStatus = jobInfo.getResultStatus();
		if (resultStatus == null || ! resultStatus.hasCollectableResult()) {
			MiningJobPlugin.handleError(ERROR_TITLE_NO_RESULT, "The job with the Id '" + jobInfo.getJobId() + "' and description '"
					+ jobInfo.getJobDescription() + "' has no downloadable result.", true);
			return;
		}

		/* Execute the whole request and processing of the result in an eclipse job to avoid freezing of the eclipse UI during a possible larger download. */
		final Job job = Job.create("Getting result of '" + jobInfo.getJobId() + "'", (@Nullable final IProgressMonitor monitor) ->
		MiningServiceExecutor
		.create(() ->
		getJobServiceProvider(projectNameToInstance.get(remoteJobInfo.getProjectName()))
		.getJobResult()
		.setJobId(jobInfo.getJobId())
		.setCustomResponseHandler(jobResultHandler))
		.setValidResultConsumer(jobResultContainer -> {
			transitionToOnlineMode();
			if (jobResultContainer != null) {
				/* We currently expect a downloadable result, handled by the JobResultHandler.
				 * Any custom serialized objects are not supported and therefore not handled. */
				MiningJobPlugin.handleError(ERROR_TITLE_UNSUPPORTED_RESULT,
						"A job result of type " + jobResultContainer.getObject().getClass().getCanonicalName() + " is not supported.", true);
			}
		})
		.setInvalidResultConsumer(invalidResult -> {
			transitionToOnlineMode();
			MiningJobPlugin.handleError(ERROR_TITLE_REQUESTING_RESULT, invalidResult.getExtendedStatusMessage(), true);
		})
		.setExceptionConsumer(exception -> {
			if (exception instanceof ConnectException) {
				transitionToOfflineMode();
				MiningJobPlugin.handleError(ERROR_TITLE_CONNECTION, exception, true);
			} else {
				MiningJobPlugin.handleError(ERROR_TITLE_REQUESTING_RESULT, exception, true);
			}
		})
		.execute()
				);
		job.setSystem(false);
		job.setJobGroup(MiningJobGroup.INSTANCE);
		job.schedule();
	}

	/**
	 * @return an unmodifiable map of all jobs known to this manager mapped by eclipse project name
	 */
	public Map<String, Set<RemoteJobInfo>> getJobs() {
		return Collections.unmodifiableMap(jobsForProject);
	}

	/**
	 * Refreshes the state of all involved UI components.
	 */
	public void refreshUi() {
		updateRemoteProgressJobs();
		if (stateUpdateListener != null) {
			stateUpdateListener.remoteJobsUpdated();
		}
	}

	/**
	 * @return the {@link JobServiceProvider} to execute REST calls
	 *
	 * @param project the {@link IProject} to get the connection settings for
	 * @throws CoreException if unable to resolve the {@link JobServiceProvider}
	 * @throws StorageException if unable to resolve the {@link JobServiceProvider}
	 */
	protected JobServiceProvider getJobServiceProvider(final IProject project) throws CoreException, StorageException {
		return ApiClient.jobService(project);
	}

	/**
	 * @param jobInfo the initial {@link JobInformation} instance
	 * @return the {@link RemoteProgressJob} instance
	 */
	protected RemoteProgressJob getRemoteProgressJob(final JobInformation jobInfo) {
		return new RemoteProgressJob(this, jobInfo);
	}

	private Set<Tuple2<String, JobInformation>> queryJobInformations(final Map<String, Set<RemoteJobInfo>> jobsToQuery) {
		final Set<Tuple2<String, JobInformation>> result = new HashSet<>();
		for (final Map.Entry<String, Set<RemoteJobInfo>> entry : jobsToQuery.entrySet()) {
			final List<String> ids = new ArrayList<>();
			final Set<RemoteJobInfo> remoteJobInfos = entry.getValue();
			if ( ! remoteJobInfos.isEmpty()) {
				remoteJobInfos.forEach(remoteJobInfo -> ids.add(remoteJobInfo.getJobInfo().getJobId()));
				final Map<JobInfoFieldName, Map<String, Object>> filterObject = new HashMap<>();
				filterObject.put(JobInfoFieldName.ID, Map.of(FilterOperators.OPERATOR_IN, ids));
				final IProject project = projectNameToInstance.get(entry.getKey());
				if (project != null) {
					final Optional<JobInformation[]> requestOptional = executeJobQueryForProjects(project, filterObject);
					if (requestOptional.isPresent()) {
						final JobInformation[] jobInfos = requestOptional.get();
						for (final JobInformation jobInfo : jobInfos) {
							result.add(Tuple2.of(entry.getKey(), jobInfo));
						}
					}
				}
			}
		}
		return result;
	}
	
	private Optional<JobInformation[]> executeJobQueryForProjects(final IProject project, final Map<JobInfoFieldName, Map<String, Object>> filterObject) {
		statusCode = HttpStatus.SC_OK;
		return MiningServiceExecutor
				.create(() -> getJobServiceProvider(project).getJobInfos().setFilter(filterObject))
				.setValidResultConsumer(jobInfos -> transitionToOnlineMode())
				.setInvalidResultConsumer(invalidResult -> {
					statusCode = invalidResult.getStatusCode();
					transitionToOnlineMode();
					MiningJobPlugin.handleError(ERROR_TITLE_UPDATE_JOBS, EnglishReasonPhraseCatalog.INSTANCE.getReason(statusCode, null)
																			+ "\n\n" + invalidResult.getExtendedStatusMessage(), false);
				})
				.setExceptionConsumer(exception -> {
					if (exception instanceof ConnectException) {
						transitionToOfflineMode();
						MiningJobPlugin.handleError(ERROR_TITLE_CONNECTION, exception, false);
					} else {
						statusCode = HttpStatus.SC_INTERNAL_SERVER_ERROR;
						MiningJobPlugin.handleError(ERROR_TITLE_UPDATE_JOBS, exception, false);
					}
				})
				.execute();
	}

	private RemoteProgressJob scheduleRemoteProgressJob(final JobInformation jobInfo) {
		final RemoteProgressJob remoteJob = getRemoteProgressJob(jobInfo);
		remoteJob.schedule();
		return remoteJob;
	}

	private void updateRemoteProgressJobs() {
		final Set<RemoteJobInfo> remoteJobInfos = jobsForProject.values().stream().flatMap(Set<RemoteJobInfo>::stream).collect(Collectors.toSet());
		for (final RemoteJobInfo remoteJobInfo : remoteJobInfos) {
			final JobInformation jobInfo = remoteJobInfo.getJobInfo();
			final String jobId = jobInfo.getJobId();
			final RemoteProgressJob remoteProgressJob = remoteProgressJobs.get(jobId);

			/* Update existing RemoteProgressJobs or create new ones for active remote jobs. */
			if (remoteProgressJob != null) {
				remoteProgressJob.setJobInfo(jobInfo);
			} else if (isJobInActiveState(jobInfo)) {
				remoteProgressJobs.put(jobId, scheduleRemoteProgressJob(jobInfo));
			}
		}

		if (remoteProgressJobs.isEmpty()) {
			updateActiveJob.cancel();
		} else {
			updateActiveJob.schedule(SCHEDULE_IN_5_SECONDS);
		}
	}

	private void transitionToOnlineMode() {
		isOffline = false;
		/* mark all jobs currently contained in the progress view as online. */
		remoteProgressJobs.values().forEach(job -> job.setOffline(false));

		if (stateUpdateListener != null) {
			stateUpdateListener.transitionedConnectionMode(false);
		}
	}

	private void transitionToOfflineMode() {
		isOffline = true;
		/* stop updating active jobs until we have a connection again. */
		updateActiveJob.cancel();
		/* schedule global update to check if connection is back again. */
		updateAllJob.schedule(SCHEDULE_IN_1_MINUTE);
		/* mark all jobs currently contained in the progress view as offline. */
		remoteProgressJobs.values().forEach(job -> job.setOffline(true));

		if (stateUpdateListener != null) {
			stateUpdateListener.transitionedConnectionMode(true);
		}
	}

	/**
	 * Listener to inform about various state changes like receiving job information
	 * updates or when transitioning between online and offline mode.
	 */
	public static interface StateUpdateListener {

		/**
		 * Called whenever the job information has been updated.
		 */
		void remoteJobsUpdated();

		/**
		 * Called whenever transitioning between online and offline mode.
		 *
		 * @param offline {@code true} when entering offline mode; {@code false} otherwise
		 */
		void transitionedConnectionMode(final boolean offline);
	}

	/**
	 * Holds all information of a managed remote job like the {@link JobInformation}, the URL of the API server
	 * where the job had been submitted to and the name of the eclipse project.
	 */
	public static class RemoteJobInfo implements Comparable<RemoteJobInfo> {

		@JsonProperty("jobInfo")
		private JobInformation jobInfo;
		@JsonProperty("apiServer")
		private final String apiServer;
		@JsonProperty("projectName")
		private final String projectName;

		/**
		 * Constructor.
		 *
		 * @param jobInfo the {@link JobInformation}
		 * @param apiServer the URL of the API server where the job had been submitted to
		 * @param projectName the name of the eclipse project
		 */
		@JsonCreator
		public RemoteJobInfo(@JsonProperty("jobInfo") final JobInformation jobInfo, @JsonProperty("apiServer") final String apiServer,
				@JsonProperty("projectName") final String projectName) {
			this.jobInfo = jobInfo;
			this.apiServer = apiServer;
			this.projectName = projectName;
		}

		/**
		 * @return the {@link JobInformation}
		 */
		public JobInformation getJobInfo() {
			return jobInfo;
		}

		/**
		 * Sets a new {@link JobInformation} instance.
		 *
		 * @param jobInfo the new {@link JobInformation} instance
		 */
		public void setJobInfo(final JobInformation jobInfo) {
			this.jobInfo = jobInfo;
		}

		/**
		 * @return the URL of the API server where the job had been submitted to
		 */
		public String getApiServer() {
			return apiServer;
		}

		/**
		 * @return the name of the eclipse project
		 */
		public String getProjectName() {
			return projectName;
		}

		@Override
		public int hashCode() {
			return jobInfo.getJobId().hashCode();
		}

		@Override
		public boolean equals(@Nullable final Object obj) {
			if (obj == null) {
				return false;
			}
			if (obj == this) {
				return true;
			}
			if (obj.getClass() != getClass()) {
				return false;
			}
			final RemoteJobInfo other = (RemoteJobInfo) obj;
			return jobInfo.getJobId().equals(other.jobInfo.getJobId());
		}

		@Override
		public int compareTo(@Nullable final RemoteJobInfo o) {
			if (o != null) {
				return jobInfo.getJobId().compareTo(o.jobInfo.getJobId());
			}
			return 0;
		}
	}

}
