/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import static innowake.mining.plugin.base.JobUtil.POLL_INTERVAL;
import static innowake.mining.plugin.base.JobUtil.isJobNotRunning;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.RestService;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.JobLogUtil;
import innowake.mining.plugin.base.JobUtil;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.model.job.JobInformation;
import innowake.product.base.core.api.ApiException;
import innowake.product.base.core.api.util.ResourceUtil2;

/**
 * Executes Discovery on the Mining server.
 */
public abstract class AbstractBaseDiscoverHandler extends AbstractBaseHandler {

	public static final String FORCE_FULL_SCAN_COMMAND = "innowake.mining.discovery.commands.discoverMetricsForceFullScan";
	
	private static final DateTimeFormatter LOG_FILE_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd-hh-mm-ss");
	protected Optional<IStructuredSelection> selection = Optional.empty();
	
	@Nullable
	protected Shell shell;
	protected boolean forceFullScan;

	/**
	 * {@inheritDoc}
	 * 
	 * Creates and schedules a {@link Job} that invokes {@link #process(IProject, IProgressMonitor, ExecutionEvent)}. The jobs scheduling rule is determined by 
	 * {@link #shouldJobBlocking()}.
	 */
	@Override
	@Nullable
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		forceFullScan = event != null && FORCE_FULL_SCAN_COMMAND.equals(event.getCommand().getId());
		selection = SelectionUtil.getResourceSelection();
		shell = HandlerUtil.getActiveShell(event);
		final IProject project;
		try {
			project = getProject();
		} catch (final ValidationException e) {
			Logging.error(e.getLocalizedMessage(), e);
			return null;
		}
		
		final Job job = Job.create(jobName(), monitor -> {
			process(project, monitor, event);
		});
		if (shouldJobBlocking()) {
			job.setRule(project);
		}
		job.schedule();

		return null;
	}
	
	/**
	 * Job name to assign.
	 * E.g., Discover Code
	 *
	 * @return Job name.
	 */
	protected abstract String jobName();
	
	/**
	 * The RestService which has to be invoked as a job.
	 *
	 * @param project the {@link IProject}
	 * @return the rest service.
	 * @throws CoreException exception.
	 * @throws StorageException exception.
	 */
	protected abstract RestService<String> serviceToExecute(final IProject project) throws CoreException, StorageException;
	
	/**
	 * Gets the current selection.
	 * 
	 * @return {@link IStructuredSelection} the current selection.
	 */
	protected Optional<IStructuredSelection> getSelection() {
		return selection;
	}

	/**
	 * Sets the selection.
	 * 
	 * @param selection {@link IStructuredSelection} the current selection.
	 */
	protected void setSelection(final Optional<IStructuredSelection> selection) {
		this.selection = selection;
	}

	/**
	 * The process to be executed before the main action.
	 * For e.g., For Discover code job, the Source code & configurations need to be uploaded.
	 *
	 * @param project the IProject.
	 * @param monitor the progress monitor for this task
	 * @param event the ExecutionEvent that triggered this action
	 * @return true if it can continue execution.
	 */
	protected boolean preProcess(final IProject project, final IProgressMonitor monitor, @Nullable final ExecutionEvent event) {
		@SuppressWarnings("unchecked")
		final List<Object> selectedObjects = selection.get().toList();
		final List<IResource> selectedResources = new ArrayList<>();
		for (final Object obj : selectedObjects) {
			if (obj instanceof IResource) {
				selectedResources.add((IResource) obj);
			} else if (obj instanceof IJavaElement) {
				selectedResources.add(((IJavaElement) obj).getResource());
			}
		}
		new UploadConfigurationHandler().upload(project);
		try {
			if (preventSourceObjectUpload(event)) {
				Logging.info("Skipping SourceObject upload");
			} else {
				return new UploadSourceObjectsHandler().process(HandlerUtil.getActiveShell(event), project, selectedResources, true, false);
			}
		} catch (final ExecutionException e) {
			throw new IllegalStateException(e);
		}
		return true;
	}


	/**
	 * The process to be executed after the main action.
	 * For e.g., Once the Discover code job is executed, the Source code needs to be synch and the job log needs to be downloaded.
	 *
	 * @param project the IProject.
	 * @param monitor the progress monitor for this task
	 * @param jobInfo information about the job of the main action.
	 * @param event the ExecutionEvent that triggered this action
	 */
	protected void postProcess(final IProject project, final IProgressMonitor monitor, final JobInformation jobInfo, @Nullable final ExecutionEvent event) {
		postProcessDefault(project, jobInfo);
	}

	/**
	 * Default implementation of the post processing which should be called by any
	 * sub-class overriding {@link #postProcess}
	 *
	 * @param project the IProject.
	 * @param jobInfo information about the job of the main action.
	 */
	protected final void postProcessDefault(final IProject project, final JobInformation jobInfo) {
		final String logFileName = new StringBuilder(getLogPrefix())
				.append("-")
				.append(jobInfo.getJobId())
				.append("-")
				.append(LocalDateTime.now().format(LOG_FILE_FORMAT))
				.toString();
		try {
			JobLogUtil.saveJobLogAsZipOrAsLog(jobInfo.getJobId(), project, logFileName, false);
		} catch (final Exception e) {
			System.out.println(e);
		}

		new DownloadConfigurationHandler().download(project);
	}

	/**
	 * Triggers the Discovery job on the mining server, waits until the job on server is complete and makes another call to server for downloading the logs.
	 * Here are the sequence of steps.
	 * 	1.) Triggers the discovery job on server by making a rest call
	 * 	2.) Server returns the job id
	 * 	3.) Client uses the job id for checking if the job is complete by polling every {@code getPollIntervalSeconds()} seconds
	 * 	4.) Once the client sees the job is not running, it calls server for Job log.
	 * 	5.) Client then saves the log in project folder.
	 *
	 * @param project the {@link IProject}
	 * @param monitor the progress monitor
	 * @param event the ExecutionEvent that triggered this action
	 */
	protected void process(final IProject project, final IProgressMonitor monitor, @Nullable final ExecutionEvent event) {
		if ( ! preProcess(project, monitor, event)) {
			return;
		}
		final Optional<String> jobId = MiningServiceExecutor.create(() -> serviceToExecute(project))
				.setInvalidResultConsumer(
						invalidResult -> Logging.error("Failed to schedule " + jobName() + " Job. Response was: " + invalidResult.getExtendedStatusMessage()))
				.setExceptionConsumer(exception -> Logging.error(exception.getLocalizedMessage(), exception)).execute();

		if ( ! jobId.isPresent()) {
			return;
		}
		JobUtil.submittedRemoteJob(project, jobId.get());

		while ( ! monitor.isCanceled()) {
			try {
				Thread.sleep(getPollIntervalSeconds() * 1000L);
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
			}

			final Optional<JobInformation> jobInfo = MiningServiceExecutor.create(() -> ApiClient.jobService(project).getJobInfo().setJobId(jobId.get()))
					.setInvalidResultConsumer(invalidResult -> Logging
							.error("Failed to retrieve " + jobName() + " Job information. Response was: " + invalidResult.getExtendedStatusMessage()))
					.setExceptionConsumer(exception -> Logging.error(exception.getLocalizedMessage(), exception)).execute();

			if ( ! jobInfo.isPresent()) {
				return;
			}
			/* when job is no longer running ... */
			if (isJobNotRunning(jobInfo.get().getStatus())) {
				postProcess(project, monitor, jobInfo.get(), event);
				return;
			}
		}

		/* IProgressMonitor was cancelled */
		MiningServiceExecutor.create(() -> ApiClient.jobService(project).cancelJob().setJobId(jobId.get()))
				.setExceptionConsumer(exception -> Logging.error(exception.getLocalizedMessage(), exception)).execute();
	}
	
	/**
	 * Checks if SourceObject upload process needs to be skipped.
	 * If user presses and holds the CTRL/ALT key while clicking the Discover code/metrics/DNA, the SourceObject upload process is skipped.
	 *
	 * @param executionEvent the event which is used to know if a CTRL/ALT key is pressed.
	 * 
	 * @return {@code true} if SourceObject upload needs to be skipped else {@code false}. 
	 */
	protected boolean preventSourceObjectUpload(@Nullable final ExecutionEvent executionEvent) {
		if (executionEvent != null && executionEvent.getTrigger() instanceof Event) {
			final Event event = (Event) executionEvent.getTrigger();
			return (event.stateMask == SWT.ALT) || (event.stateMask == SWT.CTRL);
		}

		return false;
	}
	
	/**
	 * Returns the number of seconds to wait before each poll.
	 *
	 * @return seconds to wait.
	 */
	protected int getPollIntervalSeconds() {
		return POLL_INTERVAL;
	}
	
	/**
	 * Returns whether the Job has to block the project from any changes.
	 *
	 * @return should job needs to block the project.
	 */
	protected boolean shouldJobBlocking() {
		return true;
	}
	
	/**
	 * Job log prefix to assign.
	 * E.g., discover-code
	 *
	 * @return job log prefix.
	 */
	protected String getLogPrefix() {
		return StringUtils.EMPTY;
	}
}
