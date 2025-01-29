/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base.ui;

import static innowake.mining.plugin.base.JobUtil.isJobNotRunning;

import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.Result;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.JobUtil;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.plugin.module.ui.handler.AbstractBaseHandler;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.shared.model.job.JobInformation;

/**
 * Base implementation for multi-file selections with confirmation and backend job-triggering.
 * <p>
 * This handler shows a confirmation dialog if the user selected multiple files.
 */
public abstract class MultiFileBaseHandler extends AbstractBaseHandler {

	@Override
	public @Nullable Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		final Shell shell = HandlerUtil.getActiveShell(event);
		final IProject project;
		final List<String> filePaths;
		try {
			project = getProject();
			filePaths = getFilePaths();
		} catch (final ValidationException e) {
			Logging.error(e.getLocalizedMessage(), e);
			MessageDialog.openError(shell, e.getTitle(), e.getMessage());
			return null;
		}
		
		final int numberOfFilesInSelection = filePaths.size();
		if (numberOfFilesInSelection > 1) {
			final boolean continueIdentification = MessageDialog.openConfirm(shell, 
					"Multiple files selected", 
					String.format(getConfirmationMessagePattern(), 
							      Integer.valueOf(numberOfFilesInSelection)));
			
			if ( ! continueIdentification) {
				return null;
			}
		}
		
		final Long projectId = MiningPreferences.getApiProject(project).orElseThrow(IllegalStateException::new).getProjectId();
		JobUtil.submittedRemoteJob(project, createResultSupplier(project, projectId, filePaths));

		return null;
	}

	/**
	 * This method allows you to wait while a server job is run in the background and shows the progress updates on the job view.
	 *
	 * @param project the Eclipse project of the selection.
	 * @param monitor the progress monitor.
	 * @param jobId the ID of the server job. 
	 */
	protected void waitForJobCompletion(final IProject project, final IProgressMonitor monitor, final Optional<String> jobId) {
		if ( ! jobId.isPresent()) {
			return;
		}
		JobUtil.submittedRemoteJob(project, jobId.get());

		while ( ! monitor.isCanceled()) {
			try {
				Thread.sleep(1000L);
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
			}

			final Optional<JobInformation> jobInfo = MiningServiceExecutor.create(() -> ApiClient.jobService(project).getJobInfo().setJobId(jobId.get()))
					.setInvalidResultConsumer(invalidResult -> Logging
							.error("Failed to retrieve Job information. Response was: " + invalidResult.getExtendedStatusMessage()))
					.setExceptionConsumer(exception -> Logging.error(exception.getLocalizedMessage(), exception)).execute();

			if ( ! jobInfo.isPresent()) {
				return;
			}
			/* when job is no longer running ... */
			if (isJobNotRunning(jobInfo.get().getStatus())) {
				return;
			}
		}
	}
	
	/**
	 * Creates a result supplier, i.e. service call, triggering a job on the mining backend and returning the Job ID.
	 *
	 * @param project the Eclipse project of the selection
	 * @param projectId the ID of the project on the mining server
	 * @param filePaths the paths of the files to handle
	 * @return the result supplier, i.e. service call, handling the paths
	 */
	protected abstract Supplier<Result<String>> createResultSupplier(IProject project, Long projectId, List<String> filePaths);

	/**
	 * Returns the message pattern which is used when the confirmation dialog is shown, if the user selected multiple files.
	 * <p>
	 * The pattern <b>must</b> have one numerical parameter for the number of files selected.
	 *
	 * @return the message pattern shown as confirmation when multiple files are selected.
	 */
	protected abstract String getConfirmationMessagePattern();

}
