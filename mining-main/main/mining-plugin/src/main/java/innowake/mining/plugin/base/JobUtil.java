/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import org.apache.http.HttpStatus;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import innowake.base.eclipse.common.ui.util.WorkbenchUtil;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.service.Result;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.shared.model.job.JobStatus;

/**
 * This utility class can be used to notify any interested parties that a remote job has been submitted by using the
 * {@link IRemoteJobController} of the {@code innowake.mining.plugin.remoteJobController} extension point.
 */
public class JobUtil {
	
	/** Number of seconds to wait before each poll */
	public static final int POLL_INTERVAL = 5;
	
	private static final String ERROR_TITLE = "Error submitting remote job";
	private static final String EXTENSION_POINT_ID = "innowake.mining.plugin.remoteJobController";
	private static final List<IRemoteJobController> remoteJobControllers = new ArrayList<>();
	
	private JobUtil() {}
	
	static {
		/* Load the extension points and store the instances for later use. */
		final IConfigurationElement[] configElements = Platform.getExtensionRegistry().getConfigurationElementsFor(EXTENSION_POINT_ID);
        for (final IConfigurationElement element : configElements) {
        	try {
	            final Object extension = element.createExecutableExtension("class");
	            remoteJobControllers.add(Assert.assertInstanceOf(extension, IRemoteJobController.class));
        	} catch (final CoreException ex) {
        		MiningPlugin.getDefaultNonNull().getPluginLog().error("Could not instantiate listener for element "
        				+ element.getNamespaceIdentifier() + "." + element.getName(), ex);
        	}
        }
	}

	/**
	 * Notifies that a remote job has been submitted by calling {@link IRemoteJobController#submittedJob(IProject,String)} for every
	 * resolved {@link IRemoteJobController} of the {@code innowake.mining.plugin.remoteJobController} extension point.
	 * It will use the job Id resolved from the {@link Result} of the REST call that started the remote job, by additionally
	 * checking for successful status codes.
	 * 
	 * @param project the {@link IProject} that has been used to submit the job
	 * @param resultSupplier the {@link Supplier} supplying the {@link Result} of the REST call that started the remote job
	 */
	public static void submittedRemoteJob(final IProject project, final Supplier<Result<String>> resultSupplier) {
		final Result<String> result = resultSupplier.get();
		if (result != null) {
			/* If a job has been started, then 202 will be returned. */
			if (result.getStatusCode() != HttpStatus.SC_ACCEPTED) {
				handleError(ERROR_TITLE, result.getExtendedStatusMessage(), null);
				return;
			}
			
			final Optional<String> value = result.getValue();
			if (value.isPresent()) {
				submittedRemoteJob(project, value.get());
			} else {
				handleError(ERROR_TITLE, "Job result is not present", null);
			}
		}
	}
	
	/**
	 * Notifies that a remote job has been submitted by calling {@link IRemoteJobController#submittedJob(IProject,String)} for every
	 * resolved {@link IRemoteJobController} of the {@code innowake.mining.plugin.remoteJobSubmitListener} extension point.
	 * It will use the provided job Id.
	 * 
	 * @param project the {@link IProject} that has been used to submit the job
	 * @param jobId the Id of the job
	 */
	public static void submittedRemoteJob(final IProject project, final String jobId) {
		final ISafeRunnable runnable = new ISafeRunnable() {
			
            @Override
            public void handleException(@Nullable final Throwable e) {
                handleError(ERROR_TITLE, "An error occured while notifying the remote job manager about the submitted job.", e);
            }

            @Override
            public void run() throws Exception {
            	remoteJobControllers.forEach(extension -> extension.submittedJob(project, jobId));
            }
        };
		innowake.mining.plugin.base.ui.WorkbenchUtil.asyncExec(innowake.mining.plugin.base.ui.WorkbenchUtil.getDisplaySafely(),
						() -> innowake.mining.plugin.base.ui.WorkbenchUtil.openView("innowake.mining.job.jobView"));
        SafeRunner.run(runnable);
	}
	
	/**
	 * Returns if there are currently any managed jobs in this workspace for the provided {@code url}
	 * by calling {@link IRemoteJobController#hasManagedJobsForUrl(String)} for every resolved
	 * {@link IRemoteJobController} of the {@code innowake.mining.plugin.remoteJobSubmitListener} extension point.
	 * 
	 * @param url the api-server URL to check for managed jobs
	 * @return {@code true} if there managed jobs for the URL; {@code false} otherwise
	 */
	public static boolean hasManagedJobsForUrl(final String url) {
		final boolean[] result = new boolean[1]; /* array as it must be declared final */
		final ISafeRunnable runnable = new ISafeRunnable() {
			
            @Override
            public void handleException(@Nullable final Throwable e) {
                handleError(ERROR_TITLE, "An error occured while checking for managed jobs for URL '" + url + "'.", e);
            }

            @Override
            public void run() throws Exception {
            	remoteJobControllers.forEach(extension -> result[0] |= extension.hasManagedJobsForUrl(url));
            }
        };
        SafeRunner.run(runnable);
        return result[0];
	}
	
	/**
	 * Refreshes all jobs managed by this workspace. This will also cleanup all jobs not known to the currently
	 * configured api-server URLs.
	 * Calls {@link IRemoteJobController#refreshManagedJobs()} for every resolved {@link IRemoteJobController}
	 * of the {@code innowake.mining.plugin.remoteJobSubmitListener} extension point.
	 */
	public static void refreshManagedRemoteJobs() {
		final ISafeRunnable runnable = new ISafeRunnable() {
			
            @Override
            public void handleException(@Nullable final Throwable e) {
                handleError(ERROR_TITLE, "An error occured refresing the managed remote jobs", e);
            }

            @Override
            public void run() throws Exception {
            	remoteJobControllers.forEach(IRemoteJobController::refreshManagedJobs);
            }
        };
        SafeRunner.run(runnable);
	}
	
	private static final void handleError(final String title, final String message, @Nullable final Throwable throwable) {
		Display.getDefault().syncExec(() -> MessageDialog.openError(WorkbenchUtil.getActiveShell(), title, message));
		
		if (throwable != null) {
			MiningPlugin.getDefaultNonNull().getPluginLog().error(message, throwable);
		} else {
			MiningPlugin.getDefaultNonNull().getPluginLog().error(message);
		}
	}

	/**
	 * Returns if a job with the given {@code jobStatus} is already done or still running.
	 *
	 * @param jobStatus The {@link JobStatus} to check
	 * @return {@code true} if the job is done. Otherwise {@code false}
	 */
	public static boolean isJobNotRunning(final JobStatus jobStatus) {
		return jobStatus == JobStatus.SUCCESS || jobStatus == JobStatus.FAILURE || jobStatus == JobStatus.CANCELED || jobStatus == JobStatus.TIMEOUT;
	}

	/**
	 * Waits {@link JobUtil#POLL_INTERVAL} seconds for the next server polling.
	 */
	public static void waitForNextPoll() {
		try {
			Thread.sleep(POLL_INTERVAL * 1000L);
		} catch (final InterruptedException exception) {
			Logging.error("Error while waiting for job result polling", exception);
			Thread.currentThread().interrupt();
		}
	}
}
