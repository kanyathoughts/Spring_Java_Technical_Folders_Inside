/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;


import java.util.Arrays;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.dev.StopWatch;
import innowake.mining.client.service.RestService;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.discovery.config.core.IdentificationMapper;
import innowake.mining.shared.discovery.dna.Constants;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;


/**
 * Executes Discover Dna on the Mining server.
 */
public class DiscoverDnaHandler extends AbstractBaseDiscoverHandler {
	
	private final StopWatch watch = new StopWatch();
	
	@Override
	protected String jobName() {
		return "Discover Dna";
	}

	@Override
	protected RestService<String> serviceToExecute(final IProject project) throws CoreException, StorageException {
		return ApiClient.discoveryService(project).findCommunities();
	}

	@Override
	protected String getLogPrefix() {
		return "discover-dna";
	}

	@Override
	protected boolean preProcess(final IProject project, final IProgressMonitor monitor, @Nullable final ExecutionEvent event) {
		watch.start();
		Logging.info("Discover Dna started");
		new UploadConfigurationHandler().upload(project);
		try {
			if (preventSourceObjectUpload(event)) {
				Logging.info("Skipping SourceObject upload");
			} else {
				final IResource srcFolder = project.getFolder(IdentificationMapper.SRC_ROOT);
				if (srcFolder.exists() && ! uploadSourceObjects(project, event, srcFolder)) {
					return false;
				}
			}
		} catch (final ExecutionException e) {
			throw new IllegalStateException(e);
		}
		return true;
	}
	
	@Override
	protected void postProcess(final IProject project, final IProgressMonitor monitor, final JobInformation jobInfo, @Nullable final ExecutionEvent event) {
		super.postProcess(project, monitor, jobInfo, event);
		try {
			if (jobInfo.getStatus() != JobStatus.SUCCESS) {				
				return;
			}
			final IFolder dnaFolder = project.getFolder(Constants.DNA_BASE_FOLDER);
			if (dnaFolder.exists() && acceptDeletionOfDnaFolder(HandlerUtil.getActiveShell(event))) {
				dnaFolder.delete(true, null);
			}
			watch.stop();
			Logging.info(String.format("Discover Dna took %s (H:mm:ss.SSS)", watch.toString()));
		} catch (final CoreException e) {
			throw new IllegalStateException(e);
		}
	}
	
	private static boolean acceptDeletionOfDnaFolder(final Shell shell) {
		/* if user answers "No" then we will not delete the DNA folder */
		return UIHandlerUtils.getUserResponseFunction(shell)
				.apply("The data inside the DNA folder will no longer be used and the folder can be removed. Ok?", "Delete the existing DNA folder?")
				.booleanValue();
	}
	
	private boolean uploadSourceObjects(final IProject project, @Nullable final ExecutionEvent event, final IResource srcFolder) throws ExecutionException {
		return new UploadSourceObjectsHandler().process(HandlerUtil.getActiveShell(event), project, Arrays.asList(srcFolder), true, false);
	}
}
