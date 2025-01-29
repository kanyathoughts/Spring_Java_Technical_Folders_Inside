/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.viewers.StructuredSelection;
import innowake.lib.core.lang.Nullable;
import innowake.mining.client.service.RestService;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.ConfigResources;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.core.IdentificationMapper;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Executes Discover code on the Mining server.
 */
public class DiscoverCodeHandler extends AbstractBaseDiscoverHandler {

	@Override
	protected String jobName() {
		return "Discover Code";
	}

	@Override
	protected RestService<String> serviceToExecute(final IProject project) throws CoreException, StorageException {
		return ApiClient.discoveryService(project).discoverCode();
	}

	@Override
	protected String getLogPrefix() {
		return "discover-code";
	}

	@Override
	protected boolean preProcess(final IProject project, final IProgressMonitor monitor, @Nullable final ExecutionEvent event) {
		@SuppressWarnings("unchecked")
		final List<Object> selectedObjects = new ArrayList<>(selection.orElse(StructuredSelection.EMPTY).toList());
		final IFolder sourceFolder = project.getFolder(IdentificationMapper.SRC_ROOT);
		selectedObjects.add(createFolder(sourceFolder));
		final IFolder undiscoveredEntitiesFolder = getUndiscoveredFolder(project);
		selectedObjects.add(createFolder(undiscoveredEntitiesFolder));

		selection = Optional.of(new StructuredSelection(selectedObjects));
		return super.preProcess(project, monitor, event);
	}
	
	@Override
	protected void postProcess(final IProject project, final IProgressMonitor monitor, final JobInformation jobInfo, @Nullable final ExecutionEvent event) {
		super.postProcess(project, monitor, jobInfo, event);
		try {
			if (jobInfo.getStatus() == JobStatus.SUCCESS) {
				/* Deletes the empty undiscovered entity folder */
				final IFolder undiscoveredEntitiesFolder = getUndiscoveredFolder(project);
				if (undiscoveredEntitiesFolder.exists() && undiscoveredEntitiesFolder.members().length == 0) {
					undiscoveredEntitiesFolder.delete(false, null);
				}
				/*
				 * Create a job for downloading just the source code deltas instead of entire project contents by instantiating SourceObjectDownloader with
				 * forceFullDownload flag as false.
				 */
				Job.create(DownloadSourceObjectsHandler.JOB_NAME, new SourceObjectDownloader(project, ApiClient.getConnectionInfo(project), false)).schedule();
			}
		} catch (final IOException | ValidationException | CoreException | StorageException e) {
			Logging.error(e.getLocalizedMessage(), e);
		}
	}

	/**
	 * Returns the undiscovered entity folder name configured in the Discovery config file if exists else returns the default undiscovered folder name
	 *
	 * @param project the Project
	 * @return undiscovered entity folder
	 */
	private IFolder getUndiscoveredFolder(final IProject project) {
		final Config config;
		IFolder undiscoveredEntitiesFolder;
		final IFile file = project.getFile(ConfigResources.DISCOVERY_CONFIG.getResourceName());

		if (file.exists()) {
			try {
				config = Config.loadConfig(new InputStreamReader(file.getContents(), file.getCharset()));
				undiscoveredEntitiesFolder = project.getFolder(config.getUndiscoveredFolder());
			} catch (final UnsupportedEncodingException | DiscoveryException | CoreException exception) {
				undiscoveredEntitiesFolder = project.getFolder(Config.getDefaultUndiscoveredFolder());
				Logging.error(exception.getLocalizedMessage(), exception);
			}
		} else {
			undiscoveredEntitiesFolder = project.getFolder(Config.getDefaultUndiscoveredFolder());
		}
		return undiscoveredEntitiesFolder;
	}
	
	private IFolder createFolder(final IFolder folder) {
		try {
			if ( ! folder.exists()) {
				folder.create(true, true, null);
			}
		} catch (final CoreException exception) {
			Logging.error(exception.getLocalizedMessage(), exception);
		}
		return folder;
	}
}
