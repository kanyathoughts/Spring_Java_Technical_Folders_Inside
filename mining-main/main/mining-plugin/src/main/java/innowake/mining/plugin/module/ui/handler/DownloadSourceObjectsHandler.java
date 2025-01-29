/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.module.ui.handler;

import java.io.IOException;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.security.storage.StorageException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.client.ApiClient;

/**
 * Downloads source objects from the Mining server. It downloads the entire project contents.
 */
public class DownloadSourceObjectsHandler extends AbstractBaseHandler {
	
	static final String JOB_NAME = "Downloading source code files";
	
	@Override
	@Nullable
	public Object execute(final @Nullable ExecutionEvent event) throws ExecutionException {
		try {
			final IProject project = getProject();
			/* Create a job for downloading the entire project contents by instantiating
			 * SourceObjectDownloader with forceFullDownload flag as true.*/
			Job.create(JOB_NAME, new SourceObjectDownloader(project, ApiClient.getConnectionInfo(project), true)).schedule();
		} catch (final ValidationException | IOException | CoreException | StorageException e) {
			throw new ExecutionException("Failed to load mining file index from project", e);
		}
		
		return null;
	}
}
