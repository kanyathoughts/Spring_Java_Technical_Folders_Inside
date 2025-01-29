/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.discovery.categorize;

import static innowake.lib.core.lang.Assert.assertNotNull;
import java.io.IOException;
import java.util.Optional;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.AbstractRemoteResourcesTest;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.module.ui.handler.DiscoverCodeHandler;
import innowake.mining.plugin.module.ui.handler.SourceObjectDownloader;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Base class for {@code Discover Code} tests.
 */
public abstract class AbstractDiscoverCodeTest extends AbstractRemoteResourcesTest {

	/**
	 * {@inheritDoc}
	 * 
	 * Additionally:
	 * <ul>
	 * <li>Uploads all source objects in Mining server with project ID {@code 1}.
	 * <li>Executes Discover Code in Mining server with project ID {@code 1}.
	 * <li>Downloads all source objects from Mining server with project ID {@code 1}.
	 * </ul>
	 * 
	 * @throws IOException if an error occurs 
	 * @throws CoreException if an error occurs 
	 */
	@Override
	protected void preprocess() throws CoreException, IOException {
		super.preprocess();
		uploadSourceObjects();
		executeDiscoverCode();
		downloadSourceObjects();
	}

	protected void executeDiscoverCode() {
		try {
			new DiscoverCodeHandlerNoJob().execute(new ExecutionEvent());
		} catch (final ExecutionException e) {
			throw new IllegalStateException(e);
		}
	}	
	
	/**
	 * Overrides {@link DiscoverCodeHandler} by not supplying a {@link Job}. 
	 */
	private class DiscoverCodeHandlerNoJob extends DiscoverCodeHandler {
		
		@Override
		@Nullable
		public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
			final IProject project;
			try {
				project = getProject();
				final IFolder folder = getTargetProject().getFolder(getTestFolder());
				final IStructuredSelection sel = new StructuredSelection(assertNotNull(folder));
				setSelection(Optional.of(assertNotNull(sel)));
			} catch (final ValidationException | IllegalStateException e) {
				Logging.error(e.getLocalizedMessage(), e);
				return null;
			}
			process(project, MONITOR, event);
			return null;
		}
		
		@Override
		protected void postProcess(final IProject project, final IProgressMonitor monitor,
				final JobInformation jobInfo, @Nullable final ExecutionEvent event) {
			postProcessDefault(project, jobInfo);
			try {
				if (jobInfo.getStatus() == JobStatus.SUCCESS) {
					/* Downloading just the source code deltas instead of entire project contents
					 * by instantiating SourceObjectDownloader with forceFullDownload flag as false.*/
					new SourceObjectDownloader(project, CONNECTION_INFO, false).run(MONITOR);
				}
			} catch (final IOException | ValidationException | CoreException e) {
				Logging.error(e.getLocalizedMessage(), e);
			}
		}
		
		@Override
		protected IProject getProject() throws ValidationException {
			return getTargetProject();
		}
	}
}
