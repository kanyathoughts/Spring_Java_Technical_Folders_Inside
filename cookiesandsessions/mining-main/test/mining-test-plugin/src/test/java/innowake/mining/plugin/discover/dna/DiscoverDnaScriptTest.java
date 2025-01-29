/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.discover.dna;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.nio.file.Path;
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
import org.junit.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.AbstractRemoteResourcesTest;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.module.ui.handler.DiscoverDnaHandler;
import innowake.mining.shared.discovery.dna.Constants;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Test for Discover DNA.
 */
public class DiscoverDnaScriptTest extends AbstractRemoteResourcesTest {

	@Test
	public void discoverDna() throws IOException, CoreException, InterruptedException {
		final Path project = getTargetTestResourcesProjectPath();
		assertNotNull(project);

		final Path dna = project.resolve("DNA");
		refreshLocal();
		assertFolder(dna, 2);
		assertFiles(dna, "MMRS7111.cbl", "MMRS71C1.cbl");
	}
	
	@Override
	protected String getTestFolder() {
		return "DNA";
	}

	/**
	 * {@inheritDoc}
	 * 
	 * Additionally:
	 * <ul>
	 * <li>Uploads all source objects in Mining server with project ID {@code 1}.
	 * <li>Executes Discover DNA in Mining server with project ID {@code 1}.
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
		executeDiscoverDna();
		downloadSourceObjects();
	}

	private void executeDiscoverDna() {
		try {
			new DiscoverDNAHandlerNoJob().execute(new ExecutionEvent());
		} catch (final ExecutionException e) {
			throw new IllegalStateException(e);
		}
	}

	private void assertFiles(final Path folder, final String... files) {
		assertTrue(String.format("Folder %s missing", folder), folder.toFile().exists());
		for (final String file : files) {
			assertTrue(String.format("File %s missing in %s", file, folder), folder.resolve(file).toFile().exists());
		}
	}

	/**
	 * Overrides {@link DiscoverDnaHandler} by not supplying a {@link Job}. 
	 */
	private class DiscoverDNAHandlerNoJob extends DiscoverDnaHandler {

		@Override
		@Nullable
		public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
			final IProject project;
			try {
				project = getProject();
				final IFolder folder = getTargetProject().getFolder(getTestFolder());
				final IStructuredSelection sel = new StructuredSelection(folder);
				setSelection(Optional.of(assertNotNull(sel)));
			} catch (final ValidationException | IllegalStateException e) {
				Logging.error(e.getLocalizedMessage(), e);
				return null;
			}
			process(project, MONITOR, event);
			return null;
		}
	
		@Override
		protected void postProcess(final IProject project, final IProgressMonitor monitor, final JobInformation jobInfo, @Nullable final ExecutionEvent event) {
			try {
				if (jobInfo.getStatus() != JobStatus.SUCCESS) {				
					return;
				}
				final IFolder dnaFolder = project.getFolder(Constants.DNA_BASE_FOLDER);
				if (dnaFolder.exists()) {
					dnaFolder.delete(true, null);
				}
			} catch (final CoreException e) {
				throw new IllegalStateException(e);
			}
			super.postProcess(project, monitor, jobInfo, event);

		}
		@Override
		protected IProject getProject() throws ValidationException {
			return getTargetProject();
		}
	}
}
