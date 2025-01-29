/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ICoreRunnable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Shell;

import innowake.base.eclipse.core.util.ProgressUtil;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.plugin.module.importer.SourceObjectImporter;
import innowake.mining.plugin.preferences.MiningPreferences;

/**
 * Uploads source objects and references to the Mining server.
 */
public class UploadSourceObjectsHandler extends AbstractBaseHandler {

	@Override
	@Nullable
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {		

		final Optional<IStructuredSelection> resourceSelection = SelectionUtil.getResourceSelection();
		final Optional<String> selectedPath = resourceSelection
				.flatMap(selection -> selection.isEmpty() ? Optional.empty() : Optional.of(selection.getFirstElement().toString()));
		if ( ! selectedPath.isPresent()) {
			Logging.error("Cannot determine current selection");
			return null;
		}

		final IProject project;
		try {
			project = getProject();
		} catch (final ValidationException e) {
			Logging.error("Error while fetching the IProject information : " + e.getLocalizedMessage());
			throw new ExecutionException(e.getLocalizedMessage());
		}
		
		if (resourceSelection.isPresent()) {
			@SuppressWarnings("unchecked")
			final List<Object> selectedObjects = resourceSelection.get().toList();
			final List<IResource> selectedResources = new ArrayList<>();
			for (final Object obj : selectedObjects) {
				if (obj instanceof IResource) {
					selectedResources.add((IResource) obj);
				} else if (obj instanceof IJavaElement) {
					selectedResources.add(((IJavaElement) obj).getResource());
				}
			}
			process(assertNotNull(WorkbenchUtil.getActiveShell(), "Unable to determine the active workbench shell"), project, selectedResources, false, true);
		}
		
		return null;
	}
	
	/**
	 * Performs Source object upload.
	 * 
	 * @param shell the active workbench shell 
	 * @param project the selected {@link IProject}
	 * @param rootFolders list of root folders to be uploaded
	 * @param wait Set to {@code true} to block until upload completes
	 * @param forceUpload Setting it to {@code true} will skip the check if the Source code was modified after the previous successful upload
	 * @return whether process was successful or not
	 * @throws ExecutionException upon error
	 */
	public boolean process(final Shell shell, final IProject project, final List<IResource> rootFolders, final boolean wait, final boolean forceUpload) throws ExecutionException {
		final ConnectionInfo connectionInfo;
		final Long projectId;
		try {
			connectionInfo = MiningPreferences.getConnectionInfo(project).orElseThrow(
					() -> new ValidationException("Configuration error", String.format("API server for project '%s' not configured", project.getName())));
			projectId = MiningPreferences.getApiProject(project).orElseThrow(
					() -> new ValidationException("Configuration error", String.format("API server for project '%s' not configured", project.getName())))
					.getProjectId();
		} catch (final ValidationException e) {
			Logging.error(e.getLocalizedMessage(), e);
			return false;
		}
		final List<IResource> sanitizedSelectionPaths = new ArrayList<>();
		IProject projectSelection = null;

		final List<IFolder> folders = new ArrayList<>();
		final List<IFile> individualFiles = new ArrayList<>();
		final IResource iResource = rootFolders.get(0);
		if (iResource instanceof IProject) {
			projectSelection = (IProject) iResource;
		} else {
			IPath temp = null;
			final List<IResource> resourcesOrderedByPath = new ArrayList<>(rootFolders);
			resourcesOrderedByPath.sort((e1, e2) -> e1.getFullPath().toString().compareTo(e2.getFullPath().toString()));
			for (final IResource selection : resourcesOrderedByPath) {
				if (temp == null || temp.matchingFirstSegments(selection.getFullPath()) != temp.segmentCount()) {
					temp = selection.getFullPath();
					sanitizedSelectionPaths.add(selection);
				}
			}

			for (final IResource selectedElement : sanitizedSelectionPaths) {
				if (selectedElement instanceof IFolder) {
					folders.add((IFolder) selectedElement);
				} else if (selectedElement instanceof IFile) {
					individualFiles.add((IFile) selectedElement);
				}
			}
		}
		final Set<Job> jobs = new HashSet<>();
		
		if (projectSelection != null) {
			final List<IFile> files = SelectionUtil.getFiles(new StructuredSelection(projectSelection));
			jobs.add(createJobForFilesUpload(project, shell, connectionInfo, projectId, files, "/",
					"Uploading source code in project: " + projectSelection.getFullPath().removeFirstSegments(1), forceUpload));
		}

		for (final IResource folder : folders) {
			final IPath scope = folder.getFullPath().removeFirstSegments(1);
			final List<IFile> files = SelectionUtil.getFiles(new StructuredSelection(folder));
			jobs.add(createJobForFilesUpload(project, shell, connectionInfo, projectId, files, String.valueOf(scope),
					"Uploading source code in folder: " + scope, forceUpload));
		}

		if ( ! individualFiles.isEmpty()) {
			jobs.add(createJobForFilesUpload(project, shell, connectionInfo, projectId, individualFiles, "",
					"Uploading source code files: " + individualFiles.size(), forceUpload));
		}
		
		/* create parent job which ensures that folders are uploaded one at a time and not concurrently */
		final Job parentJob = Job.create("Uploading source code", new ICoreRunnable() {
			
			@Override
			public void run(@Nullable final IProgressMonitor monitor) throws CoreException {
				for (Job job : jobs) {
					try {
						ProgressUtil.checkCanceled(monitor);
						job.schedule();
						job.join();

						switch (job.getResult().getSeverity()) {
							case IStatus.CANCEL:
								if (monitor != null) {
									monitor.setCanceled(true);
								}
								throw new OperationCanceledException();
							case IStatus.ERROR:
								throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "Error while uploading files.", job.getResult().getException()));
							default:
								/* nothing to do */
						}
					} catch (final InterruptedException e) {
						Thread.currentThread().interrupt();
						throw new IllegalStateException(e);
					}
				}
			}
		});
		
		parentJob.schedule();
		if (wait) {
			/* wait until all uploads are finished */
			try {
				parentJob.join();
				switch (parentJob.getResult().getSeverity()) {
					case IStatus.CANCEL:
						return false;
					case IStatus.ERROR:
						throw new ExecutionException("Error while finishing files upload.", parentJob.getResult().getException());
					default:
						/* nothing to do */
				}
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
				throw new IllegalStateException(e);
			}
		}

		return true;
	}

	private Job createJobForFilesUpload(final IProject project, final Shell shell, final ConnectionInfo connectionInfo, final Long projectId,
			final List<IFile> files, final String scope, final String jobMessage, final boolean forceUpload) {
		if (files.isEmpty()) {
			Logging.error("Cannot determine files from current selection");
		}
		
		return Job.create(jobMessage, new SourceObjectImporter(connectionInfo, project, projectId, shell, scope, files, forceUpload));
	}
}
