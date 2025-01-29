/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink;

import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.ITextEditor;

import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * This class offers methods to execute some Eclipse methods, like opening files, jumping to a certain line, etc.
 */
public class EclipseApi {
	private EclipseApi() {}
	
	/**
	 * This method returns a list of all mining projects whose ProjectData fulfills the given predicate.
	 *
	 * @param dataPredicate The predicate the project data needs to pass, to be returned.
	 * @return A list of mining projects in the workspace, which passed the predicate. 
	 */
	public static List<IProject> getMiningProjects(final Predicate<ProjectData> dataPredicate) {
		final List<IProject> miningProjects = new LinkedList<>();
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		final IProject[] projects = root.getProjects();
		for (final IProject project : projects) {
			final Optional<ProjectData> optData = MiningPreferences.getApiProject(project);
			optData.ifPresent(data -> {
				if (dataPredicate.test(data)) {
					miningProjects.add(project);
				}
			});
		}
		return miningProjects;
	}
	
	/**
	 * Returns a list of mining projects in the workspace which are linked to the project with the given project id
	 *
	 * @param projectId Project id which needs to be matched
	 * @return The list of projects matching the project id
	 */
	public static List<IProject> getMiningProjects(final Long projectId) {
		return getMiningProjects(data -> data.getProjectId().equals(projectId));
		
	}
	
	/**
	 * Opens the given file, setting the cursor at offset and highlighting until offset + length
	 * The file must be in the current workspace.
	 * 
	 * @param file the file to open
	 * @param offset The position to set the cursor to
	 * @param length The length of the section to highlight
	 * @throws EclipseApiException if an error occurs handling the request
	 */
	public static void openInternalFile(final IFile file, final int offset, final int length) throws EclipseApiException {
		if ( ! file.exists()) {
			throw new EclipseApiException("Could not open file", new FileNotFoundException(file.getFullPath().toString()));
		}
		
		Exception[] exception = new Exception[1];
		Display.getDefault().syncExec(() -> { 
			final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
			final IWorkbenchPage page = window.getActivePage();
			try {
				final ITextEditor editor = (ITextEditor) IDE.openEditor(page, file, true);
				editor.selectAndReveal(offset, length);
			} catch (final PartInitException e) {
				exception[0] = e;
			}
		});
		
		if (exception[0] != null) {
			throw new EclipseApiException("Could not open file", exception[0]);
		}
	}
}
