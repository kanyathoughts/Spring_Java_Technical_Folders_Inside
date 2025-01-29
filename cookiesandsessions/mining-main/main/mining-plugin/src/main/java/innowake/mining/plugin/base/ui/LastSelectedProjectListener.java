/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base.ui;

import java.util.Optional;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Adapters;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;

import innowake.lib.core.api.lang.Nullable;


/**
 * Selection listener which determines the last selected project from the selection.
 */
public class LastSelectedProjectListener implements ISelectionListener {

	private Optional<IProject> lastSelectedProject = Optional.empty();

	@Override
	public void selectionChanged(@Nullable final IWorkbenchPart part, @Nullable final ISelection selection) {
		Object selectedObject = null;
		if (selection instanceof IStructuredSelection) {
			selectedObject = ((IStructuredSelection) selection).getFirstElement();
		} else if (part instanceof IEditorPart) {
			selectedObject = ((IEditorPart) part).getEditorInput();
		} 

		final IResource selectedResource = Adapters.adapt(selectedObject, IResource.class);
		final IProject project;
		if (selectedResource != null && (project = selectedResource.getProject()) != null) {
			lastSelectedProject = Optional.of(project);
		}
	}
	
	
	/**
	 * Returns the last selected project.
	 *
	 * @return the last selected project, or {@link Optional#empty()} if there was no project selected
	 */
	public Optional<IProject> getLastSelectedProject() {
		return lastSelectedProject;
	}
	
}
