/*
 * Copyright (c) 2022 Deloitte innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences;

import java.util.Collections;
import java.util.List;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ISetSelectionTarget;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.shared.discovery.config.core.IdentificationMapper;

/**
 * Tester for enabling or disabling discovery menu.
 */
public class IsDiscoveryPropertyTester extends PropertyTester {

	private static final String SHOW_DISCOVERY_MENU = "showDiscoveryMenu";
	private static final String IS_DISCOVER_CODE = "isValidDiscoverCodeSelection";
	private static final String IS_DISCOVER_METRICS = "isValidDiscoverMetricsSelection";
	private static final String IS_DISCOVER_DNA = "isValidDiscoverDnaSelection";
	private static final String IS_VALID_UPLOAD_SOURCE = "isValidUploadSourceCodeSelection";

	@Override
	public boolean test(
			@Nullable final Object receiver,
			@Nullable final String property,
			@Nullable final Object[] args,
			@Nullable final Object expectedValue) {

		if (property == null) {
			return false;
		}
		@SuppressWarnings("unchecked")
		final List<Object> selectedResources = SelectionUtil.getResourceSelection().map(IStructuredSelection::toList).orElse(Collections.emptyList());
		final IProject project = SelectionUtil.getProjectFromSelectedResource().orElse(null);
		if (project == null) {
			return false;
		}
		switch (property) {
			case SHOW_DISCOVERY_MENU:
				return showDiscoveryMenu();
			case IS_DISCOVER_CODE:
				return isValidDiscoverCodeSelection(selectedResources, project);
			case IS_DISCOVER_METRICS:
			case IS_DISCOVER_DNA:
				return isValidDiscoverMetricsDnaSelection(selectedResources, project);
			case IS_VALID_UPLOAD_SOURCE:	
				return isValidUploadSourceCodeSelection(selectedResources);
			default:
				return false;
		}
	}

	private boolean showDiscoveryMenu() {
		final IWorkbench workbench = PlatformUI.getWorkbench();
		if (workbench == null) {
			return false;
		}
		final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null) {
			return false;
		}
		final IPartService partService = activeWorkbenchWindow.getPartService();
		if (partService == null) {
			return false;
		}
		/* only allow on view selection - not on editor selection */
		return partService.getActivePart() instanceof ISetSelectionTarget;
	}

	private boolean isValidDiscoverMetricsDnaSelection(@Nullable final List<Object> selectedResources, final IProject project) {
		if (selectedResources == null || selectedResources.size() != 1) {
			return false;
		}

		final Object selectedResource = selectedResources.get(0);
		if (selectedResource instanceof IProject || selectedResource instanceof IJavaProject) {
			return true;
		}

		return isSourceResource(project.getFolder(IdentificationMapper.SRC_ROOT), selectedResource);
	}

	private boolean isValidDiscoverCodeSelection(@Nullable final List<Object> selectedResources, final IProject project) {
		if (selectedResources == null) {
			return false;
		}

		final IResource srcFolder = project.getFolder(IdentificationMapper.SRC_ROOT);

		for (final Object selectedResource : selectedResources) {
			/* project nature changes from IProject to IJavaProject in different view(project Explorer/Package Explorer) */
			if (selectedResource instanceof IProject || selectedResource instanceof IJavaProject) {
				return false;
			}
			/* Discover code cannot happen on a files in the project root folder */
			if ((selectedResource instanceof IFile) && (((IResource) selectedResource).getParent() instanceof IProject)) {
				return false;
			}

			if (isSourceResource(srcFolder, selectedResource)) {
				return false;
			}
		}
		return true;
	}

	private static boolean isSourceResource(final IResource srcFolder, final Object selectedResource) {
		IResource resource = null;
		if (selectedResource instanceof IJavaElement) {
			resource = ((IJavaElement) selectedResource).getResource();
		} else if (selectedResource instanceof IResource) {
			resource = (IResource) selectedResource;
		}

		return resource != null && (resource.getProjectRelativePath().toString() + "/").startsWith(srcFolder.getProjectRelativePath().toString() + "/");
	}

	/**
	 * Method to validate if the resources being uploaded are not from project level.
	 *
	 * @param selectedResources list of resources selected to upload
	 * @return {@code true} if valid upload {@code false} otherwise
	 */
	private boolean isValidUploadSourceCodeSelection(@Nullable final List<Object> selectedResources) {				
		if (selectedResources == null) {
			return false;
		}
		for (final Object selectedResource : selectedResources) {
			/* project nature changes from IProject to IJavaProject in different view(project Explorer/Package Explorer) */
			if (selectedResource instanceof IProject || selectedResource instanceof IJavaProject) {
				return false;
			}
			if ((selectedResource instanceof IFile) && (((IResource) selectedResource).getParent() instanceof IProject)) {
				return false;
			}
		}
		return true;
	}
}
