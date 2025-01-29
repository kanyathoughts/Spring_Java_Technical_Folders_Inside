/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences;

import java.util.Optional;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.part.ISetSelectionTarget;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningProjectNature;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.base.ui.SelectionUtil;

/**
 * Tester for enabling or disabling mining menu.
 */
public class IsMiningProjectPropertyTester extends org.eclipse.core.expressions.PropertyTester {
	
	@Override
	public boolean test(
			@Nullable final Object receiver, 
			@Nullable final String property, 
			@Nullable final Object[] args, 
			@Nullable final Object expectedValue) {
		
		if ("showMiningMenu".equalsIgnoreCase(property)) {
			return doShow();
		} else if("showMiningNatureMenu".equalsIgnoreCase(property)) {
			return showMiningNatureMenu();
		}else if ("enableMiningMenu".equalsIgnoreCase(property)) {
			return  doShow() && isMiningProjectSelected();
		}
		return false;
	}

	private boolean doShow() {
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
		final IWorkbenchPart part = partService.getActivePart();
		if ((part instanceof EditorPart) || (part instanceof ISetSelectionTarget)) {
			final Optional<IProject> project = SelectionUtil.getProjectFromSelectedResource();
			try {
				return project.isPresent() && project.get().hasNature(MiningProjectNature.NATURE_ID);
			} catch (final CoreException e) {
				Logging.warn(String.format("Could not determine if the project : %s has mining nature", project.get().getName()));
				return false;
			}
		}
		return false;
	}
	
	private boolean isMiningProjectSelected() {
		final Optional<IProject> project = SelectionUtil.getProjectFromSelectedResource();
		if (project.isPresent()) {
			try {
				ProjectValidator.validate(project.get());
				return true;
			} catch (final ValidationException e) {
				/* no mining settings -> do not show this menu */
			}
		}
		return false;
	}
	
	private boolean showMiningNatureMenu() {
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
		final IWorkbenchPart part = partService.getActivePart();
		if ((part instanceof EditorPart) || (part instanceof ISetSelectionTarget)) {
			final Optional<IProject> project = SelectionUtil.getProjectFromSelectedResource();
			try {
				return project.isPresent() && ! project.get().hasNature(MiningProjectNature.NATURE_ID);
			} catch (final CoreException e) {
				Logging.warn(String.format("Could not determine if the project : %s has mining nature", project.get().getName()));
				return false;		
				
			
			}
		}
		return false;
	
	}
	
}
