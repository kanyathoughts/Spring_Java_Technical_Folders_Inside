/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences;

import java.util.Optional;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelection;

import innowake.mining.plugin.MiningProjectNature;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.base.ui.SelectionUtil;

/**
 * Checks if an mining API project can be derived from a {@link IProject} or {@link ISelection}.
 */
public class ProjectValidator {

	private ProjectValidator() {}
	
	/**
	 * Checks the given selection for the presence of a mining API project.
	 *
	 * @param selection the selection to check
	 * @return the project if a mining API project can be derived
	 * @throws ValidationException if no mining API project can be derived
	 */
	public static IProject validate(final ISelection selection) throws ValidationException {
		final Optional<IProject> project = SelectionUtil.getProject(selection);
		return validate(project.orElseThrow(() -> new ValidationException("Selection Error", "Cannot determine project from current selection.")));
	}
	
	/**
	 * Checks the given project for the presence of a mining API project.
	 *
	 * @param project the project to check
	 * @return the project if a mining API project can be derived
	 * @throws ValidationException if no mining API project can be derived
	 */
	public static IProject validate(final IProject project) throws ValidationException {
		final Optional<ProjectData> apiProject = MiningPreferences.getApiProject(project);

		if ( ! apiProject.isPresent()) {
			throw new ValidationException("Mining preferences not available", "The Mining preferences for the selected project are not available. Please define them in the project properties.");
		}
		
		return project;
	}
	
	/**
	 * Checks the given selection for the presence of a mining API project.
	 *
	 * @param selection the selection to check
	 * @return {@code true} if a mining API project is present
	 */
	public static boolean isValid(final ISelection selection) {
		try {
			validate(selection);
			return true;
		} catch (final ValidationException e) {
			/* ignore */
			return false;
		}
	}
	
	/**
	 * Checks the given project for the presence of a mining API project.
	 *
	 * @param project the project to check
	 * @return {@code true} if a mining API project is present
	 */
	public static boolean isValid(final IProject project) {
		try {
			validate(project);
			return true;
		} catch (final ValidationException e) {
			/* ignore */
			return false;
		}
	}
	
	/**
	 * Checks the given {@code project} for the presence of the mining nature and a mining API project.
	 * 
	 * @param project the {@link IProject} to check
	 * @return {@code true} if mining nature and mining API project is present; {@code false} otherwise
	 */
	public static boolean isValidWithMiningNature(final IProject project) {
		try {
			if (project.isOpen() && project.hasNature(MiningProjectNature.NATURE_ID)) { 
				return isValid(project);
			}
		} catch (CoreException e) {
			/* ignore */
			return false;
		}
		return false;
	}

}
