/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import static innowake.mining.plugin.base.ui.SelectionUtil.getFiles;
import static innowake.mining.plugin.base.ui.SelectionUtil.getFileAndFolderPaths;
import static innowake.mining.plugin.base.ui.SelectionUtil.getProjectFromSelectedResource;
import static innowake.mining.plugin.base.ui.SelectionUtil.getResourceSelection;

import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;

import innowake.mining.plugin.base.ResourceUtil;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.preferences.ProjectValidator;

/**
 * Base class for commands
 */
public abstract class AbstractBaseHandler extends AbstractHandler {

	private static final String EMPTY_SELECTION_TITLE = "Selection Error";
	private static final String EMPTY_PROJECT_SELECTION_MESSAGE = "Cannot determine project from current selection.";
	private static final String EMPTY_FILES_SELECTION_MESSAGE = "Cannot determine files from current selection.";

	/**
	 * Returns the {@link IProject} resolved from user selection.
	 *
	 * @return the project instance
	 * @throws ValidationException in case of no selection or the project could not be identified
	 */
	protected IProject getProject() throws ValidationException {
		final IProject project = getProjectFromSelectedResource()
				.orElseThrow(() -> new ValidationException(EMPTY_SELECTION_TITLE, EMPTY_PROJECT_SELECTION_MESSAGE));
		
		return ProjectValidator.validate(project);
	}

	/**
	 * @return the project-relative file paths of the current selection. 
	 * @throws ValidationException if no files can be identified in the current selection
	 */
	protected List<String> getFilePaths() throws ValidationException {
		final List<IFile> selectedFiles = getFiles(getResourceSelection()
				.orElseThrow(() -> new ValidationException(EMPTY_SELECTION_TITLE, EMPTY_FILES_SELECTION_MESSAGE)));
		
		if (selectedFiles.isEmpty()) {
			throw new ValidationException("No file in current selection", "Please make sure that a file or directory is selected.");
		}
		
		return selectedFiles.stream().map(ResourceUtil::getProjectRelativePath).collect(Collectors.toList());
	}
	
	/**
	 * Returns a list of paths and folders as ANT based patterns.
	 * <p>
	 * If for example a folder is selected the corresponding entry in the list will have the ANT suffix pattern for recursively selecting all files.
	 * 
	 * @return a list of path and folder patterns
	 * @throws ValidationException if there is no selection determinable from which to retrieve the files and folders
	 */
	protected List<String> getFileAndFolderPathsWithAntPattern() throws ValidationException {
		return getFileAndFolderPaths(
				getResourceSelection().orElseThrow(() -> new ValidationException(EMPTY_SELECTION_TITLE, EMPTY_FILES_SELECTION_MESSAGE)));
	}

}
