/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.view;

import java.util.Optional;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.MessageDialog;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.base.ui.EditorActionUtil;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.model.ModuleLocation;

class AnnotationTableElement {
	
	private static final String OPEN_FILE_ERROR_TITLE = "Failed to open File";
	
	private final String modulePath;
	private final IProject project;
	private final AnnotationPojo annotation;
	
	AnnotationTableElement(final AnnotationPojo annnotation, final String modulePath, final IProject project) {
		this.project = project;
		this.annotation = annnotation;
		this.modulePath = modulePath;
	}
	
	String getTypeLabel() {
		return annotation.getType().getDisplayName();
	}
	
	@Nullable
	String getCategoryLabel() {
		return annotation.getCategoryName().orElse(null);
	}
	
	@Nullable
	String getResourceLabel() {
		return modulePath;
	}
	
	String getStateLabel() {
		return annotation.getState().getName();
	}
	
	String getDescriptionLabel() {
		return annotation.getName();
	}
	
	AnnotationPojo getAnnotation() {
		return annotation;
	}
	
	void openInEditor() {
		final Optional<ModuleLocation> location = annotation.getLocation();
		if (location.isPresent()) {
			EditorActionUtil.openEditorAndGotoLine(project.getFile(modulePath), location.get().getOffset().intValue(), location.get().getLength().intValue());
		} else {
			MessageDialog.openError(WorkbenchUtil.getActiveShell(), OPEN_FILE_ERROR_TITLE, "No location information was found.");
		}			
	}
}