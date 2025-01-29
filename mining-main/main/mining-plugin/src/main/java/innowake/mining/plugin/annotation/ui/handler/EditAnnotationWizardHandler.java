/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.handler;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.plugin.ui.WebBasedViewHandlerHelper.openWebBasedView;

import java.util.Optional;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.annotation.ui.view.AnnotationEditorView;
import innowake.mining.plugin.base.ui.EditorActionUtil;
import innowake.mining.plugin.base.ui.FileSelectionWrapper;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.plugin.preferences.ProjectValidator;
import innowake.mining.shared.entities.AnnotationPojo;

/**
 * Handler used to show the {@linkplain AnnotationEditorView} for editing {@linkplain AnnotationPojo}.
 */
public class EditAnnotationWizardHandler extends AbstractHandler {

	/**
	 * Public id of the command.
	 */
	public static final String ID = "innowake.mining.commands.editAnnotation";
	public static final String PARAMETER_ANNOTATION = "innowake.mining.commands.editAnnotation.parameterAnnotation";
	public static final String ERROR_WHILE_EDITING_ANNOTATION = "Error while editing annotation.";
	
	private static final String NO_FILE_SELECTED = "No File selected. Please ensure that the file is open in the editor.";
	private static final String MISSING_MINING_NATURE_OR_INVALID_PROJECT = "Either project is missing mining nature or is not a mining API project.";
	
	@Override
	@Nullable
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		final AnnotationPojo annotation = (AnnotationPojo) assertNotNull(event).getParameters().get(PARAMETER_ANNOTATION);
		final FileSelectionWrapper fileSelectionWrapper = new FileSelectionWrapper(EditorActionUtil.getFile());
		
		if (fileSelectionWrapper.isEmpty()) {
			MessageDialog.openError(WorkbenchUtil.getActiveShell(), ERROR_WHILE_EDITING_ANNOTATION, NO_FILE_SELECTED);
			return null;
		}
		
		final Optional<IProject> project = SelectionUtil.getProject(fileSelectionWrapper);
		if ( ! project.isPresent() || ! ProjectValidator.isValidWithMiningNature(project.get())) {
			MessageDialog.openError(WorkbenchUtil.getActiveShell(), ERROR_WHILE_EDITING_ANNOTATION, MISSING_MINING_NATURE_OR_INVALID_PROJECT);
			return null;
		}
		
		final Optional<AnnotationPojo> updatedAnnotation = updateAnnotation(annotation, project);
		if ( ! updatedAnnotation.isPresent()) {
			final String title = "Could not update the annotation for editing";
			final String message = "Please check your connection to the mining server.";
			Logging.error(String.format("%s. %s", title, message));
			return null;
		}

		openWebBasedView(AnnotationEditorView.ID, event, updatedAnnotation.get());
		return null;
	}

	private Optional<AnnotationPojo> updateAnnotation(final AnnotationPojo annotation, final Optional<IProject> optionalProject) { 
		if ( ! optionalProject.isPresent()) {
			final String message = "Could not determine project from current selection.";
			Logging.error(message);
			showError("Error retrieving project", message);
			return Optional.empty();
		}
		return MiningServiceExecutor
				.create(() ->
					ApiClient.annotationService(optionalProject.get())
					.findAnnotationById()
					.setAnnotationId(assertNotNull(annotation.identity())))
				.setInvalidResultConsumer(annotationResult -> {
					final String message = annotationResult.getExtendedStatusMessage();
					Logging.error(message);
					showError("Invalid result from server received", message);
				})
				.setExceptionConsumer(exception -> {
					final String title = "Error while retrieving annotation.";
					Logging.error(title, exception);
					showError(title, exception.getLocalizedMessage());
				})
				.execute();
	}

	private void showError(final String title, final String message) {
		Display.getDefault().asyncExec(() -> MessageDialog.openError(WorkbenchUtil.getActiveShell(), title, message));
	}

}
