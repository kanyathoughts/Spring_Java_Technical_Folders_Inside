/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.handler;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Optional;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningJobGroup;
import innowake.mining.plugin.annotation.ui.editor.EditorDecorationManager;
import innowake.mining.plugin.base.ui.EditorActionUtil;
import innowake.mining.plugin.base.ui.FileSelectionWrapper;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.plugin.preferences.ProjectValidator;
import innowake.mining.shared.entities.AnnotationPojo;

/**
 * Handler used to delete a {@link AnnotationPojo}.
 */
public class DeleteAnnotationHandler extends AbstractHandler {

	/**
	 * Public id of the command.
	 */
	public static final String ID = "innowake.mining.commands.deleteAnnotation";
	public static final String PARAMETER_ANNOTATION = "innowake.mining.commands.deleteAnnotation.parameterAnnotation";
	public static final String ERROR_WHILE_DELETING_ANNOTATION = "Error while deleting annotation.";
	
	private static final String NO_FILE_SELECTED = "No File selected. Please ensure that the file is open in the editor.";
	private static final String MISSING_MINING_NATURE_OR_INVALID_PROJECT = "Either project is missing mining nature or is not a mining API project.";
	
	@Override
	@Nullable
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		final AnnotationPojo annotation = (AnnotationPojo) assertNotNull(event).getParameters().get(PARAMETER_ANNOTATION);

		if ( ! MessageDialog.openConfirm(WorkbenchUtil.getActiveShell(), "Delete Annotation.", "Are you sure you want to delete the annotation '" + annotation.getName() + "'?")) {
			return null;
		}
		
		final FileSelectionWrapper fileSelectionWrapper = new FileSelectionWrapper(EditorActionUtil.getFile());
		if (fileSelectionWrapper.isEmpty()) {
			MessageDialog.openError(WorkbenchUtil.getActiveShell(), ERROR_WHILE_DELETING_ANNOTATION, NO_FILE_SELECTED);
			return null;
		}
		
		final Optional<IProject> project = SelectionUtil.getProject(fileSelectionWrapper);
		if ( ! project.isPresent() || ! ProjectValidator.isValidWithMiningNature(project.get())) {
			MessageDialog.openError(WorkbenchUtil.getActiveShell(), ERROR_WHILE_DELETING_ANNOTATION, MISSING_MINING_NATURE_OR_INVALID_PROJECT);
			return null;
		}
		
		final Job job = Job.create("Deleting Annotation", (@Nullable IProgressMonitor monitor) ->
			MiningServiceExecutor
				.create(() ->
					ApiClient.annotationService(project.get())
					.deleteAnnotation()
					.setProjectId(annotation.getProject())
					.setAnnotationId(annotation.identity()))
				.setInvalidResultConsumer(invalidResult -> {
					Logging.error(invalidResult.getExtendedStatusMessage());
					Display.getDefault().asyncExec(() ->
						MessageDialog.openError(WorkbenchUtil.getActiveShell(), ERROR_WHILE_DELETING_ANNOTATION, invalidResult.getStatusMessage()));
				})
				.setExceptionConsumer(exception -> {
					Logging.error(ERROR_WHILE_DELETING_ANNOTATION, exception);
					Display.getDefault().asyncExec(() ->
						MessageDialog.openError(WorkbenchUtil.getActiveShell(), ERROR_WHILE_DELETING_ANNOTATION, exception.getLocalizedMessage())
					);
				})
				.execute()
		);
		job.setSystem(false);
		job.setJobGroup(MiningJobGroup.INSTANCE);
		job.schedule();

		EditorDecorationManager.INSTANCE.reinstall(WorkbenchUtil.getActiveEditor());
		return null;
	}
}
