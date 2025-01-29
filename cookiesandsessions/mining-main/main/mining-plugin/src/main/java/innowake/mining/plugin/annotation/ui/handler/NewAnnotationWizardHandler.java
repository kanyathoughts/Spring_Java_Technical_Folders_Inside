/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.handler;

import static innowake.mining.plugin.ui.WebBasedViewHandlerHelper.openWebBasedView;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.annotation.ui.view.AnnotationEditorView;
import innowake.mining.shared.entities.AnnotationPojo;

/**
 * Handler used to show the {@linkplain AnnotationEditorView} for creating new {@linkplain AnnotationPojo}.
 */
public class NewAnnotationWizardHandler extends AbstractHandler {

	/**
	 * Public id of the command.
	 */
	public static final String ID = "innowake.mining.commands.addSelectionToAnnotation";
	
	@Override
	@Nullable
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		openWebBasedView(AnnotationEditorView.ID, event);
		return null;
	}
}
