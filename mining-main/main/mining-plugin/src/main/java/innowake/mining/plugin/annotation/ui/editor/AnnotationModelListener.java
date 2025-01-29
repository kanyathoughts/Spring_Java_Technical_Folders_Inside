/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.editor;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.source.AnnotationModelEvent;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelListener;
import org.eclipse.jface.text.source.IAnnotationModelListenerExtension;
import org.eclipse.jface.text.source.ISourceViewerExtension5;
import org.eclipse.ui.IEditorPart;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.base.ui.WorkbenchUtil;

/**
 * Listener for changes to the model inside an editor to update the code mining annotations.
 */
public class AnnotationModelListener implements IAnnotationModelListener, IAnnotationModelListenerExtension {
	
	@Override
	public void modelChanged(@Nullable final AnnotationModelEvent event) {
		updateCodeMinings();
	}

	@Override
	public void modelChanged(@Nullable final IAnnotationModel model) {
		updateCodeMinings();
	}

	private void updateCodeMinings() {
		final IEditorPart activeEditor = WorkbenchUtil.getActiveEditor();
		if (activeEditor != null) {
			@Nullable final ITextViewer viewer = activeEditor.getAdapter(ITextViewer.class);
			if (viewer instanceof ISourceViewerExtension5) {
				/* org.eclipse.jface.text.source.inlined.InlinedAnnotationSupport.VisibleLines only refreshes the end offset for the visible part of the editor
				 * in case of a resize, viewport change, or document change event. This code is only used to trigger a viewport change event so that the 
				 * CodeMinings are updated with the correct end offset */
				final int currentIndex = viewer.getTopIndex();
				viewer.setTopIndex(0 == currentIndex ? 1 : 0);
				viewer.setTopIndex(currentIndex);
				
				((ISourceViewerExtension5) viewer).updateCodeMinings();
			}
		}
	}
}