/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.editor;

import static innowake.lib.core.lang.Assert.assertNotNull;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewerExtension2;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.preferences.ProjectValidator;
import innowake.ndt.natclipse.ui.editor.ProtectedProjectionViewer;


/**
 * {@link IPartListener2} for {@link EditorDecorationManager}.
 */
public class EditorDecorationListener implements IPartListener2 {
	
	@Override
	public void partActivated(@Nullable final IWorkbenchPartReference partRef) {
		/* do nothing */
	}

	@Override
	public void partBroughtToTop(@Nullable final IWorkbenchPartReference partRef) {
		/* do nothing */
	}

	@Override
	public void partClosed(@Nullable final IWorkbenchPartReference partRef) {
		EditorDecorationManager.INSTANCE.uninstall(assertNotNull(partRef).getPart(false));
	}

	@Override
	public void partDeactivated(@Nullable final IWorkbenchPartReference partRef) {
		/* do nothing */
	}

	@Override
	public void partOpened(@Nullable final IWorkbenchPartReference partRef) {
		@Nullable final IWorkbenchPart part = assertNotNull(partRef).getPart(false);
		@Nullable final ITextViewer viewer = assertNotNull(part).getAdapter(ITextViewer.class);
		if (viewer instanceof ISourceViewerExtension2 && part instanceof IEditorPart) {
			final IResource resource = ((IEditorPart) part).getEditorInput().getAdapter(IResource.class);
			if (resource != null && ProjectValidator.isValidWithMiningNature(resource.getProject())) {
				final IAnnotationModel visualAnnotationModel = ((ISourceViewerExtension2) viewer).getVisualAnnotationModel();
				if ( ! (visualAnnotationModel == null || viewer instanceof ProtectedProjectionViewer)) {
					visualAnnotationModel.addAnnotationModelListener(new AnnotationModelListener());
				}
			}
		}
		EditorDecorationManager.INSTANCE.install(part);
	}

	@Override
	public void partHidden(@Nullable final IWorkbenchPartReference partRef) {
		/* do nothing */
	}

	@Override
	public void partVisible(@Nullable final IWorkbenchPartReference partRef) {
		/* do nothing */
	}

	@Override
	public void partInputChanged(@Nullable final IWorkbenchPartReference partRef) {
		EditorDecorationManager.INSTANCE.reinstall(assertNotNull(partRef).getPart(false));
	}
}