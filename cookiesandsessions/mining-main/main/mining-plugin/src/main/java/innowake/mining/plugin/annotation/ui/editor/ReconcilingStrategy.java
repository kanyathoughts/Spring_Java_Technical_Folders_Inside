/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.editor;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.reconciler.IReconcilingStrategy;
import org.eclipse.jface.text.source.ISourceViewerExtension5;

import innowake.lib.core.api.lang.Nullable;

/**
 * Implements {@link IReconcilingStrategy} for code mining.
 */
class ReconcilingStrategy implements IReconcilingStrategy {

	private final ISourceViewerExtension5 viewer;

	ReconcilingStrategy(final ISourceViewerExtension5 viewer) {
		this.viewer = viewer;
	}

	@Override
	public void setDocument(@Nullable final IDocument document) {
		viewer.updateCodeMinings();
	}

	@Override
	public void reconcile(@Nullable final DirtyRegion dirtyRegion, @Nullable final IRegion subRegion) {
		viewer.updateCodeMinings();
	}

	@Override
	public void reconcile(@Nullable final IRegion partition) {
		viewer.updateCodeMinings();
	}
}