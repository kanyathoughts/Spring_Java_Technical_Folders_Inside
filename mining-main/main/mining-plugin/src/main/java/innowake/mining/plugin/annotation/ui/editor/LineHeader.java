/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.editor;

import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.codemining.ICodeMiningProvider;
import org.eclipse.jface.text.codemining.LineHeaderCodeMining;
import org.eclipse.swt.events.MouseEvent;

import innowake.lib.core.api.lang.Nullable;

/**
 * A code mining line header.
 */
class LineHeader extends LineHeaderCodeMining {

	private final String text;
	
	LineHeader(
			final int lineNumber, 
			final String text, 
			final IDocument document, 
			final ICodeMiningProvider provider) throws BadLocationException {
		super(lineNumber, document, provider);
		this.text = text;
	}
	
	LineHeader(
			final int lineNumber, 
			final String text, 
			final IDocument document, 
			final ICodeMiningProvider provider, 
			final Consumer<MouseEvent> action) throws BadLocationException {
		super(lineNumber, document, provider, action);
		this.text = text;
	}

	@Override
	protected CompletableFuture<Void> doResolve(@Nullable final ITextViewer viewer, @Nullable final IProgressMonitor monitor) {
		return CompletableFuture.runAsync(() -> setLabel(text));
	}
}