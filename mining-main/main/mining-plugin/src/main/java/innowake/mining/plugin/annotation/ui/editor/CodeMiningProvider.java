/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.editor;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITextViewerExtension5;
import org.eclipse.jface.text.codemining.AbstractCodeMiningProvider;
import org.eclipse.jface.text.codemining.ICodeMining;
import org.eclipse.swt.events.MouseEvent;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.annotation.ui.handler.DeleteAnnotationHandler;
import innowake.mining.plugin.annotation.ui.handler.EditAnnotationWizardHandler;
import innowake.mining.plugin.base.ui.CommandUtil;
import innowake.mining.shared.entities.AnnotationPojo;

/**
 * Provides code mining {@linkplain LineHeader line headers} for {@linkplain Annotation annotations} to be displayed in text editors.
 */
@SuppressWarnings("javadoc")
class CodeMiningProvider extends AbstractCodeMiningProvider {
	
	private final AnnotationPojo[] annotations;
	
	CodeMiningProvider(final AnnotationPojo[] annotations) {
		this.annotations = annotations;
	}

	@Override
	public CompletableFuture<List<? extends ICodeMining>> provideCodeMinings(@Nullable final ITextViewer viewer, @Nullable final IProgressMonitor monitor) {
		return CompletableFuture.supplyAsync(() -> createMarkers(assertNotNull(viewer)));
	}
	
	private List<ICodeMining> createMarkers(final ITextViewer viewer) {
		return Arrays.stream(annotations)
			.flatMap(annotation -> createMarkers(viewer, annotation))
			.collect(Collectors.toList());
	}

	private Stream<ICodeMining> createMarkers(final ITextViewer viewer, final AnnotationPojo annotation) {
		final int offset = annotation.getLocation().orElseThrow(() -> new IllegalStateException("Location in annotation must be present")).getOffset().intValue();
		if (viewer instanceof ITextViewerExtension5 && -1 == ((ITextViewerExtension5) viewer).modelOffset2WidgetOffset(offset)) {
			/* line is not visible, needs to be excluded since org.eclipse.jface.text.source.inlined.InlinedAnnotationSupport.VisibleLines does only consider 
			 * start and end offset of the visible part of the editor and ignores code folding what would lead to not visible annotations to still be clickable 
			 * at the last position where they were shown on the screen. */
			return Stream.empty();
		}
		final StringBuilder annotationDescription = new StringBuilder();
		annotationDescription.append('[').append(annotation.getType().getDisplayName()).append(']');
		annotationDescription.append(" ").append(StringUtils.abbreviate(annotation.getName(), 80));
		try {
			final IDocument document = viewer.getDocument();
			final int currentLine = document.getLineOfOffset(offset);
			final ICodeMining[] lineHeaders = new ICodeMining[] { 
					new LineHeader(currentLine, annotationDescription.toString(), document, this),
					new LineHeader(currentLine, "Edit", document, this, event -> editAnnotation(event, annotation)), 
					new LineHeader(currentLine, "Delete", document, this, event -> deleteAnnotation(event, annotation)) 
					};
			return Stream.of(lineHeaders);
		} catch (final BadLocationException e) {
			Logging.error("Error while creating code mining markers.", e);
			return Stream.empty();
		}
	}
	
	@SuppressWarnings("unused")
	private void editAnnotation(final MouseEvent event, final AnnotationPojo annotation) {
		final Map<String, AnnotationPojo> parameters = Collections.singletonMap(EditAnnotationWizardHandler.PARAMETER_ANNOTATION, annotation);
		CommandUtil.executeCommand(EditAnnotationWizardHandler.ID, parameters);
	}

	@SuppressWarnings("unused")
	private void deleteAnnotation(final MouseEvent event, final AnnotationPojo annotation) {
		final Map<String, AnnotationPojo> parameters = Collections.singletonMap(DeleteAnnotationHandler.PARAMETER_ANNOTATION, annotation);
		CommandUtil.executeCommand(DeleteAnnotationHandler.ID, parameters);
	}
}