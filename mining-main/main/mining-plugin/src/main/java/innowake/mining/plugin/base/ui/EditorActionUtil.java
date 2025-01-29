/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base.ui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelExtension;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditor;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * Wrapper class for editor input selections
 */
public final class EditorActionUtil {

	private EditorActionUtil() {}
	
	/**
	 * Check if the active editor has a valid - not empty - text selection.
	 *
	 * @return true or false.
	 */
	public static boolean hasValidSelection() {
		final Optional<ITextSelection> textSelection = getSelection();
		
		return textSelection
			.map( selection -> Boolean.valueOf( selection.getLength() > 0 ) )
			.orElseGet( () -> Boolean.FALSE)
			.booleanValue();
	}
	
	/**
	 * Get the selected text from the active editor.
	 *
	 * @return The selected text as string or an empty string.
	 */
	public static String getSelectedText() {
		final Optional<ITextSelection> textSelection = getSelection();
		
		return textSelection
			.map( ITextSelection::getText )
			.orElseGet( () -> "");
	}
	
	/**
	 * Get the {@code IFile} assigned to the active editor or an empty optional.
	 *
	 * @return The get {@code IFile} of the editor or empty.
	 */
	public static Optional<IFile> getFile() {
		final IEditorPart editorPart = WorkbenchUtil.getActiveEditor();
		if (editorPart != null) {
			final IEditorInput editorInput = editorPart.getEditorInput();
			if (editorInput instanceof FileEditorInput) {
				return Optional.of( ((FileEditorInput) editorInput).getFile() );
			}			
		}		
		return Optional.empty();
	}
	
	/**
	 * @return the currently active editor or {@link Optional#empty()} if no editor is active
	 */
	public static Optional<IEditorPart> getActiveEditor() {
		return Optional.ofNullable(WorkbenchUtil.getActiveEditor());
	}
	
	/**
	 * Get the {@code IFile} assigned to the specified editor or an empty optional.
	 * 
	 * @param editorPart the editor
	 * @return The get {@code IFile} of the editor or empty.
	 */
	public static Optional<IFile> getFile(final IEditorPart editorPart) {
		final IEditorInput editorInput = editorPart.getEditorInput();
		if (editorInput instanceof FileEditorInput) {
			return Optional.of( ((FileEditorInput) editorInput).getFile() );
		}
		return Optional.empty();
	}

	/**
	 * Get the editor selection object or empty if not available.
	 *
	 * @return The editor selection or empty.
	 */
	public static Optional<ITextSelection> getSelection() {
		final ITextEditor editor = (ITextEditor) WorkbenchUtil.getActiveEditor();
		if (editor != null) {
			final ISelection editorSelection = editor.getSelectionProvider().getSelection();
			if (editorSelection instanceof ITextSelection) {
				return Optional.of( (ITextSelection) editorSelection );
			}
		}
		return Optional.empty();
	}
	
	/**
	 * Open the editor for a give file.
	 *
	 * @param inputFile The file to open in an editor.
	 * @return The editor instance.
	 */
	public static IEditorPart openEditor(final IFile inputFile) {
		try {
			return IDE.openEditor(WorkbenchUtil.getActivePage(), inputFile);
		} catch (final PartInitException e) {
			throw new IllegalStateException(e);
		}
	}

	/**
	 * Open the editor and select a give line.
	 *
	 * @param inputFile The input file to open the editor for.
	 * @param line The 1-based line to highlight via selection.
	 * @return The editor instance.
	 * @throws BadLocationException Thrown if the location is not available.
	 */
	public static IEditorPart openEditorAndGotoLine(final IFile inputFile, final int line) throws BadLocationException {
		final ITextEditor editor = (ITextEditor) openEditor(inputFile);
		final IDocument document = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		editor.selectAndReveal(document.getLineOffset(line - 1), document.getLineLength(line - 1));
		return editor;
	}

	/**
	 * Open the editor and select a give range with offsets.
	 *
	 * @param inputFile The input file to open the editor for.
	 * @param beginOffset The begin offset.
	 * @param length The length of the text to select.
	 * @return The editor instance.
	 */
	public static IEditorPart openEditorAndGotoLine(final IFile inputFile, final int beginOffset, final int length) {
		final ITextEditor editor = (ITextEditor) openEditor(inputFile);
		editor.getDocumentProvider().getDocument(editor.getEditorInput());
		editor.selectAndReveal(beginOffset, length);
		return editor;
	}
	
	/**
	 * Get access to the current active editor text viewer.
	 *
	 * @return Access to the text viewer or empty.
	 */
	public static Optional<ITextViewer> getTextViewer() {
		final Optional<ITextEditor> editorOpt = Optional.ofNullable( (ITextEditor) WorkbenchUtil.getActiveEditor());
		return editorOpt.map(editor -> editor.getAdapter(ITextOperationTarget.class))
				.map(ITextViewer.class::cast)
				.map(Optional::of)
				.orElseGet(Optional::empty);
	}
	
	/**
	 * Decorate the current active editor with a new set of annotations.
	 * The existing ones of the same type will be replaced. 
	 *
	 * @param newAnnotations The new set of annotations.
	 * @param annotationTypes The annotation types to use. Example The jdt info type {@code org.eclipse.jdt.ui.info}
	 * @return true if the annotations could be set, otherwise false.
	 * false if
	 * <br> - The new set of annotations is empty
	 * <br> - The current text editor could not be accessed / is not available. 
	 */
	public static final boolean decorateActiveEditor(final Map<Annotation, Position> newAnnotations, final Collection<String> annotationTypes) {
		final ITextEditor textEditor = (ITextEditor) WorkbenchUtil.getActiveEditor();
		if (textEditor == null) {
			return false;
		}
		
		final IEditorInput editorInput = textEditor.getEditorInput();
		final IAnnotationModel annotationModel = textEditor.getDocumentProvider().getAnnotationModel(editorInput);
		
		if (newAnnotations.isEmpty()) {
			removePreviousAnnotations(annotationModel, annotationTypes);
			return false;
		}
		
		final List<Annotation> existingFlowAnnotations = existingFlowAnnotations(annotationModel, annotationTypes);
		
		if (annotationModel instanceof IAnnotationModelExtension) {
			((IAnnotationModelExtension) annotationModel).replaceAnnotations(existingFlowAnnotations.toArray(new Annotation[0]), newAnnotations);
		} else {
			removePreviousAnnotations(annotationModel, annotationTypes);
			newAnnotations.forEach(annotationModel::addAnnotation);
		}

		return true;
	}
	
	/**
	 * Get the {@code IFile} assigned to an {@link AbstractDecoratedTextEditor} or an empty optional.
	 * 
	 * @param editor the editor
	 * @return The get {@code IFile} of the part or empty.
	 */
	public static Optional<IFile> getEditorInputFile( final AbstractDecoratedTextEditor editor) {
		final IEditorInput editorInput = editor.getEditorInput();
		if (editorInput instanceof FileEditorInput) {
			final FileEditorInput fileEditorInput = (FileEditorInput) editorInput;
			final IFile file = fileEditorInput.getFile();
			return Optional.of(file);
		}
		return Optional.empty();
	}
	
	private static final List<Annotation> existingFlowAnnotations(final IAnnotationModel annotationModel, final Collection<String> annotationTypes) {
		final List<Annotation> result = new ArrayList<>();
		annotationModel.getAnnotationIterator().forEachRemaining(a -> {
			for (final String annotationType : annotationTypes) {
				if (annotationType.equals(a.getType())) {
					result.add(a);
				}
			}
		});
		return result;
	}
	
	private static final void removePreviousAnnotations(final IAnnotationModel annotationModel, final Collection<String> annotationTypes) {
		final List<Annotation> existingFlowAnnotations = existingFlowAnnotations(annotationModel, annotationTypes);
		if (annotationModel instanceof IAnnotationModelExtension) {
			((IAnnotationModelExtension) annotationModel).replaceAnnotations(existingFlowAnnotations.toArray(new Annotation[0]), Collections.emptyMap());
		} else {
			existingFlowAnnotations.forEach(annotationModel::removeAnnotation);
		}
	}
}
