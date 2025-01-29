/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.editor;

import java.util.Optional;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.source.ISourceViewerExtension5;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditor;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.base.ui.EditorActionUtil;
import innowake.mining.plugin.preferences.ProjectValidator;


/**
 * Manages {@linkplain IEditorDecorator editor decorators} to visualize mining for any text editor.
 */
public final class EditorDecorationManager {
	
	/**
	 * Singleton instance.
	 */
	public static final EditorDecorationManager INSTANCE = new EditorDecorationManager();
	
	private final ListMultimap<ISourceViewerExtension5, IEditorDecorator> decorators = ArrayListMultimap.create();
	
	private EditorDecorationManager() {}
	
	/**
	 * Installs all {@link IEditorDecorator editor decorators} on the given {@link IWorkbenchPart}.
	 *
	 * @param part the workbench part
	 */
	public void install(@Nullable final IWorkbenchPart part) {
		if (part instanceof AbstractDecoratedTextEditor) {
			final Optional<IFile> file = EditorActionUtil.getEditorInputFile((AbstractDecoratedTextEditor) part);
			if (file.isPresent() && ProjectValidator.isValidWithMiningNature(file.get().getProject())) {
				final Optional<ISourceViewerExtension5> viewer = getViewer(part);
				if (viewer.isPresent() && ! decorators.containsKey(viewer.get())) {
					
					final ISourceViewerExtension5 sourceViewerExtension = viewer.get();
					final IEditorDecorator editorAnnotationDecorator = new AnnotationEditorDecorator(file.get());
					if (editorAnnotationDecorator.isActive()) {
						editorAnnotationDecorator.install(sourceViewerExtension);
						decorators.put(sourceViewerExtension, editorAnnotationDecorator);
					}
					final IEditorDecorator editorDataDictionaryDecorator = new DataDictionaryEditorDecorator(file.get());
					if (editorDataDictionaryDecorator.isActive()) {
						editorDataDictionaryDecorator.install(sourceViewerExtension);
						decorators.put(sourceViewerExtension, editorDataDictionaryDecorator);
					}
				}
			}
		}
	}
	
	/**
	 * Uninstalls all {@link IEditorDecorator editor decorators} on the given {@link IWorkbenchPart}.
	 *
	 * @param part the workbench part
	 */
	public void uninstall(@Nullable final IWorkbenchPart part) {
		final Optional<ISourceViewerExtension5> viewer = getViewer(part);
		if (viewer.isPresent() && decorators.containsKey(viewer.get())) {
			for (final IEditorDecorator decorator : decorators.removeAll(viewer.get())) {
				decorator.uninstall();
			}
		}
	}

	/**
	 * Uninstalls and installs all {@link IEditorDecorator editor decorators} on the given {@link IWorkbenchPart}.
	 *
	 * @param part the workbench part
	 */
	public void reinstall(@Nullable final IWorkbenchPart part) {
		uninstall(part);
		install(part);
	}

	private static Optional<ISourceViewerExtension5> getViewer(@Nullable final IWorkbenchPart part) {
		if (part != null) {
			@Nullable final ITextViewer viewer = part.getAdapter(ITextViewer.class);
			if (viewer instanceof ISourceViewerExtension5) {
				return Optional.of((ISourceViewerExtension5) viewer);
			}
		}
		return Optional.empty();
	}
}
