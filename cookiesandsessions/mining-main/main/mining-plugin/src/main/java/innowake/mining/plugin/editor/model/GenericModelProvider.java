/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.model;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.eclipse.core.filebuffers.FileBuffers;
import org.eclipse.core.filebuffers.ITextFileBuffer;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.content.IContentType;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.editor.basic.BasicModelProvider;
import innowake.mining.plugin.editor.generic.ContentType;
import innowake.mining.plugin.editor.hyperlink.IdeEventCallBackHandler;
import innowake.mining.plugin.editor.hyperlink.IdeEventListeners;

/**
 * Generic Model provider.
 */
public class GenericModelProvider implements IdeEventCallBackHandler {
	
	private final Map<IDocument, IModelProvider<IDocument>> modelProviders = new HashMap<>();
	
	private GenericModelProvider() {
		IdeEventListeners.getInstance().registerCallBack(this);
	}
	
	/**
	 * Returns the instance of {@link GenericModelProvider}.
	 *
	 * @return instance of {@link GenericModelProvider}
	 */
	public static GenericModelProvider getInstance() {
		return LazyHolder.INSTANCE;
	}
	
	/**
	 * Returns the Optional model result based on the document provided from the language specific model provider.
	 *
	 * @param document from which the instanceof {@link ModelResult} needs to be obtained
	 * @return optional instance of {@link ModelResult}
	 */
	public Optional<ModelResult> getModel(final IDocument document) {
		final IModelProvider<IDocument> modelProvider = getModelProvider(document);
		if (modelProvider != null) {
			return modelProvider.getModel(document);
		}
		return Optional.empty();
	}
	
	@Override
	public void partOpened(@Nullable final IWorkbenchPart part) {
		final IDocument document = getDocumentFromWorkbenchPart(part);
		if (document != null) {
			final IModelProvider<IDocument> modelProvider = getModelProvider(document);
			if (modelProvider != null) {
				modelProvider.updateModel(document);
			}
		}
	}

	@Override
	public void partClosed(@Nullable final IWorkbenchPart part) {
		final IDocument document = getDocumentFromWorkbenchPart(part);
		if (document != null) {
			final IModelProvider<IDocument> modelProvider = getModelProvider(document);
			if (modelProvider != null) {
				modelProvider.invalidateModel(document);
			}
		}
	}

	@Override
	public void documentChanged(@Nullable final DocumentEvent event) {
		if (event != null) {
			final IModelProvider<IDocument> modelProvider = getModelProvider(event.getDocument());
			if (modelProvider != null) {
				modelProvider.updateModel(event.getDocument());
			}
		}
	}
	
	@Nullable
	private IModelProvider<IDocument> getModelProvider(final IDocument document) {
		final IModelProvider<IDocument> modelProvider = modelProviders.get(document);
		if (modelProvider != null) {
			return modelProvider;
		} else {
			final ITextFileBuffer buffer = FileBuffers.getTextFileBufferManager().getTextFileBuffer(document);
			if (buffer != null) {
				final String fileName = buffer.getLocation().lastSegment();
				final IContentType[] contentTypesFor = Platform.getContentTypeManager().findContentTypesFor(fileName);
				for (final IContentType contentType : contentTypesFor) {
					if (contentType.getId().equalsIgnoreCase(ContentType.BASIC_CONTENT_TYPE.getId())) {
						final IModelProvider<IDocument> basicModelProvider = new BasicModelProvider();
						modelProviders.put(document, basicModelProvider);
						return basicModelProvider;
					}
				}
			}
		}
		return null;
	}
	
	@Nullable
	private IDocument getDocumentFromWorkbenchPart(@Nullable final IWorkbenchPart part) {
		if (part instanceof ITextEditor) {
			final ITextEditor editor = part.getAdapter(ITextEditor.class);
			if (editor != null) {
				final IDocumentProvider provider = editor.getDocumentProvider();
				return provider.getDocument(editor.getEditorInput());
			}
		}
		return null;
	}
	
	private static class LazyHolder {
		private static final GenericModelProvider INSTANCE = new GenericModelProvider(); 
	}
	
}
