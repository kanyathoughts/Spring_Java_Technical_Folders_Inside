/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.editor.generic;

import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.content.IContentType;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.editor.basic.BasicOutlinePage;

/**
 * Factory method to return outline pages based on the content type id.
 */
public class OutlineFactory extends ContentOutlinePage implements IAdapterFactory {

	@Override
	@Nullable
	public <T> T getAdapter(@Nullable final Object adaptableObject, @Nullable final Class<T> adapterType) {
		if (adapterType == IContentOutlinePage.class && adaptableObject instanceof ITextEditor) {
			/* determines content type of existing editor and call content outline method*/
			final ITextEditor textEditor = (ITextEditor) adaptableObject;
			final IEditorInput editorInput = textEditor.getEditorInput();
			if (editorInput != null) {
				final List<IContentType> contentTypes = Arrays.asList(Platform.getContentTypeManager().findContentTypesFor(editorInput.getName()));
				final String contentType = contentTypes.isEmpty() ? ""
						: contentTypes.stream()
									.map(IContentType::getId)
									.filter(conType -> Arrays.stream(ContentType.values()).anyMatch(predefinedType -> predefinedType.getId().equals(conType)))
									.findFirst()
									.orElse("");
				return adapterType.cast(createOutline(contentType, textEditor));
			}
		}
		return null;
	}

	@Override
	public Class<?>[] getAdapterList() {
		return new Class<?>[] {
				IContentOutlinePage.class
		};
	}

	/**
	 * Returns a outline page based on the content type id .
	 *
	 * @param contentTypeId content type id of the editor for which outline page needs to be created.
	 * @param editor instance of generic editor for which outline page will be created.
	 * @return IContentOutlinePage implementation.
	 */
	@Nullable
	private IContentOutlinePage createOutline(final String contentTypeId, final ITextEditor editor) {
		if (contentTypeId.equalsIgnoreCase(ContentType.BASIC_CONTENT_TYPE.getId())) {
			return new BasicOutlinePage(editor);
		}
		return null;
	}
}
