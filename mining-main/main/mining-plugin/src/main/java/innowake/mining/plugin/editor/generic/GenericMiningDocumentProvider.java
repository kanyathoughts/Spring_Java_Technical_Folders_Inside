/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.editor.generic;

import org.eclipse.ui.editors.text.TextFileDocumentProvider;

import innowake.lib.core.api.lang.Nullable;

/**
 * Document provider to override default properties of a TextFileDocumentProvider
 */
public class GenericMiningDocumentProvider extends TextFileDocumentProvider {
	
	@Override
	public boolean isModifiable(final @Nullable Object element) {
		return false;
	}
	
}