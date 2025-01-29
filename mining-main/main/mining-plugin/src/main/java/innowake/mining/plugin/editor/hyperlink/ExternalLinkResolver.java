/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.hyperlink;

import org.eclipse.jface.text.IDocument;

import innowake.lib.core.api.lang.Nullable;

/**
 * Generic External link resolver.
 */
public interface ExternalLinkResolver {
	
	/**
	 * Resolves the external links present at the given offset of the given document.
	 *
	 * @param document for which the external links need to be resolved
	 * @param offset for which the external links need to be resolved
	 * @return the hyperlink for the given offset
	 */
	@Nullable
	IdeExternalHyperlink resolve(IDocument document, int offset);

}
