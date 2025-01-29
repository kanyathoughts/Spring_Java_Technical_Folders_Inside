/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.generic;

import org.eclipse.core.filebuffers.IDocumentSetupParticipant;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.PlatformUI;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.editor.hyperlink.IdeEventListeners;
import innowake.mining.plugin.editor.hyperlink.MiningBasedExternalLinkResolver;
import innowake.mining.plugin.editor.model.GenericModelProvider;


/**
 * Default document setup participant to be used for multiple text editors.
 */
public class DocumentSetupParticipant implements IDocumentSetupParticipant {

	@Override
	public void setup(@Nullable final IDocument document) {
		/*
		 * Calling getInstance of GenericModelProvider and MiningBasedExternalLinkResolver to trigger the registering of event 
		 * call back listeners (partOpened, partClosed, documentChanged) which is being done in their respective constructors.
		 */
		GenericModelProvider.getInstance();
		MiningBasedExternalLinkResolver.getInstance();
		final IdeEventListeners listeners = IdeEventListeners.getInstance();
		if (document != null) {
			document.addDocumentListener(listeners.getDocumentListener());
			PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().addPartListener(listeners.getPartListener());
		}
		
	}

}
