/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.handler;

import javax.swing.JOptionPane;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import innowake.lib.core.api.lang.Nullable;

/**
 * Handler to open Info dialog
 */
public class AnnotationSearchViewHandler extends AbstractHandler {

	@Override
	@Nullable
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		JOptionPane.showMessageDialog(null, "The Annotation Search has been moved from Eclipse into the Annotations view in the Mining UI.", "Message",
				JOptionPane.INFORMATION_MESSAGE);
		return null;
	}
}
