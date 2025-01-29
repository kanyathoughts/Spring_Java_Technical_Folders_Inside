/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.hyperlink;

import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.ui.IWorkbenchPart;

import innowake.lib.core.api.lang.Nullable;

/**
 * Implementation of this class would handle the call backs for eclipse events.
 */
public interface IdeEventCallBackHandler {
	
	/**
	 * Callback method to be called when the current workbench part is opened.
	 *
	 * @param part the part that was opened
	 */
	public void partOpened(@Nullable IWorkbenchPart part);
	
	/**
	 * Callback method to be called when the current workbench part is closed.
	 *
	 * @param part the part that was closed
	 */
	public void partClosed(@Nullable IWorkbenchPart part);
	
	/**
	 * Callback to be called when the current document is changed.
	 *
	 * @param event the document event describing the document change
	 */
	public void documentChanged(@Nullable DocumentEvent event);
	
}
