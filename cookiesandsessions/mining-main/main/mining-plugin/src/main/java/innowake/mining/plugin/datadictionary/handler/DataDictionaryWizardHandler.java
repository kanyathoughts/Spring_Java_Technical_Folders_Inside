/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.datadictionary.handler;

import static innowake.mining.plugin.ui.WebBasedViewHandlerHelper.openWebBasedView;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import innowake.mining.plugin.datadictionary.view.DataDictionaryEditorView;
import innowake.mining.shared.entities.DataDictionaryPojo;


/**
 * Handler for showing the {@link DataDictionaryPojo}.
 */
public class DataDictionaryWizardHandler extends DataDictionaryBaseHandler {


	@Override
	protected boolean handle(final ExecutionEvent event) throws ExecutionException {
		final Shell shell = HandlerUtil.getActiveShell(event);

		if ( ! dataField.isPresent()) {
			MessageDialog.openWarning(shell, "Error retrieving Cobol data field", "No data field declaration found for the current selection.");
			return false;
		}

		showWebBasedView(event);

		return true;
	}

	private void showWebBasedView(final ExecutionEvent event) throws ExecutionException {
		if (existingDataDictionaryEntry.isPresent()) {
			openWebBasedView(DataDictionaryEditorView.ID, event, existingDataDictionaryEntry.get());
		} else {
			openWebBasedView(DataDictionaryEditorView.ID, event);
		}
	}
}
