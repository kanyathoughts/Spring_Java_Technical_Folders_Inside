/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.datadictionary.handler;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.datadictionary.DeleteDataDictionaryEntry;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningJobGroup;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.entities.DataDictionaryPojo;


/**
 * Removes the selected data dictionary entry.
 */
public class DataDictionaryRemoveHandler extends DataDictionaryBaseHandler {
	
	@Override
	protected boolean handle(final ExecutionEvent event) {
		final Shell shell = HandlerUtil.getActiveShell(event);
		
		if ( ! module.isPresent()) {
			MessageDialog.openError(shell, INVALID_SELECTION_ERROR_TITLE, "Mining module not found for selected file.");
			return false;
		}
		
		if ( ! existingDataDictionaryEntry.isPresent()) {
			MessageDialog.openError(shell, INVALID_SELECTION_ERROR_TITLE, "No data dictionary entry at selection available.");
			return false;
		}

		final String message = String.format("Do you really want to delete the entry for %s?", existingDataDictionaryEntry.get().getName());
		if (MessageDialog.openConfirm(shell, "Confirm deletion", message)) {
			final Job job = Job.create("Deleting Data Dictionary Entry", (@Nullable IProgressMonitor monitor) -> 
				MiningServiceExecutor
					.create(() -> deleteDataDictionaryEntry(existingDataDictionaryEntry.get()))
					.setInvalidResultConsumer(r -> {
						Logging.error(r.getExtendedStatusMessage());
						Display.getDefault().asyncExec(() ->
							MessageDialog.openError(shell, "Server error while deleting data dictionary entry.", r.getStatusMessage())
						);
					})
					.setExceptionConsumer(e -> {
						Logging.error("Error while deleting data dictionary entry.", e);
						Display.getDefault().asyncExec(() -> 
							MessageDialog.openError(shell, "Error while deleting data dictionary entry.", e.getLocalizedMessage())
						);
					})
					.execute()
			);
			job.setSystem(false);
			job.setJobGroup(MiningJobGroup.INSTANCE);
			job.schedule();
			job.addJobChangeListener(new JobChangeAdapter() {
				@Override
				public void done(@Nullable final IJobChangeEvent event) {
					if (event == null) return;
					if (event.getResult().getCode() == IStatus.OK) {
						refreshView();
					}
				}
			});
		}
		
		return true;
	}

	private DeleteDataDictionaryEntry deleteDataDictionaryEntry(final DataDictionaryPojo entry) throws CoreException, StorageException {
		final IProject presentProject = project.orElseThrow(() -> new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID,
				"Cannot determine project from current selection.")));
		return ApiClient.dataDictionaryService(presentProject)
				.deleteDataDictionaryEntry()
				.setModuleId(module.get().identity())
				.setDataDictionaryEntryId(entry.identity());
	}

}
