/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */

package innowake.mining.plugin.ui;

import java.util.List;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.MiningPlugin;

/**
 * Custom {@link ErrorDialog} for displaying the list of files that cannot be processed.
 */
public class SourceObjectErrorDialog extends ErrorDialog {


	private SourceObjectErrorDialog(final Shell parentShell, final List<IFile> files, final String message) {
		super(parentShell, "Warning", null, getMultiStatus(files, message), 2);
	}

	/**
	 * Opens the {@link MessageDialog} displaying the list of files to be skipped.
	 * 
	 * @param parentShell the parent Shell
	 * @param files the list of files to skip
	 * @param message the message to be displayed in the dialog
	 * @return the user pressed button id
	 */
	public static int open(final Shell parentShell, final List<IFile> files, final String message) {
		final SourceObjectErrorDialog dialog = new SourceObjectErrorDialog(parentShell, files, message);
		return dialog.open();
	}


	@Override
	protected void createButtonsForButtonBar(@Nullable final Composite parent) {
		createButton(parent, IDialogConstants.SKIP_ID, "Skip files", true);
		createButton(parent, IDialogConstants.ABORT_ID, IDialogConstants.ABORT_LABEL, false);
		createDetailsButton(parent);
	}

	@Override
	protected void buttonPressed(final int id) {
		super.buttonPressed(id);
		if (id == IDialogConstants.SKIP_ID || id == IDialogConstants.ABORT_ID) {
			setReturnCode(id);
			close();
		}
	}

	private static MultiStatus getMultiStatus(final List<IFile> files, final String message) {
		final MultiStatus status = new MultiStatus(MiningPlugin.ID, IStatus.WARNING, message);
		files.stream()
			.map(IFile::getFullPath)
			.map(IPath::toOSString)
			.forEach(file -> status.add(new Status(IStatus.WARNING, MiningPlugin.ID, file)));

		return status;
	}

}
