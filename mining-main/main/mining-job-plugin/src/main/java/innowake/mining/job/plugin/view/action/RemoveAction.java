/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.action;

import java.net.URL;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.osgi.framework.Bundle;

import innowake.base.eclipse.common.ui.util.WorkbenchUtil;
import innowake.mining.job.plugin.MiningJobPlugin;
import innowake.mining.job.plugin.manager.RemoteJobManager;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.plugin.view.JobView;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.ResultStatus;

/**
 * This is a context menu action for the {@link JobView} that can be used to remove single jobs from the {@link RemoteJobManager}.
 * With this action every job can be removed.
 */
public class RemoveAction extends Action {
	
	private final RemoteJobManager remoteJobManager;
	private final TreeViewer viewer;
	
	/**
	 * Constructor.
	 * 
	 * @param remoteJobManager the {@link RemoteJobManager} instance
	 * @param viewer the {@link TreeViewer} to get the current selection from
	 */
	public RemoveAction(final RemoteJobManager remoteJobManager, final TreeViewer viewer) {
		this.remoteJobManager = remoteJobManager;
		this.viewer = viewer;
		
		final Bundle bundle = Platform.getBundle(MiningJobPlugin.ID);
		final URL removeUrl = bundle.getResource("icons/remove.png");
		final URL removeDisabledUrl = bundle.getResource("icons/remove_disabled.png");
		setImageDescriptor(ImageDescriptor.createFromURL(removeUrl));
		setDisabledImageDescriptor(ImageDescriptor.createFromURL(removeDisabledUrl));
		setToolTipText("Remove selected job");
		setText("Remove");
	}
	
	@Override
	public void run() {
		final IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();
		final Object element = selection.getFirstElement(); /* the TreeViewer only allows single selection */
		
		if (element instanceof RemoteJobInfo) {
			final RemoteJobInfo remoteJobInfo = (RemoteJobInfo) element;
			final JobInformation jobInfo = remoteJobInfo.getJobInfo();
			final JobStatus status = jobInfo.getStatus();
			final ResultStatus result = jobInfo.getResultStatus();
			final boolean hasCollectableResult = result != null && result.hasCollectableResult();
			
			boolean shouldDelete = ! RemoteJobManager.isJobInActiveState(jobInfo);
			if (status == JobStatus.SUCCESS && hasCollectableResult) {
				shouldDelete = MessageDialog.openConfirm(WorkbenchUtil.getActiveShell(), "Remove job",
						"Are you sure you want to remove this job which has a downloadable result?");
			}
			
			if (shouldDelete) {
				remoteJobManager.removeJob(remoteJobInfo);
				remoteJobManager.refreshUi();
			}
		}
	}
}
