/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.action;

import java.net.URL;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
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
 * This is a toolbar action for the {@link JobView} that removes all jobs from the {@link RemoteJobManager}
 * that are not in an active state. Jobs with {@link JobStatus#SUCCESS} having a result will trigger a confirmation dialog.
 */
public class ClearAction extends Action {
	
	private final RemoteJobManager remoteJobManager;
	
	/**
	 * Constructor.
	 * 
	 * @param remoteJobManager the {@link RemoteJobManager} instance
	 */
	public ClearAction(final RemoteJobManager remoteJobManager) {
		this.remoteJobManager = remoteJobManager;
		
		final Bundle bundle = Platform.getBundle(MiningJobPlugin.ID);
		final URL refreshUrl = bundle.getResource("icons/clear.png");
		setImageDescriptor(ImageDescriptor.createFromURL(refreshUrl));
		setToolTipText("Clear jobs");
	}
	
	@Override
	public void run() {
		final Set<RemoteJobInfo> remoteJobInfos = remoteJobManager.getJobs().values().stream()
				.flatMap(rji -> rji.stream())
				.collect(Collectors.toSet());
		final Set<RemoteJobInfo> jobsToRemove = new HashSet<>(); /* just to avoid ConcurrentModificationException */
		
		boolean alreadyPrompted = false;
		for (final RemoteJobInfo remoteJobInfo : remoteJobInfos) {
			final JobInformation jobInfo = remoteJobInfo.getJobInfo();
			final JobStatus status = jobInfo.getStatus();
			final ResultStatus result = jobInfo.getResultStatus();
			final boolean hasCollectableResult = result != null && result.hasCollectableResult() && ! result.isInternalResult();
			
			/* Prompt the user in case there is a job with a downloadable result. */
			if (status == JobStatus.SUCCESS && hasCollectableResult && ! alreadyPrompted) {
				if ( ! MessageDialog.openConfirm(WorkbenchUtil.getActiveShell(), "Clear jobs",
					"At least one job has a downloadable result. It will not be possible to download the result after clearing the list."
							+ System.lineSeparator() + "Do you still want to clear the list?")) {
					/* Don't delete anything if the user did not confirm. */
					jobsToRemove.clear();
					break;
				}
				
				/* Only prompt for the first occurrence. */
				alreadyPrompted = true;
			}
			
			/* Remove everything, except for jobs currently being active. */
			if ( ! RemoteJobManager.isJobInActiveState(jobInfo)) {
				jobsToRemove.add(remoteJobInfo);
			}
		}
		
		if ( ! jobsToRemove.isEmpty()) {
			jobsToRemove.forEach(remoteJobManager::removeJob);
			remoteJobManager.refreshUi();
		}
	}
}
