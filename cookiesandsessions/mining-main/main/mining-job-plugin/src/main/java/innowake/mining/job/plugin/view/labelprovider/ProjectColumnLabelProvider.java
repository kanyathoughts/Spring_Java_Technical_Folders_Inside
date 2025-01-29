/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import org.eclipse.jface.viewers.StyledString;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.plugin.view.JobView;

/**
 * Label provider for the "Project" column of the {@link JobView} showing the name of the eclipse project that triggered the job
 * and also the URL of the api-server (either project specific or workspace global).
 */
public class ProjectColumnLabelProvider extends AbstractColumnLabelProvider {

	@Override
	public StyledString getStyledText(@Nullable final Object element) {
		if (element instanceof RemoteJobInfo) {
			final RemoteJobInfo remoteJobInfo = (RemoteJobInfo) element;
			return getStyledString(remoteJobInfo.getProjectName() + " (" + remoteJobInfo.getApiServer() + ")", remoteJobInfo.getJobInfo().getStatus());
		}
		return new StyledString();
	}

}
