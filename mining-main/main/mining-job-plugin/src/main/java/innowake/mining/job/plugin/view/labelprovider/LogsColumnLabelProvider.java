/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import org.eclipse.jface.viewers.StyledString;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.plugin.view.JobView;

/**
 * Label provider for the "Logs" column of the {@link JobView} showing if a downloadable job logs is available.
 */
public class LogsColumnLabelProvider extends AbstractColumnLabelProvider {
	
	@Override
	public StyledString getStyledText(@Nullable final Object element) {
		final StyledString styledString = new StyledString();
		if ( ! (element instanceof RemoteJobInfo)) {
			return styledString;
		}
		return styledString.append("Logs", isOffline ? LinkStyler.INSTANCE_OFFLINE : LinkStyler.INSTANCE);
	}
}
