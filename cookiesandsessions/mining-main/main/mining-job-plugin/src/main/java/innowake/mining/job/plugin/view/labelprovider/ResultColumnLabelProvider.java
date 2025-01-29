/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import org.eclipse.jface.viewers.StyledString;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.plugin.view.JobView;
import innowake.mining.shared.model.job.ResultStatus;

/**
 * Label provider for the "Result" column of the {@link JobView} showing if a downloadable job result is available.
 */
public class ResultColumnLabelProvider extends AbstractColumnLabelProvider {
	
	/** The 'Get result' label of this label provider */
	public static final String GET_RESULT = "Get result";

	@Override
	public StyledString getStyledText(@Nullable final Object element) {
		final StyledString styledString = new StyledString();
		if ( ! (element instanceof RemoteJobInfo)) {
			return styledString;
		}
		
		final RemoteJobInfo remoteJobInfo = (RemoteJobInfo) element;
		final ResultStatus resultStatus = remoteJobInfo.getJobInfo().getResultStatus();
		
		if (resultStatus != null && resultStatus.hasCollectableResult() && ! resultStatus.isInternalResult()) {
			/* As the TreeViewer doesn't support embedding elements like hyperlinks or buttons,
			 * we use a custom styler to make the text look like a hyperlink as a workaround. */
			return styledString.append(GET_RESULT, isOffline ? LinkStyler.INSTANCE_OFFLINE : LinkStyler.INSTANCE);
		}
		return styledString;
	}
}
