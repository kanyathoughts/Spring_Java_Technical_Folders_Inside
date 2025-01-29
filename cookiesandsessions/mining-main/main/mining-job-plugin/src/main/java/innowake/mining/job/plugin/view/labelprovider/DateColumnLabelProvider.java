/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;

import org.eclipse.jface.viewers.StyledString;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.plugin.view.JobView;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Label provider for the "Date" column of the {@link JobView}, displaying when a job has been scheduled,
 * started, finished and so on.
 */
public class DateColumnLabelProvider extends AbstractColumnLabelProvider {
	
	private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM).withZone(ZoneId.systemDefault());

	@Override
	public StyledString getStyledText(@Nullable final Object element) {
		if ( ! (element instanceof RemoteJobInfo)) {
			return new StyledString();
		}
		
		final RemoteJobInfo remoteJobInfo = (RemoteJobInfo) element;
		final JobInformation jobInfo = remoteJobInfo.getJobInfo();
		final JobStatus status = jobInfo.getStatus();
		final StringBuilder sb = new StringBuilder(100);
		
		switch (status) {
			case UNKNOWN:
				sb.append("Submitted: ").append(DATE_TIME_FORMATTER.format(jobInfo.getSubmitTime()));
				break;
			case RUNNING:
			case CANCEL_REQUESTED:
				sb.append("Started: ").append(DATE_TIME_FORMATTER.format(jobInfo.getStartTime()));
				break;
			case SCHEDULED:
				sb.append("Scheduled: ").append(DATE_TIME_FORMATTER.format(jobInfo.getScheduledStartTime()));
				break;
			default:
				sb.append("Finished: ").append(DATE_TIME_FORMATTER.format(jobInfo.getFinishTime()));
				break;
		}
		return getStyledString(sb.toString(), status);
	}

}
