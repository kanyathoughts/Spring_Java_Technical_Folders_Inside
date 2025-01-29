/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import java.time.Duration;
import java.time.Instant;

import org.eclipse.jface.viewers.StyledString;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.plugin.view.JobView;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Label provider for the "Time" column of the {@link JobView} showing either the remaining, the elapsed or the consumed time of a job.
 */
public class TimeColumnLabelProvider extends AbstractColumnLabelProvider {
	
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
			case RUNNING:
			case CANCEL_REQUESTED:
				final Instant startTime = jobInfo.getStartTime();
				if (startTime != null) {
					sb.append("Running since ").append(getElapsedTime(startTime));
				}
				break;
			case CANCELED:
			case FAILURE:
			case SUCCESS:
			case TIMEOUT:
				final Instant jobStartTime = jobInfo.getStartTime();
				final Instant finishTime = jobInfo.getFinishTime();
				if (jobStartTime != null && finishTime != null) {
					sb.append(getExecutionTime(jobStartTime, finishTime));
				}
				break;
			default:
				break;
		}
		return getStyledString(sb.toString(), status);
		
	}
	
	/**
	 * Calculates the elapsed time between {@code startTime} and the current system time
	 * and returns it in the format "hh:mm:ss".
	 * 
	 * @param startTime the {@link Instant} the job execution has started
	 * @return the elapsed time
	 */
	public static String getElapsedTime(final Instant startTime) {
		final Duration duration = Duration.between(startTime, Instant.now());
		return formatTime(duration);
	}
	
	/**
	 * Calculates the remaining execution time based on the difference of the current system
	 * time and the provided {@code eta} and returns it in the format "hh:mm:ss".
	 * 
	 * @param eta the {@link Instant} with the ETA
	 * @return the remaining time.
	 */
	public static String getRemainingTime(final Instant eta) {
		final Duration duration = Duration.between(Instant.now(), eta);
		return formatTime(duration);
	}
	
	private static String getExecutionTime(final Instant startTime, final Instant finishTime) {
		final Duration duration = Duration.between(startTime, finishTime);
		return formatTime(duration);
	}
	
	private static String formatTime(final Duration duration) {
		return String.format("%02d:%02d:%02d", Long.valueOf(duration.toHours() % 24), Long.valueOf(duration.toMinutes() % 60),
				Long.valueOf(duration.getSeconds() % 60));
	}

}
