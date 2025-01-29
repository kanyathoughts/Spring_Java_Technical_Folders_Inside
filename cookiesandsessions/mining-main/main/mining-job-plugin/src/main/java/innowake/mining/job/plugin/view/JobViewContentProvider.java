/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view;

import java.time.Instant;
import java.util.Arrays;
import java.util.Comparator;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;

/**
 * Content provider for the {@link JobView}.
 */
public class JobViewContentProvider implements ITreeContentProvider {
	
	private static final Comparator<RemoteJobInfo> JOB_INFO_COMPARATOR = new JobInfoComparator();

	@Override
	@Nullable
	public Object[] getElements(@Nullable final Object inputElement) {
		if ( ! (inputElement instanceof RemoteJobInfo[])) {
			return new Object[] {};
		}
		
		Arrays.sort((RemoteJobInfo[]) inputElement, JOB_INFO_COMPARATOR);
		return (Object[]) inputElement;
	}

	@Override
	@Nullable
	public Object[] getChildren(@Nullable final Object parentElement) {
		if ( ! (parentElement instanceof RemoteJobInfo)) {
			return new Object[] {};
		}
		
		/* We have to convert every message to be able to provide parent node information. */
		return ((RemoteJobInfo) parentElement).getJobInfo().getMessages().stream()
			.map(message -> new ParentAwareMessage(message, (RemoteJobInfo) parentElement))
			.toArray();
	}

	@Override
	@Nullable
	public Object getParent(@Nullable final Object element) {
		if (element instanceof ParentAwareMessage) {
			return ((ParentAwareMessage) element).getParent();
		}
		return null;
	}

	@Override
	public boolean hasChildren(@Nullable final Object element) {
		if (element instanceof RemoteJobInfo) {
			return ! ((RemoteJobInfo) element).getJobInfo().getMessages().isEmpty();
		}
		return false;
	}
	
	/**
	 * Special implementation of {@link Message} that enables to provide parent
	 * node information for the {@link TreeViewer} and also to be able to query
	 * the state of parent nodes.
	 */
	public static class ParentAwareMessage extends Message {
		
		private final RemoteJobInfo parentJobInfo;

		/**
		 * Constructor.
		 * 
		 * @param originalMessage the original {@link Message} instance
		 * @param parentJobInfo the parent {@link JobInformation} node
		 */
		public ParentAwareMessage(final Message originalMessage, final RemoteJobInfo parentJobInfo) {
			super(originalMessage.getSeverity(), originalMessage.getText());
			this.parentJobInfo = parentJobInfo;
		}
		
		/**
		 * @return the parent {@link JobInformation} node
		 */
		public RemoteJobInfo getParent() {
			return parentJobInfo;
		}
		
		@Override
		public int hashCode() {
			return super.hashCode() + parentJobInfo.hashCode();
		}
		
		@Override
		public boolean equals(@Nullable final Object obj) {
			if (obj == null) {
				return false;
			}
			if (obj == this) {
				return true;
			}
			if (obj.getClass() != getClass()) {
				return false;
			}
			final ParentAwareMessage other = (ParentAwareMessage) obj;
			return super.equals(other) && parentJobInfo.equals(other.parentJobInfo);
		}
		
	}
	
	/** 
	 * This sorts all {@link JobInformation} instances by their state as
	 * scheduled > running (and canceling) > finished (no matter if successful or not).
	 * Elements with the same state are ordered according to {@link Instant#compareTo(Instant)}.
	 */
	private static class JobInfoComparator implements Comparator<RemoteJobInfo> {

		@Override
		public int compare(@Nullable final RemoteJobInfo remoteJobInfo1, @Nullable final RemoteJobInfo remoteJobInfo2) {
			if (remoteJobInfo1 == null && remoteJobInfo2 == null) {
				return 0;
			} else if (remoteJobInfo1 == null) {
				return -1;
			} else if (remoteJobInfo2 == null) {
				return 1;
			} else if (remoteJobInfo1.equals(remoteJobInfo2)) {
				return 0;
			} else {
				final JobInformation jobInfo1 = remoteJobInfo1.getJobInfo();
				final JobInformation jobInfo2 = remoteJobInfo2.getJobInfo();
				final JobStatus state1 = jobInfo1.getStatus();
				final JobStatus state2 = jobInfo2.getStatus();
				if (state1 == JobStatus.SCHEDULED) {
					if (state1 == state2) {
						return compareTime(jobInfo1.getScheduledStartTime(), jobInfo2.getScheduledStartTime());
					} else {
						return -1;
					}
				} else if (state1 == JobStatus.RUNNING || state1 == JobStatus.CANCEL_REQUESTED) {
					if (state2 == JobStatus.RUNNING || state2 == JobStatus.CANCEL_REQUESTED) {
						return compareTime(jobInfo1.getStartTime(), jobInfo2.getStartTime());
					}
					return state2 == JobStatus.SCHEDULED ? 1 : -1;
				} else {
					final Instant finishTime1 = jobInfo1.getFinishTime();
					final Instant finishTime2 = jobInfo2.getFinishTime();
					return finishTime1 != null && finishTime2 != null ? compareTime(finishTime1, finishTime2) : 1;
				}
			}
		}
		
		private int compareTime(@Nullable final Instant time1, @Nullable final Instant time2) {
			if (time1 == null && time2 == null) {
				return 0;
			} else if (time1 == null) {
				return -1;
			} else if ( time2 == null) {
				return 1;
			} else {
				return time1.compareTo(time2);
			}
		}
	}

}
