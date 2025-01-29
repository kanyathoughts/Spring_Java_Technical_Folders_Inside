/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

import java.io.Serializable;
import java.time.Instant;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

/**
 * Record containing the cluster information. In a cluster one shared instance of this class is updated by its members.
 */
public class ClusterInfoRecord implements Serializable {
	
	private static final long serialVersionUID = 1L;
	private static final Logger LOG = LoggerFactory.getLogger(Logging.CLUSTER_INFO);
	
	public static final int MEMBER_STATUS_REPORT_INTERVAL_IN_SECONDS = 90;
	public static final int MAXIMUM_MEMBER_STATUS_AGE_IN_SECONDS = 5 * 60;

	/* contains one entry for each cluster member */
	private final Map<String, MemberInfoRecord> members = new HashMap<>();
	
	/* have to keep this in separate maps and not part of MemberInfoRecord. Otherwise the information would not be synchronized between the different nodes. */
	private final Map<String, Integer> activeMemberJobCount = new HashMap<>();
	private final Map<String, Integer> activeMemberTaskCount = new HashMap<>();

	private int maximumJobCount;
	private int maximumTaskCount;
	private int activeJobCount;
	private int activeTaskCount;
	
	/**
	 * @return the current active number of cluster members
	 */
	public int getActiveMembers() {
		return members.size();
	}

	/**
	 * @return the number of jobs the cluster can handle in parallel
	 */
	public int getMaximumJobCount() {
		return maximumJobCount;
	}

	/**
	 * @return the number of tasks the cluster can handle in parallel
	 */
	public final int getMaximumTaskCount() {
		return maximumTaskCount;
	}

	/**
	 * Sets the current amount of active jobs in the cluster and for a specific member.
	 *
	 * @param memberId the Id of the cluster member
	 * @param delta the delta to apply
	 */
	public final void modifyActiveJobCount(final String memberId, final int delta) {
		activeJobCount += delta;
		if (activeJobCount < 0) {
			activeJobCount = 0;
		}
		
		int memberJobCount = getActiveMemberJobCount(memberId);
		memberJobCount += delta;
		if (memberJobCount < 0) {
			memberJobCount = 0;
		}
		activeMemberJobCount.put(memberId, Integer.valueOf(memberJobCount));
	}
	
	/**
	 * @return the current amount of active jobs in the cluster
	 */
	public final int getActiveJobCount() {
		return activeJobCount;
	}

	/**
	 * Sets the current amount of active tasks in the cluster and for a specific member.
	 *
	 * @param memberId the Id of the cluster member
	 * @param delta the delta to apply
	 */
	public final void modifyActiveTaskCount(final String memberId, final int delta) {
		activeTaskCount += delta;
		if (activeTaskCount < 0) {
			activeTaskCount = 0;
		}
		
		int memberTaskCount = getActiveMemberJobCount(memberId);
		memberTaskCount += delta;
		if (memberTaskCount < 0) {
			memberTaskCount = 0;
		}
		activeMemberTaskCount.put(memberId, Integer.valueOf(memberTaskCount));
	}
	
	/**
	 * @return the current amount of active tasks in the cluster
	 */
	public final int getActiveTaskCount() {
		return activeTaskCount;
	}
	
	/**
	 * @param memberId the Id of the cluster member
	 * @return the current amount of active jobs on the member or 0 if the member doesn't exist
	 */
	public final int getActiveMemberJobCount(final String memberId) {
		final Integer jobCount = activeMemberJobCount.get(memberId);
		if (jobCount != null) {
			return jobCount.intValue();
		}
		return 0;
	}
	
	/**
	 * @param memberId the Id of the cluster member
	 * @return the current amount of active tasks on the member or 0 if the member doesn't exist
	 */
	public final int getActiveMemberTaskCount(final String memberId) {
		final Integer taskCount = activeMemberTaskCount.get(memberId);
		if (taskCount != null) {
			return taskCount.intValue();
		}
		return 0;
	}
	
	/**
	 * @param memberId the Id of the cluster node
	 * @return {@code true} if the member has initiated a shutdown (i.e. due to an autoscaler terminating the machine); {@code false} otherwise
	 */
	public final boolean isMemberInShutdownState(final String memberId) {
		final MemberInfoRecord memberInfo = members.get(memberId);
		if (memberInfo != null) {
			return memberInfo.isInShutdownState();
		}
		return false;
	}
	
	/**
	 * Removes the member with the provided Id.
	 * 
	 * @param memberId the Id of the member to remove
	 */
	public final void removeMember(final String memberId) {
		if (members.remove(memberId) != null) {
			updateInternal();
		}
	}

	/**
	 * Updates this record with the provided information of the {@link MemberInfoRecord}.
	 *
	 * @param memberInformation the {@link MemberInfoRecord} of a cluster node
	 */
	public final void update(final MemberInfoRecord memberInformation) {
		/* register record for this node */
		members.put(memberInformation.memberId, memberInformation);
		
		updateInternal();
		
		LOG.trace(() -> "Got newest node information: " + memberInformation);
		if (LOG.isTraceEnabled()) {
			final StringBuilder sb = new StringBuilder(1000);
			sb.append("All node information:\n");
			members.values().forEach(m -> sb.append(m).append("\n"));
			LOG.trace(sb.toString());
		}
	}
	
	private void updateInternal() {
		/* entries older than this are removed */
		final Instant oldestAccepted = Instant.now().minusSeconds(MAXIMUM_MEMBER_STATUS_AGE_IN_SECONDS);
		
		/* reset counts, will be re-populated from the current records */
		setMaximumJobCount(0);
		setMaximumTaskCount(0);
		
		/* iterate over the records, remove outdated and sum up current */
		final Iterator<MemberInfoRecord> iterator = members.values().iterator();
		while (iterator.hasNext()) {
			final MemberInfoRecord memberInformation2 = iterator.next();
			if (memberInformation2.getTimestamp().isBefore(oldestAccepted)) {
				LOG.warn(() -> "No capacity message received since " + oldestAccepted + ". Removing information of member " + memberInformation2.memberId);
				iterator.remove();
			} else {
				setMaximumJobCount(getMaximumJobCount() + memberInformation2.maximumJobCount);
				setMaximumTaskCount(getMaximumTaskCount() + memberInformation2.maximumTaskCount);
			}
		}
	}
	
	private void setMaximumJobCount(final int maximumJobCount) {
		this.maximumJobCount = maximumJobCount;
	}
	
	private void setMaximumTaskCount(final int maximumTaskCount) {
		this.maximumTaskCount = maximumTaskCount;
	}

	@Override
	public final String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE)
				.append("nodes", members.size())
				.append("maximumJobCount", getMaximumJobCount())
				.append("maximumTaskCount", getMaximumTaskCount())
				.append("activeJobCount", getActiveJobCount())
				.append("activeTaskCount", getActiveTaskCount())
				.toString();
	}

}
