/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

import java.io.Serializable;
import java.time.Instant;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import innowake.lib.job.api.management.ClusterInformation;

/**
 * Information of one cluster member. Each cluster member has a local scheduled execution that regularly submits such a record and updates the central shared
 * {@link ClusterInformation} status.
 */
public class MemberInfoRecord implements Serializable {

	/** The time this record has been updated. */
	private Instant lastUpdate;
	
	/** The unique Id of the cluster member. */
	public final String memberId;
	/** The maximum amount of jobs that can be handled by this member. */
	public final int maximumJobCount;
	/** The maximum amount of tasks that can be handled by this member. */
	public final int maximumTaskCount;
	
	private boolean isInShutdownState;
	
	/**
	 * Constructor.
	 * 
	 * @param memberId the unique Id of the cluster member
	 * @param maximumJobCount the maximum amount of jobs that can be handled by this member
	 * @param maximumTaskCount the maximum amount of tasks that can be handled by this member
	 */
	public MemberInfoRecord(final String memberId, final int maximumJobCount, final int maximumTaskCount) {
		this.lastUpdate = Instant.now();
		this.memberId = memberId;
		this.maximumJobCount = maximumJobCount;
		this.maximumTaskCount = maximumTaskCount;
		this.isInShutdownState = false;
	}
	
	/**
	 * Updates the timestamp of this record.
	 */
	public final void updateTimestamp() {
		this.lastUpdate = Instant.now();
	}
	
	/**
	 * @return the {@link Instant} when this record has last been updated
	 */
	public final Instant getTimestamp() {
		return lastUpdate;
	}
	
	/**
	 * @return {@code true} if the member is currently in a shutdown state no longer accepting any new jobs; {@code false} otherwise
	 */
	public final boolean isInShutdownState() {
		return isInShutdownState;
	}
	
	/**
	 * Sets the shutdown state of the member.
	 * 
	 * @param isInShutdownState {@code true} if the member should be set in a shutdown state no longer accepting any new jobs; {@code false} otherwise
	 */
	public final void setInShutdownState(final boolean isInShutdownState) {
		this.isInShutdownState = isInShutdownState;
	}
	
	@Override
	public final String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE)
				.append("lastUpdate", lastUpdate)
				.append("memberId", memberId)
				.append("maximumJobCount", maximumJobCount)
				.append("maximumTaskCount", maximumTaskCount)
				.append("isInShutdownState", isInShutdownState).toString();
	}
	
}
