/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal.hazelcast;

import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.core.IFunction;
import com.hazelcast.cp.IAtomicReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.management.ClusterInformation;
import innowake.lib.job.internal.ClusterInfoRecord;
import innowake.lib.job.internal.MemberInfoRecord;

/**
 * Contains information about the cluster like capacity and utilization. In each cluster there is one shared instance of this class. Each hazelcast member
 * instance updates its current status in regular intervals. If one member doesn't report it's status within a certain period it is removed.
 */
public class HzClusterInformation implements ClusterInformation {
	
	private static final String CLUSTER_INFORMATION_REFERENCE_ID = "cluster-information-reference";
	
	private final String localMemberId;
	private final IAtomicReference<ClusterInfoRecord> clusterInformationReference;
	
	/**
	 * Constructor.
	 * 
	 * @param hz the {@link HazelcastInstance}
	 */
	public HzClusterInformation(final HazelcastInstance hz) {
		this.localMemberId = hz.getCluster().getLocalMember().getUuid().toString();
		this.clusterInformationReference = hz.getCPSubsystem().getAtomicReference(CLUSTER_INFORMATION_REFERENCE_ID);
	}
	
	@Override
	public final int getActiveMembers() {
		final ClusterInfoRecord clusterInformation = clusterInformationReference.get();
		return clusterInformation != null ? clusterInformation.getActiveMembers() : 0;
	}
	
	@Override
	public final int getMaximumJobCount() {
		final ClusterInfoRecord clusterInformation = clusterInformationReference.get();
		return clusterInformation != null ? clusterInformation.getMaximumJobCount() : 0;
	}
	
	@Override
	public final int getMaximumTaskCount() {
		final ClusterInfoRecord clusterInformation = clusterInformationReference.get();
		return clusterInformation != null ? clusterInformation.getMaximumTaskCount() : 0;
	}

	@Override
	public final int getActiveJobCount() {
		final ClusterInfoRecord clusterInformation = clusterInformationReference.get();
		return clusterInformation != null ? clusterInformation.getActiveJobCount() : 0;
	}
	
	@Override
	public final int getActiveTaskCount() {
		final ClusterInfoRecord clusterInformation = clusterInformationReference.get();
		return clusterInformation != null ? clusterInformation.getActiveTaskCount() : 0;
	}
	
	@Override
	public final int getActiveMemberJobCount(final String memberId) {
		final ClusterInfoRecord clusterInformation = clusterInformationReference.get();
		return clusterInformation != null ? clusterInformation.getActiveMemberJobCount(memberId) : 0;
	}
	
	@Override
	public final int getActiveMemberTaskCount(String memberId) {
		final ClusterInfoRecord clusterInformation = clusterInformationReference.get();
		return clusterInformation != null ? clusterInformation.getActiveMemberTaskCount(memberId) : 0;
	}
	
	@Override
	public final int modifyActiveJobCount(final int delta) {
		final String memberId = localMemberId; /* Required for hazelcast lambda serialization */
		return modifyAndGet(clusterInformation -> clusterInformation.modifyActiveJobCount(memberId, delta)).getActiveJobCount();
	}
	
	@Override
	public final int modifyActiveTaskCount(final int delta) {
		final String memberId = localMemberId; /* Required for hazelcast lambda serialization */
		return modifyAndGet(clusterInformation -> clusterInformation.modifyActiveTaskCount(memberId, delta)).getActiveTaskCount();
	}
	
	@Override
	public final boolean isMemberInShutdownState(final String memberId) {
		final ClusterInfoRecord clusterInformation = clusterInformationReference.get();
		return clusterInformation != null ? clusterInformation.isMemberInShutdownState(memberId) : false;
	}
	
	@Override
	public final String toString() {
		final ClusterInfoRecord clusterInformation = clusterInformationReference.get();
		return clusterInformation != null ? clusterInformation.toString() : "<HzClusterInformation N/A>";
	}
	
	/**
	 * Removes the member with the provided Id from the cluster information.
	 * 
	 * @param memberId the Id of the member to remove
	 */
	public final void removeMember(final String memberId) {
		modify(clusterInformation -> clusterInformation.removeMember(memberId));
	}
	
	/**
	 * Updates the cluster information with the provided {@link MemberInfoRecord}.
	 *
	 * @param memberInformation the {@link MemberInfoRecord}
	 */
	public final void update(final MemberInfoRecord memberInformation) {
		modify(clusterInformation -> clusterInformation.update(memberInformation));
	}
	
	private void modify(final Modifier modifier) {
		clusterInformationReference.alter(modifier);
	}

	private ClusterInfoRecord modifyAndGet(final Modifier modifier) {
		return clusterInformationReference.alterAndGet(modifier);
	}
	
	private static interface Modifier extends IFunction<ClusterInfoRecord, ClusterInfoRecord> {
		
		@Override
		default ClusterInfoRecord apply(@Nullable final ClusterInfoRecord input) {
			if (input != null) {
				modify(input);
				return input;
			} else {
				final ClusterInfoRecord clusterInformation = new ClusterInfoRecord();
				modify(clusterInformation);
				return clusterInformation;
			}
		}
		
		void modify(final ClusterInfoRecord clusterInformation);
		
	}

}
