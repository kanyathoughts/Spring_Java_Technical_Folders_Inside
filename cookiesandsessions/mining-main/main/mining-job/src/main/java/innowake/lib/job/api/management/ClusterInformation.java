/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.management;

/**
 * Interface to get and modify information of the cluster.
 */
public interface ClusterInformation {
	
	/**
	 * @return the amount of active members in the cluster
	 */
	int getActiveMembers();

	/**
	 * @return the number of jobs the cluster can handle in parallel
	 */
	int getMaximumJobCount();

	/**
	 * @return the number of tasks the cluster can handle in parallel
	 */
	int getMaximumTaskCount();
	
	/**
	 * @return the number of currently running jobs the cluster
	 */
	int getActiveJobCount();

	/**
	 * @return the number of currently running tasks the cluster
	 */
	int getActiveTaskCount();
	
	/**
	 * @param memberId the Id of the cluster node
	 * @return the number of currently running jobs on the node
	 */
	int getActiveMemberJobCount(final String memberId);
	
	/**
	 * @param memberId the Id of the cluster node
	 * @return the number of currently running tasks on the node
	 */
	int getActiveMemberTaskCount(final String memberId);
	
	/**
	 * Modifies the number of currently running jobs the cluster.
	 *
	 * @param delta the delta to add; can be negative to decrease
	 * @return the number of currently running jobs the cluster
	 */
	int modifyActiveJobCount(int delta);

	/**
	 * Modifies the number of currently running tasks the cluster.
	 *
	 * @param delta the delta to add; can be negative to decrease
	 * @return the number of currently running tasks the cluster
	 */
	int modifyActiveTaskCount(int delta);
	
	/**
	 * @param memberId the Id of the cluster node
	 * @return {@code true} if the member has initiated a shutdown (i.e. due to an autoscaler terminating the machine); {@code false} otherwise
	 */
	boolean isMemberInShutdownState(final String memberId);
	
}
