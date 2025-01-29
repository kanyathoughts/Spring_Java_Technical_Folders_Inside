/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.pi;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ActiveProfiles;

import com.hazelcast.cluster.Member;
import com.hazelcast.core.HazelcastInstance;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.AbstractJobTest;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.management.ClusterInformation;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.junit.Config;
import innowake.lib.junit.Level;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Test that calculates pi on a cluster with two nodes.
 */
@ActiveProfiles("cluster_mode")
@Config(level = Level.INTEGRATION)
public class CalculatePiClusterTest extends AbstractJobTest {

	@Autowired
	private HazelcastInstance hz;
	@Autowired
	private JobConfigurationProperties jobProperties;

	@Test
	public void testPiJob() {
		final CalculatePiJob job = new CalculatePiJob(1000);
		ClusterInfoRunnable clusterInfoRunnable = new ClusterInfoRunnable(jobManager.getClusterInformation());
		startJobInClusterAndAwaitCompletion("cluster_mode", job, t -> fail("Job execution failed: " + t.getMessage()), monitor -> {
			final Member[] members = hz.getCluster().getMembers().toArray(new Member[] {});
			assertEquals(2, members.length);
			clusterInfoRunnable.memberId1 = members[0].getUuid().toString();
			clusterInfoRunnable.memberId2 = members[1].getUuid().toString();
			new Thread(clusterInfoRunnable).start();
		}, monitor -> clusterInfoRunnable.stop = true);

		/* check job result */
		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertEquals(JobStatus.SUCCESS, jobInfo.getStatus());
		final Result<Serializable> result = jobInfo.getResult();
		final BigDecimal resultValue = (BigDecimal) assertNotNull(result).value;
		assertNotNull(resultValue);
		assertEquals(CalculatePiLocalTest.PI, resultValue);

		/* check cluster information gathered during execution */
		assertEquals(2, clusterInfoRunnable.maxNodes);
		assertEquals(jobProperties.getMaximumLocalJobThreads() * 2, clusterInfoRunnable.maxClusterJobs);
		assertEquals(jobProperties.getMaximumLocalTaskThreads() * 2, clusterInfoRunnable.maxClusterTasks);
		assertEquals(1, clusterInfoRunnable.maxActiveClusterJobs);
		assertTrue(clusterInfoRunnable.maxActiveClusterTasks > 0);
		assertTrue((clusterInfoRunnable.maxActiveMemberJobs1 == 1 && clusterInfoRunnable.maxActiveMemberJobs2 == 0) ||
				(clusterInfoRunnable.maxActiveMemberJobs1 == 0 && clusterInfoRunnable.maxActiveMemberJobs2 == 1));
		assertTrue(clusterInfoRunnable.maxActiveMemberTasks1 > 0);
		assertTrue(clusterInfoRunnable.maxActiveMemberTasks2 > 0);
	}

	/**
	 * This runnable is used to gather status information of the cluster during the job execution to ensure
	 * that the tasks are actually executed on multiple nodes.
	 */
	private class ClusterInfoRunnable implements Runnable {

		private final ClusterInformation clusterInfo;
		@Nullable
		private String memberId1;
		@Nullable
		private String memberId2;

		private int maxNodes;
		private int maxClusterJobs;
		private int maxClusterTasks;
		private int maxActiveClusterJobs;
		private int maxActiveClusterTasks;
		private int maxActiveMemberJobs1;
		private int maxActiveMemberTasks1;
		private int maxActiveMemberJobs2;
		private int maxActiveMemberTasks2;

		private boolean stop;

		private ClusterInfoRunnable(final ClusterInformation clusterInfo) {
			this.clusterInfo = clusterInfo;
		}

		@Override
		public void run() {
			while ( ! stop) {
				maxNodes = Math.max(maxNodes, clusterInfo.getActiveMembers());
				maxClusterJobs = Math.max(maxClusterJobs, clusterInfo.getMaximumJobCount());
				maxClusterTasks = Math.max(maxClusterTasks, clusterInfo.getMaximumTaskCount());
				maxActiveClusterJobs = Math.max(maxActiveClusterJobs, clusterInfo.getActiveJobCount());
				maxActiveClusterTasks = Math.max(maxActiveClusterTasks, clusterInfo.getActiveTaskCount());
				maxActiveMemberJobs1 = Math.max(maxActiveMemberJobs1, clusterInfo.getActiveMemberJobCount(assertNotNull(memberId1)));
				maxActiveMemberTasks1 = Math.max(maxActiveMemberTasks1, clusterInfo.getActiveMemberTaskCount(assertNotNull(memberId1)));
				maxActiveMemberJobs2 = Math.max(maxActiveMemberJobs2, clusterInfo.getActiveMemberJobCount(assertNotNull(memberId2)));
				maxActiveMemberTasks2 = Math.max(maxActiveMemberTasks2, clusterInfo.getActiveMemberTaskCount(assertNotNull(memberId2)));

				try {
					TimeUnit.MILLISECONDS.sleep(500);
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
		}
	}
}
