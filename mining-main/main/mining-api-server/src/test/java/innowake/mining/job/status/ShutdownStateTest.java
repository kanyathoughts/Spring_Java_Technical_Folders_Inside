/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.status;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.lang.ProcessBuilder.Redirect;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ActiveProfiles;

import com.hazelcast.cluster.Member;
import com.hazelcast.core.HazelcastInstance;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.AbstractJobTest;
import innowake.mining.job.pi.CalculatePiJob;
import innowake.mining.server.MiningApiApplication;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.management.ClusterInformation;
import innowake.lib.job.internal.MemberInfoRecord;
import innowake.lib.job.internal.hazelcast.HzClusterInformation;
import innowake.lib.junit.Config;
import innowake.lib.junit.Level;

/**
 * Test that a member set to shutdown state no longer accepts any jobs.
 */
@ActiveProfiles("cluster_mode")
@Config(level = Level.INTEGRATION)
public class ShutdownStateTest extends AbstractJobTest {

	@Autowired
	private HazelcastInstance hz;
	@Autowired
	private MemberInfoRecord memberInfo;
	@Autowired
	private HzClusterInformation clusterInfo;

	@Test
	public void testMemberShutdown() {
		ClusterInfoRunnable clusterInfoRunnable = new ClusterInfoRunnable(jobManager.getClusterInformation());

		final List<String> command = new ArrayList<>();
		command.add(System.getProperty("java.home") + File.separator + "bin" + File.separator + "java");
		command.add("-cp");
		command.add(System.getProperty("java.class.path"));
		command.add(MiningApiApplication.class.getName());
		command.add("--spring.profiles.active=cluster_mode");

		try {
			final ProcessBuilder pb = new ProcessBuilder(command);
			final Process process = pb.redirectError(Redirect.INHERIT).start();

			try (final BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream(), Charset.defaultCharset()));
					final Writer writer = new OutputStreamWriter(process.getOutputStream(), Charset.defaultCharset())) {
				final CountDownLatch startLatch = new CountDownLatch(2);
				new Thread(new InputRunnable(reader, startLatch)).start();
				try {
					startLatch.await(2, TimeUnit.MINUTES);
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
					fail("Interrupted while waiting for second cluster node to start up");
				}

				new Thread(clusterInfoRunnable).start();
				final Member[] members = hz.getCluster().getMembers().toArray(new Member[] {});
				assertEquals(2, members.length);
				clusterInfoRunnable.memberId1 = members[0].getUuid().toString();
				clusterInfoRunnable.memberId2 = members[1].getUuid().toString();

				memberInfo.setInShutdownState(true);
				clusterInfo.update(memberInfo);

				final CountDownLatch latch = new CountDownLatch(10);
				for (int i = 0; i < 10; i++) {
					jobManager.submit(new CalculatePiJob(10), new JobExecutionCallback() {

						@Override
						public void onCompletion() {
							latch.countDown();
						}

						@Override
						public void onFailure(@Nullable final Throwable throwable) {
							latch.countDown();
						}
					});
				}

				try {
					latch.await(5, TimeUnit.MINUTES);
				} catch (final InterruptedException e) {
					throw new IllegalStateException(e);
				}

				process.destroy();
			} catch (final IOException e) {
				throw new IllegalStateException("Unable to start second cluster node.", e);
			} finally {
				clusterInfoRunnable.stop = true;
				if (process != null) {
					process.destroyForcibly();
				}
			}
		} catch (final IOException e) {
			throw new IllegalStateException("Unable to start second cluster node.", e);
		}

		/* check that only one node executed any of the jobs and the other none. */
		assertEquals(2, clusterInfoRunnable.maxNodes);
		assertTrue((clusterInfoRunnable.maxActiveMemberJobs1 >= 1 && clusterInfoRunnable.maxActiveMemberJobs2 == 0)
				|| (clusterInfoRunnable.maxActiveMemberJobs1 == 0 && clusterInfoRunnable.maxActiveMemberJobs2 >= 1));
	}

	/**
	 * This runnable is used to gather status information of the cluster during the job execution.
	 */
	private class ClusterInfoRunnable implements Runnable {

		@SuppressWarnings("hiding")
		private final ClusterInformation clusterInfo;
		@Nullable
		private String memberId1;
		@Nullable
		private String memberId2;

		private int maxNodes;
		private int maxActiveMemberJobs1;
		private int maxActiveMemberJobs2;

		private boolean stop;

		private ClusterInfoRunnable(final ClusterInformation clusterInfo) {
			this.clusterInfo = clusterInfo;
		}

		@Override
		public void run() {
			while ( ! stop) {
				maxNodes = Math.max(maxNodes, clusterInfo.getActiveMembers());
				maxActiveMemberJobs1 = Math.max(maxActiveMemberJobs1, clusterInfo.getActiveMemberJobCount(assertNotNull(memberId1)));
				maxActiveMemberJobs2 = Math.max(maxActiveMemberJobs2, clusterInfo.getActiveMemberJobCount(assertNotNull(memberId2)));

				try {
					TimeUnit.MILLISECONDS.sleep(500);
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
		}
	}
}
