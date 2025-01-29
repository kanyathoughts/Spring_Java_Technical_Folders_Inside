/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.job.log;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.MathContext;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;

import org.apache.commons.io.FilenameUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ActiveProfiles;

import com.hazelcast.cluster.Member;
import com.hazelcast.core.HazelcastInstance;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.internal.JobConfiguration;
import innowake.lib.junit.Config;
import innowake.lib.junit.Level;
import innowake.mining.job.pi.CalculatePiJob;
import innowake.mining.job.pi.CalculatePiTask;

/**
 * Test for job {@link JobLogTestJob} testing that log paths are correctly set and that every log output contains
 * a trace and span Id.
 */
@ActiveProfiles("cluster_mode")
@Config(level = Level.INTEGRATION)
class JobLogClusterTest extends AbstractJobLogTest {
	
	@Autowired
	@Qualifier(JobConfiguration.NODE_NAME)
	private String nodeName;
	@Autowired
	private HazelcastInstance hz;
	
	@Override
	protected String getExpectedLogPath() {
		return "logs/" + nodeName;
	}
	
	/**
	 * Tests that in a clustered environment there is one separate job log file for the same job Id, each contained in a subdirectory
	 * of the main logs directory named like the hazelcast member node: {@code <logs-dir>/<node-name>/<file-prefix><job-id>.log}
	 * 
	 * @throws IOException if the log files cannot be read
	 */
	@Test
	public void testMultipleLogsPresent() throws IOException {
		final LoggingPiJob job = new LoggingPiJob();
		final String[] memberIds = new String[2];
		startJobInClusterAndAwaitCompletion("cluster_mode", job, t -> fail("Job execution failed: " + t.getMessage()), monitor -> {
			final Member[] members = hz.getCluster().getMembers().toArray(new Member[] {});
			assertEquals(2, members.length);
			memberIds[0] = members[0].getUuid().toString();
			memberIds[1] = members[1].getUuid().toString();
		}, null);
		
		final Optional<Path> logFile1 = Files.list(Paths.get("logs", memberIds[0]))
				.filter(file -> FilenameUtils.getBaseName(file.getFileName().toString()).endsWith(job.getJobId()))
				.findFirst();
		assertTrue(logFile1.isPresent());
		
		final Optional<Path> logFile2 = Files.list(Paths.get("logs", memberIds[1]))
				.filter(file -> FilenameUtils.getBaseName(file.getFileName().toString()).endsWith(job.getJobId()))
				.findFirst();
		assertTrue(logFile2.isPresent());
	}
	
	private static class LoggingPiJob extends CalculatePiJob {

		public LoggingPiJob() {
			super(500);
		}
		
		@Override
		protected CalculatePiTask createPiTask(final MathContext mc, final int fromIncluding, final int toExcluding,
				final ProgressMonitor subProgressMonitor, final String jobId) {
			return new LoggingPiTask(mc, fromIncluding, toExcluding, subProgressMonitor, jobId);
		}
	}
	
	private static class LoggingPiTask extends CalculatePiTask {
		
		private static final Logger LOG = LoggerFactory.getLogger("test.siftingFile");

		protected LoggingPiTask(final MathContext mc, final int fromIncluding, final int toExcluding, final ProgressMonitor progressMonitor, final String jobId) {
			super(mc, fromIncluding, toExcluding, progressMonitor, jobId);
		}
		
		@Override
		protected Result<BigDecimal> run(final ProgressMonitor progressMonitor) {
			final Result<BigDecimal> result = super.run(progressMonitor);
			LOG.info(() -> "Got intermediate result: " + result.value);
			return result;
		}
		
	}
}
