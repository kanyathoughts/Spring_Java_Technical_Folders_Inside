/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.StringWriter;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.core.Appender;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.appender.WriterAppender;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.LoggerConfig;
import org.apache.logging.log4j.core.layout.PatternLayout;
import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.categorize.jcl.JclFileTypeDetection;
import innowake.mining.shared.access.EntityId;

/**
 * Tests that {@link JclFileTypeDetection} ignores PROCs and doesn't throw exceptions when identifying dependencies.
 */
@WithMockUser
class DiscoveryJobLogTest extends BaseDiscoveryTest {

/**
	 * Test to create project and run Discover Code and then validate there is no 
	 * JCLCoreParserException: Expected content to be of type Job, but got a procedure. 
	 * happening for the job file WCFD229
	 * 
	 * WMIN-1878 fixed this issue
	 */
	@Test
	void discoverCodeShouldNotThrowJCLCoreParserExceptionForWCFD229() {
		/* add custom appender to log4j2 */
		final Configuration config = LoggerContext.getContext(false).getConfiguration();
		final StringWriter stringWriter = new StringWriter();
		final Appender appender = WriterAppender.createAppender(PatternLayout.createDefaultLayout(config), null, stringWriter, "DiscoveryJobLogTestWriter", false, true);
		appender.start();
		final LoggerConfig loggerConfig = config.getLoggerConfig(Logging.CATEGORIZE_FILETYPE_DETECTION);
		config.addAppender(appender);
		loggerConfig.addAppender(appender, Level.INFO, null);

		try {
			final EntityId projectId = assertNotNull(createProject()).identity();
			uploadResources(projectId);
			submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));

			assertThat("Discovery log should not contain DiscoveryException/JCLCoreParserException for file src/jcl/WCFD229/procs/MGOPROC1.proc",
					stringWriter.toString(), allOf(
							/* Ensure appender received log messages */
							containsString("Overall selected objects: 12"),
							/* No DiscoveryException must have been thrown */
							not(containsString("innowake.mining.shared.discovery.DiscoveryException")),
							/* No JCLCoreParserException must have been thrown */
							not(containsString("innowake.ndt.jcl.parser.env.JCLCoreParserException: Expected content to be of type Job, but got a procedure."))));
		} finally {
			config.getAppenders().remove(appender.getName());
			loggerConfig.removeAppender(appender.getName());
		}
	}

	@Override
	protected String getTestFolder() {
		return "iris/WCFD229";
	}

}
