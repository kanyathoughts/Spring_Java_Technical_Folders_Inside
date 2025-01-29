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

@WithMockUser
class CSDParserExceptionLogTest extends BaseDiscoveryTest {
	/**
	 * Test method to validate the presence of the exception:
	 * innowake.mining.server.discovery.parser.csd while parsing Discover Code log.
	 */
	@Test
	void discoverCodeShouldNotThrowCSDParserException() {
		final Configuration config = LoggerContext.getContext(false).getConfiguration();
		final StringWriter stringWriter = new StringWriter();
		final Appender appender = WriterAppender.createAppender(PatternLayout.createDefaultLayout(config), null, stringWriter, "DiscoverCodeLogTestWriter",
				false, true);
		appender.start();
		final LoggerConfig loggerConfig = config.getLoggerConfig(Logging.CATEGORIZE_STATISTIC);
		config.addAppender(appender);
		loggerConfig.addAppender(appender, Level.INFO, null);
		try {
			projectId = assertNotNull(createProject()).identity();
			uploadResources(projectId);
			submitJob(jobManager, tracer, new DiscoverCodeJob(assertNotNull(projectId)));
			assertThat("Discovery log should not contain: Exception while parsing. Current context for file WMIN2059/MMRS71A", stringWriter.toString(), 
					allOf(
					/* Ensure appender received log messages */
					containsString("Overall selected objects: 1"),
					not(containsString("innowake.mining.server.discovery.parser.csd")),
					/* No CSD Parser Exception must have been thrown */
					not(containsString("Exception while parsing. Current context: ################DUMP################"))));
		} finally {
			config.getAppenders().remove(appender.getName());
			loggerConfig.removeAppender(appender.getName());
		}
	}

	@Override
	protected String getTestFolder() {
		return "WMIN2059";
	}

}
