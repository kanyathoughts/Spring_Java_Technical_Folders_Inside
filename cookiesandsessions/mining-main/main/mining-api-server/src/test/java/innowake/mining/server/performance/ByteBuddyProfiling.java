/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.performance;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.instrument.Instrumentation;
import java.nio.file.Files;
import java.nio.file.Path;

import innowake.lib.core.profile.DefaultProfiler;
import innowake.lib.core.profile.DefaultProfiler.Level;
import innowake.lib.core.profile.export.SummaryExportCsv;
import net.bytebuddy.agent.ByteBuddyAgent;
import net.bytebuddy.agent.builder.AgentBuilder;
import net.bytebuddy.description.type.TypeDescription;

/**
 * Handles the instrumentation with {@code startProfiling} and the printing of the results with {@code writeToCsv}.
 * All methods are only enabled if DefaultProfiler.isProfilingEnabled() returns true.
 */
public class ByteBuddyProfiling {

	/**
	 * Instruments the current jvm and starts the profiling of the packages defined in the log4j.xml,
	 * but only if DefaultProfiler.isProfilingEnabled() == true.
	 * log4j.xml example:
	 * 
	 * <pre>
	 * &#60;Logger name="prof.innowake.mining" level="debug"&#62;
	 *     &#60;AppenderRef ref="job"/&#62;
	 * &#60;/Logger&#62;
	 * </pre>
	 */
	static void startProfiling() {
		if ( ! DefaultProfiler.isProfilingEnabled()) {
			return;
		}

		final Instrumentation currentInstrumentation = ByteBuddyAgent.install();
		new AgentBuilder.Default()
			.type(ByteBuddyProfiling::isToBeProfiled)
			.transform(new ProfilingTransformer())
			.with(AgentBuilder.TypeStrategy.Default.REDEFINE)
			.installOn(currentInstrumentation);
	}

	/**
	 * Writes the profiling results to {@code outFile},
	 * but only if DefaultProfiler.isProfilingEnabled() == true.
	 *
	 * @param outFile the file to write to, will be created or overwritten.
	 */
	static void writeToCsv(final Path outFile) {
		if ( ! DefaultProfiler.isProfilingEnabled()) {
			return;
		}
		
		try (final OutputStream out = Files.newOutputStream(outFile)) {
			SummaryExportCsv.skipParentElements();
			ProfilingTransformer.profilingSession.getMetrics().exportCsv(out);
		} catch (IOException e) {
			throw new IllegalStateException("Could not write the profiling to CSV: ", e);
		}
	}

	private static boolean isToBeProfiled(final TypeDescription type) {
		return DefaultProfiler.getLevel(type.getPackage().getActualName()) != Level.OFF;
	}
}
