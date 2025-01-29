/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared;

import java.util.function.Supplier;

import innowake.lib.core.api.profiling.Profiler;

/**
 * Helper class for executing a given method with performance profiling enabled.
 */
public class ProfilingHelper {

	private ProfilingHelper() {
		/* static utility class */
	}
	
	/**
	 * Executes the given task and records the time on the given Profiler. The profiler will be invoked with the given sub-categories.
	 * 
	 * @param task the task to execute
	 * @param profiler the profile to use for measuring execution time
	 * @param categories the profiling sub-categories that are passed to the profiler
	 */
	public static void executeWithProfiling(final Runnable task, final Profiler profiler, final String... categories) {
		profiler.start(categories);
		try {
			task.run();
		} finally {
			profiler.stop();
		}
	}
	
	/**
	 * Executes the given task and records the time on the given Profiler. The profiler will be invoked with the given sub-categories.
	 * 
	 * @param <T> the type of the result of the task
	 * @param task the task to execute
	 * @param profiler the profile to use for measuring execution time
	 * @param categories the profiling sub-categories that are passed to the profiler
	 * @return the result of the task
	 */
	public static <T> T executeWithProfiling(final Supplier<T> task, final Profiler profiler, final String... categories) {
		profiler.start(categories);
		try {
			return task.get();
		} finally {
			profiler.stop();
		}
	}
}
