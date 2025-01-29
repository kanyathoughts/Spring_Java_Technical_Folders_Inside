/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.core;

import brave.Span;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleParameters;

/**
 * Provides Discovery Context.
 */
public interface DiscoveryContextProvider {

	/**
	 * Creates a discovery context object using the provided parameters.
	 *
	 * @param projectId the id of the project on which the Discovery is executed
	 * @param jobId the id of the job in which the Discovery is executed
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by the Discovery
	 * @param progressMonitor a progress monitor instance which allows users to track progress and cancel the operation
	 * @param span the current span for tracing purposes
	 * @return a discovery context object
	 */
	DiscoveryContext createContext(EntityId projectId, String jobId, final int iteration, ModuleParameters moduleParameters,
			ProgressMonitor progressMonitor, Span span);
}
