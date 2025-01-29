/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.core;

import java.util.Map;

import brave.Span;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleParameters;

/**
 * Encapsulates context information about a Discovery job.
 */
public interface DiscoveryContext extends AutoCloseable {
	
	/**
	 * Returns the id of the project on which the Discovery is executed.
	 *
	 * @return the project id
	 */
	EntityId getProjectId();
	
	/**
	 * Returns the id of the job in which the Discovery is executed.
	 *
	 * @return the job id
	 */
	String getJobId();

	/**
	 * The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 *
	 * @return The incremented iteration number.
	 */
	int getIteration();
	
	/**
	 * Returns the configuration used by the current Discovery.
	 *
	 * @return the Discovery configuration
	 */
	Config getConfig();
	
	/**
	 * Returns the search orders used by the current Discovery.
	 *
	 * @return the Discovery search orders
	 */
	SearchOrders getSearchOrders();

	/**
	 * Returns additional common attributes that need to be set on Modules created or modified by the Discovery.
	 *
	 * @return the Module parameters
	 */
	ModuleParameters getModuleParameters();
	
	/**
	 * Returns a {@code SourceObjectResolver} instance that can be used by contributors to assemble source files for parsing.
	 *
	 * @return a SourceObjectResolver configured with the current search orders
	 */
	SourceObjectResolver getSourceObjectResolver();

	/**
	 * Returns the current span for tracing purposes.
	 *
	 * @return the current span
	 */
	Span getCurrentSpan();
	
	/**
	 * Gets a timed worked instance that is configured for the current Discovery run. The timed worker can be used when invoking potentially
	 * long-running tasks, such as parsing. It allows to set a timeout on the operation and also allows users to cancel the operation manually.
	 *
	 * @return a TimedWorker instance
	 */
	TimedWorker getTimedWorker();

	/**
	 * Returns a map with the current FF4j feature settings.
	 *
	 * @return the current span
	 */
	Map<FeatureId, Boolean> getFeatureMap();

	/* overridden because we don't throw Exception */
	@Override
	void close();
}
