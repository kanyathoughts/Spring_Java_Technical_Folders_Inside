/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.core;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Map;
import java.util.Objects;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorkerImpl;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleParameters;

/**
 * Implementation of DiscoveryContext.
 */
public class DiscoveryContextImpl implements DiscoveryContext {

	private final EntityId projectId;
	private final String jobId;
	private final int iteration;
	private final ProgressMonitor monitor;
	private final Config config;
	private final SearchOrders searchOrders;
	private final SourceObjectResolver sourceObjectResolver;
	private final ModuleParameters moduleParameters;
	private final Tracer tracer;
	private final Span span;
	private final Map<FeatureId, Boolean> featureMap;

	@Nullable
	private TimedWorkerImpl timedWorker;

	/**
	 * Creates a new discovery context.
	 * 
	 * @param projectId id of the project on which the discovery is executed
	 * @param jobId id of the job in which the discovery is executed
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param monitor progress monitor which allows interruption of long-running tasks
	 * @param config discovery configuration for this run
	 * @param searchOrders search order configuration for this run
	 * @param sourceObjectResolver source object resolver used for assembling files
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by the Discovery
	 * @param tracer tracer used when creating additional tasks or threads
	 * @param span span used when creating additional tasks or threads
	 * @param featureMap map with the current FF4j feature settings
	 */
	public DiscoveryContextImpl(final EntityId projectId, final String jobId, final int iteration, final ProgressMonitor monitor,
								final Config config, final SearchOrders searchOrders, final SourceObjectResolver sourceObjectResolver,
								final ModuleParameters moduleParameters, final Tracer tracer, final Span span, final Map<FeatureId, Boolean> featureMap) {
		this.projectId = projectId;
		this.jobId = jobId;
		this.iteration = iteration;
		this.monitor = monitor;
		this.config = config;
		this.searchOrders = searchOrders;
		this.sourceObjectResolver = sourceObjectResolver;
		this.moduleParameters = moduleParameters;
		this.tracer = tracer;
		this.span = span;
		this.featureMap = featureMap;
	}
	
	@Override
	public EntityId getProjectId() {
		return projectId;
	}

	@Override
	public String getJobId() {
		return jobId;
	}

	@Override
	public int getIteration() {
		return iteration;
	}

	@Override
	public Config getConfig() {
		return config;
	}

	@Override
	public SearchOrders getSearchOrders() {
		return searchOrders;
	}

	@Override
	public SourceObjectResolver getSourceObjectResolver() {
		return sourceObjectResolver;
	}

	@Override
	public ModuleParameters getModuleParameters() {
		return moduleParameters;
	}

	@Override
	public Span getCurrentSpan() {
		return span;
	}

	@Override
	public TimedWorker getTimedWorker() {
		if (timedWorker == null) {
			timedWorker = new TimedWorkerImpl(monitor, tracer, span);
		}
		return assertNotNull(timedWorker);
	}

	@Override
	public Map<FeatureId, Boolean> getFeatureMap() {
		return featureMap;
	}

	@Override
	public void close() {
		if (timedWorker != null) {
			timedWorker.shutdown();
		}
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof DiscoveryContextImpl)) return false;
		final DiscoveryContextImpl that = (DiscoveryContextImpl) o;
		return iteration == that.iteration && Objects.equals(projectId, that.projectId) && Objects.equals(jobId, that.jobId);
	}

	@Override
	public int hashCode() {
		return Objects.hash(projectId, jobId, iteration);
	}
}
