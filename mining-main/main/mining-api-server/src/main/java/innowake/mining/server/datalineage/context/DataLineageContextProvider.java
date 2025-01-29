/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.context;

import com.google.common.cache.Cache;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.ndt.fieldtracing.model.TracedModule;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

/**
 * Factory for {@link DataLineageContext}.
 */
@Service
public class DataLineageContextProvider {

	/**
	 * Creates a data lineage context with the given projectId and no progress monitor.
	 * @param projectId the id of the project on which data lineage is executed
	 * @return a data lineage context
	 */
	public DataLineageContext createContext (final EntityId projectId) {
		return new DataLineageContext(projectId);
	}

	/**
	 * Creates a data lineage context with the given projectId and progress monitor, allowing to cancel the operation.
	 * @param projectId the id of the project on which data lineage is executed
	 * @param progressMonitor a progress monitor allowing to cancel the operation
	 * @param fieldTracerCache a createContext cache where the field tracer can cache parsed programs
	 * @param incomingDependenciesCache a cache where incoming dependencies can be cached for a module
	 * @param outgoingDependenciesCache a cache where outgoing dependencies can be cached for a module
	 * @return a data lineage context
	 */
	public DataLineageContext createContext(final EntityId projectId, final ProgressMonitor progressMonitor,
			@Nullable final Cache<TracedModule<ModuleLightweightPojo>, Object> fieldTracerCache,
			final Cache<Long, List<TracedModule<ModuleLightweightPojo>>> incomingDependenciesCache,
			final Cache<Long, List<TracedModule<ModuleLightweightPojo>>> outgoingDependenciesCache) {
		final String jobId;
		if (progressMonitor instanceof JobMonitor) {
			jobId = ((JobMonitor) progressMonitor).getJobId();
		} else {
			/* data lineage running outside of job -> use random job id */
			jobId = UUID.randomUUID().toString();
		}
		return new DataLineageContext(projectId, jobId, progressMonitor, fieldTracerCache, incomingDependenciesCache, outgoingDependenciesCache);
	}
}
