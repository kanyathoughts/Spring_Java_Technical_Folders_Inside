package innowake.mining.server.datalineage.context;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorkerImpl;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.ndt.fieldtracing.CachingFunction;
import innowake.ndt.fieldtracing.model.TracedModule;

import java.util.UUID;
import java.util.concurrent.ExecutionException;

import static innowake.lib.core.lang.Assert.assertNotNull;

/**
 * Class holding contextual information about a data lineage operation.
 */
public class DataLineageContext implements AutoCloseable {

	private final EntityId projectId;
	private final String jobId;
	private final ProgressMonitor progressMonitor;
	private final Set<UUID> parseErrorModules = new HashSet<>();
	
	@Nullable
	private final Cache<TracedModule<ModuleLightweightPojo>, Object> fieldTracerCache;

	@Nullable
	private Cache<Long, List<TracedModule<ModuleLightweightPojo>>> incomingDependenciesCache;
	@Nullable
	private Cache<Long, List<TracedModule<ModuleLightweightPojo>>> outgoingDependenciesCache;

	private static final int MAX_CACHE_SIZE = 100;

	@Nullable
	private TimedWorkerImpl timedWorker;

	/**
	 * Constructor for DataLineageContext if no ProgressMonitor is present
	 *
	 * @param projectId The id of the project
	 */
	public DataLineageContext(final EntityId projectId) {
		this.projectId = projectId;
		this.jobId = UUID.randomUUID().toString();
		progressMonitor = new NullProgressMonitor();
		fieldTracerCache = null;
		incomingDependenciesCache = null;
		outgoingDependenciesCache = null;
	}

	/**
	 * Constructor for DataLineageContext
	 *
	 * @param projectId
	 * 		The id of the project
	 * @param jobId
	 * 		The id of the job in which data lineage is executed
	 * @param progressMonitor
	 * 		The ProgressMonitor monitoring the operations
	 * @param fieldTracerCache
	 * 		a cache where the field tracer can cache parsed programs
	 * @param incomingDependenciesCache a cache where incoming dependencies can be cached for a given module
	 * @param outgoingDependenciesCache a cache where outgoing dependencies can be cached for a given module
	 */
	public DataLineageContext(final EntityId projectId, final String jobId, final ProgressMonitor progressMonitor,
			@Nullable final Cache<TracedModule<ModuleLightweightPojo>, Object> fieldTracerCache,
			final Cache<Long, List<TracedModule<ModuleLightweightPojo>>> incomingDependenciesCache,
			final Cache<Long, List<TracedModule<ModuleLightweightPojo>>> outgoingDependenciesCache) {
		this.projectId = projectId;
		this.jobId = jobId;
		this.progressMonitor = progressMonitor;
		this.fieldTracerCache = fieldTracerCache;
		this.incomingDependenciesCache = incomingDependenciesCache;
		this.outgoingDependenciesCache = outgoingDependenciesCache;
	}

	/**
	 * Returns the id of the project on which data lineage is executed.
	 *
	 * @return the project id
	 */
	public EntityId getProjectId() {
		return projectId;
	}

	/**
	 * Returns the id of the job in which data lineage is executed.
	 *
	 * @return the job id
	 */
	public String getJobId() {
		return jobId;
	}

	/**
	 * Returns the progress monitor for the progress of the data lineage operation.
	 *
	 * @return the progress monitor
	 */
	public ProgressMonitor getProgressMonitor() {
		return progressMonitor;
	}

	/**
	 * Gets a timed worked instance that is configured for the current Data Lineage run. The timed worker can be used when invoking potentially
	 * long-running tasks, such as parsing. It allows to set a timeout on the operation and also allows users to cancel the operation manually.
	 *
	 * @return a timed worker instance
	 */
	public TimedWorker getTimedWorker() {
		if (timedWorker == null) {
			/* not handling tracer and span here for now */
			timedWorker = new TimedWorkerImpl(progressMonitor, null, null);
		}
		return timedWorker;
	}

	/**
	 * Returns a caching function that can be passed to the field tracer in order to cache parsed programs.
	 * @return a caching function, or {@code null} if no cache is available
	 * @param <T> the type of the objects that are held by the cache
	 */
	@Nullable
	public <T> CachingFunction<TracedModule<ModuleLightweightPojo>, T> getCachingFunctionForFieldTracer() {
		if (fieldTracerCache == null) {
			return null;
		}

		return (key, loader) -> {
			try {
				@SuppressWarnings("unchecked")
				final T ret = (T) assertNotNull(fieldTracerCache).get(key, loader::get);
				return ret;
			} catch (final ExecutionException e) {
				throw new IllegalStateException("Failed to retrieve traced module from cache: " + key, e);
			}
		};
	}

	@Override
	public void close() {
		if (timedWorker != null) {
			timedWorker.shutdown();
		}
	}
	
	/**
	 * Returns all the module ids which were not successfully parsed.
	 *
	 * @return the module ids which were not successfully parsed
	 */
	public Set<UUID> getParseErrorModules() {
		return parseErrorModules;
	}
	
	/**
	 * Add the module id which was not successfully parsed.
	 *
	 * @param moduleId the module id
	 */
	public void addErrorModule(final UUID moduleId) {
		parseErrorModules.add(moduleId);
	}

	/**
	 * Retrieves the cached incoming dependencies for a given module.
	 *
	 * @param moduleId The ID of the module for which to retrieve the cached incoming dependencies.
	 * @return A list of traced modules representing the incoming dependencies, or null if no cached dependencies exist for the given module.
	 */
	@Nullable
	public List<TracedModule<ModuleLightweightPojo>> getCachedIncomingDependencies(final Long moduleId) {
		return incomingDependenciesCache != null ? incomingDependenciesCache.getIfPresent(moduleId): null;
	}

	/**
	 * Retrieves the cached outgoing dependencies for a given module.
	 *
	 * @param moduleId The ID of the module for which to retrieve the cached outgoing dependencies.
	 * @return A list of traced modules representing the outgoing dependencies, or null if no cached dependencies exist for the given module.
	 */
	@Nullable
	public List<TracedModule<ModuleLightweightPojo>> getCachedOutgoingDependencies(final Long moduleId) {
		return outgoingDependenciesCache != null ? outgoingDependenciesCache.getIfPresent(moduleId): null;
	}

	/**
	 * Caches the incoming dependencies for a given module.
	 *
	 * @param moduleId The ID of the module for which to cache the incoming dependencies.
	 * @param incomingDependencies A list of traced modules representing the incoming dependencies to be cached.
	 */
	public void cacheIncomingDependencies(final Long moduleId, final List<TracedModule<ModuleLightweightPojo>> incomingDependencies) {
		if (incomingDependenciesCache == null) {
			incomingDependenciesCache = CacheBuilder.newBuilder()
					.maximumSize(MAX_CACHE_SIZE)
					.build();
		}
		incomingDependenciesCache.put(moduleId, incomingDependencies);
	}

	/**
	 * Caches the outgoing dependencies for a given module.
	 *
	 * @param moduleId The ID of the module for which to cache the outgoing dependencies.
	 * @param outgoingDependencies A list of traced modules representing the outgoing dependencies to be cached.
	 */
	public void cacheOutgoingDependencies(final Long moduleId, final List<TracedModule<ModuleLightweightPojo>> outgoingDependencies) {
		if (outgoingDependenciesCache == null) {
			outgoingDependenciesCache = CacheBuilder.newBuilder()
					.maximumSize(MAX_CACHE_SIZE)
					.build();
		}
		outgoingDependenciesCache.put(moduleId, outgoingDependencies);
	}
}
