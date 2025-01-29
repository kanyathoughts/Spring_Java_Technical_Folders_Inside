/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.discovery.metrics;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.Callable;

/**
 * Distributed cache that can be used to store intermediate results in the cluster.
 */
public interface DiscoveryCache {

	/**
	 * Puts the provided {@code value} into the discovery cache {@code MultiMap}, which uses a Set as its nested collection.
	 * <p>
	 * <b>IMPORTANT:</b> This method uses {@code hashCode} and {@code equals} of the binary form of the key, not the actual implementations of
	 * {@code hashCode} and {@code equals} defined in the key's class.
	 * <p>
	 * <b>IMPORTANT:</b> Be very cautious of what and with which structure values are inserted as every put and get operation will
	 * lead to serialization (inserted value) and deserialization (full Set) operations! This may additionally also introduce network
	 * roundtrips depending on which cluster node the map key is being held!
	 * <p>
	 * <b>IMPORTANT:</b> Always call {@link #clearDiscoveryJobCache(String)} with the jobId after the job is finished to properly clean
	 * any resources! Otherwise they will be held forever in the cluster.
	 *
	 * @param jobId the Id of the {@code Job} for which the entry should be stored
	 * @param key the actual key of the value to store
	 * @param value the value to store into the Set of the backing {@code MultiMap}. This must be serializable
	 */
	void putMultiValue(String jobId, String key, Object value);
	
	/**
	 * Gets the nested Set from the discovery cache {@code MultiMap} with the provided {@code key}.
	 * <p>
	 * <b>IMPORTANT:</b> Changes done to the resulting Set are not reflected in the cache!
	 * <p>
	 * <b>IMPORTANT:</b> This will lead to a deserialization of the full nested Set, which may also introduce additional network
	 * roundtrips depending on which cluster node the map key is being held! Avoid calling this very often, especially if the
	 * nested Set is very large!
	 * <p>
	 * <b>IMPORTANT:</b> Always call {@link #clearDiscoveryJobCache(String)} with the jobId after the job is finished to properly clean
	 * any resources! Otherwise they will be held forever in the cluster.
	 *
	 * @param jobId the Id of the {@code Job} for which the entry had originally been stored
	 * @param key the actual key of the stored value
	 * @return the deserialized nested Set; not {@code null}
	 */
	Set<Object> getMultiValue(String jobId, String key);
	
	/**
	 * Removes the value with the provided {@code key} from the discovery cache {@code MultiMap} previously
	 * added via {@link #putMultiValue(String, String, Object)}.
	 * 
	 * @param jobId the Id of the {@code Job} for which the entry had originally been stored
	 * @param key the actual key of the stored value
	 */
	void removeMultiValue(String jobId, String key);

	/**
	 * Checks if the cache has value for the given key
	 *
	 * @param jobId The id of the job
	 * @param key The key to get the value for
	 * @return true if the cache has the value for the key else false
	 */
	boolean hasMultiValue(String jobId, String key);
	
	/**
	 * Puts the provided {@code value} into the discovery cache {@code IMap}.
	 * <p>
	 * <b>IMPORTANT:</b> This method uses {@code hashCode} and {@code equals} of the binary form of the key, not the actual implementations of
	 * {@code hashCode} and {@code equals} defined in the key's class.
	 * <p>
	 * <b>IMPORTANT:</b> Be very cautious of what and with which structure values are inserted as every put and get operation will
	 * lead to serialization (inserted value) and deserialization operations! This may additionally also introduce network
	 * roundtrips depending on which cluster node the map key is being held!
	 * <p>
	 * <b>IMPORTANT:</b> Always call {@link #clearDiscoveryJobCache(String)} with the jobId after the job is finished to properly clean
	 * any resources! Otherwise they will be held forever in the cluster.
	 *
	 * @param jobId the Id of the {@code Job} for which the entry should be stored
	 * @param key the actual key of the value to store
	 * @param value the value to store into the Set of the backing {@code MultiMap}. This must be serializable
	 */
	void putValue(String jobId, String key, Object value);
	
	/**
	 * Removes the value with the provided {@code key} from the discovery cache {@code IMap} previously
	 * added via {@link #putValue(String, String, Object)}.
	 * 
	 * @param jobId the Id of the {@code Job} for which the entry had originally been stored
	 * @param key the actual key of the stored value
	 */
	void removeValue(String jobId, String key); 
	
	/**
	 * If the {@code key} is not already present in the discovery cache {@code IMap} it will be inserted based on the value created by the provided
	 * {@code callable}.
	 * <p>
	 * <b>IMPORTANT:</b> This method uses {@code hashCode} and {@code equals} of the binary form of the key, not the actual implementations of
	 * {@code hashCode} and {@code equals} defined in the key's class.
	 * <p>
	 * <b>IMPORTANT:</b> Be very cautious of what and with which structure values are inserted as every put and get operation will
	 * lead to serialization (inserted value) and deserialization operations! This may additionally also introduce network
	 * roundtrips depending on which cluster node the map key is being held!
	 * <p>
	 * <b>IMPORTANT:</b> Always call {@link #clearDiscoveryJobCache(String)} with the jobId after the job is finished to properly clean
	 * any resources! Otherwise they will be held forever in the cluster.
	 * 
	 * @param jobId the Id of the {@code Job} for which the entry should be stored
	 * @param key the actual key of the value to store
	 * @param callable the Callable to compute the value with if not already mapped
	 * @return either the existing value or the newly computed one
	 * @throws Exception if an error occurs during execution of the Callable
	 */
	Object computeValueIfAbsent(String jobId, String key, Callable<? extends Object> callable) throws Exception;
	
	/**
	 * Gets the value from the discovery cache {@code IMap} with the provided {@code key}.
	 * <p>
	 * <b>IMPORTANT:</b> Changes done to the resulting value are not reflected in the cache!
	 * <p>
	 * <b>IMPORTANT:</b> This will lead to a deserialization of the value, which may also introduce additional network
	 * roundtrips depending on which cluster node the map key is being held! Avoid calling this very often, especially if the
	 * stored value is very large!
	 * <p>
	 * <b>IMPORTANT:</b> Always call {@link #clearDiscoveryJobCache(String)} with the jobId after the job is finished to properly clean
	 * any resources! Otherwise they will be held forever in the cluster.
	 *
	 * @param jobId the Id of the {@code Job} for which the entry had originally been stored
	 * @param key the actual key of the stored value
	 * @return the deserialized value; not {@code null}
	 */
	@Nullable
	Object getValue(String jobId, String key);
	
	/**
	 * Clears every cache entry assigned to the provided {@code jobId}.
	 * <p>
	 * <b>IMPORTANT:</b> This must always be called latest at the end of every job that made use of the cache!
	 * Otherwise the data will be held forever in the cluster.
	 *
	 * @param jobId the Id of the job
	 */
	void clearDiscoveryJobCache(String jobId);
	
	/**
	 * Sequentially creates a distributed lock for each given {@code key} available to all cluster nodes. 
	 * All other cluster nodes trying to create a lock with the same key will block at this method, until the lock has been released.
	 * <p>
	 * <b>IMPORTANT:</b> It must be ensured that {@link #releaseLocks(String, String...)} is always being called. Otherwise the lock persists as long as the cluster
	 * node lives!
	 * 
	 * @param jobId the Id of the job
	 * @param keys the keys to create the locks for
	 */
	void createLocks(String jobId, String... keys);
	
	/**
	 * Sequentially creates a distributed lock for each given {@code key} available to all cluster nodes. 
	 * All other cluster nodes trying to create a lock with the same key will block at this method, until the lock has been released.
	 * <p>
	 * <b>IMPORTANT:</b> It must be ensured that {@link #releaseLocks(String, String...)} is always being called. Otherwise the lock persists as long as the cluster
	 * node lives!
	 * 
	 * @param jobId the Id of the job
	 * @param keys the keys to create the locks for
	 */
	void createLocks(String jobId, List<String> keys);
	
	/**
	 * Releases a distributed lock that has previously been created by {@link #createLocks(String, String...)} with the same {@code key}.
	 * 
	 * @param jobId the Id of the job
	 * @param keys the keys to release the locks for
	 */
	void releaseLocks(String jobId, String... keys);
	
	/**
	 * Releases a distributed lock that has previously been created by {@link #createLocks(String, String...)} with the same {@code key}.
	 * 
	 * @param jobId the Id of the job
	 * @param keys the keys to release the locks for
	 */
	void releaseLocks(String jobId, List<String> keys);
	
	/**
	 * Creates a lock key that should be used with {@link #createLocks(String, String...)} when locking a {@link ModelArtifact}.
	 * <p>If the given {@code modelArtifact} has no path then the path of the parent {@link ModelArtifact} is used to create a more specific key and to avoid
	 * delays when acquiring the locks for different virtual modules that have no path but the same name.</p>
	 *
	 * @param modelArtifact the model artifact
	 * @return a somewhat unique lock key for the artifact
	 */
	public static String createLockKeyForArtifact(final ModelArtifact modelArtifact) {
		Optional<String> path = modelArtifact.getPath();
		if ( ! path.isPresent()) {
			/* for performance reasons first check if the containsPath is set before loading the parent. The LazyModelArtifact will do a DB lockup if
			 * the parent is present in the DB but wasn't resolved yet */
			path = modelArtifact.getParentPath();

			if ( ! path.isPresent()) {
				final ModelArtifact parentModule = modelArtifact.getParentModule();
				if (parentModule != null) {
					path = parentModule.getPath();
				}
			}
		}

		return createLockKeyForParameters(
				modelArtifact.getName(),
				ResolveTargetHelper.toTechnology(modelArtifact.getLanguage()),
				ResolveTargetHelper.toType(modelArtifact.getType()),
				path);
	}

	/**
	 * Creates a lock key that should be used with {@link #createLocks(String, String...)} when locking {@ode virtual} {@link ModelArtifact}.
	 * <p>If the given {@code modelArtifact} has no path then the path of the given {@code parentArtifact} is used to create a more specific key and to avoid
	 * delays when acquiring the locks for different virtual modules that have no path but the same name.</p>
	 *
	 * @param modelArtifact the model artifact to create the lock for
	 * @param parentArtifact the parent model artifact of {@ode modelArtifact}
	 * @return a somewhat unique lock key for the artifact
	 */
	public static String createLockKeyForArtifact(final ModelArtifact modelArtifact, final ModelArtifact parentArtifact) {
		Optional<String> path = modelArtifact.getPath();
		if ( ! path.isPresent()) {
			path = modelArtifact.getParentPath();

			if ( ! path.isPresent()) {
				path = parentArtifact.getPath();
			}
		}
		return createLockKeyForParameters(
				modelArtifact.getName(),
				ResolveTargetHelper.toTechnology(modelArtifact.getLanguage()),
				ResolveTargetHelper.toType(modelArtifact.getType()),
				path);
	}

	/**
	 * Create a lock key that should be used with {@link #createLocks(String, String...)} when locking a {@link ModelArtifact}
	 * that has the provided parameters. Use this when the actual artifact instance is not available.
	 * 
	 * @param name name of the artifact
	 * @param technology technology of the artifact
	 * @param type type of the artifact
	 * @return a somewhat unique lock key for the artifact
	 */
	public static String createLockKeyForParameters(final String name, final Technology technology, final Type type) {
		return createLockKeyForParameters(name, technology, type, Optional.empty());
	}
	
	/**
	 * Create a lock key that should be used with {@link #createLocks(String, String...)} when locking a {@link ModelArtifact}
	 * that has the provided parameters. Use this when the actual artifact instance is not available.
	 * 
	 * @param name name of the artifact
	 * @param technology technology of the artifact
	 * @param type type of the artifact
	 * @param path the path of the artifact
	 * @return a somewhat unique lock key for the artifact
	 */
	public static String createLockKeyForParameters(final String name, final Technology technology, final Type type, final String path) {
		return createLockKeyForParameters(name, technology, type, Optional.of(path));
	}
	
	/**
	 * Create a lock key that should be used with {@link #createLocks(String, String...)} when locking a {@link ModelArtifact}
	 * that has the provided parameters. Use this when the actual artifact instance is not available.
	 * 
	 * @param name name of the artifact
	 * @param technology technology of the artifact
	 * @param type type of the artifact
	 * @param path optionally, the path of the artifact
	 * @return a somewhat unique lock key for the artifact
	 */
	public static String createLockKeyForParameters(final String name, final Technology technology, final Type type, final Optional<String> path) {
		final StringBuilder sb = new StringBuilder();
		sb.append(name.toLowerCase());
		sb.append(technology);
		sb.append(type);
		sb.append(path.orElse(""));
		
		return sb.toString();
	}
}
