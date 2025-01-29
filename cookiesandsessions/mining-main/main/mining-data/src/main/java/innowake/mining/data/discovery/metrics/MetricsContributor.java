/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.discovery.metrics;

import innowake.lib.core.lang.Nullable;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.ModelDependency;
import innowake.mining.shared.discovery.DiscoveryException;
import org.springframework.util.ClassUtils;

/**
 * Base interface for every metrics contributor implementation to be used with innowake.mining.server.discovery.metrics.MetricsCollector.
 * <p>
 * Every metrics contributor additionally requires a public static method called "accept" accepting a {@link ModelArtifact} and a {@link Phase} as argument
 * that has to return {@code true} if the artifact can be processed by this contributor in the given phase.
 * <p>
 * <pre>
 * public static boolean accept(final ModelArtifact artifact, final Phase phase) {
 *     // return true or false
 * }
 * </pre>
 * <p>
 * Note: For backwards compatibility or if all phases are supported, the contributor class may also implement the "accept" method without the Phase argument.
 * <pre>
 * public static boolean accept(final ModelArtifact artifact) {
 *     // return true or false
 * }
 * </pre>
 */
public interface MetricsContributor {

	public enum Phase {
		/** 
		 * Collects all generic metrics for one single module like lines of code.
		 * No dependency information is available at this time of execution!
		 * <p>
		 * {@link MetricsContributor#calculateGenericMetrics(ModelArtifact)} is invoked in this phase.
		 */
		GENERIC_METRICS,
		/** 
		 * Collects all metrics for one single module with full access to all other modules to resolve dependency information.
		 * <p>
		 * {@link MetricsContributor#calculateDependentMetrics(ModelArtifact)} is invoked in this phase.
		 */
		DEPENDENT_METRICS,
		/**
		 * Collects metrics for one single module with full access to all other modules and dependencies.
		 * <p>
		 * {@link MetricsContributor#calculateTransitiveMetrics(ModelArtifact)} is invoked in this phase.
		 */
		TRANSITIVE_METRICS;
	}
	
	
	/**
	 * This method will be called during the {@link Phase#GENERIC_METRICS} phase to calculate generic metrics like lines of code.
	 * <p>
	 * <b>Note:</b> At this time of execution no other {@linkplain ModelArtifact ModelArtifacts} are accessible via the {@link IModuleRepository}! If any
	 * other {@linkplain ModelArtifact ModelArtifacts} are necessary to get hold of specific information, then this should be done in
	 * {@link #calculateDependentMetrics(ModelArtifact)}.
	 * 
	 * @param artifact the {@link ModelArtifact} to calculate the generic metrics for
	 * @throws DiscoveryException if the metrics could not be calculated
	 */
	default void calculateGenericMetrics(final ModelArtifact artifact) throws DiscoveryException {
		
	}
	
	/**
	 * This method will be called during the {@link Phase#DEPENDENT_METRICS} phase after <b>all</b>
	 * source objects have been processed once by {@link #calculateGenericMetrics(ModelArtifact)}. At this time of execution all
	 * {@linkplain ModelArtifact ModelArtifacts} are accessible via the {@link IModuleRepository} to resolve inter-module information like dependencies.
	 * 
	 * @param artifact the {@link ModelArtifact} to calculate the dependent metrics for
	 * @throws DiscoveryException if the metrics could not be calculated
	 */
	default void calculateDependentMetrics(final ModelArtifact artifact) throws DiscoveryException {
		
	}
	
	/**
	 * This method will be called during the {@link Phase#TRANSITIVE_METRICS} phase
	 * after all modules have been processed and their dependencies have been established.
	 * <p>
	 * This is the correct phase to compute additional metrics or create additional dependencies,
	 * for which it is required to traverse multiple {@linkplain ModelArtifact ModelArtifacts} using the established dependencies.
	 * <p>
	 * Supporting this phase is optional. The default implementation of this method does nothing.
	 * 
	 * @param artifact artifact the {@link ModelArtifact} to calculate the transitive metrics for
	 * @throws DiscoveryException if the metrics could not be calculated
	 */
	default void calculateTransitiveMetrics(final ModelArtifact artifact) throws DiscoveryException {
		/* does nothing by default */
	}
	
	/**
	 * Closes any resources of this contributor.
	 */
	default void close() {
		
	}

	/**
	 * Returns the name of this contributor, mainly for debugging purposes.
	 *
	 * @return the contributor name; defaults to the class name of the contributor
	 */
	default String getName() {
		return ClassUtils.getUserClass(this).getSimpleName();
	}
	
	/**
	 * This method is used to add dependency between two ModelArtifacts with attributes.
	 * 
	 * @param source the {@link ModelArtifact}
	 * @param target the {@link ModelArtifact}
	 * @param attributeMap the {@link ModelAttributeMap}
	 */
	default void addDependency(final ModelArtifact source, final ModelArtifact target, @Nullable final ModelAttributeMap<Object> attributeMap) {
		final ModelDependency dependency = new ModelDependency()
				.setEarlyBinding()
				.setTarget(target);
		if (attributeMap != null) {
			dependency.setAttributes(attributeMap);
		}
		dependency.validate();

		/* Only add dependency if not present yet. */
		if (source.getDependencies().noneMatch(dep -> dep.equals(dependency) && (attributeMap == null || attributeMap.equals(dep.getAttributes())))) {
			source.addDependency(dependency);
		}
	}
}
