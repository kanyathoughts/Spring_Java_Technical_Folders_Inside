/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.contributor;

import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.entities.SourcePojo;

/**
 * Interface for Discovery Contributors that operate on a source file. If multiple contributors are applicable
 * to a source file, they will be run in sequence, so only one contributor will operate on a source file at a time.
 * The order in which files are processed is unspecified.
 * <p>
 * Note that both {@link #accept(DiscoveryContext, SourcePojo)} and {@link #contribute(DiscoveryBuilderFromSource, DiscoveryContext, SourcePojo)}
 * are invoked concurrently. Therefore the implementing class must be stateless (at least it must be thread-safe but apart from caching
 * the implementing class should not store any local state).
 */
public interface DiscoveryContributorFromSource {

	/**
	 * Returns whether the contributor can process the given {@code sourceObject} under the current {@code context}.
	 * <p>
	 * If this method returns {@code true}, then {@link #contribute(DiscoveryBuilderFromSource, DiscoveryContext, SourcePojo)}
	 * will be invoked on the source object.
	 *
	 * @param context the current discovery context
	 * @param sourceObject a source file
	 * @return {@code true} if the contributor can process this file
	 */
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject);
	
	/**
	 * Invokes the contributor on a source file. The contributor must use the provided {@code builder} to contribute
	 * modules, metrics, dependencies, statements and other Discovery related information.
	 *
	 * @param builder a discovery builder for the source file
	 * @param context the current discovery context
	 * @param sourceObject the source object for the current source file
	 */
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject);
}
