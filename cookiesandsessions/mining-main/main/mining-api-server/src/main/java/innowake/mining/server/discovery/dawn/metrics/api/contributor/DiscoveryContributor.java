/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.contributor;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;

/**
 * Interface for Discovery Contributors that are not executed on a particular source file. These contributors
 * will be invoked at an unspecified time during the discovery process. Like other contributors they can defer
 * actions to be invoked after other modules and dependencies have been contributed.
 */
public interface DiscoveryContributor {

	/**
	 * Invokes the contributor. The contributor can use the provided {@code builder} to contribute
	 * modules, metrics, dependencies, statements and other Discovery related information.
	 * <p>
	 * Note that the results are collected from the provided {@code builder} immediately when this method returns. It is not possible
	 * to store a reference to the builder in order to contribute additional things at a later time. If you need to do this,
	 * use {@link ModuleBuilder#deferAction(String)} instead. The deferred action will be provided with a new builder.
	 * Never store references to discovery builders.
	 *
	 * @param builder a discovery builder
	 * @param context the current discovery context
	 */
	public void contribute(final DiscoveryBuilder builder, final DiscoveryContext context);
}
