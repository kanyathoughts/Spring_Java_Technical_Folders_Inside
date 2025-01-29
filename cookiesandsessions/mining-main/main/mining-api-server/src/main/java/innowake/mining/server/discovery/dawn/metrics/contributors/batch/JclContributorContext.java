/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.batch;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;

import java.util.Optional;
import java.util.function.Consumer;

/**
 * Context for JCL contributors.
 */
public class JclContributorContext {
	private final DiscoveryBuilder discoveryBuilder;
	private final DiscoveryBuilder.ModuleBuilder rootModule;
	private final DiscoveryContext discoveryContext;
	private final JobControl jobControl;
	private final Optional<Consumer<DiscoveryBuilder.DependencyBuilder>> conditionalDependency;

	/**
	 * Constructor.
	 *
	 * @param discoveryBuilder the discovery builder of the JCL
	 * @param rootModule the root module of the JCL
	 * @param discoveryContext the discovery context
	 * @param jobControl the job control
	 * @param conditionalDependency the conditional dependency if applicable
	 */
	public JclContributorContext(final DiscoveryBuilder discoveryBuilder, final DiscoveryBuilder.ModuleBuilder rootModule,
			final DiscoveryContext discoveryContext, final JobControl jobControl,
			final Optional<Consumer<DiscoveryBuilder.DependencyBuilder>> conditionalDependency) {
		this.discoveryBuilder = discoveryBuilder;
		this.rootModule = rootModule;
		this.discoveryContext = discoveryContext;
		this.jobControl = jobControl;
		this.conditionalDependency = conditionalDependency;
	}

	public DiscoveryBuilder getDiscoveryBuilder() {
		return discoveryBuilder;
	}

	public DiscoveryBuilder.ModuleBuilder getRootModule() {
		return rootModule;
	}

	public DiscoveryContext getDiscoveryContext() {
		return discoveryContext;
	}

	public JobControl getJobControl() {
		return jobControl;
	}

	public Optional<Consumer<DiscoveryBuilder.DependencyBuilder>> getConditionalDependency() {
		return conditionalDependency;
	}

}
