/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.batch;

import innowake.ndt.jcl.parser.model.StepExec;

import static innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;

/**
 * Contributor for JCL utility steps.
 */
public interface JclUtilityContributor {

	/**
	 * Returns whether the contributor can process the given {@code stepExec}.
	 *
	 * @param stepExec the step to check
	 * @return {@code true} if the contributor can process this step
	 */
	boolean accept(StepExec stepExec);

	/**
	 * Invokes the contributor on a JCL utility step. The contributor must use the provided {@code stepModule} to contribute
	 * modules, metrics, dependencies, statements and other Discovery related information.
	 *
	 * @param context the current jcl context
	 * @param stepExec the JCL utility step
	 * @param stepModule the module builder for the step
	 */
	void contribute(JclContributorContext context, StepExec stepExec, ModuleBuilder stepModule);
}
