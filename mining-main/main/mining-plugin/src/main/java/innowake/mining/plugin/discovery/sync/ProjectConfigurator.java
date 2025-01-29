/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.discovery.sync;

import java.util.Collection;

import org.eclipse.core.resources.IProject;

import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.product.base.core.api.ApiException;

/**
 * Interface to be implements for language specific project configuration actions.
 */
public interface ProjectConfigurator {

	void configure(final IProject project) throws ApiException;

	/**
	 * Tests whether the given collection contains a {@link ResolveTarget}
	 * this configurator is interested in.
	 *
	 * @param identifiedLanguages a list of types to test for.
	 * @return {@code true} if this configurator is interested in at least one
	 *         given {@link ResolveTarget}, {@code false} otherwise.
	 */
	boolean accepts(Collection<ResolveTarget> identifiedLanguages);
	
}
