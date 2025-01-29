/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import java.util.List;

import innowake.mining.shared.entities.SourcePojo;

/**
 * Interface for {@link SourcePojo} managers that provides {@link SourceObjectDependency} for {@link SourcePojo}s.
 * 
 * @param <D> the actual {@link SourceObjectDependency} type 
 */
public interface SourceObjectManager<D extends SourceObjectDependency> {

	/**
	 * Provides a list of outgoing {@link SourceObjectDependency} for the given {@link SourcePojo}.
	 *
	 * @param sourceObject the {@link SourcePojo} to provide the dependencies for
	 * @return the list of outgoing {@link SourceObjectDependency} for the given {@link SourcePojo}
	 */
	public List<D> getOutgoingDependencies(final SourcePojo sourceObject);
}
