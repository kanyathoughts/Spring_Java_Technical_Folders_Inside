/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Type;

/**
 * An outgoing dependency to target {@link SourcePojo}(s). One source can reference multiple targets in case of multilingual placeholders (e.g. Natural maps).
 */
public interface SourceObjectDependency {

	/**
	 * Provides the name of the target as defined in the source {@link SourcePojo}.
	 *
	 * @return the name of the target
	 */
	public String getTargetName();

	/**
	 * Provides the {@link Type} of the target {@link SourcePojo}(s).
	 *
	 * @return the {@link Type} of the target
	 */
	@Nullable
	public Type getTargetType();

	/**
	 * Provides whether the target {@link SourcePojo}(s) exists, means it is resolvable.
	 *
	 * @return whether the target exists
	 */
	public boolean targetExists();

	/**
	 * Provides the target {@link SourcePojo}s.
	 *
	 * @return the target {@link SourcePojo}s
	 */
	public Set<SourcePojo> getTargets();
}
