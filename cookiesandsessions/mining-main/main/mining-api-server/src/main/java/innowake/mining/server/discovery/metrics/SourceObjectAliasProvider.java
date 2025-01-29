/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import java.util.Set;

import innowake.mining.shared.entities.SourcePojo;

/**
 * Provides {@link SourcePojo} names for alias names.
 */
public interface SourceObjectAliasProvider {

	/**
	 * Returns all {@link SourcePojo} names for an {@code alias}.
	 *
	 * @param alias the alias of a {@link SourcePojo}
	 * @return all {@link SourcePojo} names for the given {@code alias}
	 */
	Set<String> getSourceObjectNamesForAlias(String alias);
}
