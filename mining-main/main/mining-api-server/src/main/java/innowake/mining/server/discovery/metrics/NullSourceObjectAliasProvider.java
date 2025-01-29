/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import java.util.Collections;
import java.util.Set;

/**
 * Implementation of {@link SourceObjectAliasProvider} which's method {@link #getSourceObjectNamesForAlias(String)} always returns an empty {@link Set}.
 */
public class NullSourceObjectAliasProvider implements SourceObjectAliasProvider {

	public static final NullSourceObjectAliasProvider INSTANCE = new NullSourceObjectAliasProvider();

	private NullSourceObjectAliasProvider() {}

	@Override
	public Set<String> getSourceObjectNamesForAlias(final String alias) {
		return Collections.emptySet();
	}
}
