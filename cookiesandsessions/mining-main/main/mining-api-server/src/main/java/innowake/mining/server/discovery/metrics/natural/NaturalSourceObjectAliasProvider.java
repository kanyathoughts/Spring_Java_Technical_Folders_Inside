/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import java.util.Set;

import innowake.mining.server.discovery.metrics.SourceObjectAliasProvider;

/**
 * Natural implementation of {@link SourceObjectAliasProvider}.
 */
public class NaturalSourceObjectAliasProvider implements SourceObjectAliasProvider {

	private final String jobId;
	private final NaturalSourceObjectAliasManager aliasManager;

	public NaturalSourceObjectAliasProvider(final String jobId, final NaturalSourceObjectAliasManager aliasManager) {
		this.jobId = jobId;
		this.aliasManager = aliasManager;
	}

	@Override
	public Set<String> getSourceObjectNamesForAlias(final String alias) {
		return aliasManager.getSourceObjectNamesForAlias(jobId, alias);
	}
}
