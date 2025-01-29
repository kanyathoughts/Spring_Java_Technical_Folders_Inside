/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.core;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContributorProvider;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Default provider for DiscoveryContributorProvider.
 */
@Service
public class DiscoveryContributorProviderImpl implements DiscoveryContributorProvider {

	private final List<DiscoveryContributor> contributors;
	private final List<DiscoveryContributorFromSource> fromSourceContributors;

	/**
	 * Creates a new discovery contributor provider.
	 * 
	 * @param contributors all discovery contributors that are not executed on a particular source file.
	 * @param fromSourceContributors all discovery contributors that operate on a source file.
	 */
	@Autowired
	public DiscoveryContributorProviderImpl(final List<DiscoveryContributor> contributors, final List<DiscoveryContributorFromSource> fromSourceContributors) {
		this.contributors = contributors;
		this.fromSourceContributors = fromSourceContributors;
	}

	@Override
	public List<DiscoveryContributor> getNoSourceContributors() {
		return contributors;
	}

	@Override
	public List<DiscoveryContributorFromSource> getContributorsForSourceObject(final DiscoveryContext context, final SourcePojo sourceObject) {
		return fromSourceContributors.stream().filter(contributor -> contributor.accept(context, sourceObject)
				&& context.getConfig().enableLanguage(ResolveTargetHelper.fromTechnology(sourceObject.getTechnology()))).collect(Collectors.toList());
	}
}