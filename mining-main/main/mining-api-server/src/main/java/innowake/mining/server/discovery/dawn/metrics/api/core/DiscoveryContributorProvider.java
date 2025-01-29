/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.core;

import java.util.List;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.shared.entities.SourcePojo;

/**
 * Provides Discovery Contributors.
 */
public interface DiscoveryContributorProvider {

	/**
	 * Returns the list of contributors that are not operating on a particular source file.
	 *
	 * @return the list of contributors without source file
	 */
	List<DiscoveryContributor> getNoSourceContributors();
	
	/**
	 * Returns the list of contributors that are applicable to the given source file. This method will use
	 * {@link DiscoveryContributorFromSource#accept(DiscoveryContext, SourcePojo)} to check whether a contributor can run on the given source file.
	 * <p>
	 * This method takes into account the current configuration for checking if a certain language is enabled.
	 *
	 * @param context the current discovery context
	 * @param sourceObject the source object to check
	 * @return the list of contributors that can run on the source object
	 */
	List<DiscoveryContributorFromSource> getContributorsForSourceObject(DiscoveryContext context, SourcePojo sourceObject);
}
