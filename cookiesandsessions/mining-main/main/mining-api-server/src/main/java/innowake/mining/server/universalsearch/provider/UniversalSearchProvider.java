/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.universalsearch.provider;

import java.util.List;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.universalsearch.UniversalSearchResult;

/**
 * Interface that provides the ability to search the query in module, data dictionaries etc.
 */
public interface UniversalSearchProvider {

	/**
	 * Returns a unique id for the provider.
	 * @return a string that uniquely identifies the provider
	 */
	String getIdentifier();

	/**
	 * Searches for the query in modules or data dictionary using respective provider and provides any matching results.
	 *
	 * @param projectId the project id
	 * @param query the term to be searched
	 * @return List of {@link UniversalSearchResult}
	 */
	List<UniversalSearchResult> query(final EntityId projectId, final String query);

}
