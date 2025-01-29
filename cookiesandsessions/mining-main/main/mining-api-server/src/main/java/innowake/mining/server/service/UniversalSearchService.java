/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.mining.server.universalsearch.provider.UniversalSearchProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.universalsearch.UniversalSearchResult;

/**
 * Service to search for a module or data-dictionary with in the project.
 */
@Service
public class UniversalSearchService {
	
	private static final int LIMIT = 10;
	private List<UniversalSearchProvider> searchProviders;
	 
	/**
	 * Initializes the providers.
	 * @param searchProviders the search providers
	 */
	@Autowired
	public UniversalSearchService(final List<UniversalSearchProvider> searchProviders) {
		this.searchProviders = searchProviders;
	}

	/**
	 * Searches the query with in the project using  module and data dictionary providers.
	 *
	 * @param projectId the project id
	 * @param query the query to be searched
	 * @return List of {@link UniversalSearchResult}
	 */
	public List<UniversalSearchResult> query(final EntityId projectId, final String query) {
		final int minimumResultsPerProvider = LIMIT / searchProviders.size();
		final Map<String, List<UniversalSearchResult>> providerResults = new HashMap<>();
		final List<UniversalSearchResult> additionalResults = new ArrayList<>();
		for (final UniversalSearchProvider provider : searchProviders) {
			final List<UniversalSearchResult> providerResult = provider.query(projectId, query).stream().limit(LIMIT).collect(Collectors.toList());
			final int size = Math.min(minimumResultsPerProvider, providerResult.size());
			providerResults.put(provider.getIdentifier(), providerResult.subList(0, size));
			if (providerResult.size() > minimumResultsPerProvider) {
				additionalResults.addAll(providerResult.subList(minimumResultsPerProvider, providerResult.size()));
			}
		}
		final List<UniversalSearchResult> results = providerResults.values().stream().flatMap(List::stream).collect(Collectors.toList());
		if (results.size() < LIMIT) {
			results.addAll(additionalResults.subList(0, Math.min(LIMIT - results.size(), additionalResults.size())));
		}
		return results;
	} 

}
