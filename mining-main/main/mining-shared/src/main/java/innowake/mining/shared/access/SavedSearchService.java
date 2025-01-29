/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

import javax.persistence.EntityNotFoundException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.SavedSearchPojo;
import innowake.mining.shared.entities.SavedSearchPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.SavedSearchCountResponse;
import innowake.mining.shared.model.ScopeEnum;

/**
 * Functions for accessing {@code saved_search} entities.
 */
public interface SavedSearchService {

	interface SavedSearchInquiryBuilder {

		/**
		 * Filters saved searches by id.
		 * 
		 * @param id the id of the saved search
		 * @return this instance for method chaining
		 */
		SavedSearchInquiryBuilder byId(Long id);

		/**
		 * Filters saved searches by {@code client}. If the given {@code client} is {@code null}, it will filter for saved searches without a client.
		 * 
		 * @param client the id of the client of the saved search
		 * @return this instance for method chaining
		 */
		SavedSearchInquiryBuilder ofClient(@Nullable EntityId client);

		/**
		 * Filters saved searches by {@code project}. If the given {@code project} is {@code null}, it will filter for saved searches without a project.
		 * 
		 * @param project the id of the project of the saved search
		 * @return this instance for method chaining
		 */
		SavedSearchInquiryBuilder ofProject(@Nullable EntityId project);

		/**
		 * Filters saved searches by name.
		 * 
		 * @param name the name of the saved search
		 * @return this instance for method chaining
		 */
		SavedSearchInquiryBuilder withName(String name);

		/**
		 * Filters saved searches by names. A saved search matches if its name is equal to any of the given names.
		 * 
		 * @param names the names of the saved search
		 * @return this instance for method chaining
		 */
		SavedSearchInquiryBuilder withNames(Collection<String> names);

		/**
		 * Filters saved searches by usage.
		 * 
		 * @param usage the usage of the saved search
		 * @return this instance for method chaining
		 */
		SavedSearchInquiryBuilder withUsage(String usage);

		/**
		 * Filters saved searches by scope.
		 * 
		 * @param scope the scope of the saved search
		 * @return this instance for method chaining
		 */
		SavedSearchInquiryBuilder withScope(ScopeEnum scope);

		/**
		 * Filters saved searches by the user that created the saved search.
		 * 
		 * @param createdByUserId the id of the user that created the saved search
		 * @return this instance for method chaining
		 */
		SavedSearchInquiryBuilder withCreatedByUserId(String createdByUserId);
	}

	/**
	 * Creates a new {@code saved_search} entity.
	 *
	 * @param savedSearch the {@link SavedSearchPojoPrototype} to create
	 * @return the id of the new {@code saved_search} entity
	 */
	Long create(SavedSearchPojoPrototype savedSearch);

	/**
	 * Updates the {@code saved_search} entity.
	 *
	 * @param project the project if of the {@link SavedSearchPojoPrototype} to update
	 * @param savedSearch the {@link SavedSearchPojoPrototype} to update
	 */
	void update(EntityId project, SavedSearchPojoPrototype savedSearch);

	/**
	 * Deletes all {@code saved_search} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SavedSearchInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted {@code saved_search} entities
	 */
	int delete(BuildingConsumer<SavedSearchInquiryBuilder> builder);

	/**
	 * Returns the {@link SavedSearchPojo} for the specified ID.
	 * 
	 * @param id ID of the saved search to be found.
	 * @return the matching {@link SavedSearchPojo}
	 * @throws EntityNotFoundException If no match was found.
	 */
	SavedSearchPojo get(Long id);

	/**
	 * Returns all {@code saved_search} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SavedSearchInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain SavedSearchPojo SavedSearchPojos}
	 */
	List<SavedSearchPojo> find(BuildingConsumer<SavedSearchInquiryBuilder> builder);

	/**
	 * Returns paged subset of {@code saved_search} entities that match with the filters in the given {@code builder}.
	 *
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain SavedSearchInquiryBuilder} containing the filter criteria and sort options.
	 * @return Paged subset of matching {@code saved_search} entities.
	 */
	Paged<SavedSearchPojo> find(Pagination paging, BuildingConsumer<SavedSearchInquiryBuilder> builder);

	/**
	 * Returns the first {@code saved_search} entity that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SavedSearchInquiryBuilder} containing the filter criteria and sort options.
	 * @return matching {@linkplain SavedSearchPojo}
	 */
	Optional<SavedSearchPojo> findAny(BuildingConsumer<SavedSearchInquiryBuilder> builder);

	/**
	 * Returns all the {@link SavedSearchPojo SavedSearchPojos} by the provided project, usage and createdByUserId.
	 *
	 * @param projectId of {@link SavedSearchPojo}
	 * @param usage of {@link SavedSearchPojo}
	 * @param createdByUserId of {@link SavedSearchPojo}
	 * @return List of {@link SavedSearchPojo}
	 */
	List<SavedSearchPojo> findByUsageAndUserId(EntityId projectId, String usage, String createdByUserId);

	/**
	 * Returns a list of {@link SavedSearchCountResponse SavedSearchCountResponses} for all SavedSearches and their counts for the given {@code project}.
	 * 
	 * @param projectId the ID of the project.
	 * @return list of {@link SavedSearchCountResponse SavedSearchCountResponses}
	 */
	List<SavedSearchCountResponse> getSavedSearchCounts(EntityId projectId);
}
