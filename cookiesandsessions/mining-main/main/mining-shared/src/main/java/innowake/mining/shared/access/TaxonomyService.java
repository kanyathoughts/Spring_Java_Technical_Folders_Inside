/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.TaxonomyCategoryPojo;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.TaxonomyFieldName;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;

/**
 * Specifies basic functions for accessing the Taxonomy database entity.
 */
public interface TaxonomyService {

	/**
	 * Query sort builder for sorting {@code taxonomy} entities when performing queries on {@code taxonomy} entities.
	 */
	interface TaxonomyOrderBuilder {
		/**
		 * Sorts Taxonomies by their numeric ID.
		 * @param direction Direction.
		 * @return Sorting builder.
		 */
		TaxonomyOrderBuilder sortId(final SortDirection direction);

		/**
		 * Sorts Taxonomies by their name using binary collation.
		 * @param direction Direction.
		 * @return Sorting builder.
		 */
		TaxonomyOrderBuilder sortName(final SortDirection direction);
	}

	/**
	 * Query builder for filtering {@code taxonomy} entities when performing queries on {@code taxonomy} entities.
	 */
	interface TaxonomyInquiryBuilder extends TaxonomyOrderBuilder {
		/**
		 * Filters a single Taxonomy by it's ID.
		 * @param id ID of a Taxonomy.
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder byId(EntityId id);
		
		/**
		 * Filters Taxonomies by IDs.
		 * @param ids IDs of Taxonomies to filter.
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder byIds(Collection<EntityId> ids);

		/**
		 * Filters by the Category ID of the Taxonomy.
		 * @param id ID of Taxonomy Category to filter.
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder ofCategory(Long id);
		
		/**
		 * Filters by the Type ID of the Taxonomy.
		 * @param id ID of Taxonomy Type to filter.
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder ofType(UUID id);

		/**
		 * Filters Taxonomies by Project.
		 * @param projectId ID of the Project.
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder ofProject(EntityId projectId);
		
		/**
		 * Filters Taxonomies by Module.
		 * @param moduleId ID of the Module.
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder ofModule(EntityId moduleId);

		/**
		 * Filters Taxonomies by Modules.
		 * @param moduleIds ID of the Modules.
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder ofModules(Collection<EntityId> moduleIds);

		/**
		 * Filters Taxonomies by type name.
		 * @param typeName name of the type
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder withTypeName(String typeName);
		
		/**
		 * Filters Taxonomies that don't have given type name.
		 * @param typeName name of the type
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder notWithTypeName(String typeName);
		
		/**
		 * Filters Taxonomies by their name.
		 * @param name of the taxonomy
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder withName(String name);
		
		/**
		 * Filters Taxonomies by given names.
		 * @param name of the taxonomy
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder withNames(Collection<String> name);
		
		/**
		 * Filters Taxonomies by Types.
		 * @param types names of the types
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder withTypeNames(Collection<String> types);

		/**
		 * Filters the values so that only modules with all {@code taxonomyIds} are count.
		 * @param taxonomyIds ids of the taxonomies
		 * @return Filter builder.
		 */
		TaxonomyInquiryBuilder withModuleCountsReferencingTaxonomies(Collection<EntityId> taxonomyIds);
	}

	/**
	 * Query builder for filtering {@code Type} entities when performing queries on {@code Type} entities.
	 */
	interface TaxonomyTypeInquiryBuilder extends TaxonomyOrderBuilder {
		/**
		 * Filters a single TaxonomyType by it's ID.
		 * @param id ID of a Taxonomy.
		 * @return Filter builder.
		 */
		TaxonomyTypeInquiryBuilder byId(UUID id);
		
		/**
		 * Filters by the Category ID of the Taxonomy.
		 * @param id ID of Taxonomy Category to filter.
		 * @return Filter builder.
		 */
		TaxonomyTypeInquiryBuilder ofCategory(Long id);
		
		/**
		 * Filters Taxonomies by Project.
		 * @param projectId ID of the Project.
		 * @return Filter builder.
		 */
		TaxonomyTypeInquiryBuilder ofProject(EntityId projectId);
		
		/**
		 * Filters Taxonomy Types by name.
		 * @param typeName name of the type
		 * @return Filter builder.
		 */
		TaxonomyTypeInquiryBuilder withName(String typeName);
	}

	/**
	 * Query builder for filtering {@code Category} entities when performing queries on {@code Category} entities.
	 */
	interface TaxonomyCategoryInquiryBuilder extends TaxonomyOrderBuilder {
		/**
		 * Filters a single Taxonomy Category by its ID.
		 * @param id ID of a Taxonomy.
		 * @return Filter builder.
		 */
		TaxonomyCategoryInquiryBuilder byId(Long id);
		
		/**
		 * Filters Taxonomy Categories by Project.
		 * @param projectId ID of the Project.
		 * @return Filter builder.
		 */
		TaxonomyCategoryInquiryBuilder ofProject(EntityId projectId);

		/**
		 * Filters Taxonomy Categories by given Project or the default Project with NID = 0.
		 * @param projectId ID of the Project.
		 * @return Filter builder.
		 */
		TaxonomyCategoryInquiryBuilder ofProjectWithDefault(EntityId projectId);
		
		/**
		 * Filters Taxonomy Categories by name.
		 * @param typeName name of the type
		 * @return Filter builder.
		 */
		TaxonomyCategoryInquiryBuilder withName(String typeName);

	}

	interface TaxonomyAggregationInquiryBuilder< B extends TaxonomyAggregationInquiryBuilder<B>> extends AggregationInquiryBuilder<TaxonomyFieldName, B> {

		/**
		 * Filters Taxonomies by project id.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofProject(String operator, Object value);

		/**
		 * Filters Taxonomies by their id.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B byId(String operator, Object value);

		/**
		 * Filters Taxonomies by their name.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withName(String operator, Object value);

		/**
		 * Filters Taxonomies by their type name.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofTypeName(String operator, Object value);

		/**
		 * Filters Taxonomies by their category name.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofCategoryName(String operator, Object value);

		/**
		 * Filters Taxonomies by their module id.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofModuleId(String operator, Object value);

		/**
		 * Filters Taxonomies by their module name.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofModuleName(String operator, Object value);

		/**
		 * Filters Taxonomies by their module technology name.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofModuleTechnology(String operator, Object value);

		/**
		 * Filters Taxonomies by their module type name.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofModuleType(String operator, Object value);

		/**
		 * Filters Taxonomies by their module representation.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofModuleRepresentation(String operator, Object value);

		/**
		 * Filters Taxonomies by their module complexity McCabe value.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofModuleComplexity(String operator, Object value);

		/**
		 * Filters Taxonomies by their module source metrics code lines value.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofModuleCodeLines(String operator, Object value);

		/**
		 * Filters Taxonomies by their module source metrics comment lines value.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofModuleCommentLines(String operator, Object value);

		/**
		 * Filters Taxonomies by their module source metrics dead code lines value.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofModuleDeadCodeLines(String operator, Object value);
	}
	
	/**
	 * Counts Taxonomies of given filter,
	 * e.g. by module or by project
	 *
	 * @param builder Builder for filter criteria and sorting options.
	 * @return the count
	 */
	long count(BuildingConsumer<TaxonomyInquiryBuilder> builder);
	
	/**
	 * Returns the Taxonomy Types for given filters
	 *
	 * @param builder Builder for filter criteria and sorting options.
	 * @return list of matching {@link TaxonomyTypePojo}s
	 */
	List<TaxonomyTypePojo> findTypes(BuildingConsumer<TaxonomyTypeInquiryBuilder> builder);
	
	/**
	 * Returns the Taxonomy Categories for given filters
	 *
	 * @param builder Builder for filter criteria and sorting options.
	 * @return list of matching {@link TaxonomyTypePojo}s
	 */
	List<TaxonomyCategoryPojo> findCategories(BuildingConsumer<TaxonomyCategoryInquiryBuilder> builder);
	
	/**
	 * Returns the Taxonomy Pojos for given filters
	 *
	 * @param builder Builder for filter criteria and sorting options.
	 * @return list of matching {@link TaxonomyTypePojo}s
	 */
	List<TaxonomyPojo> find(BuildingConsumer<TaxonomyInquiryBuilder> builder);
	
	/**
	 * Returns an Optional<TaxonomyPojo> for given filter
	 *
	 * @param builder Builder for filter criteria and sorting options.
	 * @return an optional Taxonomy Pojo
	 */
	Optional<TaxonomyPojo> findAny(BuildingConsumer<TaxonomyInquiryBuilder> builder);
	
	/**
	 * Returns the Taxonomy Pojos for given filters, Paged.
	 * @param paging Specification for retrieving a subset of a query result.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return list of matching {@link TaxonomyTypePojo}s
	 */
	Paged<TaxonomyPojo> find(Pagination paging, BuildingConsumer<TaxonomyInquiryBuilder> builder);
	
	/**
	 * Returns the {@link EntityId EntityIds} of all {@code taxonomy} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder Builder for filter criteria and sorting options.
	 * @return list of matching {@code taxonomy} {@link EntityId EntityIds}.
	 */
	List<EntityId> findIds(BuildingConsumer<TaxonomyInquiryBuilder> builder);

	/**
	 * Returns aggregation {@code module} values for the filters and selected aggregations in the given {@code builder}.
	 *
	 * @param project the {@code project} for which to get the aggregations
	 * @param aggregationRequest the {@linkplain AggregationRequest} containing the aggregation operations and filter criteria
	 * @return container with the aggregation values
	 */
	/* project is required for caching */
	List<AggregationResult<TaxonomyFieldName>> getAggregations(EntityId project, AggregationRequest<TaxonomyFieldName> aggregationRequest);
	
	/**
	 * Creates a new Taxonomy.
	 * @param taxonomy Initial specification of the Taxonomy. 
	 * @return The ID of the newly created Taxonomy.
	 */
	EntityId create(TaxonomyPojoPrototype taxonomy);

	/**
	 * Creates multiple new Taxonomies.
	 * @param taxonomies specification of the Taxonomies to create.
	 * @return A List of IDs of the newly created Taxonomies, matching the ordering of the given prototypes.
	 */
	List<EntityId> create(List<TaxonomyPojoPrototype> taxonomies);

	/**
	 * Creates a new TaxonomyTypePojo.
	 * @param taxonomyType Initial specification of the TaxonomyType. 
	 * @return The ID of the newly created TaxonomyType.
	 */
	UUID createType(TaxonomyTypePojoPrototype taxonomyType);
	
	/**
	 * Creates a new TaxonomyCategory.
	 * @param taxonomyCategory Initial specification of the TaxonomyCategory. 
	 * @return The ID of the newly created TaxonomyCategory.
	 */
	long createCategory(TaxonomyCategoryPojoPrototype taxonomyCategory);
	
	/**
	 * Modifies an existing Taxonomy.
	 * @param taxonomy Specification of the Taxonomy attributes to modify.
	 */
	void update(TaxonomyPojoPrototype taxonomy);
	
	/**
	 * Modifies an existing TaxonomyType.
	 * @param taxonomyType Specification of the TaxonomyType attributes to modify.
	 */
	void updateType(TaxonomyTypePojoPrototype taxonomyType);
	
	/**
	 * Modifies an existing TaxonomyCategory.
	 * @param taxonomyCategory Specification of the TaxonomyCategory attributes to modify.
	 */
	void updateCategory(TaxonomyCategoryPojoPrototype taxonomyCategory);
	
	/**
	 * Modifies or creates an existing TaxonomyCategory.
	 * @param taxonomyCategory Specification of the TaxonomyCategory attributes to modify.
	 * @return ID of the modified TaxonomyCategory.
	 * 
	 * Not optimal, as put/upsert can hide errors.
	 * But the old Mining used upsert heavily for Category, so we won't refactor it for now.
	 */
	Long upsertCategory(TaxonomyCategoryPojoPrototype taxonomyCategory);
	
	/**
	 * Retrieves a Taxonomy by its ID.
	 * @param taxonomyId ID of the Taxonomy.
	 * @return The {@link TaxonomyPojo}
	 */
	TaxonomyPojo get(EntityId taxonomyId);
	
	/**
	 * Retrieves a TaxonomyCategory by its ID.
	 * @param taxonomyCategoryId ID of the TaxonomyCategory.
	 * @return The {@link TaxonomyCategoryPojo}
	 */
	TaxonomyCategoryPojo getCategory(Long taxonomyCategoryId);
	
	/**
	 * Retrieves a TaxonomyType by its ID.
	 * @param taxonomyId ID of the TaxonomyType.
	 * @return The {@link TaxonomyTypePojo}
	 */
	TaxonomyTypePojo getType(UUID taxonomyId);
	
	/**
	 * Deletes a Taxonomy by its ID.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return the deletion count
	 */
	long delete(BuildingConsumer<TaxonomyInquiryBuilder> builder);
	
	/**
	 * Deletes a Taxonomy Category by its ID.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return the deletion count
	 */
	long deleteCategory(BuildingConsumer<TaxonomyCategoryInquiryBuilder> builder);
	
	/**
	 * Deletes a Taxonomy Type by its ID.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return the deletion count
	 */
	long deleteType(BuildingConsumer<TaxonomyTypeInquiryBuilder> builder);

	/**
	 * Returns a mapping of ModuleId to Taxonomies for all Taxonomies of given inquiry builder
	 * @param builder Builder for filter criteria and sorting options.
	 * @return a mapping from ModuleId to List<Taxonomy>
	 */
	Map<EntityId, List<TaxonomyPojo>> findTaxonomiesPerModule(BuildingConsumer<TaxonomyService.TaxonomyInquiryBuilder> builder);

	/**
	 * Returns a mapping of ModuleId to TaxonomyIds for all Taxonomies of given inquiry builder
	 * @param builder Builder for filter criteria and sorting options.
	 * @return a mapping from ModuleId to List<Taxonomy>
	 */
	Map<EntityId, Set<EntityId>> findTaxonomyIdPerModule(BuildingConsumer<TaxonomyService.TaxonomyInquiryBuilder> builder);

	/**
	 * Returns a mapping of Taxonomy to the Module ids that reference this Taxonomy in given Project. 
	 *
	 * @param builder Builder for filter criteria and sorting options.
	 * @return a mapping from Taxonomy to Module IDs (List<EntityId>) 
	 */
	Map<TaxonomyPojo, Set<EntityId>> findModulesPerTaxonomy(BuildingConsumer<TaxonomyInquiryBuilder> builder);

	/**
	 * Adds a link to the module_taxonomies table from module to each taxonomy.
	 * Creates 1 * taxonomies.size() links. 
	 *
	 * @param module to link all taxonomies to
	 * @param taxonomies to link the module to
	 * @return the ID of the created link
	 */
	long createModuleLinks(EntityId module, Collection<EntityId> taxonomies);
	
	/**
	 * Adds a link to the module_taxonomies table from module to taxonomy.
	 * Creates 1 link. 
	 * 
	 * @param module to link the Taxonomy to
	 * @param taxonomy to link the module to
	 * @return the ID of the created link
	 */
	long createModuleLink(UUID module, EntityId taxonomy);

	/**
	 * Adds a link to the module_taxonomies table from each module to each taxonomy.
	 * Creates modules.size() * taxonomies.size() links. 
	 *
	 * @param modules to link all taxonomies to
	 * @param taxonomies to link all modules to
	 * @return the IDs the of created links
	 */
	long createModuleLinks(final Collection<EntityId> modules, final Collection<EntityId> taxonomies);
	
	/**
	 * Removes all links found for taxonomies matching the inquiry builder
	 *
	 * @param builder Builder for filter criteria and sorting options.
	 * @return the number of rows updated
	 */
	long deleteModuleLinks(BuildingConsumer<TaxonomyInquiryBuilder> builder);

	/**
	 * Returns a mapping of Taxonomy Category to the count of modules transitively assigned to it. 
	 * Also returns categories that are not assigned to any modules
	 *
	 * @param builder Builder for filter criteria and sorting options.
	 * @return a mapping from TaxonomyCategory Id to reference count
	 */
	Map<Long, Long> countModulesPerCategory(BuildingConsumer<TaxonomyCategoryInquiryBuilder> builder);

	/**
	 * Returns a mapping of Taxonomy Type to the count of modules assigned to it. 
	 *
	 * @param builder Builder for filter criteria and sorting options.
	 * @return a mapping from TaxonomyType to reference count 
	 */
	Map<TaxonomyTypePojo, Long> countModulesPerType(BuildingConsumer<TaxonomyTypeInquiryBuilder> builder);

	/**
	 * Returns a map containing the sum of {@code source_metrics} {@code code_lines} per technology for the given {@code project}.
	 *
	 * @param project the {@code project} for which to get the count.
	 * @return map with sums
	 */
	Map<String, Long> countSourceMetricsCodeLinesByTypeName(final EntityId project);
	
	/**
	 * Returns a collection of Module Entity Ids, that have taxonomies assigned.
	 * Filtered by project
	 *
	 * @param builder Builder for filter criteria and sorting options.
	 * @return module ids
	 */
	List<EntityId> findTaxonomyModulesIds(BuildingConsumer<TaxonomyInquiryBuilder> builder);
	
	/**
	 * Retrieves the number of Modules assigned to each Taxonomy in a Project.
	 * @param projectId ID of the project.
	 * @param moduleIds Optional list of IDs limiting the Modules to include in the count.
	 * @return Map of Taxonomy IDs to number of assigned Modules.
	 */
	Map<UUID, Long> getTaxonomyModuleCounts(EntityId projectId, @Nullable Collection<UUID> moduleIds);
	
}
