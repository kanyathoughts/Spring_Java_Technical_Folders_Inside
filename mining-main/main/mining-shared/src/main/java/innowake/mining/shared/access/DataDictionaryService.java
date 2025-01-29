/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.DataDictionaryFieldName;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;

/**
 * Specifies functions for accessing the DataDictionary database entity.
 */
public interface DataDictionaryService {
	
	interface DataDictionaryInquiryBuilder extends CustomPropertiesService.CustomPropertiesInquiryBuilder {
		
		/**
		 * Finds a {@linkplain DataDictionaryPojo Data Dictionary} for the provided ID.
		 *
		 * @param id the ID of Data Dictionary
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder byId(EntityId id);

		/**
		 * Finds a {@linkplain DataDictionaryPojo Data Dictionary} for the provided ID.
		 *
		 * @param ids the ID of Data Dictionaries
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder byIds(Collection<EntityId> ids);

		/**
		 * Finds all {@linkplain DataDictionaryPojo Data Dictionary entities} for the provided numeric IDs.
		 *
		 * @param nids the numeric IDs of Data Dictionary entities
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder byNids(Collection<Long> nids);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@linkplain ModulePojo Module} ID.
		 *
		 * @param module the ID of Module
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder ofModule(EntityId module);

		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@linkplain ModulePojo Module} IDs.
		 *
		 * @param modules the IDs of the Modules
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder ofModules(Collection<EntityId> modules);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@linkplain ModulePojo Module} IDs.
		 * 
		 * @param modules the UUIDs
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder ofModuleUuids(Collection<UUID> modules);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@linkplain ModulePojo Module} IDs.
		 * 
		 * @param modules the numeric IDs
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder ofModuleNids(Collection<Long> modules);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@linkplain ModulePojo Module} name.
		 *
		 * @param moduleName the name of the Module
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder ofModuleName(String moduleName);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@linkplain ModulePojo Module} path.
		 *
		 * @param path the path of the Module
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder ofModulePath(String path);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@linkplain ModulePojo Module} project.
		 *
		 * @param project the project of the Module
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder ofModuleProject(EntityId project);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@code isBusiness}.
		 *
		 * @param isBusiness the boolean value to filter by
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withIsBusiness(boolean isBusiness);

		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@code data flow ids}.
		 * @param dataFlowIds the data flow id from data lineage
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withDataFlowIds(Collection<String> dataFlowIds);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@linkplain DefinedLocation}
		 *
		 * @param locations the location values to filter by
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withDefinedLocations(Collection<DefinedLocation> locations);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@code fieldTransformation}
		 *
		 * @param transformation the field transformation
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withFieldTransformation(String transformation);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@code usages}
		 *
		 * @param usages the field usages
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withUsages(Collection<String> usages);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@code picClause}
		 *
		 * @param picClause the PIC clause
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withPicClause(String picClause);

		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@code isReferenced}.
		 *
		 * @param isReferenced the boolean value to filter by
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withIsReferenced(boolean isReferenced);

		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@code isReferenced}.
		 * 
		 * @param values filter values
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withIsReferencedIn(Collection<Boolean> values);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided scopes.
		 *
		 * @param scopes the scopes to filter by
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withScopes(Collection<String> scopes);

		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided scope attributes.
		 *
		 * @param scope the scope to filter by
		 * @param attribute the scope attribute to filter by
		 * @param attributes the scope attribute values to filter by
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withScopeAttributes(DataDictionaryVariableScope scope, String attribute, Collection<String> attributes);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@linkplain WorkingState WorkingStates}.
		 *
		 * @param states the {@linkplain WorkingState}s to filter by
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withStates(Collection<WorkingState> states);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided name.
		 *
		 * @param name the name
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withName(String name);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided location.
		 *
		 * @param location the location
		 * @param isWithin {@code true} if the location should be matched for values within the provided location
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withLocation(ModuleLocation location, boolean isWithin);

		/**
		 * Filters {@linkplain DataDictionaryPojo Data Dictionaries} by their {@link ModuleLocation} offset. A {@linkplain DataDictionaryPojo Data Dictionary}
		 * matches when its offset is greater than or equals with the given {@code offset}.
		 *
		 * @param offset the offset to filter
		 * @return this instance for method chaining 
		 */
		DataDictionaryInquiryBuilder withMinOffset(Integer offset);

		/**
		 * Filters {@linkplain DataDictionaryPojo Data Dictionaries} by their {@link ModuleLocation} offset. A {@linkplain DataDictionaryPojo Data Dictionary}
		 * matches when its offset is between {@code start} and {@code end} ({@code start} &lt= {@code offset} &lt= {@code end}).
		 *
		 * @param start the offset to filter
		 * @param end the offset to filter
		 * @return this instance for method chaining 
		 */
		DataDictionaryInquiryBuilder withOffsetBetween(final Integer start, final Integer end);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@code sourceInput}.
		 *
		 * @param input the source input
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withSourceInput(String input);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@code targetOutput}.
		 *
		 * @param output the target output
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withTargetOutput(String output);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided field formats.
		 *
		 * @param formats the field formats to filter by
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withFormats(Collection<String> formats);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@code parentGroup}.
		 *
		 * @param group the parent group
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withGroupField(String group);
		
		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided description.
		 *
		 * @param description the description
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withDescription(String description);

		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@code initialValue}.
		 *
		 * @param initialValue the initial value
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withInitialValue(String initialValue);

		/**
		 * Finds {@linkplain DataDictionaryPojo Data Dictionaries} for the provided {@code parentGroup}.
		 *
		 * @param parentGroup the initial value
		 * @return Filter builder
		 */
		DataDictionaryInquiryBuilder withParentGroup(String parentGroup);
		
		/**
		 * Finds Data Dictionaries linked to a Business Rule
		 * @param annotation the Annotation Entity ID
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder ofAnnotation(EntityId annotation);
		
		/**
		 * Finds Data Dictionaries linked to a Business Rule
		 * @param annotation the Annotation Entity IDs
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder ofAnnotations(Collection<EntityId> annotation);

		/**
		 * Finds Data Dictionaries that are not linked to any Business Rules
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder notOfAnnotations();

		/**
		 * Orders Data Dictionaries by module name
		 * @param direction the sort direction
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder sortModuleName(SortDirection direction);
		
		/**
		 * Orders Data Dictionaries by numeric ids
		 * @param direction the sort direction
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder sortNid(SortDirection direction);

		/**
		 * Orders Data Dictionaries by name
		 * @param direction the sort direction
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder sortName(SortDirection direction);
		
		/**
		 * Orders Data Dictionaries by field level
		 * @param direction the sort direction
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder sortFieldLevel(SortDirection direction);
		
		/**
		 * Orders Data Dictionaries by {@code length}
		 * @param direction the sort direction
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder sortLength(SortDirection direction);

		/**
		 * Orders Data Dictionaries by {@code parentGroup}
		 * @param direction the sort direction
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder sortParentGroup(SortDirection direction);
		
		/**
		 * Orders Data Dictionaries by {@code translatedFieldValue}
		 * @param direction the sort direction
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder sortTranslatedFieldValue(SortDirection direction);
		
		/**
		 * Orders Data Dictionaries by name
		 * @param direction the sort direction
		 * @return filter builder
		 */
		DataDictionaryInquiryBuilder sortCustomProperties(SortDirection direction);
	}

	interface DataDictionaryAggregationInquiryBuilder<B extends DataDictionaryAggregationInquiryBuilder<B>> extends AggregationInquiryBuilder<DataDictionaryFieldName, B> {

		/**
		 * Filters Data Dictionaries by project id.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofProject(String operator, Object value);

		/**
		 * Filters Data Dictionaries by ID
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B byId(String operator, Object value);

		/**
		 * Filters Data Dictionaries by name
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withName(String operator, Object value);

		/**
		 * Filters Data Dictionaries by description
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withDescription(String operator, Object value);

		/**
		 * Filters Data Dictionaries by format
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withFormat(String operator, Object value);

		/**
		 * Filters Data Dictionaries by length
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withLength(String operator, Object value);

		/**
		 * Filters Data Dictionaries by other scope name
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withOtherScope(String operator, Object value);

		/**
		 * Filters Data Dictionaries by other scope source
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withOtherScopeSource(String operator, Object value);

		/**
		 * Filters Data Dictionaries by created by user ID
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withCreatedByUserId(String operator, Object value);

		/**
		 * Filters Data Dictionaries by updated by user ID
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withUpdatedByUserId(String operator, Object value);

		/**
		 * Filters Data Dictionaries by module technology
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B ofModuleTechnology(String operator, Object value);

		/**
		 * Filters Data Dictionaries by module type
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B ofModuleType(String operator, Object value);

		/**
		 * Filters Data Dictionaries by scope
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withScope(String operator, Object value);

		/**
		 * Filters Data Dictionaries by scope attributes
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withScopeAttribute(String operator, Object value);

		/**
		 * Filters Data Dictionaries by the scope attribute "PARAMETER" -> accessType"
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withScopeAccessType(String operator, Object value);

		/**
		 * Filters Data Dictionaries by is candidate
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withIsCandidate(String operator, Object value);

		/**
		 * Filters Data Dictionaries by PIC clause
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withPicClause(String operator, Object value);

		/**
		 * Filters Data Dictionaries by defined location
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withDefinedLocation(String operator, Object value);

		/**
		 * Filters Data Dictionaries by state
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withState(String operator, Object value);

		/**
		 * Filters Data Dictionaries by is business
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withIsBusiness(String operator, Object value);

		/**
		 * Filters Data Dictionaries by field transformation
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withFieldTransformation(String operator, Object value);

		/**
		 * Filters Data Dictionaries by source input
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withSourceInput(String operator, Object value);

		/**
		 * Filters Data Dictionaries by target output
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withTargetOutput(String operator, Object value);

		/**
		 * Filters Data Dictionaries by is referenced
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withIsReferenced(String operator, Object value);

		/**
		 * Filters Data Dictionaries by field usage
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withFieldUsage(String operator, Object value);

		/**
		 * Filters Data Dictionaries by taxonomy
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withTaxonomy(String operator, Object value);
	}

	/**
	 * Counts all Data Dictionary based on provided {@code QueryBuilder}
	 *
	 * @param qb the {@code QueryBuilder}
	 * @return the count of {@link DataDictionaryPojo}s
	 */
	Long count(BuildingConsumer<DataDictionaryInquiryBuilder> qb);

	/**
	 * Finds a Data Dictionary entry by its ID.
	 *
	 * @param qb the {@code QueryBuilder}
	 * @return The {@link DataDictionaryPojo} if it exists.
	 */
	DataDictionaryPojo get(BuildingConsumer<DataDictionaryInquiryBuilder> qb);

	/**
	 * Returns aggregation {@code data_dictionary} values for the filters and selected aggregations in the given {@code request}.
	 *
	 * @param project the {@code project} for which to get the aggregations
	 * @param request the {@linkplain AggregationRequest} containing the aggregation operations and filter criteria
	 * @return container with the aggregation values
	 */
	/* project is required for caching */
	List<AggregationResult<DataDictionaryFieldName>> getAggregations(EntityId project, AggregationRequest<DataDictionaryFieldName> request);

	/**
	 * Finds all Data Dictionary based on provided {@code QueryBuilder}
	 *
	 * @param qb the {@code QueryBuilder}
	 * @return the List of {@link DataDictionaryPojo}s
	 */
	List<DataDictionaryPojo> find(BuildingConsumer<DataDictionaryInquiryBuilder> qb);

	/**
	 * Finds paged Data Dictionaries based on provided {@code QueryBuilder}
	 *
	 * @param pagination the pagination details
	 * @param qb the {@code QueryBuilder}
	 * @return the List of {@link DataDictionaryPojo}s
	 */
	Paged<DataDictionaryPojo> find(Pagination pagination, BuildingConsumer<DataDictionaryInquiryBuilder> qb);

	/**
	 * Finds one Data Dictionary based on provided {@code QueryBuilder}
	 *
	 * @param qb the {@code QueryBuilder}
	 * @return the optional of {@link DataDictionaryPojo}s
	 */
	Optional<DataDictionaryPojo> findAny(BuildingConsumer<DataDictionaryInquiryBuilder> qb);

	/**
	 * Creates a new Data Dictionary
	 *
	 * @param dde the Data Dictionary
	 * @return the created Entity
	 */
	DataDictionaryPojo create(DataDictionaryPojoPrototype dde);

	/**
	 * Creates multiple Data Dictionaries that were identified as candidates
	 * 
	 * @param identifiedEntries the Data Dictionaries that were identified
	 * @return the number of Data Dictionaries stored/updated
	 */
	Long createCandidates(Collection<DataDictionaryPojoPrototype> identifiedEntries);

	/**
	 * Updates a new Data Dictionaries
	 *
	 * @param dde the Data Dictionary
	 * @return the updated Entity
	 */
	DataDictionaryPojo update(DataDictionaryPojoPrototype dde);
	
	/**
	 * This method allows you to update multiple Data Dictionaries with the same values.
	 * 
	 * <p>Updates all Data Dictionaries that match with the filters in the given {@code builder}. The defined (set) fields in the given {@code values} are used
	 * for building the update query.</p>
	 *
	 * @param builder the {@linkplain DataDictionaryInquiryBuilder} containing the filter criteria
	 * @param values the {@link DataDictionaryPojoPrototype} containing the to be updated fields
	 * @return the number of updated {@code data dictionaries}, 0 if no data dictionary matched with the filer
	 */
	int update(BuildingConsumer<DataDictionaryInquiryBuilder> builder, DataDictionaryPojoPrototype values);
	
	/**
	 * Update the Data Dictionaries with provided IDs to be business variables
	 *
	 * @param ids the Data Dictionary IDs
	 */
	void markAsBusinessVariables(Collection<EntityId> ids);

	/**
	 * Deletes Data Dictionaries based on query builder
	 * @param qb the {@code QueryBuilder}
	 * @return the number of records deleted
	 */
	int delete(BuildingConsumer<DataDictionaryInquiryBuilder> qb);
	
	/**
	 * Link Annotations to Data Dictionaries
	 * @param dde the Data Dictionary ID
	 * @param annotation the Annotation ID
	 */
	void linkAnnotations(EntityId dde, EntityId annotation);

	/**
	 * Returns all {@code data_dictionary_other_scope} entities for the given {@code project}.
	 *
	 * @param project the project to query for
	 * @return list of DataDictionaryOtherScopes
	 */
	List<String> findDataDictionaryOtherScope(EntityId project);
	
	/**
     * This method replaces all occurrences of the field in the corresponding annotation translation only if the translated field has changed
     * 
     * @param projectId Id of the project 
     * @param ddeId Id of the old DDE
     * @param dataDictionaryEntry The new DDE to update
	 * @param userId The Id of the user
     */
	void updateRelatedAnnotationsEnglishTranslation(EntityId projectId, EntityId ddeId, DataDictionaryPojoPrototype dataDictionaryEntry, String userId);

	/**
	 * Returns all Data Dictionaries Ids that are linked to the provided Data Flow IDs.
	 *
	 * @param dataFlowIds the Data Flow IDs
	 * @return all Data Dictionaries Ids that are linked to the provided Data Flow IDs
	 */
	List<EntityId> getDataDictionaryIdsFromDataFlowIds(List<String> dataFlowIds);
}
