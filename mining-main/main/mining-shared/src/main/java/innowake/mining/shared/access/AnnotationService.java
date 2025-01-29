/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.CustomPropertiesService.CustomPropertiesInquiryBuilder;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationFieldName;
import innowake.mining.shared.model.AnnotationReport;
import innowake.mining.shared.model.AnnotationReportSearchParameter;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;

/**
 * Specifies functions for accessing the Annotation database entity.
 */
public interface AnnotationService {
	
	interface AnnotationInquiryBuilder extends CustomPropertiesInquiryBuilder {
		AnnotationInquiryBuilder sortNid(SortDirection direction);
		AnnotationInquiryBuilder sortName(SortDirection direction);
		AnnotationInquiryBuilder sortType(SortDirection direction);
		AnnotationInquiryBuilder sortState(SortDirection direction);
		AnnotationInquiryBuilder sortCategory(SortDirection direction);
		AnnotationInquiryBuilder sortReasons(SortDirection direction);
		/**
		 * Sorts the result by the Name of the module
		 * @param direction sorting direction, ASC or DESC
		 * @return this instance for method chaining
		 */
		AnnotationInquiryBuilder sortByModuleName(SortDirection direction);
		AnnotationInquiryBuilder sortByFunctionalGroupName(SortDirection direction);

		/**
		 * Sorts the annotations by the annotations module location.
		 * @param direction sort direction
		 * @return this instance for method chaining
		 */
		AnnotationInquiryBuilder sortByModuleLocation(SortDirection direction);
		AnnotationInquiryBuilder sortByCustomProperties(SortDirection direction);
		
		AnnotationInquiryBuilder byId(EntityId id);
		AnnotationInquiryBuilder byIds(Collection<EntityId> ids);
		AnnotationInquiryBuilder byNids(Collection<Long> ids);
		AnnotationInquiryBuilder byMinId(EntityId id);
		AnnotationInquiryBuilder byMaxId(EntityId id);
		AnnotationInquiryBuilder ofProject(EntityId project);
		AnnotationInquiryBuilder ofDataDictionaryEntry(EntityId dde);
		AnnotationInquiryBuilder withType(AnnotationType types);
		AnnotationInquiryBuilder withTypes(Collection<AnnotationType> types);
		AnnotationInquiryBuilder withState(WorkingState states);
		AnnotationInquiryBuilder withStates(Collection<WorkingState> states);
		AnnotationInquiryBuilder withCategory(@Nullable String categoryName);
		AnnotationInquiryBuilder withCategoryNames(Collection<String> categoryNames);
		AnnotationInquiryBuilder withCategories(Collection<Long> categories);
		AnnotationInquiryBuilder withName(String name);
		AnnotationInquiryBuilder withReason(@Nullable String reason);
		AnnotationInquiryBuilder withReasons(@Nullable Collection<String> reasons);
		AnnotationInquiryBuilder withModuleName(String name, boolean caseInsensitive);
		AnnotationInquiryBuilder withLikeModuleName(String name, boolean caseInsensitive);
		AnnotationInquiryBuilder withModulePath(String path);
		AnnotationInquiryBuilder withLocation(ModuleLocation location);
		AnnotationInquiryBuilder withLocationInRange(ModuleLocation location);
		AnnotationInquiryBuilder withOffsetBetween(Integer start, Integer end);
		AnnotationInquiryBuilder withMinOffset(Integer offset);
		
		//functional blocks
		AnnotationInquiryBuilder withFunctionalGroupName(String name);
		AnnotationInquiryBuilder withoutFunctionalGroupAssignment();
		AnnotationInquiryBuilder withFunctionalBlocks(final List<UUID> uids);

		//taxonomies
		AnnotationInquiryBuilder withTaxonomyNames(Collection<String> name);
		AnnotationInquiryBuilder withTaxonomyTypeName(String typeName);
		AnnotationInquiryBuilder notWithTaxonomyTypeName(String typeName);

		//module
		AnnotationInquiryBuilder ofModule(EntityId module);
		AnnotationInquiryBuilder ofModules(Collection<EntityId> modules);
		AnnotationInquiryBuilder withModuleType(Type type);
		AnnotationInquiryBuilder withModuleTypes(Collection<Type> types);
		AnnotationInquiryBuilder withModuleTechnology(Technology technologyName);
		AnnotationInquiryBuilder withModuleTechnologies(Collection<Technology> technologyNames);
		

		/**
		 * Filters annotations based on if they have a source or not.
		 * Ignores case
		 *
		 * @param hasSource {@code true} to filter annotations having a source. Otherwise {@code false}.
		 * @return this instance for method chaining
		 */
		AnnotationInquiryBuilder filterHasSource(boolean hasSource);

		/**
		 * Filters annotations based on their source content
		 *
		 * @param content to filter if the source attachment contains given content
		 * @return this instance for method chaining
		 */
		AnnotationInquiryBuilder filterSourceContains(String content);
	}

	interface AnnotationCategoryInquiryBuilder {
		AnnotationCategoryInquiryBuilder ofProject(EntityId project);
		
		/**
		 * Filters annotations categories by project, where the Project is either the default (Nid = 0) or given {@code project}.
		 *
		 * @param project the project to select
		 * @return this instance for method chaining
		 */
		AnnotationCategoryInquiryBuilder ofProjectWithDefault(EntityId project);
		AnnotationCategoryInquiryBuilder byId(Long id);
		AnnotationCategoryInquiryBuilder withName(String name);
		AnnotationCategoryInquiryBuilder withTypes(Collection<AnnotationType> types);
	}

	interface AnnotationAggregationInquiryBuilder<B extends AnnotationAggregationInquiryBuilder<B>> extends AggregationInquiryBuilder<AnnotationFieldName, B> {

		/**
		 * Filters annotations by their id.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B byId(String operator, Object value);

		/**
		 * Filters annotations by their name.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withName(String operator, Object value);

		/**
		 * Filters annotations by their project id.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofProject(String operator, Object value);

		/**
		 * Filters annotations by their category.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withCategory(String operator, Object value);

		/**
		 * Filters annotations by their state.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withState(String operator, Object value);

		/**
		 * Filters annotations by their type.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withType(String operator, Object value);

		/**
		 * Filters annotations by their creator user id.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withCreatedByUserId(String operator, Object value);

		/**
		 * Filters annotations by their updater user id.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withUpdatedByUserId(String operator, Object value);

		/**
		 * Filters annotations by their source attachment.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withSourceAttachment(String operator, Object value);

		/**
		 * Filters annotations by their module technology.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withModuleTechnology(String operator, Object value);

		/**
		 * Filters annotations by their module type.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withModuleType(String operator, Object value);

		/**
		 * Filters annotations by their metadata.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withMetadata(String operator, Object value);

		/**
		 * Filters annotations by taxonomy.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withTaxonomy(String operator, Object value);
	}
	
	AnnotationPojo get(EntityId id);
	AnnotationPojo get(BuildingConsumer<AnnotationInquiryBuilder> builder);
	List<AnnotationPojo> find(BuildingConsumer<AnnotationInquiryBuilder> builder);
	Optional<AnnotationPojo> findAny(BuildingConsumer<AnnotationInquiryBuilder> builder);

	/**
	 * Gets the aggregation results for the given request.
	 *
	 * @param project the {@code project} for which to get the aggregations
	 * @param request the aggregation request
	 * @return the aggregation results
	 */
	/* project is required for caching */
	List<AggregationResult<AnnotationFieldName>> getAggregations(EntityId project, AggregationRequest<AnnotationFieldName> request);

	long count(BuildingConsumer<AnnotationInquiryBuilder> builder);
	Paged<AnnotationPojo> find(Pagination page, BuildingConsumer<AnnotationInquiryBuilder> builder);
	EntityId create(AnnotationPojoPrototype annotation);
	EntityId update(AnnotationPojoPrototype annotation);
	int[][] update(List<AnnotationPojoPrototype> annotation, int batchSize);
	Map.Entry<EntityId, EntityId> delete(EntityId project, EntityId annotation);
	Map<EntityId, EntityId> delete(BuildingConsumer<AnnotationInquiryBuilder> builder);
	
	AnnotationCategory getCategory(EntityId project, Long id);
	Optional<AnnotationCategory> findCategory(final BuildingConsumer<AnnotationCategoryInquiryBuilder> builder);
	List<AnnotationCategory> findCategories(BuildingConsumer<AnnotationCategoryInquiryBuilder> builder);
	List<Long> findCategoryIds(BuildingConsumer<AnnotationCategoryInquiryBuilder> builder);
	List<AnnotationType> findDeclaredTypes(EntityId project);
	
	long createCategory(EntityId project, String name, Collection<AnnotationType> types);
	void updateCategory(EntityId project, Long id, Optional<String> name, Optional<Collection<AnnotationType>> types);
	void deleteCategory(EntityId project, Long id);
	
	Paged<AnnotationReport> getReport(EntityId project, AnnotationReportSearchParameter parameters, int limit);
	
	/**
	 * Find source attachment which belongs to {@link AnnotationPojo}.
	 *
	 * @param annotation the {@linkplain AnnotationPojo} with the selected code.
	 * @param projectId the Project ID.
	 * @param moduleId the {@linkplain Module} ID.
	 * @return the Source Attachment of {@link AnnotationPojo}.
	 */
	@Nullable
	String getContent(AnnotationPojo annotation, EntityId projectId, EntityId moduleId);

	/**
	 * Returns the numeric id from the given {@code annotationId}. If {@code annotationId} contains no numeric id, then the nid is fetched from the DB.
	 *
	 * @param annotationId the annotation {@link EntityId}
	 * @return the numeric id of the project
	 */
	Long getNid(EntityId annotationId);

	/**
	 * Returns a Map of annotation Id with assigned value(s) for the given custom property.
	 *
	 * @param projectId ID of Project
	 * @param customPropertyName the custom property name
	 * @return Map of annotation Id with assigned default values of given custom property
	 */
	Map<Long, Object> getCustomProperties(EntityId projectId, String customPropertyName);
}
