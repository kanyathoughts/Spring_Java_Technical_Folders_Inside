/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import javax.persistence.EntityNotFoundException;

import org.apache.commons.lang3.tuple.Pair;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.DependencyDefinitionPojoPrototype;
import innowake.mining.shared.entities.ErrorMarkerPojo;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.entities.ModuleDeadCodePojo;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ModuleUndiscoveredPojo;
import innowake.mining.shared.entities.ModuleUndiscoveredPojoPrototype;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SchemaInfoPojo;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.HotSpot;
import innowake.mining.shared.model.HotSpot.FilterType;
import innowake.mining.shared.model.LinkedModule;
import innowake.mining.shared.model.ModuleFieldName;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleStatisticsResponse;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipFieldName;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.StatementFieldName;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.shared.model.dependency.graph.DependencyGraph;
import innowake.mining.shared.model.dependency.graph.NodeType;
import innowake.mining.shared.model.discovery.ErrorKey;

/**
 * Functions for accessing module and module related database entities.
 */
public interface ModuleService {
	
	public enum MetricField {
		PHYSICAL_LINES,
		CODE_LINES,
		COMMENT_LINES,
		COMPLEXITY_MCCABE,
		DEAD_CODE_LINES;
	}
	
	public enum RelationshipField {
		TYPE,
		DIRECTION,
		SOURCE,
		DESTINATION,
		SOURCE_LOCATION,
		DESTINATION_LOCATION;
	}
	
	public interface ModuleOrderBuilder<B extends ModuleOrderBuilder<B>> {
		/**
		 * Sorts modules by {@code metrics_date} in the given {@code sortDirection}.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortMetricsDate(SortDirection sortDirection);

		/**
		 * Sorts modules by {@code nid} in the given {@code sortDirection}.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortId(SortDirection sortDirection);

		/**
		 * Sorts modules by {@code name} in the given {@code sortDirection}.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortName(SortDirection sortDirection);

		/**
		 * Sorts modules by {@code path} in the given {@code sortDirection}.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortPath(SortDirection sortDirection);
		
		/**
		 * Sorts modules by {@code technology} in the given {@code sortDirection}.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortTechnology(SortDirection sortDirection);

		/**
		 * Sorts modules by {@code technology} in the given {@code sortDirection}.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortCreator(SortDirection sortDirection);

		/**
		 * Sorts modules by {@code type} in the given {@code sortDirection}.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortType(SortDirection sortDirection);
		
		/**
		 * Sorts modules by {@code requiresReview} in the given {@code sortDirection}.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortRequiresReview(SortDirection sortDirection);
		
		/**
		 * Sort modules by modification time.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortModified(SortDirection sortDirection);
		
		/**
		 * Sort Modules by a metric.
		 * @param field Field of the metric.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortMetric(MetricField field, SortDirection sortDirection);
		/**
		 * Sort Modules by error count.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortErrorCount(SortDirection sortDirection);
		
		/**
		 * Sorts modules by a dynamically selected field.
		 * @param dynamicAlias Name of the field.
		 * @param sortDirection Direction in which to sort.
		 * @return this instance for method chaining
		 */
		B sortBy(String dynamicAlias, SortDirection sortDirection);
		
		B sortCustomProperties(SortDirection sortDirection);

		/**
		 * Sorts the result by the Name of the Functional Block
		 * @param direction sorting direction, ASC or DESC
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder sortByReachabilityBlockName(SortDirection direction);
	}

	/**
	 * Builder for filtering the containing modules.
	 * @param <B> type of inquiry builder
	 */
	interface ModuleLightInquiryBuilder<B extends ModuleLightInquiryBuilder<B>> {

		/**
		 * Filters modules by a project
		 * 
		 * @param project ID of the project
		 * @return this instance for method chaining
		 */
		B ofProject(EntityId project);

		/**
		 * Filters modules by source {@link UUID UUIDs}
		 * 
		 * @param sourceIds the {@link UUID UUIDs} of the sources
		 * @return this instance for method chaining
		 */
		B ofSources(Collection<UUID> sourceIds);

		/**
		 * Filters modules based on if they have a source or not
		 * 
		 * @param hasSource {@code true} to filter modules having a source. Otherwise {@code false}.
		 * @return this instance for method chaining
		 */
		B filterHasSource(boolean hasSource);

		/**
		 * Filters modules by numeric or unique id
		 * 
		 * @param id {@linkplain EntityId} of the module
		 * @return this instance for method chaining
		 */
		B byId(EntityId id);

		/**
		 * Filters modules by ids. A module matches when its id matches with one of the {@code ids}.
		 * 
		 * @param ids module ids
		 * @return this instance for method chaining
		 */
		B byIds(Collection<EntityId> ids);

		/**
		 * Filters modules by ids. A module matches when its id does not match with any of the {@code ids}.
		 * 
		 * @param ids module ids
		 * @return this instance for method chaining
		 */
		B notByIds(Collection<EntityId> ids);

		/**
		 * Filters modules by unique id
		 * 
		 * @param uid unique module id
		 * @return this instance for method chaining
		 */
		B byUid(UUID uid);

		/**
		 * Filters modules by unique ids. A module matches when its unique id matches with one of the entries in {@code uids}.
		 * 
		 * @param uids unique module ids
		 * @return this instance for method chaining
		 */
		B byUids(Collection<UUID> uids);

		/**
		 * Filters modules by numeric id
		 * 
		 * @param nid numeric module id
		 * @return this instance for method chaining
		 */
		B byNid(Long nid);

		/**
		 * Filters modules by numeric ids. A module matches when its numeric id matches with one of the entries in {@code nids}.
		 * 
		 * @param nids numeric module ids
		 * @return this instance for method chaining
		 */
		B byNids(Collection<Long> nids);

		/**
		 * Filters modules by Annotation
		 *
		 * @param annotation ID of the Annotation
		 * @return this instance for method chaining
		 */
		B withAnnotation(EntityId annotation);

		/**
		 * Filters modules by {@link Creator}
		 * 
		 * @param creator the module creator
		 * @return this instance for method chaining
		 */
		B withCreator(Creator creator);
		
		/**
		 * Filters modules by {@link Creator}
		 * 
		 * @param creator the module creator
		 * @return this instance for method chaining
		 */
		B withCreators(Collection<Creator> creator);

		/**
		 * Filters modules by description
		 * 
		 * @param description the module description
		 * @return this instance for method chaining
		 */
		B withDescription(String description);

		/**
		 * Filters for modules that have errors.
		 *
		 * @return this instance for method chaining
		 */
		B withErrors();

		/**
		 * Filters modules by identified flag
		 * 
		 * @param identified the module identified flag
		 * @return this instance for method chaining
		 */
		B withIdentified(boolean identified);

		/**
		 * Filters modules by link hash
		 * 
		 * @param linkHash the link hash
		 * @return this instance for method chaining
		 */
		B withLinkHash(String linkHash);

		/**
		 * Filters modules by link hashes
		 *
		 * @param linkHashes the link hashes
		 * @return this instance for method chaining
		 */
		B withLinkHashes(Collection<String> linkHashes);

		/**
		 * Filters modules by metrics_date
		 * 
		 * @param metricsDate the module metrics_date
		 * @return this instance for method chaining
		 */
		B withMetricsDate(Instant metricsDate);

		/**
		 * Filters modules with metrics date after metrics_date
		 *
		 * @param metricsDate the module metrics_date
		 * @return this instance for method chaining
		 */
		B withMetricsDateAfter(Instant metricsDate);

		/**
		 * Filters modules by name
		 * 
		 * @param name the module name
		 * @return this instance for method chaining
		 */
		B withName(String name);

		/**
		 * Filters modules by name
		 *
		 * @param name the module name
		 * @param caseInsensitive {@code true} to match names case insensitive. {@code false} to match case sensitive (default)
		 * @return this instance for method chaining
		 */
		B withName(String name, boolean caseInsensitive);

		/**
		 * Filters modules by name
		 * 
		 * @param name the module name
		 * @return this instance for method chaining
		 */
		B notWithNameLike(String name);

		/**
		 * Filters modules by names (case sensitive). A module matches when its name matches with one of the entries in {@code names}.
		 * 
		 * @param names the module names
		 * @return this instance for method chaining
		 */
		B withNames(Collection<String> names);

		/**
		 * Filters modules by names. A module matches when its name matches with one of the entries in {@code names}.
		 * 
		 * @param names the module names
		 * @param caseInsensitive {@code true} to match names case insensitive. {@code false} to match case sensitive (default)
		 * @return this instance for method chaining
		 */
		B withNames(Collection<String> names, boolean caseInsensitive);

		/**
		 * Filters modules by {@link Origin}
		 * 
		 * @param origin the origin
		 * @return this instance for method chaining
		 */
		B withOrigin(Origin origin);

		/**
		 * Filters modules by path
		 * 
		 * @param path the module path
		 * @return this instance for method chaining
		 */
		B withPath(String path);

		/**
		 * Filters modules having a path but no source {@link UUID}
		 * 
		 * @return this instance for method chaining
		 */
		B filterHasPathNoSource();

		/**
		 * Filters modules by representation
		 * 
		 * @param representation the type of representation
		 * @return this instance for method chaining
		 */
		B withRepresentation(Representation representation);

		/**
		 * Filters modules by {@code requires_review}.
		 *
		 * @param requiresReview the requires review value
		 * @return this instance for method chaining
		 */
		B withRequiresReview(boolean requiresReview);

		/**
		 * Filters modules by technologies and types. A module matches when its technology and type matches with one of the pairs in {@code technologiesAndTypes}.
		 * 
		 * @param technologiesAndTypes {@link Tuple2 tuples} of {@link Technology} and {@link Type}
		 * @return this instance for method chaining
		 */
		B withTechnologiesAndTypes(Collection<Tuple2<Technology, Type>> technologiesAndTypes);

		/**
		 * Filters out modules by technologies and types. A module matches when its technology and type matches with one of the pairs in {@code technologiesAndTypes}.
		 *
		 * @param technologiesAndTypes {@link Tuple2 tuples} of {@link Technology} and {@link Type}
		 * @return this instance for method chaining
		 */
		B notWithTechnologiesAndTypes(Collection<Tuple2<Technology, Type>> technologiesAndTypes);

		/**
		 * Filters modules by technology
		 * 
		 * @param technology the technology
		 * @return this instance for method chaining
		 */
		B withTechnology(Technology technology);

		/**
		 * Filters modules by technologies
		 *
		 * @param technology the technology
		 * @return this instance for method chaining
		 */
		B withTechnologies(Collection<Technology> technology);

		/**
		 * Filters modules by type
		 * 
		 * @param type the type
		 * @return this instance for method chaining
		 */
		B withType(Type type);

		/**
		 * Filters modules by types. A module matches when its type matches with one of the entries in {@code types}.
		 * 
		 * @param types the types
		 * @return this instance for method chaining
		 */
		B withTypes(Collection<Type> types);

		/**
		 * Filters modules by paths. A module matches when its own path or the path of its containing module matches with one of the entries in {@code paths}.
		 * 
		 * @param project the project of the containing module
		 * @param paths the module paths
		 * @return this instance for method chaining
		 */
		B withPathsSelfOrContaining(EntityId project, Collection<String> paths);

		/**
		 * Filters modules by path patterns. A module matches when its own path or the path of its containing module with one of the entries in {@code patterns}.
		 * 
		 * @param project the project of the containing module
		 * @param patterns regex patterns
		 * @return this instance for method chaining
		 */
		B withPathPatternsSelfOrContaining(EntityId project, Collection<String> patterns);

		/**
		 * Filters modules by {@link Storage}.
		 *
		 * @param storage the module storage 
		 * @return this instance for method chaining
		 */
		B withStorage(Storage storage);
	}

	interface ModuleInquiryBuilder extends ModuleLightInquiryBuilder<ModuleInquiryBuilder>, ModuleOrderBuilder<ModuleInquiryBuilder>, 
			QueryLimitBuilder<ModuleInquiryBuilder> {

		/**
		 * Filters for all modules for which a relationship with any of the relationships in {@code types} exists and with the module being the {@code dst} module.
		 * 
		 * @param types the types of the module relationships or empty for no filtering by {@link RelationshipType} types
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withSourceRelationships(RelationshipType... types);

		/**
		 * Filters for all modules having any of the relationships in {@code types} with the given {@code module} as source module.
		 *
		 * @param moduleId the id of the source module 
		 * @param types the types of the module relationships or empty for no filtering by {@link RelationshipType} types
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withSourceRelationshipsFrom(EntityId moduleId, RelationshipType... types);
	
		/**
		 * Filters for all modules having any of the relationships in {@code types} with the given {@code module}
		 * or any by this module referenced module with the same type.
		 * 
		 * Does so recursively, e.g. if filtering for Module C and module Id A,
		 * if A includes ModuleInquiryBuilder which includes C, this method finds Module C
		 * 
		 * @param moduleId the id of the source module 
		 * @param types the types of the module relationships or empty for no filtering by {@link RelationshipType} types
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withTransitiveSourceRelationshipsFrom(EntityId moduleId, RelationshipType... types);

		/**
		 * Filters for all modules having any of the relationships in {@code types} with modules matching the given {@code other} filters as source module.
		 * 
		 * @param other the {@link ModuleLightInquiryBuilder} containing the filters for the referenced resp. referencing module
		 * @param types the types of the module relationships or empty for no filtering by {@link RelationshipType} types
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withSourceRelationshipsFrom(BuildingConsumer<ModuleLightInquiryBuilder<?>> other, RelationshipType... types);

		/**
		 * Filters for all modules for which a relationship with any of the relationships in {@code types} exists and with the module being the {@code dst} module.
		 * 
		 * @param types the types of the module relationships or empty for no filtering by {@link RelationshipType} types
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withDestinationRelationships(RelationshipType... types);

		/**
		 * Filters for all modules having any of the relationships in {@code types} with the given {@code module} as source module.
		 * 
		 * @param moduleId the id of the source module 
		 * @param types the types of the module relationships or empty for no filtering by {@link RelationshipType} types
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withDestinationRelationshipsTo(EntityId moduleId, RelationshipType... types);

		/**
		 * Filters for all modules having any of the relationships in {@code types} with modules matching the given {@code other} filters as destination module.
		 * 
		 * @param other the {@link ModuleLightInquiryBuilder} containing the filters for the referenced resp. referencing module
		 * @param types the types of the module relationships or empty for no filtering by {@link RelationshipType} types
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withDestinationRelationshipsTo(BuildingConsumer<ModuleLightInquiryBuilder<?>> other, RelationshipType... types);

		/**
		 * Filters modules by module relationship property.
		 * 
		 * @param property the name of the property
		 * @param value the value of the property to match
		 * @param contain {@code true} if {@code value} can be contained or must be equal with the property value. {@code false} if {@code value} must be equal
		 * with the property value
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withRelationshipProperty(String property, String value, boolean contain);
		
		/**
		 * Filters modules by module relationship property.
		 * 
		 * @param property the name of the property
		 * @param values The values any of which must match the property.
		 * with the property value
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withRelationshipPropertyAny(String property, Collection<String> values);
		
		/**
		 * Filters modules depending on the presence of module relationship properties.
		 * 
		 * @param properties the names of the properties
		 * @param present if the properties must all be present ({@code true}) or absent ({@code false})
		 * with the property value
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withRelationshipProperties(Collection<String> properties, boolean present);
		
		/**
		 * Require Modules to have a certain kind of relationship with one module.
		 * 
		 * @param module Related module.
		 * @param direction Direction of relations to include.
		 * @param types Types of relations to include.
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withRelationship(EntityId module, RelationshipDirection direction, @Nullable Collection<RelationshipType> types);
		
		/**
		 * Filters modules that don't have relationships of the given {@code types} with the module being the source module.
		 * 
		 * @param types the {@link RelationshipType Relationships} of the module relationships
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder notWithSourceRelationships(Collection<RelationshipType> types);

		/**
		 * Filters modules that don't have relationships of the given {@code types} with the module being the destination module.
		 * 
		 * @param types the {@link RelationshipType Relationships} of the module relationships
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder notWithDestinationRelationships(Collection<RelationshipType> types);

		/**
		 * Filters modules that don't have either no source <b>OR</b> no destination relationships of the given {@code types}.
		 * 
		 * @param types the {@link RelationshipType Relationships} of the module relationships
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder notWithSourceOrDestinationRelationships(Collection<RelationshipType> types);

		/**
		 * Filters modules that don't have any taxonomies of the given {@code taxonomyIds} with the module.
		 *
		 * @param taxonomyIds the {@link EntityId EntityIds} of the taxonomies
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder notWithTaxonomies(Collection<EntityId> taxonomyIds);

		/**
		 * Filters modules by conditional reference
		 * 
		 * @param relationship the {@link UUID} of the conditional relationship
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withConditionalRelationship(UUID relationship);

		/**
		 * Filters modules having a source match by name, technology and type
		 * 
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withSourceMatch();

		/**
		 * Specifies if the source content of the module should be loaded as well. By default the content is not loaded.
		 *
		 * @param includeContent {@code true} to include the content. Otherwise {@code false}
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder includeContent(boolean includeContent);
		
		/**
		 * Queries the number of certain relations as a dynamic property. 
		 * @param dynamicAlias Name for the property.
		 * @param direction Direction of the relation.
		 * @param type Type of the relation.
		 * @param distinctModuleRelationships {@code true} to fetch distinct dependencies. Otherwise {@code false}
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder includeDependencyCount(String dynamicAlias, @Nullable RelationshipDirection direction, @Nullable RelationshipType type, boolean distinctModuleRelationships);
		
		
		/**
		 * Queries the number of certain Annotations as a dynamic property. 
		 * @param dynamicAlias Name for the property.
		 * @param type Type of Annotations to include.
		 * @param state State of Annotations to include. 
		 * @param categoryName Category of Annotations to include. 
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder includeAnnotationCount(String dynamicAlias, 
				@Nullable AnnotationType type, @Nullable WorkingState state, @Nullable String categoryName);

		/**
		 * Filters modules by taxonomy ids
		 * @param taxonomyIds to filter by
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withTaxonomies(Collection<EntityId> taxonomyIds);

		/**
		 * Queries the list of Taxonomies assigned to the Module as a dynamic property.
		 * @param dynamicAlias Name for the property.
		 * @param typeId Type of Taxonomies to include.
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder includeTaxonomies(String dynamicAlias, @Nullable UUID typeId);
		
		/**
		 * Queries the number of Taxonomies assigned to the Module as a dynamic property.
		 * @param dynamicAlias Name for the property.
		 * @param typeId Type of Taxonomies to include.
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder includeTaxonomyCount(String dynamicAlias, @Nullable UUID typeId);

		/**
		 * Filters modules by a dynamically selected property.
		 * @param dynamicAlias Name of the property.
		 * @param comperator Comparison to perform.
		 * @param value Value to compare the property to.
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder filterBy(String dynamicAlias, Comperator comperator, Object value);
		
		/**
		 * Filters modules based on whether a dynamically selected array overlaps with a given array.
		 * @param dynamicAlias Name of the property.
		 * @param values Array to match.
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder filterArray(String dynamicAlias, Collection<String> values);
		
		/**
		 * Filter modules based on the length of a dynamically selected array.
		 * @param dynamicAlias Name of the property.
		 * @param comperator Comparison to perform.
		 * @param value Required array length.
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder filterArrayLength(String dynamicAlias, Comperator comperator, Long value);
		
		/**
		 * Filters modules based on whether a dynamically selected property is or is not {@code null}.
		 * @param dynamicAlias Name of the property.
		 * @param isNull If it must be {@code null} or not {@code null}.
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder filterNull(String dynamicAlias, boolean isNull);

		/**
		 * Filters modules based on the assigned Reachability Block
		 * @param reachabilityBlockName Name of the Reachability Block
		 * @return this instance for method chaining
		 */
		ModuleInquiryBuilder withReachabilityBlockName(String reachabilityBlockName);
	}

	interface ModuleAggregationInquiryBuilder<B extends ModuleAggregationInquiryBuilder<B>> extends AggregationInquiryBuilder<ModuleFieldName, B> {

		/**
		 * Filters modules by ID
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B byId(String operator, Object value);

		/**
		 * Filters modules by Name
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withName(String operator, Object value);

		/**
		 * Filters modules by Project ID
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B ofProject(String operator, Object value);

		/**
		 * Filters modules by Technology
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withTechnology(String operator, Object value);

		/**
		 * Filters modules by Type
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withType(String operator, Object value);

		/**
		 * Filters modules by requires_review
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withRequiresReview(String operator, Object value);

		/**
		 * Filters modules by Storage
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withStorage(String operator, Object value);

		/**
		 * Filters modules by Creator
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withCreator(String operator, Object value);

		/**
		 * Filters modules by Identification
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withIdentification(String operator, Object value);

		/**
		 * Filters modules by Physical Lines of Code
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withPhysicalLines(String operator, Object value);

		/**
		 * Filters modules by Line of Code
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withCodeLines(String operator, Object value);

		/**
		 * Filters modules by Line of Comment
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withCommentLines(String operator, Object value);

		/**
		 * Filters modules by Line of Dead Code
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withDeadCodeLines(String operator, Object value);

		/**
		 * Filters modules by Complexity
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withComplexity(String operator, Object value);

		/**
		 * Filters modules by Error Count
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withErrors(String operator, Object value);

		/**
		 * Filters modules by SQL Statements Count
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withSqlStatements(String operator, Object value);

		/**
		 * Filters modules by Containing Module ID
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withContainingModuleId(String operator, Object value);

		/**
		 * Filters modules by Containing Module Name
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withContainingModuleName(String operator, Object value);

		/**
		 * Filters modules by Representation
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withRepresentation(String operator, Object value);

		/**
		 * Filters modules by Taxonomy ID(s)
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withTaxonomy(String operator, Object value);

		/**
		 * Filters modules by Taxonomy name(s)
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withTaxonomyName(String operator, Object value);

		/**
		 * Filters modules by {@link Origin}
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withOrigin(String operator, Object value);

		/**
		 * Applies a legacy filterObject. 
		 * @param filterObject Filter definitions.
		 * @return this instance for method chaining
		 */
		B applyFilters(Map<ModuleFieldName, Map<String, Object>> filterObject);
		
		/**
		 * Filters for all modules having any of the relationships in {@code types} with modules matching the given {@code other} filters as source module.
		 * 
		 * @param other the {@link ModuleLightInquiryBuilder} containing the filters for the referenced resp. referencing module
		 * @param types the types of the module relationships or empty for no filtering by {@link RelationshipType} types
		 * @return this instance for method chaining
		 */
		B withSourceRelationshipsFrom(BuildingConsumer<ModuleLightInquiryBuilder<?>> other, RelationshipType... types);
	}

	interface ModuleRelationshipAggregationInquiryBuilder <B extends ModuleRelationshipAggregationInquiryBuilder<B>> extends AggregationInquiryBuilder<RelationshipFieldName, B> {

		/**
		 * Filters module relationships by {@code id}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B byId(String operator, Object value);

		/**
		 * Filters module relationships by {@code relationship}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withRelationship(String operator, Object value);

		/**
		 * Filters module relationships by {@code in_id}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withDstId(String operator, Object value);

		/**
		 * Filters module relationships by {@code out_id}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withSrcId(String operator, Object value);

		/**
		 * Filters module relationships by {@code in_name}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withDstName(String operator, Object value);

		/**
		 * Filters module relationships by {@code out_name}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withSrcName(String operator, Object value);

		/**
		 * Filters module relationships by {@code in_project_id}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withDstProjectId(String operator, Object value);

		/**
		 * Filters module relationships by {@code out_project_id}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withSrcProjectId(String operator, Object value);

		/**
		 * Filters module relationships by {@code in_technology}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withDstTechnology(String operator, Object value);

		/**
		 * Filters module relationships by {@code out_technology}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withSrcTechnology(String operator, Object value);

		/**
		 * Filters module relationships by {@code in_type}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withDstType(String operator, Object value);

		/**
		 * Filters module relationships by {@code out_type}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withSrcType(String operator, Object value);

		/**
		 * Filters module relationships by {@code property_db_access_type}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withPropertyDbType(String operator, Object value);

		/**
		 * Filters module relationships by {@code property_db_access_operation}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withPropertyDbOperation(String operator, Object value);

		/**
		 * Filters module relationships by {@code in_linkhash}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withDstLinkhash(String operator, Object value);

		/**
		 * Filters module relationships by {@code out_linkhash}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withSrcLinkhash(String operator, Object value);

		/**
		 * Filters module relationships by {@code out_storage}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withSrcStorage(String operator, Object value);

		/**
		 * Filters module relationships by {@code in_storage}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withDstStorage(String operator, Object value);

		/**
		 * Filters module relationships by {@code taxonomy} id.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withTaxonomy(String operator, Object value);
	}

	interface ModuleRelationshipInquiryBuilder extends QueryLimitBuilder<ModuleRelationshipInquiryBuilder> {

		/**
		 * Filters module relationships by {@code id}.
		 * 
		 * @param id the {@link UUID} of the module relationship
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder byId(UUID id);

		/**
		 * Filters module relationships by {@code ids}.
		 *
		 * @param ids the {@link UUID UUIDs} of the module relationships
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder byIds(Collection<UUID> ids);

		/**
		 * Filters module relationships by {@code project} in references modules.
		 * 
		 * @param project the id of the project
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder ofProject(EntityId project);

		/**
		 * Filters module relationships by {@code src} module id.
		 * 
		 * @param module the id of the module
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder ofSource(EntityId module);

		/**
		 * Filters module relationships by {@code src} module {@link Type}.
		 * 
		 * @param type the id of the module
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder withSourceType(Type type);

		/**
		 * Filters module relationships by {@code src} module {@link UUID}.
		 * 
		 * @param module the {@link UUID} of the module
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder ofSource(UUID module);

		/**
		 * Filters module relationships by {@code dst} module {@link UUID}.
		 *
		 * @param module the {@link UUID} of the module
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder ofDestination(UUID module);

		/**
		 * Filters module relationships by {@code module} and {@code direction}.
		 * 
		 * @param module the {@link UUID} of the module
		 * @param direction the {@link RelationshipDirection} of the module relationships
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder ofModuleInDirection(UUID module, RelationshipDirection direction);

		/**
		 * Filters module relationships by {@code modules} and {@code direction}.
		 * 
		 * @param modules the {@link UUID UUIDs} of the modules
		 * @param direction the {@link RelationshipDirection} of the module relationships
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder ofModulesInDirection(Collection<UUID> modules, RelationshipDirection direction);

		/**
		 * Filters module relationships by {@code module} and {@code direction}.
		 * 
		 * @param module the id of the module
		 * @param direction the {@link RelationshipDirection} of the module relationships
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder ofModuleInDirection(EntityId module, RelationshipDirection direction);

		/**
		 * Filters module relationships by {@code dst} module id.
		 * 
		 * @param module the id of the module
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder ofDestination(EntityId module);

		/**
		 * Filters module relationships by {@code dst} module {@link Type}.
		 *
		 * @param type the type of the module
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder withDestinationType(Type type);

		ModuleRelationshipInquiryBuilder ofDependencyDefinition(UUID dependencyDefinitionId);

		/**
		 * Filters module relationships by {@code src_location}.
		 * 
		 * @param location the {@link ModuleLocation} of the source
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder withSourceLocation(ModuleLocation location);
		
		/**
		 * Filters module relationships by {@code src_location} within the provided range.
		 *
		 * @param location the {@link ModuleLocation} designating the range
		 * @return this instance of method chaining
		 */
		ModuleRelationshipInquiryBuilder withSourceLocationInRange(ModuleLocation location);
		
		/**
		 * Filters module relationships by {@code dst_location}.
		 * 
		 * @param location the {@link ModuleLocation} of the destination
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder withDestinationLocation(ModuleLocation location);
		
		/**
		 * Filters module relationships by {@code dst_location} within the provided range.
		 *
		 * @param location the {@link ModuleLocation} designating the range
		 * @return this instance of method chaining
		 */
		ModuleRelationshipInquiryBuilder withDestinationLocationInRange(ModuleLocation location);

		/**
		 * Filters module relationships by {@code origin} of the destination.
		 * 
		 * @param origin the {@link Origin} of the destination
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder withDestinationOrigin(Origin origin);

		/**
		 * Filters module relationships by {@code relationship} type.
		 * 
		 * @param relationship the {@link RelationshipType}
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder withType(RelationshipType relationship);

		/**
		 * Filters module relationships by {@code types}.
		 * 
		 * @param relationships the {@link RelationshipType Relationships}
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder withTypes(Collection<RelationshipType> relationships);

		/**
		 * Include only relationships for which certain properties are defined.
		 * 
		 * @param pattern Pattern to match against property keys.
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder withProperties(String pattern);

		/**
		 * Filters module relationships by {@code properties}.
		 * 
		 * @param properties the {@link Map} containing all properties
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder withProperties(Map<String, Object> properties);

		/**
		 * Filters modules relationships by properties where any of the {@code properties} must match or contain one of {@code values}.
		 *
		 * @param properties the names of the properties
		 * @param values the values one of the properties has to match with
		 * @param contain {@code true} if {@code value} can be contained or must be equal with the property value. {@code false} if {@code value} must be equal
		 * with the property value
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder withProperties(List<String> properties, List<String> values, boolean contain);

		/**
		 * Return only unique value combinations for the specified fields.
		 * <p>CAUTION: If multiple values exist for fields other than those specified here, only the first (according to the current ordering) of them will appear in the result.<p>
		 *
		 * @param fields Combination of fields to distinguish relationships by.
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder distinct(Collection<RelationshipField> fields);
		
		/**
		 * Convenience method for {@link #distinct(Collection)}.
		 * @param fields Combination of fields to distinguish relationships by.
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder distinct(RelationshipField... fields);

		/**
		 * Filters modules relationships by conditional dependencies.
		 *
		 * @param reachedFromModules the {@link EntityId EntityIds} of the reached from modules
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder withConditionalDependencies(Collection<EntityId> reachedFromModules);
		
		/**
		 * Fetches Module details 
		 * @param src source Module details
		 * @param dst destination Module details
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder includeModuleDetails(boolean src, boolean dst);

		/**
		 * Defines the Module by which the direction of the relationship is determined.
		 * @param module Optional Module ID.
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder setBaseModule(@Nullable EntityId module);

		/**
		 * Sorts module relationships by {@code nid} in the given {@code sortDirection}.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder sortNid(SortDirection sortDirection);

		/**
		 * Sorts module relationships by {@code type} in the given {@code sortDirection}.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder sortType(SortDirection sortDirection);

		/**
		 * Sorts module relationships by {@code direction} in the given {@code sortDirection}.
		 * This is only effective if {@link #setBaseModule(EntityId)} has been specified.
		 * @param sortDirection the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		ModuleRelationshipInquiryBuilder sortByRelationshipDirection(SortDirection sortDirection);
	}

	interface LinkedModuleInquiryBuilder {
		/**
		 * Filters {@link LinkedModule LinkedModules} by {@code project} id.
		 * 
		 * @param project the id of the project
		 * @return this instance for method chaining
		 */
		LinkedModuleInquiryBuilder ofProject(EntityId project);

		/**
		 * Filters {@link LinkedModule LinkedModules} by the path of the source module.
		 * 
		 * @param path the module path
		 * @return this instance for method chaining
		 */
		LinkedModuleInquiryBuilder withSourcePath(String path);

		/**
		 * Filters {@link LinkedModule LinkedModules} by the {@link Storage} of the source module.
		 *
		 * @param storage the module storage 
		 * @return this instance for method chaining
		 */
		LinkedModuleInquiryBuilder withSourceStorage(Storage storage);

		/**
		 * Filters {@link LinkedModule LinkedModules} by {@link RelationshipType} types.
		 * 
		 * @param types the {@link RelationshipType} types
		 * @return this instance for method chaining
		 */
		LinkedModuleInquiryBuilder withRelationships(Collection<RelationshipType> types);
	}

	interface StatementInquiryBuilder {

		/**
		 * Filters statements by {@code project} id.
		 * 
		 * @param project the id of the project
		 * @return this instance for method chaining
		 */
		StatementInquiryBuilder ofProject(EntityId project);

		/**
		 * Filters statements by {@code module} id.
		 * 
		 * @param module the id of the module
		 * @return this instance for method chaining
		 */
		StatementInquiryBuilder ofModule(EntityId module);

		/**
		 * Filters statements by {@code taxonomy} id.
		 * 
		 * @param taxonomy the id of the taxonomy
		 * @return this instance for method chaining
		 */
		StatementInquiryBuilder ofTaxonomy(EntityId taxonomy);

		/**
		 * Filters statements by {@code taxonomy} ids.
		 * 
		 * @param taxonomies the ids of the taxonomies
		 * @return this instance for method chaining
		 */
		StatementInquiryBuilder ofTaxonomies(Collection<EntityId> taxonomies);

		/**
		 * Filters statements by technology
		 * 
		 * @param technology the technology
		 * @return this instance for method chaining
		 */
		StatementInquiryBuilder withTechnology(Technology technology);

		/**
		 * Filters statements by technology
		 * 
		 * @param technology the technology
		 * @return this instance for method chaining
		 */
		StatementInquiryBuilder notWithTechnology(Technology technology);

		/**
		 * Filters statements by type
		 * 
		 * @param type the {@link StatementType} of the statement
		 * @return this instance for method chaining
		 */
		StatementInquiryBuilder withType(StatementType type);

		/**
		 * Filters statements by types
		 *
		 * @param types the {@link StatementType}s to filter the statements for
		 * @return this instance for method chaining
		 */
		StatementInquiryBuilder withTypes(Collection<StatementType> types);

		/**
		 * Filters statements by type
		 *
		 * @param types the {@link StatementType} of the statement
		 * @return this instance for method chaining
		 */
		StatementInquiryBuilder notWithTypes(Collection<StatementType> types);

		/**
		 * Filters statements by text
		 *
		 * @param text the text of the statement
		 * @return this instance for method chaining
		 */
		StatementInquiryBuilder withText(String text);

		/**
		 * Filters statements by module name
		 *
		 * @param name the name of the module to get statements for
		 * @return this instance for method chaining
		 */
		StatementInquiryBuilder withModuleName(String name);

		/**
		 * Filters statements by module path
		 *
		 * @param path the path of the module to get statements for
		 * @return this instance for method chaining
		 */
		StatementInquiryBuilder withModulePath(String path);
	}

	interface StatementAggregationInquiryBuilder <B extends StatementAggregationInquiryBuilder<B>> extends AggregationInquiryBuilder<StatementFieldName, B> {

		/**
		 * Filters statements by {@code id}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B byId(String operator, Object value);

		/**
		 * Filters statements by {@code project} id.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B ofProject(String operator, Object value);

		/**
		 * Filters statements by {@code module} id.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B ofModule(String operator, Object value);

		/**
		 * Filters statements by {@code taxonomy} id.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withTaxonomy(String operator, Object value);

		/**
		 * Filters statements by {@code technology}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withTechnology(String operator, Object value);

		/**
		 * Filters statements by {@code type}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withType(String operator, Object value);

		/**
		 * Filters statements by {@code text}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withText(String operator, Object value);

		/**
		 * Filters statements by {@code text_length}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withTextLength(String operator, Object value);

		/**
		 * Filters statements by {@code tables}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withTables(String operator, Object value);

		/**
		 * Filters statements by {@code distinct_tables}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withDistinctTables(String operator, Object value);

		/**
		 * Filters statements by {@code sql_length}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withSqlLength(String operator, Object value);

		/**
		 * Filters statements by {@code halstead_difficulty}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withHalsteadDifficulty(String operator, Object value);

		/**
		 * Filters statements by {@code halstead_complexity}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withHalsteadComplexity(String operator, Object value);

		/**
		 * Filters statements by {@code custom_complexity}.
		 *
		 * @param operator the filter operator
		 * @param value the filter value
		 * @return this instance for method chaining
		 */
		B withCustomComplexity(String operator, Object value);
	}

	interface ErrorMarkerInquiryBuilder {
		/**
		 * Filters error markers by {@code module} id.
		 * 
		 * @param module the id of the module
		 * @return this instance for method chaining
		 */
		ErrorMarkerInquiryBuilder ofModule(EntityId module);

		/**
		 * Filters error markers by {@code modules} ids.
		 *
		 * @param modules the ids of the modules
		 * @return this instance for method chaining
		 */
		ErrorMarkerInquiryBuilder ofModules(Collection<EntityId> modules);

		/**
		 * Filters error markers by {@code project} id.
		 * 
		 * @param project the id of the project
		 * @return this instance for method chaining
		 */
		ErrorMarkerInquiryBuilder ofProject(EntityId project);

		/**
		 * Filters error markers by {@linkplain ErrorKey} {@code key}.
		 *
		 * @param key the {@linkplain ErrorKey}
		 * @return this instance for method chaining
		 */
		ErrorMarkerInquiryBuilder withKey(ErrorKey key);

	}

	interface DeadCodeInquiryBuilder {

		/**
		 * Filters {@code module_dead_code} by {@code project} id.
		 * 
		 * @param project the id of the project
		 * @return this instance for method chaining
		 */
		DeadCodeInquiryBuilder ofProject(EntityId project);

		/**
		 * Filters {@code module_dead_code} by {@code module} id.
		 * 
		 * @param module the id of the module
		 * @return this instance for method chaining
		 */
		DeadCodeInquiryBuilder ofModule(EntityId module);
	}

	interface DependencyDefinitionInquiryBuilder {

		/**
		 * Filters dependency_definition by id.
		 * 
		 * @param id the id of the dependency definition
		 * @return this instance for method chaining
		 */
		DependencyDefinitionInquiryBuilder byId(UUID id);

		/**
		 * Filters dependency_definition by {@code project}.
		 * 
		 * @param project the id of the project
		 * @return this instance for method chaining
		 */
		DependencyDefinitionInquiryBuilder ofProject(EntityId project);

		/**
		 * Filters dependency_definition by {@code module}.
		 * 
		 * @param module the id of the module
		 * @return this instance for method chaining
		 */
		DependencyDefinitionInquiryBuilder ofModule(EntityId module);

		/**
		 * Filters dependency_definition by {@code resolved} status.
		 * 
		 * @param resolved the resolved status
		 * @return this instance for method chaining
		 */
		DependencyDefinitionInquiryBuilder withResolved(boolean resolved);

		/**
		 * Filters dependency_definition by provided {@code ResolutionFlag} .
		 *
		 * @param resolutionFlag the flag of the dependency definition
		 * @return this instance for method chaining
		 */
		DependencyDefinitionInquiryBuilder withFlag(ResolutionFlag resolutionFlag);

		/**
		 * Filters dependency_definition by {@code count}.
		 *
		 * @param count the count of the dependency definition
		 * @return this instance for method chaining
		 */
		DependencyDefinitionInquiryBuilder havingCountGreaterThan(int count);

		/**
		 * Filters dependency_definition by {@code Binding}.
		 * 
		 * @param binding the {@code Binding}
		 * @return this instance for method chaining
	     */
		DependencyDefinitionInquiryBuilder withBindingType(Binding binding);

		/**
		 * Filters dependency_definition by {@code relationshipType}.
		 * 
		 * @param relationshipType the relationshipType
		 * @return this instance for method chaining
		 */
		DependencyDefinitionInquiryBuilder withRelationship(final RelationshipType relationshipType);
	}

	interface SourceMetricsInquiryBuilder {

		/**
		 * Filters source metrics by {@code project} id.
		 * 
		 * @param project the id of the project
		 * @return this instance for method chaining
		 */
		SourceMetricsInquiryBuilder ofProject(EntityId project);

		/**
		 * Filters source metrics by {@code representation} of their linked modules.
		 * 
		 * @param representation the representation of the module
		 * @return this instance for method chaining
		 */
		SourceMetricsInquiryBuilder withRepresentation(Representation representation);
	}

	interface ModuleUndiscoveredOrderBuilder {

		ModuleUndiscoveredOrderBuilder sortProject(SortDirection sort);

		ModuleUndiscoveredOrderBuilder sortName(SortDirection sort);

		ModuleUndiscoveredOrderBuilder sortPath(SortDirection sortDirection);
	}

	interface ModuleUndiscoveredInquiryBuilder extends ModuleUndiscoveredOrderBuilder {

		ModuleUndiscoveredInquiryBuilder ofProject(EntityId project);

		ModuleUndiscoveredInquiryBuilder withName(String name);

		ModuleUndiscoveredInquiryBuilder withPath(String path);
	}

	/**
	 * Creates a new {@code module} entity.
	 *
	 * @param module the {@link ModulePojoPrototype} to create
	 * @return the {@link EntityId} of the created {@code module}.
	 */
	EntityId create(ModulePojoPrototype module);

	/**
	 * Updates the {@code module} entity.
	 *
	 * @param module the {@link ModulePojoPrototype} to update
	 * @return the {@link EntityId} of the updated modules
	 */
	EntityId update(ModulePojoPrototype module);

	/**
	 * Deletes the {@code module}.
	 *
	 * @param module the id of the module to delete
	 * @param deleteSource {@code true} to also delete the linked source. Otherwise {@code false}
	 * @return number of deleted modules, 1 or 0
	 */
	int deleteModule(EntityId module, boolean deleteSource);

	/**
	 * Deletes all {@code modules} for the given {@code project}.
	 *
	 * @param project the id of the project whose modules must be deleted
	 * @param deleteSource {@code true} to also delete the linked sources. Otherwise {@code false}
	 * @param deleteDiscoveryOnly {@code true} to delete modules with creator {@link Creator#DISCOVERY} only. Otherwise {@code false} to delete all modules
	 * @return number of deleted modules
	 */
	int deleteModules(EntityId project, boolean deleteSource, boolean deleteDiscoveryOnly);

	/**
	 * Deletes all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted modules
	 */
	int deleteModules(BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Returns all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModulePojo ModulePojos}
	 */
	List<ModulePojo> findModules(BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Retrieves a set of Modules by IDs and returns all that were found in a Map.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria.
	 * @return Map of Module IDs to {@linkplain ModulePojo ModulePojos}.
	 */
	Map<UUID, ModulePojo> mapModules(BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Returns paged subset of optionally filtered {@code module} entities that match with the filters in the given {@code builder}.
	 * 
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return Paged subset of matching {@code module} entities.
	 */
	Paged<ModulePojo> findModules(Pagination paging, BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Returns the {@link UUID UUIDs} of all {@code modules} that reference the given {@code module} with the specified reference {@code type} and 
	 * {@code direction}.
	 * 
	 * @param module the id of the starting module
	 * @param relationship the type of the module relationship
	 * @param direction the direction of the module relationships
	 * @return list of matching {@code module} {@link UUID UUIDs}.
	 */
	List<UUID> findRelatedModules(EntityId module, RelationshipType relationship, RelationshipDirection direction);

	/**
	 * Returns a list of names of all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModulePojo ModulePojos}
	 */
	List<String> findNames(BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Returns all {@code module} entities that match with the filters in the given {@code builder}.
	 * <p>This method loads only the basic module properties for better performance.</p>
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModuleLightweightPojo ModuleLightweightPojos}
	 */
	List<ModuleLightweightPojo> findModulesLightweight(BuildingConsumer<ModuleInquiryBuilder> builder);
	
	/**
	 * Returns the {@code module} with the specified ID.
	 * @param moduleId ID of the Module to be found.
	 * @return a single {@link ModuleLightweightPojo}
	 * @throws EntityNotFoundException If no match was found.
	 */
	ModuleLightweightPojo getModuleLightweight(EntityId moduleId);
	
	/**
	 * Returns the first {@code module} entity that match with the filters in the given {@code builder}.
	 * <p>This method loads only the basic module properties for better performance.</p>
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return {@linkplain ModuleLightweightPojo}
	 */
	Optional<ModuleLightweightPojo> findAnyModuleLightweight(BuildingConsumer<ModuleInquiryBuilder> builder);
	
	/**
	 * Find all orphaned {@code modules} that must be recollected in incremental discovery.
	 *
	 * @param projectId the ID of the project
	 * @param creator the {@link Creator} of the {@link ModuleLightweightPojo} to find
	 * @return list of {@linkplain ModuleLightweightPojo ModuleLightweightPojos}
	 */
	List<ModuleLightweightPojo> findModuleLightweightOrphaned(EntityId projectId, Creator creator);

	/**
	 * Returns all {@code module} entities that have no source or whose source has changed and which must be recollected in incremental discovery.
	 *
	 * @param projectId the ID of the project
	 * @param creator the {@link Creator} of the {@link ModuleLightweightPojo} to find
	 * @return list of {@linkplain ModuleLightweightPojo ModuleLightweightPojos}
	 */
	List<ModuleLightweightPojo> findModuleLightweightMissingOrChangedSource(UUID projectId, Creator creator);

	/**
	 * Returns all {@linkplain LinkedModule LinkedModules} that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain LinkedModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain LinkedModule LinkedModules}
	 */
	List<LinkedModule> findLinkedModules(BuildingConsumer<LinkedModuleInquiryBuilder> builder);

	/**
	 * Returns the first {@code module} that matches with the filters in the given {@code builder}.
	 * <p>If no match was found then an empty {@linkplain Optional} is returned.</p>
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options. 
	 * @return first matching {@linkplain ModulePojo} if available
	 */
	Optional<ModulePojo> findAnyModule(BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Returns the Module with the specified ID.
	 * @param moduleId Unique ID of the Module.
	 * @return The Module, if found.
	 * @throws EntityNotFoundException If no match was found.
	 */
	ModulePojo getModule(UUID moduleId);
	
	/**
	 * Returns the {@code module} with the specified ID.
	 * @param moduleId ID of the Module to be found.
	 * @return a single {@link ModulePojo}
	 * @throws EntityNotFoundException If no match was found.
	 */
	ModulePojo getModule(EntityId moduleId);

	/**
	 * Returns a substring of the content of the module if available
	 * <p>If no match was found then a <b>{@code MiningEntityNotFoundException} is thrown</b></p>
	 *
	 * @param moduleId the id of the {@code module} to find
	 * @param offset the offset in the content from where to start
	 * @param length the length of the content to read
	 * @return the substring if available
	 */
	String getContentSubstring(EntityId moduleId, Integer offset, Integer length);

	/**
	 * Returns the contents of the modules with the given {@code moduleIds}.
	 *
	 * @param moduleIds the ids of the {@code module} to find
	 * @return the contents of the modules
	 */
	Map<EntityId, String> getContents(Collection<EntityId> moduleIds);

	/**
	 * Returns the {@link Type} of the module with the given {@code moduleId}
	 * <p>If the module doesn't exists then a <b>{@code MiningEntityNotFoundException} is thrown</b></p>
	 *
	 * @param moduleId the id of the {@code module} to find
	 * @return the {@link Type}
	 */
	Type getModuleType(EntityId moduleId);

	/**
	 * Returns the {@code path} of the module for the given {@code moduleId}.
	 * <p>If the module doesn't exists then a <b>{@code MiningEntityNotFoundException} is thrown</b></p>
	 *
	 * @param moduleId the id of the {@code module} to find
	 * @return the path
	 */
	String getModulePath(EntityId moduleId);

	/**
	 * Returns the {@link UUID UUIDs} of all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options
	 * @return list of matching {@code module} {@link UUID UUIDs}.
	 */
	List<UUID> findModuleUids(BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Returns the {@link EntityId EntityIds} of all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options
	 * @return list of matching {@code module} {@link EntityId EntityIds}.
	 */
	List<EntityId> findModuleIds(BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Returns a map of link hash to module id for modules that match with the filters in the given {@code builder}.
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria
	 * @return a map of link hashes to module ids
	 */
	Map<String, EntityId> findModuleIdsByLinkHash(BuildingConsumer<ModuleInquiryBuilder> builder);
	
	/**
	 * Returns a list of pair of module uid and dependency hash for modules that match with the filters in the given {@code builder}.
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria
	 * @return a list of pair of module uid and dependency hash
	 */
	List<Pair<UUID, String>> findModuleUUIDsAndDependencyHashes(BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Returns the {@link EntityId} of the first {@code module} that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options
	 * @return matching {@code module} {@link EntityId}.
	 */
	Optional<EntityId> findAnyModuleId(BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Returns paged subset of optionally filtered {@code module} nids that match with the filters in the given {@code builder}.
	 * 
	 * @param paging Pagination specification.
	  * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options
	 * @return Paged subset of matching {@code module} {@link EntityId EntityIds}
	 */
	Paged<EntityId> findModuleIds(Pagination paging, BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Returns the number of {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code module} entities
	 */
	long countModules(BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Returns duplicate {@code module} names based on the given {@code project}, {@code technology} and {@code types}. Only {@code module} names having count
	 * &gt; 1 are returned.
	 * 
	 * @param project the project the {@code module} entities must be searched for
	 * @param technology the technology of the {@code module} 
	 * @param types types of {@code module}
	 * @return list of {@code module} names
	 */
	List<String> findDuplicateModuleNames(EntityId project, Technology technology, List<Type> types);

	/**
	 * Returns the {@link EntityId EntityIds} of {@code module} duplicates with type {@code typeToDelete}.
	 * <p>The query first searches for all modules that have the same name for the given {@code project}, {@code technology} and types ({@code typeToKeep} or
	 * {@code typeToDelete}). The {@link EntityId EntityIds} of {@code module} entities with type {@code typeToDelete} are then fetched.</p>
	 *
	 * @param project the project the {@code module} entities must be filtered with
	 * @param technology the technology the {@code module} entities must be filtered with
	 * @param typeToKeep the type of module to retain
	 * @param typeToDelete the type of module to delete
	 * @return list of {@code module} {@link EntityId EntityIds} with type {@code typeToDelete}
	 */
	List<EntityId> findDuplicateModuleIds(EntityId project, Technology technology, Type typeToKeep, Type typeToDelete);

	/**
	 * Returns the {@link EntityId EntityIds} of all {@code module} entities of the given {@code project} that have unresolved dependencies.
	 *
	 * @param project the {@code project} for which to find the modules
	 * @return list of matching {@code module} {@link UUID UUIDs}.
	 */
	List<EntityId> findModulesWithUnresolvedDependencies(EntityId project);

	/**
	 * Returns the {@link EntityId EntityIds} of all {@code module} entities of the given {@code project} for which meta data (description, annotations,
	 * taxonomies or data dictionary entries) have been collected.
	 *
	 * @param project the {@code project} for which to find the modules
	 * @return list of matching {@code module} {@link UUID UUIDs}.
	 */
	List<EntityId> findModulesWithMetaData(EntityId project);

	/**
	 * Sets the {@code content_hash} of all {@code module} entities and sets it to the {@code content_hash} of their sources.
	 *
	 * @param project the {@code project} for which to update the {@code module} entities
	 * @return number of updated {@code modules}
	 */
	int updateModuleContentHashes(EntityId project);
	
	/**
	 * Sets the {@code dependency_hash} of all {@code module} entities and sets it to the {@code dependency_hash} of their sources.
	 *
	 * @param project the {@code project} for which to update the {@code module} entities
	 * @return number of updated {@code modules}
	 */
	int updateModuleDependencyHashes(EntityId project);

	/**
	 * This method allows you to update multiple modules with the same values.
	 * 
	 * <p>Updates all modules that match with the filters in the given {@code builder}. The defined (set) fields in the given {@code values} are used for
	 * building the update query.</p>
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria
	 * @param values the {@link ModulePojoPrototype} containing the to be updated fields
	 * @return the number of updated {@code modules}, 0 if no module matched with the filer
	 */
	int updateModules(BuildingConsumer<ModuleInquiryBuilder> builder, ModulePojoPrototype values);

	/**
	 * Returns the number of {@code source_matrics} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SourceMetricsInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code source_matrics} entities
	 */
	long countSourceMetrics(BuildingConsumer<SourceMetricsInquiryBuilder> builder);

	/**
	 * Returns the number of {@code source_metrics} of all {@code source_matrics} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SourceMetricsInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of {@code code_lines}
	 */
	long countSourceMetricsCodeLines(BuildingConsumer<SourceMetricsInquiryBuilder> builder);

	/**
	 * Returns a map containing the sum of {@code source_metrics} {@code code_lines} per technology for all {@code source_matrics} entities that match with the
	 * filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SourceMetricsInquiryBuilder} containing the filter criteria and sort options.
	 * @return map with sums
	 */
	Map<String, Long> countSourceMetricsCodeLinesByTechnology(BuildingConsumer<SourceMetricsInquiryBuilder> builder);

	/**
	 * Selects the complexities for all modules mapped by their {@link Technology} and {@link Type} for all {@code source_matrics} entities that match with the
	 * filters in the given {@code builder}.
	 * 
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return Map of complexities mapped by technology and type. Outer map contains the mapping for the {@link Technology Technologies},
	 * the inner map the mapping by {@link Type Types}
	 */
	Map<String, Map<String, List<Integer>>> getComplexities(BuildingConsumer<ModuleInquiryBuilder> builder);

	/**
	 * Returns aggregation {@code module} values for the filters and selected aggregations in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleAggregationInquiryBuilder} containing the aggregation operations and filter criteria
	 * @return container with the aggregation values
	 */
	Optional<Table> getAggregations(BuildingConsumer<ModuleAggregationInquiryBuilder<?>> builder);

	/**
	 * Returns aggregation {@code module} values for the filters and selected aggregations in the given {@code builder}.
	 * 
	 * @param project the {@code project} for which to get the aggregations
	 * @param aggregationRequest the {@linkplain AggregationRequest} containing the aggregation operations and filter criteria
	 * @return the aggregation results
	 */
	/* project is required for caching */
	List<AggregationResult<ModuleFieldName>> getAggregations(EntityId project, AggregationRequest<ModuleFieldName> aggregationRequest);

	/**
	 * Queries for all {@code module} invocation (destination modules in @code module_relationship} entities) that match with the filters in the given
	 * {@code builder} and puts for each match the: 
	 * <ul>
	 * <li>unique IDs of the utility modules as {@code uid}</li>
	 * <li>numeric IDs of the utility modules as {@code nid}</li>
	 * <li>names of the utility modules as {@code name}</li>
	 * <li>relationship properties of the utility module invocations as {@code properties}</li>
	 * </ul>
	 * in the returned {@link Table}.
	 *
	 * @param filterObject the filter criteria
	 * @return {@link Table} containing the values for each utility module invocation
	 */
	Optional<Table> findRelationshipInvocations(Map<ModuleFieldName, Map<String, Object>> filterObject);

	/**
	 * Creates a new {@code source_metrics} entity if none exists yet for the {@code module} in the given {@code sourceMetrics} or updates the existing one.
	 *
	 * @param sourceMetrics the {@link SourceMetricsPojoPrototype} to create or update
	 */
	void putSourceMetrics(SourceMetricsPojoPrototype sourceMetrics);

	/**
	 * Updates the {@code dead_code_lines} of all {@code source_metrics} entities in the given {@code project}.
	 * <ul>
	 * <li>For all {@code source_metrics} entities, that have no matching {@code module_dead_code} entities, the value is set to 0.</li>
	 * <li>For all others the value is set to the sum of {@code dead_code_lines} of all matching {@code module_dead_code} entities.</li>
	 * </ul>
	 *
	 * @param project the {@code project} for which to set the {@code dead_code_lines}.
	 */
	void updateSourceMetricsLinesOfDeadCode(EntityId project);

	/**
	 * Creates a new {@code module_relationship} entity.
	 *
	 * @param moduleRelationship the {@link ModuleRelationshipPojoPrototype} to create
	 * @return the {@link UUID} of the created {@code module_relationship}.
	 */
	UUID createRelationship(ModuleRelationshipPojoPrototype moduleRelationship);

	/**
	 * Creates a new {@code module_relationship} entity if it doesn't exists yet or when {@code checkForDuplicates} is {@code false}.
	 * <p>
	 * When {@code checkForDuplicates} is {@code true} this method will first check, if a reference with the same {@code src}, {@code dest},
	 * {@code src_location} (if set), {@code dst_location} (if set) and {@code properties} (if set) already exists and return an empty {@link Optional} if so.
	 * </p>
	 *
	 * @param moduleRelationship the {@link ModuleRelationshipPojoPrototype} to create
	 * @param checkForDuplicates {@code true} to check if the relationship already exists. Otherwise {@code false}
	 * @return the {@link UUID} of the created {@code module_relationship}.
	 */
	Optional<UUID> createRelationship(ModuleRelationshipPojoPrototype moduleRelationship, boolean checkForDuplicates);

	/**
	 * Deletes all {@code module_relationship} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted entities
	 */
	int deleteRelationship(BuildingConsumer<ModuleRelationshipInquiryBuilder> builder);

	/**
	 * Returns the {@code module_relationship} entity for the given {@code id}.
	 *
	 * @param id the id of the {@code module_relationship} entity
	 * @return the {@linkplain ModuleRelationshipPojo}
	 * @throws EntityNotFoundException If no match was found.
	 */
	ModuleRelationshipPojo getRelationship(UUID id);

	/**
	 * Returns paged subset of optionally filtered {@code module_relationship} entities that match with the filters in the given {@code builder}.
	 * 
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options.
	 * @return Paged subset of matching {@code module} entities.
	 */
	Paged<ModuleRelationshipPojo> findRelationships(Pagination paging, BuildingConsumer<ModuleRelationshipInquiryBuilder> builder);

	/**
	 * Returns all {@code module_relationship} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModuleRelationshipPojo ModuleRelationshipPojos}
	 */
	List<ModuleRelationshipPojo> findRelationship(BuildingConsumer<ModuleRelationshipInquiryBuilder> builder);

	/**
	 * Returns the first {@code module_relationship} entity that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options.
	 * @return {@linkplain ModuleRelationshipPojo}
	 */
	Optional<ModuleRelationshipPojo> findAnyRelationship(BuildingConsumer<ModuleRelationshipInquiryBuilder> builder);

	/**
	 * Returns the number of {@code module_relationship} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options
	 * @return number of matching {@code module_relationship} entities
	 */
	long countRelationships(BuildingConsumer<ModuleRelationshipInquiryBuilder> builder);

	/**
	 * Returns the aggregated {@code reference} entities that match with the filters in the given {@code request}.
	 *
	 * @param project the {@code project} for which to get the aggregations
	 * @param aggregationRequest the {@linkplain AggregationRequest} containing the aggregation operations and filter criteria
	 * @return List of {@linkplain AggregationResult}
	 */
	/* project is required for caching */
	List<AggregationResult<RelationshipFieldName>> getRelationshipAggregations(EntityId project, AggregationRequest<RelationshipFieldName> aggregationRequest);

	/**
	 * Queries for all {@code module_relationship} entities that match with the given {@code project} and puts for each match for the source module the: 
	 * <ul>
	 * <li>numeric id as {@code src_nid}</li>
	 * <li>name as {@code src_name}</li>
	 * <li>technology as {@code technology}</li>
	 * </ul>
	 * for the destination module the:
	 * <ul>
	 * <li>numeric id as {@code dst_nid}</li>
	 * <li>name as {@code dst_name}</li>
	 * </ul>
	 * and for the relationship the:
	 * <ul>
	 * <li>dependency binding as {@code dependency_binding}</li>
	 * <li>dependency attributes as {@code dependency_attributes}</li>
	 * <li>source location as {@code src_location}</li>
	 * <li>destination location as {@code dst_location}</li>
	 * <li>relationship type as {@code type}</li>
	 * <li>array of reaching module UUIDs as {@code reached_from_module}</li>
	 * </ul>
	 * into the returned {@link Table}.
	 *
	 * @param project the id of the project for which the data export is done
	 * @param sorted {@code true} if the values must be sorted. Otherwise {@code false}
	 * @return {@link Table} containing the values for each relationship match
	 */
	Optional<Table> getModuleRelationshipExport(EntityId project, boolean sorted);
	
	/**
	 * Return IDs of all the source Module which have given relationship with the input destination module 
	 *
	 * @param type of Module relationship
	 * @param dstModuleId destination module ID
	 * @return List of Source module IDs
	 */
	List<Long> getSrcModuleIdsByRelationshipTypeAndDstId(RelationshipType type, UUID dstModuleId);

	/**
	 * Creates a new {@code statement} entity if it doesn't exists yet or when {@code checkForDuplicates} is {@code false}.
	 * <p>
	 * When {@code checkForDuplicates} is {@code true} this method will first check, if a statement with the same {@code type} and {@code text} already exists
	 * and return an empty {@link Optional} if so.
	 * </p>
	 *
	 * @param statement the {@link StatementPojoPrototype} to create
	 * @param checkForDuplicates {@code true} to check if the statement already exists. Otherwise {@code false}
	 * @return the {@link UUID} of the created {@code statement}.
	 */
	Optional<EntityId> createStatement(StatementPojoPrototype statement, boolean checkForDuplicates);

	/**
	 * Creates a new {@code statement} entity for each item in {@code code statements}.
	 *
	 * @param statements the {@link StatementPojoPrototype StatementPojoPrototypes} to create
	 */
	void createStatements(Collection<StatementPojoPrototype> statements);
	
	/**
	 * Deletes all {@code statement} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain StatementInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted {@code statement} entities
	 */
	int deleteStatement(BuildingConsumer<StatementInquiryBuilder> builder);
	
	/**
	 * Returns all {@code statement} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain StatementInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain StatementPojo StatementPojos}
	 */
	List<StatementPojo> findStatements(BuildingConsumer<StatementInquiryBuilder> builder);
	
	/**
	 * Returns all {@code statement} entities that match with the filters in the given {@code builder}.
	 * @param paging Pagination specification.
	 *
	 * @param builder the {@linkplain StatementInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain StatementPojo StatementPojos}
	 */
	Paged<StatementPojo> findStatements(Pagination paging, BuildingConsumer<StatementInquiryBuilder> builder);

	/**
	 * Returns the aggregated {@code statement} entities that match with the filters in the given {@code request}.
	 *
	 * @param project the {@code project} for which to get the aggregations
	 * @param aggregationRequest the {@linkplain AggregationRequest} containing the aggregation operations and filter criteria
	 * @return List of {@linkplain AggregationResult}
	 */
	/* project is required for caching */
	List<AggregationResult<StatementFieldName>> getStatementAggregations(EntityId project, AggregationRequest<StatementFieldName> aggregationRequest);

	/**
	 * Returns the number of {@code statement} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain StatementInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code statement} entities
	 */
	long countStatements(BuildingConsumer<StatementInquiryBuilder> builder);

	/**
	 * Returns a container with a list of values of all {@code module} and {@code statement} entities that match with the filters in the given {@code builder}
	 * for the discovery exporter.
	 *
	 * @param project the id of the project for which the data export is done
	 * @param isSql {@code true} if only SQL statements must be exported. {@code false} if all non SQL statements must be exported
	 * @param sorted {@code true} if the values must be sorted. Otherwise {@code false}
	 * @return container with a list of values
	 */
	Optional<Table> getModuleStatementExport(EntityId project, boolean isSql, boolean sorted);

	/**
	 * Creates a new {@code module_dead_code} entity.
	 *
	 * @param deadCode the {@link ModuleDeadCodePojoPrototype} to create
	 */
	void createDeadCode(ModuleDeadCodePojoPrototype deadCode);

	/**
	 * Creates a new {@code module_dead_code} entity for each item in {@code code errorMarkers}.
	 *
	 * @param deadCodes the {@link ModuleDeadCodePojoPrototype ModuleDeadCodePojoPrototypes} to create
	 */
	void createDeadCodes(Collection<ModuleDeadCodePojoPrototype> deadCodes);

	/**
	 * Returns all {@code module_dead_code} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain DeadCodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModuleDeadCodePojo ModuleDeadCodePojos}
	 */
	List<ModuleDeadCodePojo> findDeadCode(BuildingConsumer<DeadCodeInquiryBuilder> builder);

	/**
	 * Returns the number of {@code module_dead_code} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain DeadCodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code statement} entities
	 */
	long countDeadCode(BuildingConsumer<DeadCodeInquiryBuilder> builder);

	/**
	 * Queries for all {@code module} entities that match with the given list of {@code moduleUids} and puts for each match the: 
	 * <ul>
	 * <li>uids of contained modules and modules that have a relationship to a matching module as {@code relations}</li>
	 * </ul>
	 * in the returned {@link Table}. If {@code collectContaining} is {@code true} then additionally the:
	 * <ul>
	 * <li>module path as {@code path}</li>
	 * <li>containing module uid as {@code containing}</li>
	 * </ul>
	 * are put present in the returned {@link Table}.
	 *
	 * @param moduleUids {@link UUID UUIDs} of the modules to fetch
	 * @param collectContaining {@code true} if path and uid of the containing module must be fetched as well. Otherwise {@code false}
	 * @return {@link Table} containing the values for each module match
	 */
	Optional<Table> findContainingAndRelatedModules(Collection<UUID> moduleUids, boolean collectContaining);

	/**
	 * Returns a container with a list of values of all {@code module} and {@code module_dead_code} entities of the given {@code project} for the discovery exporter.
	 *
	 * @param project the id of the project for which the data export is done
	 * @param sorted {@code true} if the values must be sorted. Otherwise {@code false}
	 * @return container with a list of values
	 */
	Optional<Table> getModuleDeadCode(EntityId project, boolean sorted);

	/**
	 * Creates a new {@code error_marker} entity.
	 *
	 * @param errorMarker the {@link ErrorMarkerPojoPrototype} to create
	 */
	void createErrorMarker(ErrorMarkerPojoPrototype errorMarker);

	/**
	 * Creates a new {@code error_marker} entity for each item in {@code code errorMarkers}.
	 *
	 * @param errorMarkers the {@link ErrorMarkerPojoPrototype ErrorMarkerPojoPrototypes} to create
	 */
	void createErrorMarkers(Collection<ErrorMarkerPojoPrototype> errorMarkers);

	/**
	 * Returns all {@code error_marker} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ErrorMarkerInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ErrorMarkerPojo ErrorMarkerPojos}
	 */
	List<ErrorMarkerPojo> findErrorMarkers(BuildingConsumer<ErrorMarkerInquiryBuilder> builder);

	/**
	 * Returns paged subset of optionally filtered {@code error_marker} entities that match with the filters in the given {@code builder}.
	 *
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain ErrorMarkerInquiryBuilder} containing the filter criteria and sort options.
	 * @return Paged subset of matching {@code error_marker} entities.
	 */
	Paged<ErrorMarkerPojo> findErrorMarkers(Pagination paging, BuildingConsumer<ErrorMarkerInquiryBuilder> builder);

	/**
	 * Returns the number of {@code error_marker} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ErrorMarkerInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code error_marker} entities
	 */
	long countErrorMarkers(BuildingConsumer<ErrorMarkerInquiryBuilder> builder);

	/**
	 * Returns a map of module id with the number of {@code error_marker} entities for that module matching with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ErrorMarkerInquiryBuilder} containing the filter criteria and sort options.
	 * @return Map of module id with corresponding number of matching {@code error_marker} entities
	 */
	Map<UUID, Long> countErrorMarkersByModule(BuildingConsumer<ErrorMarkerInquiryBuilder> builder);

	/**
	 * Deletes all {@code ErrorMarker} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ErrorMarkerInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted {@code ErrorMarker} entities
	 */
	int deleteErrorMarkers(BuildingConsumer<ErrorMarkerInquiryBuilder> builder);
	
	/**
	 * Returns a container with a list of values of all {@code module} and {@code error_marker} entities of the given {@code project} for the discovery exporter.
	 *
	 * @param project the id of the project for which the data export is done
	 * @param sorted {@code true} if the values must be sorted. Otherwise {@code false}
	 * @return container with a list of values
	 */
	Optional<Table> getModuleErrorMarkerExport(EntityId project, boolean sorted);

	/**
	 * Creates a new {@code dependency_definition} entity for each item in {@code code errorMarkers}.
	 *
	 * @param dependencyDefinitions the {@link DependencyDefinitionPojoPrototype DependencyDefinitionPojoPrototypes} to create
	 * @return the {@link UUID UUIDs} of the created dependency definitions
	 */
	List<UUID> createDependencyDefinitions(Collection<DependencyDefinitionPojoPrototype> dependencyDefinitions);

	/**
	 * Sets the {@code resolved} property of the {@code dependency_definition} for the given {@code id}.
	 *
	 * @param id the id of the to be updated {@code dependency_definition}
	 * @param resolved the to be set resolved value
	 */
	void setDependencyDefinitionResolved(UUID id, boolean resolved);

	/**
	 * Deletes all {@code dependency_definition} entities that match with the filters in the given {@code builder}.
	 *
	 * @param dependencyDefinitionIds the {@linkplain UUID UUIDs} of the {@code dependency_definition} entities to delete
	 * @return number of deleted entities
	 */
	int deleteDependencyDefinitions(List<UUID> dependencyDefinitionIds);
	
	/**
	 * Updates the properties of {@code module_relationship} entity.
	 * 
	 * @param properties the properties of the module relationship
	 * @param dependencyAttributes the dependency attributes of the module relationship
	 * @param id the id of the to be updated {@code module_relationship}
	 * @return number of updated entities
	 */
	int updateRelationshipProperties(Map<String, Object> properties, String dependencyAttributes, UUID id);

	/**
	 * Returns a list of module uids that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain DependencyDefinitionInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted entities
	 */
	List<EntityId> findDependencyDefinitionModuleIds(BuildingConsumer<DependencyDefinitionInquiryBuilder> builder);

	/**
	 * Returns all {@code dependency_definition} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain DependencyDefinitionInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain DependencyDefinitionPojo DependencyDefinitionPojos}
	 */
	List<DependencyDefinitionPojo> findDependencyDefinitions(BuildingConsumer<DependencyDefinitionInquiryBuilder> builder);

	/**
	 * Returns the number of {@code dependency_definition} entities that match with the filters in the given {@code builder}.
	 * @param builder the {@linkplain DependencyDefinitionInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code dependency_definition} entities
	 */
	long countDependencyDefinitions(BuildingConsumer<DependencyDefinitionInquiryBuilder> builder);

	/**
	 * Returns the numeric id (nid) for the given module {@code EntityId}. If {@code moduleId} contains no nid, then the nid is loaded from the database.
	 * 
	 * @param moduleId the module {@link EntityId}
	 * @return the numeric id
	 */
	Long getModuleNid(EntityId moduleId);

	/**
	 * Returns the unique id (uid) for the given module {@code EntityId}. If {@code moduleId} contains no uid, then the uid is loaded from the database.
	 * 
	 * @param moduleId the module {@link EntityId}
	 * @return the unique id
	 */
	UUID getModuleUid(EntityId moduleId);

	/**
	 * Returns the EntityId containing both nid and uid.
	 * If none is missing the given EntityId is returned,
	 * else the missing nid/uid will be resolved from the db.
	 *
	 * @param moduleId the module {@link EntityId}
	 * @return the complete {@link EntityId}
	 */
	EntityId getModuleEntityId(EntityId moduleId);

	/**
	 * Returns the 'hot spots' containing the modules and their reference count for the given {@code filterType} and {@code project}.
	 *
	 * @param project the project the modules belong to
	 * @param filterType the filter type 
	 * @param limit the number of records to be fetched, default being 10
	 * @return list of module hot spots
	 */
	List<HotSpot> findHotSpots(EntityId project, FilterType filterType, @Nullable Integer limit);

	/**
	 * Returns the 'hot spots' containing the modules and their reference count for the given {@code filterType} and module filter in {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria
	 * @param filterType the filter type 
	 * @return list of module hot spots
	 */
	List<HotSpot> findHotSpots(BuildingConsumer<ModuleInquiryBuilder> builder, FilterType filterType);

	/**
	 * Returns {@code DependencyGraphLinks} that contains list of dependent nodes and list of references
	 * for a particular module.
	 *
	 * @param projectId the project id of the module
	 * @param moduleId the module id of the module
	 * @param maxDepth the depth of traversal to be done
	 * @param maxGraphNodes determines the number of module nodes to return (at most) in the result
	 * @param moduleNodeTypeFilter the module type and technology that can be filtered
	 * @param relationshipTypeFilter the relationship types that can be filtered
	 * @param distinct {@code true} to filter duplicate dependencies. Otherwise {@code false}
	 * @param explorable whether {@link FeatureId#DEPENDENCY_GRAPH_EXPLORE} is enabled
	 * @return object of {@link DependencyGraph}
	 */
	DependencyGraph traverseDependencies(EntityId projectId, EntityId moduleId, Long maxDepth, Optional<Integer> maxGraphNodes,
			List<NodeType> moduleNodeTypeFilter, List<RelationshipType> relationshipTypeFilter, boolean distinct, boolean explorable);

	/**
	 * Returns {@linkplain SchemaInfoPojo SchemaInfoPojos} for each {@code module} entity of type {@code 'SCHEMA'} of the given {@code project}.
	 *
	 * @param project the ID of the project
	 * @return list of {@linkplain SchemaInfoPojo SchemaInfoPojos}
	 */
	List<SchemaInfoPojo> findSchemaInfos(EntityId project);

	/**
	 * Returns the module metrics statistics for the given {@code project}.
	 *
	 * @param project the id of the project
	 * @return a {@linkplain ModuleStatisticsResponse module}
	 */
	ModuleStatisticsResponse calculateStatistics(EntityId project);
	
	/**
	 * Returns the Project a Module belongs to.
	 * @param moduleId ID of the Module
	 * @return ID of the Project.
	 */
	EntityId getProject(EntityId moduleId);

	/**
	 * Creates a new {@code module_undiscovered} entity.
	 *
	 * @param moduleUndiscovered the {@link ModuleUndiscoveredPojoPrototype} to create
	 */
	void createUndiscovered(ModuleUndiscoveredPojoPrototype moduleUndiscovered);

	/**
	 * Returns all {@code module_undiscovered} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleUndiscoveredInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModuleUndiscoveredPojo ModuleUndiscoveredPojos}
	 */
	List<ModuleUndiscoveredPojo> findUndiscovered(BuildingConsumer<ModuleUndiscoveredInquiryBuilder> builder);

	/**
	 * Returns paged subset of optionally filtered {@code module_undiscovered} entities that match with the filters in the given {@code builder}.
	 * 
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain ModuleUndiscoveredInquiryBuilder} containing the filter criteria and sort options.
	 * @return Paged subset of matching  {@linkplain ModuleUndiscoveredPojo ModuleUndiscoveredPojos}
	 */
	Paged<ModuleUndiscoveredPojo> findUndiscovered(Pagination paging, BuildingConsumer<ModuleUndiscoveredInquiryBuilder> builder);

	/**
	 * Returns the number of {@code module_undiscovered} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleUndiscoveredInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code module_undiscovered} entities
	 */
	long countUndiscovered(BuildingConsumer<ModuleUndiscoveredInquiryBuilder> builder);

	/**
	 * Deletes all {@code module_undiscovered} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleUndiscoveredInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted modules
	 */
	int deleteUndiscovered(BuildingConsumer<ModuleUndiscoveredInquiryBuilder> builder);
	
	/**
	 * Updates the from_dead_code field for all {@code module_relationship} entities that match with the filters in 
	 * the provided {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options.
	 * @param fromDeadCode the value to set from dead code to.
	 * @return number of updated entities
	 */
	int setFromDeadCode(final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder, final boolean fromDeadCode);
	
}
