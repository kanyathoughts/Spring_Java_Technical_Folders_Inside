/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.access;

import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFieldName;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockStatus;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojo;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import org.apache.commons.lang3.tuple.Pair;

import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

/**
 * Functions for accessing the FunctionalBlock database entity.
 */
public interface FunctionalBlockService {

	/**
	 * Sort builder for sorting {@link FunctionalBlockPojo}.
	 */
	interface FunctionalBlockOrderBuilder {
		/**
		 * Sort functional blocks by child order within the given parent block. Functional blocks that are not children of the given parent block
		 * will be returned in any order.
		 * @param parent uid of the parent block
		 * @return this builder
		 */
		FunctionalBlockOrderBuilder sortChildOrdinal(final UUID parent);
		/**
		 * Sort functional blocks by name.
		 * @param direction the sort direction
		 * @return this builder
		 */
		FunctionalBlockOrderBuilder sortName(final SortDirection direction);

		/**
		 * Sorts the result by the presence of flags name.
		 * @param flag the flag to check
		 * @param value the expected value that the flag should have in order for the block to match
		 * @param direction the sort direction
		 * @return this builder
		 */
		FunctionalBlockOrderBuilder sortByFlagContainsValue(final FunctionalBlockFlag flag, final Object value, final SortDirection direction);
		
		/**
		 * Sort functional blocks by updated.
		 * @param direction the sort direction
		 * @return this builder
		 */
		FunctionalBlockOrderBuilder sortByLastModified(final SortDirection direction);
	}

	/**
	 * Query builder for performing queries on {@link FunctionalBlockPojo}.
	 */
	interface FunctionalBlockInquiryBuilder extends FunctionalBlockOrderBuilder {
		/**
		 * Finds functional block by single uid. Result will contain at most 1 functional block.
		 * @param uid the uid of the functional block to find
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder byUid(final UUID uid);

		/**
		 * Finds functional blocks by list of uids.
		 * @param uids the uids of the functional blocks to find
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder byUids(final Collection<UUID> uids);

		/**
		 * Filters out functional block by uid.
		 * @param uid the uid of the functional block to find 
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder notByUid(final UUID uid);

		/**
		 * Finds functional blocks having a certain flag set to a certain String value.
		 * @param flag the flag to check
		 * @param value the expected value that the flag should have in order for the block to match
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withFlag(final FunctionalBlockFlag flag, final String value);

		/**
		 * Finds functional blocks having a certain flag set to a certain boolean value.
		 * @param flag the flag to check
		 * @param value the expected value that the flag should have in order for the block to match
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withFlag(final FunctionalBlockFlag flag, final boolean value);

		/**
		 * Filters out functional blocks having a certain flag set to a certain boolean value.
		 * @param flag the flag to check
		 * @param value the expected value that the flag should have in order for the block to match
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder notWithFlag(final FunctionalBlockFlag flag, final boolean value);

		/**
		 * Finds functional blocks by project.
		 * @param projectId id of the project
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder ofProject(final EntityId projectId);

		/**
		 * Finds functional blocks by type.
		 * @param type type of the functional blocks (see {@link FunctionalBlockFlag#TYPE})
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withType(final FunctionalBlockType type);
		
		/**
		 * Finds functional blocks generated from annotation with specified type.
		 * @param type the type of the annotation
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withAnnotationType(final AnnotationType type);

		/**
		 * Finds functional blocks not by type.
		 * @param type type of the functional blocks (see {@link FunctionalBlockFlag#TYPE})
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder notWithType(final FunctionalBlockType type);

		/**
		 * Filter functional blocks having any of given type.
		 * @param types types of the functional blocks
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withTypes(final Collection<FunctionalBlockType> types);

		/**
		 * Finds functional blocks by LowerBound AccessType.
		 * @param lbAccessType accessType of the functional blocks (see {@link FunctionalBlockFlag#RA_ACCESS_TYPE})
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withLowerBoundAccessType(final String lbAccessType);

		/**
		 * Filter functional blocks having any of given LowerBound AccessType.
		 * @param lbAccessTypes accessTypes of the functional blocks
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withLowerBoundAccessTypes(final Collection<String> lbAccessTypes);

		/**
		 * Finds functional blocks by status.
		 * @param status status of the functional blocks (see {@link FunctionalBlockFlag#STATUS})
		 * @see FunctionalBlockStatus
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withStatus(final FunctionalBlockStatus status);

		/**
		 * Filter out functional blocks by status.
		 * @param status status of the functional blocks (see {@link FunctionalBlockFlag#STATUS})
		 * @see FunctionalBlockStatus
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder notWithStatus(final FunctionalBlockStatus status);

		/**
		 * Finds functional blocks that reference any part of the given module.
		 * @param moduleId the id of the module referenced by the functional blocks
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withResolvedModulePart(EntityId moduleId);

		/**
		 * Finds functional blocks that reference any part of the List of module.
		 * @param moduleIds List of id of the modules referenced by the functional blocks
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withResolvedModuleParts(Collection<EntityId> moduleIds);

		/**
		 * Finds functional blocks that reference any part of the given module.
		 * @param moduleName the name of the module referenced by the functional blocks
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withResolvedModulePartModuleName(String moduleName);

		/**
		 * Finds functional blocks that reference any part of the given module.
		 * @param moduleNames the names of the modules referenced by the functional blocks
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withResolvedModulePartModuleNames(Collection<String> moduleNames);

		/**
		 * Finds functional blocks that reference any part of the given module having Technology.
		 * @param moduleTechnology the technology of the module referenced by the functional blocks
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withResolvedModulePartTechnology(Technology moduleTechnology);

		/**
		 * Finds functional blocks that reference any part of the given module having Technology.
		 * @param moduleTechnologies the technologies of the module referenced by the functional blocks
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withResolvedModulePartTechnologies(Collection<Technology> moduleTechnologies);

		/**
		 * Finds functional blocks that reference any part of the given module having Type.
		 * @param moduleType the type of the module referenced by the functional blocks
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withResolvedModulePartType(Type moduleType);

		/**
		 * Finds functional blocks that reference any part of the given module having Type.
		 * @param moduleTypes the types of the module referenced by the functional blocks
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withResolvedModulePartTypes(Collection<Type> moduleTypes);

		/**
		 * Finds functional blocks that reference any part of the given module having Taxonomy.
		 * @param taxonomyId the id of the taxonomy referenced by the Module
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withResolvedModulePartHavingTaxonomy(EntityId taxonomyId);

		/**
		 * Finds functional blocks that reference any part of the given module having Taxonomy.
		 * @param taxonomyIds List of id of the taxonomy referenced by the Modules
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withResolvedModulePartHavingTaxonomies(Collection<EntityId> taxonomyIds);

		/**
		 * Finds functional blocks that reference the source code of the given module at the given offset
		 * (will also find blocks that reference the entire module).
		 * @param moduleId the id of the module referenced by the functional blocks
		 * @param offset the offset in the source code of the module that is referenced
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withResolvedModulePartAtOffset(EntityId moduleId, int offset);

		/**
		 * Includes only functional blocks in the result that have at least one matching parent.
		 * @param parentFilter filter for matching the parents
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withParent(BuildingConsumer<FunctionalBlockInquiryBuilder> parentFilter);
		/**
		 * Includes only functional blocks in the result that do not have any matching parent.
		 * @param parentFilter filter for matching the parents
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder notWithParent(BuildingConsumer<FunctionalBlockInquiryBuilder> parentFilter);

		/**
		 * Includes only blocks in the result that are a child of at least one matching block.
		 * @param childrenFilter filter for matching the children
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withChild(BuildingConsumer<FunctionalBlockInquiryBuilder> childrenFilter);

		/**
		 * Includes only those blocks in the result that have a match within the first two levels of children.
		 * @param childrenFilter filter for matching the children
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withChildForMergedAndSingleReachabilityBlocks(BuildingConsumer<FunctionalBlockInquiryBuilder> childrenFilter);

		/**
		 * Includes only blocks in the result that are a peer of at least one matching block. Peer blocks are blocks that have at least one
		 * resolved module part in common (meaning they reference the same or an overlapping location in a module) but neither block is parent or child of the
		 * other block.
		 * @param peerFilter filter for matching the peers
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withPeer(BuildingConsumer<FunctionalBlockInquiryBuilder> peerFilter);

		/**
		 * Includes only blocks in the result that do not have any matching peer block. Peer blocks are blocks that have at least one
		 * resolved module part in common (meaning they reference the same or an overlapping location in a module) but neither block is parent or child of the
		 * other block.
		 * @param peerFilter filter for matching the peers
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder notWithPeer(BuildingConsumer<FunctionalBlockInquiryBuilder> peerFilter);

		/**
		 * Finds functional blocks that were generated from an Annotation.
		 * @param annotationId the id of the annotation from which the functional block was generated
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder generatedFromAnnotation(EntityId annotationId);

		/**
		 * Finds functional blocks that were generated from Annotations.
		 * @param annotationIds the ids of the annotations from which the functional blocks were generated
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder generatedFromAnnotations(List<EntityId> annotationIds);

		/**
		 * Finds functional blocks that were generated from Module.
		 * @param linkHash the linkHash of the module from which the functional block was generated
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder generatedFromModule(String linkHash);

		/**
		 * Finds functional blocks that were generated from Modules.
		 * @param linkHashes the linkHashes of the modules from which the functional blocks were generated
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder generatedFromModules(Collection<String> linkHashes);

		/**
		 * Finds functional blocks that were generated from Modules.
		 * @param name the name of the functional block
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withName(String name);

		/**
		 * Finds functional blocks that were generated from Modules.
		 * @param names the names of the functional blocks
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withNames(Collection<String> names);

		/**
		 * Finds functional blocks that were generated from a Module where the Module's content
		 * has changed some time after the given Instant.
		 * @param modifiedAfter the instant after which the Module's content has changed
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withContentChangedAfter(Instant modifiedAfter);

		/**
		 * Finds functional blocks that were generated from a Module where the Module went missing
		 * some time after the given Instant.
		 * @param missingAfter the instant after which the Module went missing
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withMissingSinceAfter(Instant missingAfter);

		/**
		 * Finds functional blocks that were generated from a Module where the Module's dependencies have changed
		 * some time after the given Instant.
		 * @param dependencyChangedAfter the instant after which the Module's dependencies have changed
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withDependencyChangedAfter(Instant dependencyChangedAfter);

		/**
		 * Finds functional blocks that were generated from a Module where the Module's content or dependencies have changed
		 * some time after the given Instant.
		 * @param modifiedAfter the instant after which the Module's content has changed
		 * @param dependencyChangedAfter the instant after which the Module's dependencies have changed
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withContentChangedOrDependencyChangedAfter(Instant modifiedAfter, Instant dependencyChangedAfter);

		/**
		 * Finds the functional blocks that refers to the given data dictionaries.
		 *
		 * @param dataDictionaryIds the ids of
		 * @return this builder
		 */
		FunctionalBlockInquiryBuilder withReferenceToDataDictionaries(List<UUID> dataDictionaryIds);

		FunctionalBlockInquiryBuilder withReferenceToDataDictionaryNames(List<String> dataDictionaryNames);
	}

	/**
	 * Inquiry builder for performing queries on {@link FunctionalBlockLink}.
	 */
	interface FunctionalBlockLinkInquiryBuilder {

		/**
		 * Filters functional block links by their {@link UUID}.
		 *
		 * @param uid the uid of the functional block link
		 * @return this instance for method chaining
		 */
		FunctionalBlockLinkInquiryBuilder byUid(UUID uid);

		/**
		 * Filters functional block links by their flag value.
		 *
		 * @param flag the flag to check
		 * @param value the expected value that the flag should have in order for the block to match
		 * @return this instance for method chaining
		 */
		FunctionalBlockLinkInquiryBuilder withFlag(FunctionalBlockLinkFlag flag, String value);

		/**
		 * Filters functional block links by their parent.
		 *
		 * @param parent the parent of the functional block link
		 * @return this instance for method chaining
		 */
		FunctionalBlockLinkInquiryBuilder ofParent(UUID parent);

		/**
		 * Filters functional block links by their creator.
		 *
		 * @param generatedBy the generated by classes
		 * @return this instance for method chaining
		 */
		FunctionalBlockLinkInquiryBuilder withGeneratedBy(Collection<String> generatedBy);

		/**
		 * Filters functional block links by their shared resource technology and type.
		 *
		 * @param technologyTypes the technology and type of the shared resources
		 * @return this instance for method chaining
		 */
		FunctionalBlockLinkInquiryBuilder withSharedResourceTechnologyTypes(Collection<Pair<Technology, Type>> technologyTypes);

		/**
		 * Filters functional block links by excluding ChildA UUIDs.
		 *
		 * @param childAs the Collection of childAs of the functional block link
		 * @return this instance for method chaining
		 */
		FunctionalBlockLinkInquiryBuilder notWithChildAs(Collection<UUID> childAs);

		/**
		 * Filters functional block links by excluding ChildB UUIDs.
		 *
		 * @param childBs the Collection of childBs of the functional block link
		 * @return this instance for method chaining
		 */
		FunctionalBlockLinkInquiryBuilder notWithChildBs(Collection<UUID> childBs);
	}

	interface FunctionalBlockAggregationInquiryBuilder <B extends FunctionalBlockAggregationInquiryBuilder<B>> extends AggregationInquiryBuilder<FunctionalBlockFieldName, B> {

		/**
		 * Filters functional blocks by their {@link UUID}.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B byUid(String operator, Object value);

		/**
		 * Filters functional blocks by their project.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B ofProject(String operator, Object value);

		/**
		 * Filters functional blocks by their type.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withType(String operator, Object value);

		/**
		 * Filters functional blocks by the type of the referenced module.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withModuleType(String operator, Object value);

		/**
		 * Filters functional blocks by the technology of the referenced module.
		 *
		 * @param operator the operator to use for comparison
		 * @param value the value to compare to
		 * @return this instance for method chaining
		 */
		B withModuleTechnology(String operator, Object value);

		/**
		 * Includes only blocks in the result that are a peer of at least one matching block. Peer blocks are blocks that have at least one
		 * resolved module part in common (meaning they reference the same or an overlapping location in a module) but neither block is parent or child of the
		 * other block.
		 * @param peerFilter filter for matching the peers
		 * @return this builder
		 */
		B withPeer(BuildingConsumer<FunctionalBlockInquiryBuilder> peerFilter);

		/**
		 * Finds functional blocks that reference any part of the given module.
		 * @param moduleId the id of the module referenced by the functional blocks
		 * @return this builder
		 */
		B withResolvedModulePart(EntityId moduleId);
	}

	interface ReachabilityDataOrderBuilder {

		/**
		 * Sorts the result by the lower bound module name.
		 * @param direction the sort direction
		 * @return this builder
		 */
		ReachabilityDataOrderBuilder sortByLowerBoundModuleName(final SortDirection direction);

		/**
		 * Sorts the result by the upper bound module name.
		 * @param direction the sort direction
		 * @return this builder
		 */
		ReachabilityDataOrderBuilder sortByUpperBoundModuleName(final SortDirection direction);

		/**
		 * Filter ReachabilityData by Upper Bound Module Taxonomy
		 * @param taxonomyId the id of the taxonomy
		 * @return this builder
		 */
		ReachabilityDataOrderBuilder ofUpperBoundTaxonomy(final EntityId taxonomyId);

		/**
		 * Filter ReachabilityData by Upper Bound Module Taxonomies
		 * @param taxonomyIds the ids of the taxonomies
		 * @return this builder
		 */
		ReachabilityDataOrderBuilder ofUpperBoundTaxonomies(final Collection<EntityId> taxonomyIds);

		/**
		 * Filter ReachabilityData by Lower Bound Module Taxonomy
		 * @param taxonomyId the id of the taxonomy
		 * @return this builder
		 */
		ReachabilityDataOrderBuilder ofLowerBoundTaxonomy(final EntityId taxonomyId);

		/**
		 * Filter ReachabilityData by Lower Bound Module Taxonomies
		 * @param taxonomyIds the ids of the taxonomies
		 * @return this builder
		 */
		ReachabilityDataOrderBuilder ofLowerBoundTaxonomies(final Collection<EntityId> taxonomyIds);

		/**
		 * Filter ReachabilityData by Path Taxonomy
		 * @param taxonomyId the id of the taxonomy
		 * @return this builder
		 */
		ReachabilityDataOrderBuilder ofPathTaxonomy(final EntityId taxonomyId);

		/**
		 * Filter ReachabilityData by Path Taxonomies
		 * @param taxonomyIds the ids of the taxonomies
		 * @return this builder
		 */
		ReachabilityDataOrderBuilder ofPathTaxonomies(final Collection<EntityId> taxonomyIds);

	}

	interface ReachabilityDataInquiryBuilder extends ReachabilityDataOrderBuilder {

		/**
		 * Finds {@link ReachabilityDataPojo} attached to the given functional block.
		 * @param uid id of the functional block ({@linkplain FunctionalBlockType#REACHABILITY reachability} block)
		 * @return this builder
		 */
		ReachabilityDataInquiryBuilder ofFunctionalBlock(final UUID uid);

		/**
		 * Finds {@link ReachabilityDataPojo} attached to the given functional blocks.
		 * @param uids ids of the functional blocks ({@linkplain FunctionalBlockType#REACHABILITY reachability} blocks)
		 * @return this builder
		 */
		ReachabilityDataInquiryBuilder ofFunctionalBlocks(final List<UUID> uids);

		/**
		 * Finds {@link ReachabilityDataPojo} with intermediate modules.
		 * @param aggregateLowerBounds <p>if {@code true}, the result will contain reachability data with access and intermediate modules aggregated per lower
		 * bound,<br>if {@code false}, the result will contain reachability data with intermediate modules aggregated per access module and lower bound pair</p>
		 * @return this builder
		 */
		ReachabilityDataInquiryBuilder aggregateAccessModulesPerLowerBound(final boolean aggregateLowerBounds);
	}

	/**
	 * Finds a functional block by its id.
	 * @param uid id of the functional block
	 * @return the functional block, if it exists
	 */
	Optional<FunctionalBlockPojo> find(UUID uid);

	/**
	 * Finds all functional blocks that match given filter criteria.
	 * @param builder a consumer function for configuring the filter criteria
	 * @return the list of matching functional blocks
	 */
	List<FunctionalBlockPojo> find(BuildingConsumer<FunctionalBlockInquiryBuilder> builder);

	/**
	 * Finds a paged subset of blocks that match given filter criteria.
	 * @param paging the pagination attributes
	 * @param builder a consumer function for configuring the filter criteria
	 * @return page of matching functional blocks
	 */
	Paged<FunctionalBlockPojo> find(Pagination paging, BuildingConsumer<FunctionalBlockInquiryBuilder> builder);
	
	/**
	 * Returns list functional block uids matching the given inquiry builder.
	 * @param builder the inquiry builder for filtering functional blocks
	 * @return the list of matching functional blocks uid
	 */
	List<UUID> findUids(BuildingConsumer<FunctionalBlockInquiryBuilder> builder);

	/**
	 * Find all reachabilityData blocks that match the given filter criteria.
	 * @param builder a consumer function for configuring the filter criteria
	 * @return List of matching {@link ReachabilityDataPojo}
	 */
	List<ReachabilityDataPojo> findReachabilityData(BuildingConsumer<ReachabilityDataInquiryBuilder> builder);

	/**
	 * Find a page subset of reachabilityData blocks that match the given filter criteria.
	 * @param paging the pagination attributes
	 * @param builder a consumer function for configuring the filter criteria
	 * @return page of matching {@link ReachabilityDataPojo}
	 */
	Paged<ReachabilityDataPojo> findReachabilityData(Pagination paging, BuildingConsumer<ReachabilityDataInquiryBuilder> builder);

	/**
	 * Finds all children of a functional block recursively, applying filters and sorting from the given inquiry builder
	 * @param maxDepth maximum recursion depth - set to negative value for unlimited
	 * @param parent the uid of the parent block of which to query the children
	 * @param builder an inquiry builder used to filter or sort the children
	 * @return list of (direct and transitive) children of the block
	 */
	List<FunctionalBlockPojo> findChildrenDeep(final UUID parent, int maxDepth, BuildingConsumer<FunctionalBlockService.FunctionalBlockInquiryBuilder> builder);

	/**
	 * Finds all children of a list of functional blocks recursively, applying filters and sorting from the given inquiry builder. Returns a map from each
	 * parent UUID to the list of recursive children.
	 * @param parents the uids of the parent blocks of which to query the children
	 * @param maxDepth maximum recursion depth - set to negative value for unlimited
	 * @param builder an inquiry builder used to filter or sort the children
	 * @return map of (direct and transitive) children of each parent block
	 */
	Map<UUID, List<FunctionalBlockPojo>> findChildrenDeep(final List<UUID> parents, int maxDepth, BuildingConsumer<FunctionalBlockService.FunctionalBlockInquiryBuilder> builder);

	/**
	 * Finds a paged subset of all children of a functional block recursively, applying filters and sorting from the given inquiry builder
	 * @param maxDepth maximum recursion depth - set to negative value for unlimited
	 * @param parent the uid of the parent block of which to query the children
	 * @param paging the pagination attributes
	 * @param builder an inquiry builder used to filter or sort the children
	 * @return page of (direct and transitive) children of the block
	 */
	Paged<FunctionalBlockPojo> findChildrenDeep(final UUID parent, final Pagination paging, int maxDepth,
												final BuildingConsumer<FunctionalBlockService.FunctionalBlockInquiryBuilder> builder);

	/**
	 * Finds all children of a list of functional blocks recursively, up to the specified max depth. This method has much better performance
	 * compared to {@link #findChildrenDeep(List, int, BuildingConsumer)} but only returns the ids of the child block and does not support filtering.
	 *
	 * @param parents the uids of the parent blocks of which to query the children
	 * @param maxDepth maximum recursion depth - set to negative value for unlimited
	 * @return map of (direct and transitive) children of each parent block
	 */
	Map<UUID, List<UUID>> findChildrenIdsDeep(List<UUID> parents, int maxDepth);

	/**
	 * Resolves a list of functional blocks from their ids. All blocks must exist or else this method throws an exception.
	 * The returned list is of same length as the passed list of ids and does not contain {@code null} entries.
	 * <p>
	 * The primary purpose of this method is to resolve links between blocks, e.g. children or parents.
	 * @param uids the list of functional block ids to resolve
	 * @return list of resolved blocks
	 */
	List<FunctionalBlockPojo> get(Collection<UUID> uids);

	/**
	 * Returns the functional blocks aggregation results for the given request.
	 *
	 * @param request the aggregation request
	 * @return the aggregation results
	 */
	List<AggregationResult<FunctionalBlockFieldName>> getAggregations(AggregationRequest<FunctionalBlockFieldName> request);

	/**
	 * Returns the functional blocks aggregation results for the given {@link FunctionalBlockInquiryBuilder} and aggregation {@code request}.
	 *
	 * @param request the aggregation request
	 * @param builder a consumer function for configuring the filter criteria
	 * @return the aggregation results
	 */
	List<AggregationResult<FunctionalBlockFieldName>> getAggregations(BuildingConsumer<FunctionalBlockInquiryBuilder> builder,
																	  AggregationRequest<FunctionalBlockFieldName> request);

	/**
	 * Returns aggregation {@code functionalBlock} values for the filters and selected aggregations in the given {@code builder}.
	 *
	 * @param builder the {@linkplain FunctionalBlockAggregationInquiryBuilder} containing the aggregation operations and filter criteria
	 * @return container with the aggregation values
	 */
	Optional<Table> getAggregations(BuildingConsumer<FunctionalBlockAggregationInquiryBuilder<?>> builder);

	/**
	 * Creates a new functional block from the given prototype object.
	 * @param functionalBlock the prototype for the new block
	 * @return the newly created functional block uid
	 */
	UUID create(FunctionalBlockPojoPrototype functionalBlock);

	/**
	 * Updates and existing functional block with data from the given prototype object.
	 * @param functionalBlock prototype object containing the fields to update
	 */
	void update(FunctionalBlockPojoPrototype functionalBlock);

	/**
	 * Updates the status of the functional blocks with the given ids.
	 * @param uids the ids of the blocks to update
	 * @param status the new status
	 */
	void updateBlocksStatus(Collection<UUID> uids, FunctionalBlockStatus status);

	/**
	 * Deletes the functional block with the given id.
	 * @param uid the id of the block to delete
	 */
	void delete(UUID uid);

	/**
	 * Deletes the functional blocks with the given ids.
	 * @param uids the id of the block to delete
	 */
	void delete(Collection<UUID> uids);

	/**
	 * Deletes all functional blocks of the given project.
	 * @param projectId the id of the project
	 */
	void deleteAllOfProject(final EntityId projectId);

	/**
	 * Deletes the functional block with the given annotationId.
	 * @param annotationId the id of the annotation to delete the corresponding functionalBlock
	 */
	void deleteGeneratedFromAnnotation(Long annotationId);

	/**
	 * Deletes the functional block with the given annotationIds.
	 * @param annotationIds the annotation ids to delete the corresponding functionalBlock
	 */
	void deleteGeneratedFromAnnotations(Collection<EntityId> annotationIds);

	/**
	 * Returns a Map of Annotation ids and ids of functional blocks that were generated from the corresponding annotation.
	 * @param annotationIds the ids of the annotations
	 * @return a map containing an entry for each annotation id and the functional block generated from that annotation, if exists
	 */
	Map<Long, UUID> findGeneratedFromAnnotations(Collection<EntityId> annotationIds);

	/**
	 * Returns a Map of Module link hashes and ids of functional blocks that were generated from the corresponding modules.
	 * @param moduleLinkHashes the link hashes of the modules
	 * @param project the id of the project
	 * @return a map containing an entry for each module link hash and the functional block generated from that module, if exists
	 */
	Map<String, UUID> findGeneratedFromModules(Collection<String> moduleLinkHashes, EntityId project);

	/**
	 * Bulk-gets the module parts for a list of functional blocks. Returns a map from functional block id to its module parts.
	 * @param uids the ids of the functional blocks of which to get the module parts
	 * @return map from functional block id to module parts
	 */
	Map<UUID, List<ModulePart>> getModuleParts(final Collection<UUID> uids);

	/**
	 * Sets the resolved module parts for the given block. Replaces any existing resolved module parts for the block.
	 * @param uid the id of the functional block
	 * @param resolvedModuleParts the list of resolved module parts to set
	 */
	void setResolvedModuleParts(UUID uid, Collection<ResolvedModulePart> resolvedModuleParts);

	/**
	 * Gets the resolved module parts for the given block.
	 * @param uid the id of the functional block
	 * @return the list of resolved module parts
	 */
	List<ResolvedModulePart> getResolvedModuleParts(UUID uid);

	/**
	 * Gets the resolved module parts for a list of blocks.
	 * @param uids the ids of the functional blocks
	 * @return a map from functional block id to resolved module parts
	 */
	Map<UUID, List<ResolvedModulePart>> getResolvedModuleParts(final Collection<UUID> uids);

	/**
	 * Sets information on how the block was generated. Can be used by block generations to record data about the entities
	 * from which the functional block was derived. Calling this method replaces any existing "generated from" data stored for the block.
	 * @param uid the uid of the functional block
	 * @param generatedFrom the "generated from" data to attach to the block
	 */
	void setGeneratedFrom(UUID uid, GeneratedFrom generatedFrom);

	/**
	 * Gets information from which entities the block was generated, if applicable.
	 * @param uid the id of the functional block
	 * @return the stored "generated from" data, if present
	 */
	Optional<GeneratedFrom> getGeneratedFrom(final UUID uid);

	/**
	 * Gets information from which entities the blocks were generated, if applicable.
	 * @param uids the ids of the functional blocks
	 * @return a map from functional block id to the stored "generated from" data, map will contain {@code null} value if no information present for block
	 */
	Map<UUID, GeneratedFrom> getGeneratedFrom(final Collection<UUID> uids);
	
	/**
	 * Attaches {@link ReachabilityDataPojo} to a functional block. Calling this method replaces existing "reachability data" for the block.
	 * @param uid the uid of the functional block (the reachability block)
	 * @param reachabilityData the "reachability data" to attach to the block
	 */
	void setReachabilityData(final UUID uid, final Collection<ReachabilityDataPojoPrototype> reachabilityData);

	/**
	 * get functional block links from parent uid
	 * @param blockUid the id of parent of the functional block
	 * @return a list of functional block link
	 */
	List<FunctionalBlockLink> getLinks(final UUID blockUid);

	/**
	 * get functional block links from parent uid
	 * @param builder the inquiry builder
	 * @return a list of functional block link
	 */
	List<FunctionalBlockLink> getLinks(BuildingConsumer<FunctionalBlockLinkInquiryBuilder> builder);

	/**
	 * get functional block links from parent uids
	 * @param uids of parents of functional blocks
	 * @return a map of parent uid and functional block
	 */
	Map<UUID, List<FunctionalBlockLink>> getLinks(final Collection<UUID> uids);

	/**
	 * create a link to a parent block. It deletes existing links to the parent block.
	 * @param blockUid parent uid
	 * @param links list of links should be created
	 */
	void setLinks(final UUID blockUid, final List<FunctionalBlockLink> links);

	/**
	 * delete links of a functional block
	 * @param blockUid
	 */
	void deleteLinks(final UUID blockUid);

	/**
	 * delete functional block children having with type FUNCTIONAL_CONDITION and FUNCTIONAL_STATEMENT.
	 * @param blockUid the id of the functional block
	 */
	void deleteConditionsAndStatementTypeChildren(UUID blockUid);

	/**
	 * Merges a functional block into another functional block. The block must have a common parent. Once merged, the block links will be added to the merged
	 * Parent while preserving details of the current links.
	 * @param commonParent the uid of the common parent block
	 * @param mergeParent the uid of the block to merge into the other block
	 * @param mergeChildren the collections of uid of the blocks to merge into the other block
	 */
	void merge(UUID commonParent, UUID mergeParent, Collection<UUID> mergeChildren);

	/**
	 * Unmerges a functional block from another functional block. The block must have a common parent. Once unmerged, the block links will be removed from the
	 * merged Parent and recreated for the unmerged block.
	 * @param commonParent the uid of the common parent block
	 * @param mergeParent the uid of the block to merge into the other block
	 * @param mergeChildren the collections of uid of the blocks to merge into the other block
	 */
	void unmerge(UUID commonParent, UUID mergeParent, Collection<UUID> mergeChildren);

	/**
	 * Persists a cached list of recursive children for the block. The list should be obtained from {@link #findChildrenDeep}
	 * with no filters.
	 * @param parent the uid of the parent block
	 * @param children the list of recursive children
	 */
	void setChildrenDeep(UUID parent, List<UUID> children);

	/**
	 * Returns cached list of recursive children of the given block. This returns the same as {@link #findChildrenDeep}
	 * with no additional filtering applied, but only if the functional block computation was executed on the block and is up to date.
	 * @param parent the uid of the parent block
	 * @return the cached list of recursive children
	 */
	List<UUID> getChildrenDeep(UUID parent);

	/**
	 * Returns set of functional block names
	 * @param annotationId the id of the annotation
	 * @return set of functional block names
	 */
	Set<String> findFunctionalBlockNamesByAnnotationId(Long annotationId);

	/**
	 * Updates the generated information based on the provided annotation ID and
	 * name.
	 * 
	 * @param annotationId      The ID of the annotation to update.
	 * @param updatedAnnotation The updated annotation containing the new
	 *                          description and name..
	 */
	void updateFunctionBlockDescriptionAndNameOnAnnotationUpdate(Long annotationId, AnnotationPojo updatedAnnotation);

	/**
	 * Finds all functional blocks that matches the given Search criteria.
	 * @param pagination the pagination attributes
	 * @param projectId id of the project
	 * @param moduleIds List of id of the modules referenced by the functional blocks
	 * @param taxonomyIds List of id of the taxonomy referenced by the Modules
	 * @param peers filter for matching the peers
	 * @param searchNames filter for matching the functional blocks by names
	 * @param ddIds filter for matching the functional blocks by reference DD
	 * @return the list of matching functional blocks
	 */
	Paged<FunctionalBlockPojo> findByName(final Pagination pagination, final Long projectId, final List<Long> moduleIds, final List<Long> taxonomyIds,
			final List<UUID> peers, final List<String> searchNames , final List<UUID> ddIds);

	/**
	 * Gets the referenced data dictionary Ids for the list of functional block.
	 *
	 * @param blockUid the uid of the functional block
	 * @return the list of ids of the referenced data dictionaries
	 */
	List<UUID> getReferencedDataDictionaries(UUID blockUid);

	/**
	 * Stores the referenced data dictionaries for the given functional block.
	 *
	 * @param blockUid the uid of the functional block
	 * @param dataDictionaryIds the list of ids of the referenced data dictionaries
	 */
	void setReferencedDataDictionaries(UUID blockUid, List<UUID> dataDictionaryIds);

	/**
	 * Gets the map of annotations and their associated functional units that have the provided annotation type and a single parent.
	 *
	 * @param projectId the project Id
	 * @param blockUid the uid of the root functional block
	 * @param  annotationType the type of the annotation
	* @return the map of annotationId and associated uids of the functional blocks
	 */
	public Map<EntityId, UUID> getSingleParentFunctionalUnitsByAnnotationType(final EntityId projectId, final UUID blockUid, final String annotationType);

	/**
	 * Finds the Peer for the functional block that matches the given filter criteria.
	 *
	 * @param blockFilter the filter criteria for the functional blocks.
	 * @param peerFilter the filter criteria for the potential peer blocks
	 * @return the list of matching functional blocks
	 */
	List<Pair<UUID, UUID>> findPeers(BuildingConsumer<FunctionalBlockInquiryBuilder> blockFilter, BuildingConsumer<FunctionalBlockInquiryBuilder> peerFilter);

	/**
	 * Finds the Peer for the functional block that matches the given filter criteria and {@code peerType}.
	 * <p>Performance optimized version of {@link #findPeers(BuildingConsumer, BuildingConsumer)} for finding peers of a specific type.</p>
	 *
	 * @param blockFilter the filter criteria for the functional blocks.
	 * @param peerType the type of the peer
	 * @return the list of matching functional blocks
	 */
	List<Pair<UUID, UUID>> findPeers(BuildingConsumer<FunctionalBlockInquiryBuilder> blockFilter, FunctionalBlockType peerType);

	/**
	 * Updates the {@link FunctionalBlockFlag#COMPUTED_AT} flag for a collection of blocks.
	 * @param blockUids the UUIDs of the functional blocks to update
	 * @param computedAt the new timestamp to set for {@link FunctionalBlockFlag#COMPUTED_AT}
	 */
	void setComputedAt(Collection<UUID> blockUids, Instant computedAt);
}
